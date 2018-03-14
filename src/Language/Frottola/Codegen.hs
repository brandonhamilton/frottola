module Language.Frottola.Codegen where

import Control.Monad.State
import Control.Applicative
import Data.ByteString.Short (ShortByteString, toShort)
import Data.Function (on)
import Data.List (sortBy)
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import LLVM.AST
import LLVM.AST.Global
import qualified LLVM.AST as AST
import qualified LLVM.AST.Linkage as L
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Attribute as A
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.FloatingPointPredicate as FP

textToShort :: Text -> ShortByteString
textToShort = toShort . T.encodeUtf8

type SymbolTable = [(Text, Operand)]
type Names = Map.Map Text Int

-- | State of the code generation
data CodegenState = CodegenState
  { currentBlock :: Name
  -- ^ Currently active block
  , blocks       :: Map.Map Name BlockState
  -- ^ Blocks for the function
  , symtab       :: SymbolTable
  -- ^ Symbol table for the function scope
  , blockCount   :: Int
  -- ^ Number of blocks
  , count        :: Word
  -- ^ Number of instructions
  , names        :: Names
  -- ^ Supply of names
  } deriving Show

-- | State of a single basic block inside a function definition
data BlockState = BlockState
  { indx       :: Int
  -- ^ Index of the block
  , stack      :: [Named Instruction]
  -- ^ Instruction stack in the block
  , term       :: Maybe (Named Terminator)
  -- ^ Block termination
  } deriving Show

-- | Code generation environment
newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState)

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name (textToShort startBlockName)) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

-- | LLVM environment
newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module)

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: Text -> AST.Module
emptyModule label = defaultModule { moduleName = textToShort label }

addDefinition :: Definition -> LLVM ()
addDefinition d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }

-- |
-- Top level definitions

-- Local functions
define :: Type -> Text -> [(Type, Name)] -> [BasicBlock] -> LLVM ()
define typ label argtyps body = addDefinition $ GlobalDefinition $
  functionDefaults
    { name = Name (textToShort label)
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtyps], False)
    , returnType = typ
    , basicBlocks = body
    }

-- External functions
external :: Type -> Text -> [(Type, Name)] -> LLVM ()
external typ label argtyps = addDefinition $ GlobalDefinition $
  functionDefaults
    { name = Name (textToShort label)
    , linkage = L.External
    , parameters = ([Parameter ty nm [] | (ty, nm) <- argtyps], False)
    , returnType = typ
    , basicBlocks = []
    }

-- |
-- Blocks

startBlockName :: Text
startBlockName = "start"

start :: Codegen Name
start = gets currentBlock

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (indx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l (reverse s) (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = error $ "Block has no terminator: " ++ show l

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

addBlock :: Text -> Codegen Name
addBlock name = do
  blks <- gets blocks
  ix   <- gets blockCount
  nms  <- gets names

  let new = emptyBlock ix
      (qname, supply) = uniqueName name nms
      sname = Name (textToShort qname)

  modify $ \s -> s { blocks = Map.insert sname new blks
                   , blockCount = ix + 1
                   , names = supply
                   }

  pure sname

setBlock :: Name -> Codegen Name
setBlock name = do
  modify $ \s -> s { currentBlock = name }
  pure name

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock blk = do
  currentBlk <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert currentBlk blk (blocks s) }

current :: Codegen BlockState
current = do
  blk <- gets currentBlock
  blks <- gets blocks
  case Map.lookup blk blks of
    Just b -> pure b
    Nothing -> error ("No such block " ++ show blk)

-- |
-- Names

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = i + 1 }
  pure (i + 1)

uniqueName :: Text -> Names -> (Text, Names)
uniqueName nm ns = case Map.lookup nm ns of
  Just c -> (nm <> T.pack (show c), Map.insert nm (c + 1) ns)
  Nothing -> (nm, Map.insert nm 1 ns)

-- | Instruction operands

double :: Type
double = FloatingPointType DoubleFP

local :: Name -> Operand
local = LocalReference double

externf :: Name -> Operand
externf = ConstantOperand . C.GlobalReference double

assign :: Text -> Operand -> Codegen ()
assign var x = do
  syms <- gets symtab
  modify $ \s -> s { symtab = (var, x) : syms }

getvar :: Text -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case lookup var syms of
    Nothing -> error ("Local variable not in scope: " ++ show var)
    Just v -> pure v

-- | Instructions

instr :: Instruction -> Codegen Operand
instr ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = (ref := ins) : i })
  pure (local ref)

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  pure trm

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

-- |
-- Expressions

fbinop :: (FastMathFlags -> Operand -> Operand -> InstructionMetadata -> Instruction) -> Operand -> Operand -> Codegen Operand
fbinop op a b = instr (op noFastMathFlags a b [])

fadd :: Operand -> Operand -> Codegen Operand
fadd = fbinop FAdd

fsub :: Operand -> Operand -> Codegen Operand
fsub = fbinop FSub

fmul :: Operand -> Operand -> Codegen Operand
fmul = fbinop FMul

fdiv :: Operand -> Operand -> Codegen Operand
fdiv = fbinop FDiv

frem :: Operand -> Operand -> Codegen Operand
frem = fbinop FRem

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr $ FCmp cond a b []

lt :: Operand -> Operand -> Codegen Operand
lt a b = fcmp FP.ULT a b >>= uitofp double

gt :: Operand -> Operand -> Codegen Operand
gt a b = fcmp FP.UGT a b >>= uitofp double

br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

cons :: C.Constant -> Operand
cons = ConstantOperand

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr $ UIToFP a ty []

-- |
-- Effectful operations

call :: Operand -> [Operand] -> Codegen Operand
call fn args = instr $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

alloca :: Type -> Codegen Operand
alloca typ = instr $ Alloca typ Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store ptr val = instr $ Store False ptr val Nothing 0 []

load :: Operand -> Codegen Operand
load ptr = instr $ Load False ptr Nothing 0 []
