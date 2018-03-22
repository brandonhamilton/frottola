module Language.Frottola.Codegen where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Data.ByteString.Short (ShortByteString, toShort, fromShort)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import LLVM.Module
import LLVM.Context
import LLVM.IRBuilder
import qualified LLVM.AST as AST
import qualified LLVM.AST.AddrSpace as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.CallingConvention as CC
import qualified LLVM.AST.Type as AT
import qualified LLVM.AST.Attribute as A

import qualified Language.Frottola.Syntax as S

type CodeGenState = [(Text, AST.Operand)]

-- |
-- Utility functions
textToShort :: Text -> ShortByteString
textToShort = toShort . T.encodeUtf8

textFromShort :: ShortByteString -> Text
textFromShort = T.decodeUtf8 . fromShort

tname :: Text -> AST.Name
tname = AST.Name . textToShort

doubleTy :: AST.Type
doubleTy = AT.FloatingPointType AT.DoubleFP

initModule :: AST.Module
initModule = AST.defaultModule { AST.moduleName = "main" }

extractName :: AST.Name -> Text
extractName (  AST.Name   n) = textFromShort n
extractName n@(AST.UnName i) = T.pack . show $ n

assign :: (Monad m, MonadState CodeGenState m) => Text -> AST.Operand -> m ()
assign nm v = modify $ \s -> (nm, v) : s

getvar :: (Monad m, MonadState CodeGenState m) => Text -> m AST.Operand
getvar nm = get >>= \nms -> case lookup nm nms of
  Just v -> return v
  Nothing ->
    error
      $  "Local variable not in scope: "
      ++ show nm
      ++ " Scope: ["
      ++ show nms
      ++ "]"

externf :: AST.Name -> [AST.Type] -> AST.Operand
externf name args = AST.ConstantOperand $ C.GlobalReference (AT.PointerType (AT.FunctionType doubleTy args False) (AST.AddrSpace 0)) name

callExtern :: AST.Operand -> [AST.Operand] -> AST.Instruction
callExtern fn args = AST.Call Nothing CC.C [] (Right fn) (toArgs args) [] []
 where
  toArgs :: [AST.Operand] -> [(AST.Operand, [A.ParameterAttribute])]
  toArgs = map (\x -> (x, []))

-- | Generate code into an existing module
codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod funs =
  withContext $ \context -> withModuleFromAST context ast $ \m -> do
    print ast
    llstr <- moduleLLVMAssembly m
    print llstr
    pure ast
 where
  mDefs = execModuleBuilder emptyModuleBuilder
                            (runStateT (mapM codegenProgram funs) [])
  ast = mod { AST.moduleDefinitions = AST.moduleDefinitions mod ++ mDefs }

-- |
-- Program level

codegenProgram
  :: (Monad m, MonadModuleBuilder m, MonadState CodeGenState m)
  => S.Expr
  -> m ()
codegenProgram (S.Function name args body) = void
  $ function (tname name) (argNames args) doubleTy bdy
 where
  argNames :: [Text] -> [(AST.Type, ParameterName)]
  argNames = map (\x -> (doubleTy, ParameterName (textToShort x)))

  bdy
    :: (Monad m, MonadState CodeGenState m) => [AST.Operand] -> IRBuilderT m ()
  bdy fargs = do
    forM_ fargs $ \r@(AST.LocalReference ty nm) -> do
      v <- alloca ty Nothing 0
      store v 0 r
      assign (extractName nm) v
    genExpr body >>= ret

codegenProgram (S.Extern name args) =
  void $ extern (tname name) (const doubleTy <$> args) doubleTy
codegenProgram exp =
  void $ function "main" [] doubleTy (\_ -> genExpr exp >>= ret)

-- |
-- Expression level

genBinop
  :: (Monad m, MonadIRBuilder m, MonadState CodeGenState m)
  => (AST.Operand -> AST.Operand -> m AST.Operand)
  -> S.Expr
  -> S.Expr
  -> m AST.Operand
genBinop o a b = join (o <$> genExpr a <*> genExpr b)

genExpr
  :: (Monad m, MonadIRBuilder m, MonadState CodeGenState m)
  => S.Expr
  -> m AST.Operand
genExpr (S.Float n     ) = double n
genExpr (S.Var   x     ) = getvar x >>= flip load 0
genExpr (S.Call fn args) = mapM genExpr args
  >>= \a -> emitInstr doubleTy $ callExtern (externf (tname fn) (map (const doubleTy) args)) a
genExpr (S.BinOp "+" a           b  ) = genBinop fadd a b
genExpr (S.BinOp "-" a           b  ) = genBinop fsub a b
genExpr (S.BinOp "*" a           b  ) = genBinop fmul a b
genExpr (S.BinOp "/" a           b  ) = genBinop fdiv a b
genExpr (S.BinOp "%" a           b  ) = genBinop frem a b
genExpr (S.BinOp "<" a           b  ) = genBinop (fcmp FP.OLT) a b
genExpr (S.BinOp ">" a           b  ) = genBinop (fcmp FP.OGT) a b
genExpr (S.BinOp "=" (S.Var var) val) = do
  a    <- getvar var
  cval <- genExpr val
  store a 0 cval
  pure cval
