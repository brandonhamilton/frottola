module Language.Frottola.Emit where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Text (Text)
import Language.Frottola.Codegen
import qualified Language.Frottola.Syntax as S
import LLVM.Module
import LLVM.Context
import qualified LLVM.AST as AST
import qualified LLVM.AST.Constant as C
import qualified LLVM.AST.Float as F
import qualified LLVM.AST.FloatingPointPredicate as FP

liftError :: ExceptT String IO a -> IO a
liftError = runExceptT >=> either fail return

codegen :: AST.Module -> [S.Expr] -> IO AST.Module
codegen mod funs = withContext $ \context ->
  withModuleFromAST context ast $ \m -> do
    llstr <- moduleLLVMAssembly m
    --print llstr
    pure ast
  where
    ast = runLLVM mod (mapM codegenProgram funs)

-- |
-- Program level

toSig :: [Text] -> [(AST.Type, AST.Name)]
toSig = map (\x -> (double, AST.Name (textToShort x)))

codegenProgram :: S.Expr -> LLVM ()
codegenProgram (S.Function name args body) = define double name funargs bls
  where
    funargs = toSig args
    bls = createBlocks $ execCodegen $ do
      entry <- addBlock startBlockName
      setBlock entry
      forM args $ \a -> do        
        var <- alloca double
        store var (local (AST.Name (textToShort a)))
        assign a var
      cgen body >>= ret

codegenProgram (S.Extern name args) = external double name (toSig args)

codegenProgram exp = define double "main" [] blks
  where
    blks = createBlocks $ execCodegen $ do
      entry <- addBlock startBlockName
      setBlock entry
      cgen exp >>= ret

-- |
-- Expression level

binop :: (AST.Operand -> AST.Operand -> Codegen AST.Operand) -> S.Expr -> S.Expr -> Codegen AST.Operand
binop o a b = join (o <$> cgen a <*> cgen b)

cgen :: S.Expr -> Codegen AST.Operand
cgen (S.Float n) = pure $ cons $ C.Float (F.Double n)
cgen (S.Var x) = getvar x >>= load
cgen (S.Call fn args) = mapM cgen args >>= call (externf (AST.Name (textToShort fn)))
cgen (S.BinOp "+" a b) = binop fadd a b
cgen (S.BinOp "-" a b) = binop fsub a b
cgen (S.BinOp "*" a b) = binop fmul a b
cgen (S.BinOp "/" a b) = binop fdiv a b
cgen (S.BinOp "%" a b) = binop frem a b
cgen (S.BinOp "<" a b) = binop lt a b
cgen (S.BinOp ">" a b) = binop gt a b
cgen (S.BinOp "=" (S.Var var) val) = do
  a <- getvar var
  cval <- cgen val
  store a cval
  pure cval
