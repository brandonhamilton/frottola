module Language.Frottola.Optimizer where

import LLVM.Analysis
import LLVM.PassManager
import LLVM.Context
import LLVM.Module as Mod
import qualified LLVM.AST as AST

passes :: PassSetSpec
passes = defaultCuratedPassSetSpec { optLevel = Just 3 }

runOpt :: AST.Module -> IO AST.Module
runOpt mod = withContext $ \context ->
  withModuleFromAST context mod $ \m -> withPassManager passes $ \pm -> do
    verify m
    runPassManager pm m
    moduleAST m
