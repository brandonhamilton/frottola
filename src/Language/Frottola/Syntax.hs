module Language.Frottola.Syntax where

import Data.Text

type Name = Text
type Program = [Expr]

-- |
-- Expressions
--
data Expr
  -- | Value
  = Float Double
  -- | Binary operation
  | BinOp Op Expr Expr
  -- | Variable binding
  | Var Text
  -- | Function call
  | Call Name [Expr]
  -- | Function definition
  | Function Name [Expr] Expr
  -- | External call
  | Extern Name [Expr]
  deriving (Eq, Ord, Show)

-- |
-- Operations
--
data Op
  -- | Addition
  = Add
  -- | Subtraction
  | Subtract
  -- | Multiplication
  | Multiply
  -- | Division
  | Divide
  deriving (Eq, Ord, Show)
