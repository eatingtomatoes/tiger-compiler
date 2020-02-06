module Tree where

import Temp
import Label
  
data Stmt
  = SeqStmt Stmt Stmt
  | LabelStmt Label.Label
  | JumpStmt Expr [Label.Label]
  | CJumpStmt RelOp Expr Expr Label Label
  | MoveStmt Expr Expr
  | ExprStmt Expr
  deriving (Show, Eq)

data Expr
  = BinOpExpr BinOp Expr Expr
  | UnOpExpr UnOp Expr
  | MemExpr Expr
  | TempExpr Temp
  | ESeqExpr Stmt Expr
  | NameExpr Label
  | ConstExpr Int
  | CallExpr Expr [Expr]
  deriving (Show, Eq)

data BinOp
  = Plus
  | Minus
  | Times
  | Divide
  | And
  | Or
  | LeftShift
  | RightShift
  | Xor
  deriving (Show, Eq)

data UnOp
  = Not
  | Negate
  deriving (Show, Eq)

data RelOp
  = Equal
  | NotEqual
  | Less
  | LessOrEqual
  | Greater
  | GreaterOrEqual
  | NotLess
  | NotLessOrEqual
  | NotGreater
  | NotGreaterOrEqual
  deriving (Show, Eq)


  
