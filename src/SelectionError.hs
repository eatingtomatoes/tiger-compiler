module SelectionError
  (
  ) where

data SelectionError
  = UnexpectedStmt Stmt
  | UnexpectedExpr (Maybe Stmt) Expr
  deriving (Show, Eq)
