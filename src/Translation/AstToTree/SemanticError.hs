module Translation.AstToTree.SemanticError
  ( SemanticError(..)
  ) where

import FrontEnd.Ast

data SemanticError
  = UnknownType TypeId
  | UncompletedType TypeId 
  | UnknownFun VarId
  | UnknownVar VarId
  | UnknownArray VarId
  | UnknownBinOp BinOp
  | UnknownUnOp UnOp
  | UnknownDec Dec
  | NotArray Var
  | NoSuchFiled Var VarId
  | NotRecord Var            
  | MismatchedType TypeId TypeId
  | InvalidOperandType TypeId
  | EmptyLetBody
  | NonExprLetBody
  | OnlyFunDecAllowedAtTopLevel
  deriving (Show)
