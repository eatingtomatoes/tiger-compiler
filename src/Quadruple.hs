module Quadruple
  ( Var (..)
  , Quadruple (..)
  ) where

import Temp
import Tree
import Label

data Var
  = TempV Temp
  | ConstV Int
  | LabelV Label
  deriving (Show)

data Quadruple
  = BinQ Temp BinOp Var Var
  | UnQ Temp UnOp Var
  | JumpQ Label
  | CJumpQ RelOp Var Var Label Label
  | LabelQ Label
  | LoadQ Temp Var
  | StoreQ Var Var
  | CallQ Label [Var]
  | MoveCallQ Temp Label [Var]
  | MoveQ Temp Var  
  deriving (Show)
