module MiddleEnd.Quadruple
  ( Var (..)
  , Quadruple (..)
  , maybeTemp
  , isTempV
  , isLabelQ
  , isBranchQ
  , branchDstOf
  , labelOf
  ) where

import Text.Printf

import Translation.Temp
import MiddleEnd.Tree
import Translation.Label

data Var
  = TempV Temp
  | ConstV Int
  | LabelV Label
  deriving (Eq, Ord)
  -- deriving (Show)
  
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
  deriving (Eq)

instance Show Var where
  show var = case var of
    TempV t -> show t
    ConstV c -> show c
    LabelV l -> toString l

instance Show Quadruple where
  show quad = case quad of
    BinQ t op v1 v2 -> printf "%r <- %s %s %s" t (show v1) (show op) (show v2)
    UnQ t op v1 -> printf "%r <- %s %s" t (show op) (show v1)
    JumpQ l -> printf "jmp %s" (toString l)
    CJumpQ op v1 v2 l1 l2 -> printf "goto (%s %s %s ? %s : %s)" (show v1) (show op) (show v2) (toString l1) (toString l2)
    LabelQ l -> printf "%s:" (toString l)
    LoadQ t v -> printf "%r <- [%s]" t (show v)
    StoreQ v1 v2 -> printf "[%s] <- %s" (show v1) (show v2)
    CallQ f args -> printf "%s(%s)" (toString f) (show args)
    MoveCallQ t f args -> printf "%r <- %s(%s)" t (toString f) (show args)
    MoveQ t v -> printf "%r <- %s" t (show v)
  
maybeTemp :: Var -> Maybe Temp
maybeTemp (TempV t) = Just t
maybeTemp _ = Nothing

isTempV :: Var -> Bool
isTempV (TempV _) = True
isTempV _ = False

isLabelQ :: Quadruple -> Bool
isLabelQ q = case q of
  LabelQ _ -> True
  _ -> False

isBranchQ :: Quadruple -> Bool
isBranchQ q = case q of
  JumpQ _ -> True
  CJumpQ _ _ _ _ _ -> True
  _ -> False

branchDstOf :: Quadruple -> Maybe [Label]
branchDstOf q = case q of
  JumpQ l -> Just [l]
  CJumpQ _ _ _ l1 l2 -> Just [l1, l2]
  _ -> Nothing
  
labelOf :: Quadruple -> Maybe Label
labelOf q = case q of
  LabelQ l -> Just l
  _ -> Nothing
