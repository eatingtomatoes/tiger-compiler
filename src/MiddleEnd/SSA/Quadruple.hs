module MiddleEnd.SSA.Quadruple
  ( TaggedTemp (..)
  , Var (..)
  , Quadruple (..)
  , isTempV
  , isLabelQ
  , isBranchQ
  , isPhiQ
  , branchDstOf
  , labelOf
  ) where

import Data.Char.Small
import Data.List (intersperse)
import Text.Printf

import Translation.Temp
import Translation.Label
import MiddleEnd.Tree

data TaggedTemp
  = TaggedTemp Temp Int
  deriving (Eq, Ord)

data Var
  = TempV TaggedTemp
  | ConstV Int
  | LabelV Label
  deriving (Eq, Ord)
  -- deriving (Show)
  
data Quadruple
  = BinQ TaggedTemp BinOp Var Var
  | UnQ TaggedTemp UnOp Var
  | MoveCallQ TaggedTemp Label [Var]
  | MoveQ TaggedTemp Var
  | PhiQ TaggedTemp [TaggedTemp]
  | JumpQ Label
  | CJumpQ RelOp Var Var Label Label
  | LabelQ Label
  | LoadQ TaggedTemp Var
  | StoreQ Var Var
  | CallQ Label [Var]
  deriving (Eq)

instance Show TaggedTemp where
  show (TaggedTemp t i) = show t <> fmap subscript (show i)

instance Show Var where
  show var = case var of
    TempV t -> show t
    ConstV c -> show c
    LabelV l -> toString l

instance Show Quadruple where
  show quad = case quad of
    BinQ t op v1 v2 -> printf "%s <- %s %s %s" (show t) (show v1) (show op) (show v2)
    UnQ t op v1 -> printf "%s <- %s %s" (show t) (show op) (show v1)
    JumpQ l -> printf "jmp %s" (toString l)
    CJumpQ op v1 v2 l1 l2 -> printf "goto (%s %s %s ? %s : %s)" (show v1) (show op) (show v2) (toString l1) (toString l2)
    LabelQ l -> printf "%s:" (toString l)
    LoadQ t v -> printf "%s <- [%s]" (show t) (show v)
    StoreQ v1 v2 -> printf "[%s] <- %s" (show v1) (show v2)
    CallQ f args -> printf "%s(%s)" (toString f) (show args)
    MoveCallQ t f args -> printf "%s <- %s(%s)" (show t) (toString f) (show args)
    MoveQ t v -> printf "%s <- %s" (show t) (show v)
    PhiQ t ts -> printf "%s <- phi(%s)" (show t) (concat $ intersperse "," $ fmap show ts)

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

isPhiQ :: Quadruple -> Bool
isPhiQ (PhiQ _ _) = True
isPhiQ _ = False

branchDstOf :: Quadruple -> Maybe [Label]
branchDstOf q = case q of
  JumpQ l -> Just [l]
  CJumpQ _ _ _ l1 l2 -> Just [l1, l2]
  _ -> Nothing
  
labelOf :: Quadruple -> Maybe Label
labelOf q = case q of
  LabelQ l -> Just l
  _ -> Nothing
