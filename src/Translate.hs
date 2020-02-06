module Translate where

import qualified Data.Map.Strict as Map

import Label
import qualified Frame
import AST

data Access
  = Access
  { _acLevel :: Level
  , _acAcess :: Frame.Access
  } deriving (Show)

data Level
  = Level
  deriving (Show)

newLevel :: Maybe Level -> Label -> [Bool] -> Level
newLevel parent name escaped = undefined

formalsOf :: Level -> [Access]
formalsOf level = undefined

allocLocal :: Level -> Bool -> Access
allocLocal level escaped = undefined

data Entry
  = VarEntry
  { _veAccess :: Access
  , _veType :: SymTab.TypeId
  }
  | FunEntry
  { _feLevel :: Level
  , _feName :: Label
  , _feFormals :: [SymTab.TypeId]
  , _feResult :: SymTab.TypeId
  }
  deriving (Show)

