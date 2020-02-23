{-# LANGUAGE FlexibleContexts #-}

module Translation.Fragment where

import qualified Data.ByteString.Lazy.Char8 as Char8
import qualified Data.DList as DList
import Control.Monad.State

import Translation.Label
import qualified MiddleEnd.Tree as Tree
import Translation.Frame

data StrFrag
  = StrFrag
  { _sfLabel :: Label
  , _sfContent :: Char8.ByteString
  } deriving (Show)

data ProcFrag
  = ProcFrag
  { _pfFrame :: Frame
  , _pfBody :: Tree.Stmt
  } deriving (Show)

type FragSet a = DList.DList a

type StrFragSet = FragSet StrFrag
type ProcFragSet = FragSet ProcFrag

addFrag :: MonadState (FragSet a) m => a -> m ()
addFrag = modify . DList.cons

