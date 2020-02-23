{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Translation.AstToTree.TransState
  ( TransState(..)
  , trsSymbolTable
  , trsFrame
  , trsStrFragSet
  , trsProcFragSet
  , trsTempPool
  , trsLabelPool
  , trsExternal
  , trsGlobal
  , trsEntry
  , trsExit
  , addGlobalName
  , addExternalName
  ) where

import Control.Lens hiding (Level)
import Data.Default
import Control.Monad.State
import qualified Data.ByteString.Lazy.Char8 as Char8

import Translation.AstToTree.SymbolTable
import Translation.Fragment
import Translation.Temp
import Translation.Label
import Translation.Frame

data TransState
  = TransState
  { _trsSymbolTable :: SymbolTable
  , _trsFrame :: Frame
  , _trsStrFragSet :: StrFragSet
  , _trsProcFragSet :: ProcFragSet
  , _trsTempPool :: TempPool
  , _trsLabelPool :: LabelPool
  , _trsExternal :: [Char8.ByteString]
  , _trsGlobal :: [Char8.ByteString]
  , _trsEntry :: Label
  , _trsExit :: Label
  } deriving (Show)

makeLenses ''TransState
 
instance Default TransState where
  def = TransState
    { _trsSymbolTable = def
    , _trsFrame = makeFrame (labelFrom "initialFrame") [] 0
    , _trsStrFragSet = def
    , _trsProcFragSet = def
    , _trsTempPool = def
    , _trsLabelPool = def
    , _trsExternal = def
    , _trsGlobal = def
    , _trsEntry = labelFrom "initialEntry"
    , _trsExit = labelFrom "initialExit"
    }

addGlobalName :: MonadState TransState m => Char8.ByteString -> m ()
addGlobalName name = modifying trsGlobal (name :)

addExternalName :: MonadState TransState m => Char8.ByteString -> m ()
addExternalName name = modifying trsExternal (name :)
