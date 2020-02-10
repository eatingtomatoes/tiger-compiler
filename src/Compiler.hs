{-# LANGUAGE OverloadedStrings #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Compiler
  ( compile
  ) where

import qualified Data.ByteString.Lazy.Char8 as Char8

import Control.Monad.State
import Control.Monad.Except
import Data.Default
import Data.Foldable

import TigerParser
import AstHelper
import SymbolTable
import Label
import TransState
import InterRepGen
import AssemblerGen
import Util

compile :: Char8.ByteString -> Except String Char8.ByteString
compile source = do
  ast <- unify $ parseTiger source
  TransState{..} <- unify $ genInterRep def ast

  funs <- mapExceptT (flip evalStateT (_trsTempPool, _trsLabelPool)) $ do
    mapM encodeProcFrag $ toList _trsProcFragSet

  let strs = fmap encodeStrFrag $ toList _trsStrFragSet
      types = fmap encodeType $ describeTypes $ _stTypes _trsSymbolTable
  let dataSection = genDataSection $ types <> strs
      textSection = genTextSection _trsGlobal _trsExternal funs      

  return $ genAssemblerCode dataSection textSection

