{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as Char8
import Control.Monad.Except
import System.Exit

import Control.Monad.State
import Data.Default
import Data.Foldable
import Text.Pretty.Simple

import TigerParser
import SymbolTable
import TransState
import InterRepGen
import AssemblerGen

import CompilerOpts
      
main :: IO ()
main = do
  CompilerOpts{..} <- getCompilerOpts
  source <- maybe Char8.getContents Char8.readFile _inPath

  ast <- access $ parseTiger source

  when _dumpAst $ do
    pPrint ast  
  
  TransState{..} <- access $ genInterRep' def ast  

  funs <- access $ runExcept $ do
    mapExceptT (flip evalStateT (_trsTempPool, _trsLabelPool)) $ do
      -- mapM encodeProcFrag $ toList _trsProcFragSet
      forM (toList _trsProcFragSet) $ \frag -> do
        -- insts <- selectInstructions frag
        encodeProcFrag frag

  let strs = fmap encodeStrFrag $ toList _trsStrFragSet
      types = fmap encodeType $ describeTypes $ _stTypes _trsSymbolTable             
      dataSection = genDataSection $ types <> strs
      textSection = genTextSection _trsGlobal _trsExternal funs      

  let assembler = genAssemblerCode dataSection textSection  
  
  maybe Char8.putStrLn Char8.writeFile _outPath assembler

  where
    access :: Either String a -> IO a
    access x = case x of
      Left msg -> putStrLn msg >> exitFailure
      Right v -> return v
    
