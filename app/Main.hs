{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Lazy.Char8 as Char8

import Control.Monad.Except
import System.Exit

import Compiler

import Options.Applicative

data CompilerOpts
  = CompilerOpts
  { _inPath :: Maybe String
  , _outPath :: Maybe String
  } deriving (Show)
      
main :: IO ()
main = do
  CompilerOpts{..} <- getCompilerOpts
  source <- maybe Char8.getContents Char8.readFile _inPath
  case runExcept $ compile source of
    Left err -> do
      putStrLn err
      exitFailure
    Right asm -> do
      maybe Char8.putStrLn Char8.writeFile _outPath asm
    
getCompilerOpts :: IO CompilerOpts
getCompilerOpts = execParser $ info (opts <**> helper) idm
  where
    opts = CompilerOpts
      <$> option (maybeReader $ Just . Just)
      ( long "input"
        <> short 'i'
        <> help "source filename"
        <> value Nothing
        <> metavar "INPUT"
      )
      <*> option (maybeReader $ Just . Just)
      ( long "output"
        <> short 'o'
        <> help "output filename"
        <> value Nothing
        <> metavar "OUTPUT"
      )
