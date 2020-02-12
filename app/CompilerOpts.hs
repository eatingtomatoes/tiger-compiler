module CompilerOpts
  ( CompilerOpts (..)
  , getCompilerOpts
  ) where

import Options.Applicative

data CompilerOpts
  = CompilerOpts
  { _inPath :: Maybe String
  , _outPath :: Maybe String
  , _dumpAst :: Bool
  } deriving (Show)

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
      <*> switch
      ( long "dump-ast"
      )
