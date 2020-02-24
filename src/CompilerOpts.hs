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
  , _dumpTreeBody :: Bool
  , _useQuadruple :: Bool
  , _dumpQuadruple :: Bool
  , _optimizeQuadruple :: Bool
  , _dumpOptimizedQuadruple :: Bool
  , _dumpPseudoInstruction :: Bool
  , _dumpPseudoInstructionWithRegister :: Bool
  , _dumpTreeFromQuadruple :: Bool
  , _useSSA :: Bool
  , _dumpSSA :: Bool
  , _dumpQuadrupleFromSSA :: Bool
  , _optimizeQuadrupleFromSSA :: Bool
  , _dumpOptimizedQuadrupleFromSSA :: Bool
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
      <*> switch (long "dump-tree-body")
      <*> switch (long "dump-ast")
      <*> switch (long "use-quadruple")
      <*> switch (long "dump-quadruple")
      <*> switch (long "optimize-quadruple")
      <*> switch (long "dump-optimized-quadruple")
      <*> switch (long "dump-pseudo-instruction")
      <*> switch (long "dump-pseudo-instruction-with-register")
      <*> switch (long "dump-tree-from-quadruple")
      <*> switch (long "use-ssa")
      <*> switch (long "dump-ssa")
      <*> switch (long "dump-quadruple-from-ssa")
      <*> switch (long "optimize-quadruple-from-ssa")
      <*> switch (long "dump-optimized-quadruple-from-ssa")
