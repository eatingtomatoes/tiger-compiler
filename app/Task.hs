module Task
  ( Task(..)
  , getTask
  ) where

import Options.Applicative

data Task
  = Test
  { caseNo :: Int
  }
  | Compile
  { filename :: String
  , output :: Maybe FilePath
  }
  deriving (Show)

getTask :: IO Task
getTask = execParser taskOpts

taskOpts :: ParserInfo Task
taskOpts = info (opts <**> helper) idm
  where
    opts = subparser $ mconcat $ fmap (uncurry command)
      [ ("test", info testOpts idm)
      , ("compile", info compileOpts idm)
      ]
    testOpts = Test
      <$> option auto
      ( long "case"
        <> short 'c'
        <> help "Run test on case <case>"
        <> metavar "INT"
      )        
    compileOpts = Compile
      <$> argument str
      ( metavar "FILE"
      )
      <*> option (maybeReader $ Just . Just)
      ( long "output"
        <> short 'o'
        <> help "Write the assemble code to <output>"
        <> value Nothing
        <> metavar "OUTPUT"
      )
