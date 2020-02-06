{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module TigerLogger where

import Control.Monad.Logger
import qualified Data.Text.Lazy as LazyText
import qualified Data.Text as StrictText
import qualified Text.Pretty.Simple as Pretty

class MonadLogger (logger m) => TigerLogger logger m where
  runTigerLogger :: logger m a -> m a

instance TigerLogger NoLoggingT IO where
  runTigerLogger = runNoLoggingT

instance TigerLogger LoggingT IO where
  runTigerLogger = runStderrLoggingT

pShow :: Show a => a -> StrictText.Text
pShow = LazyText.toStrict . Pretty.pShow
