{-# LANGUAGE RankNTypes #-}

module Utility.Debug.Trace
  ( Trace
  , mute
  , vocal
  ) where

import Debug.Pretty.Simple (pTrace)
import Text.Printf

type Trace = forall a. String -> String -> a -> a

mute :: Trace
mute _ _ = id

vocal :: Trace
vocal location message = pTrace (printf "%s: %s" location message)
