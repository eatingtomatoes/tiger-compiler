{-# LANGUAGE RankNTypes #-}

module MiddleEnd.Optimization
  ( Optimizer
  , PartialOptimizer
  , buildOptimizer
  , runOptimizer
  , identity
  , andThen
  ) where

import Control.Monad
import Control.Monad.State
import Control.Monad.Except

import Translation.Temp
import Translation.Label
import Utility.Debug.Trace

newtype Optimizer a m = Optimizer {
  unOptimizer :: [a] -> ExceptT String (StateT (TempPool, LabelPool) m) [a]
}

type PartialOptimizer a m = Trace -> Optimizer a m

buildOptimizer :: ([a] -> ExceptT String (StateT (TempPool, LabelPool) m) [a]) -> Optimizer a m
buildOptimizer = Optimizer

runOptimizer :: (Eq a, Monad m) => Optimizer a m -> [a] -> ExceptT String (StateT (TempPool, LabelPool) m) [a]
runOptimizer optimizer quads = do
  qs <- unOptimizer optimizer quads
  if qs == quads then return qs else runOptimizer optimizer qs 

identity :: Monad m => Optimizer a m
identity = Optimizer return 

andThen :: Monad m => Optimizer a m -> Optimizer a m -> Optimizer a m
x `andThen` y  = Optimizer $ unOptimizer x >=> unOptimizer y
