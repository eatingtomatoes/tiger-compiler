{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Util where

import Control.Monad.State
import Control.Monad.Except
import Data.Bifunctor
import Control.Lens hiding (lens)
import Debug.Pretty.Simple

replaceWith :: MonadState s m => Lens' s a -> a -> m b -> m b
replaceWith lens new action = do
  original <- use lens
  assign lens new
  x <- action
  assign lens original
  return x

fixpoint :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpoint p f x = let x' = f x in if p x x' then x else fixpoint p f x'

unify :: (MonadError String m, Show a) => Either a b -> m b
unify = liftEither . bimap show id 

pTraceWith :: (a -> String) -> a -> a
pTraceWith f x = pTrace (f x) x

(~~) :: Show a => String -> a -> String
x ~~ y = x <> show y
