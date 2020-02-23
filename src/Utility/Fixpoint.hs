module Utility.Fixpoint
  ( fixpoint
  ) where
  
fixpoint :: (a -> a -> Bool) -> (a -> a) -> a -> a
fixpoint p f x = let x' = f x in if p x x' then x else fixpoint p f x'
