{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Utility.Conversion
  ( Conversion(..)
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map

class Conversion f g where
  from :: f -> g

instance Conversion f f where
  from = id

instance Conversion (Set.Set a) [a] where
  from = Set.toList
  
instance Conversion (Map.Map k v) [(k, v)] where
  from = Map.toList
