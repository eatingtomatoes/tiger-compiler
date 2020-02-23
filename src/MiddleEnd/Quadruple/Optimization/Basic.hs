{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}

module MiddleEnd.Quadruple.Optimization.Basic
  ( dstOf
  , calcDefs
  ) where

import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

import MiddleEnd.Quadruple
import Translation.Temp

dstOf :: Quadruple -> Maybe Temp
dstOf quad = case quad of
  BinQ t _ _ _ -> Just t
  UnQ t _ _ -> Just t
  LoadQ t _ -> Just t
  MoveCallQ t _ _ -> Just t
  MoveQ t _ -> Just t
  _ -> Nothing

calcDefs :: [Quadruple] -> Map.Map Temp (Set.Set Int)
calcDefs = foldr h mempty . catMaybes . zipWith f [0..] 
  where
    f i quad = fmap (i, ) $ dstOf quad
    h (i, t) table = Map.alter g t table
      where
        g = Just . maybe (Set.singleton i) (Set.insert i)
