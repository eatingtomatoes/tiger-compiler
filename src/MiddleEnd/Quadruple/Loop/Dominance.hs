{-# LANGUAGE TupleSections #-} 
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module MiddleEnd.Quadruple.Loop.Dominance
  ( DominanceGraph
  , buildDominanceGraph
  , DominanceTree
  , buildDominanceTree
  , calcDominanceFrontier
  , module Utility.DirectedGraph
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Safe (atMay, foldr1May)
import Data.Maybe (fromMaybe, mapMaybe)

import MiddleEnd.Quadruple.ControlFlowGraph

import Utility.DirectedGraph
import Utility.Fixpoint
import Utility.Bucket
import Utility.Visualize

newtype DominanceGraph = DominanceGraph (MapBasedDirectedGraphForScalar Int)
  deriving (DirectedGraph, DrawableDirectedGraph)
 
newtype DominanceTree = DominanceTree (MapBasedDirectedGraphForScalar Int)
  deriving (DirectedGraph, DrawableDirectedGraph)

buildDominanceGraph :: Int -> (Int -> Maybe [Int]) -> DominanceGraph
buildDominanceGraph num cfgPredOf = foldr f empty $ zip [0..] dominators
  where
    f (i, doms) graph = foldr (flip connectTo i) graph doms
    dominators :: [Set.Set Int]
    dominators = fixpoint (==) calc initial
    initial = Set.singleton 0 : replicate (num - 1) (Set.fromList $ [0..num-1])
    calc dss = fmap g [0..num-1]
      where
        g i = Set.insert i $ fromMaybe mempty $ do
          prevs <- cfgPredOf i
          foldr1May Set.intersection $ mapMaybe (dss `atMay`) prevs

buildDominanceTree :: DominanceGraph -> DominanceTree
buildDominanceTree graph
  = foldr (\(v, d) -> connectTo d v) empty
  $ mapMaybe (\v -> predSetOf v graph >>= f . (v, ))
  $ vertexOf graph
  where
    f :: (Int, Set.Set Int) -> Maybe (Int, Int)
    f (i, dominators) = fmap (i,) $ Set.lookupMin $ Set.filter g dominators'
      where
        dominators' = Set.delete i dominators
        g d = maybe False Set.null $ do
          dominated <- succOf d graph
          return $ dominators' `Set.intersection` (Set.delete d dominated)

calcDominanceFrontier :: (Int -> Maybe (Set.Set Int)) -> (Int -> Maybe (Set.Set Int)) -> Bucket Int Int
calcDominanceFrontier cfgSuccOf idominated = fromMap $ snd $ calc 0 mempty
  where
    calc i (fs, table) = (frontier <> fs, Map.insert i frontier table')
      where
        frontier = local `Set.union` up        
        local = maybe mempty (Set.filter (not . flip Set.member idoms)) $ cfgSuccOf i
        up = Set.filter (\cf -> cf == i || not (Set.member cf idoms)) cfs        
        (cfs, table') = foldr calc (mempty, table) idoms
        idoms = fromMaybe mempty $ idominated i
        
  
