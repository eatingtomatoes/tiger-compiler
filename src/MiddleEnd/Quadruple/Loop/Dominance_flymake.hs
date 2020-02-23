{-# LANGUAGE TupleSections #-} 
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module MiddleEnd.Quadruple.Loop.Dominance
  ( DominanceGraph
  , buildDominanceGraph
  , DominanceTree
  , buildDominanceTree
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Safe (atMay, foldr1May, headMay)
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Debug.Pretty.Simple

import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.ControlFlowGraph

import Utility.Fixpoint
import Utility.DirectedGraph.MapBasedDirectedGraphForScalar
import Utility.Bucket

newtype DominanceGraph = DominanceGraph (MapBasedDirectedGraphForScalar Int)
  deriving (DirectedGraph)

newtype DominanceTree = DominanceTree (MapBasedDirectedGraphForScalar Int)
  deriving (DirectedGraph)

buildDominanceGraph :: Int -> (Int -> Maybe [Int]) -> DominanceGraph
buildDominanceGraph num predOf = foldr f empty $ zip [0..] dominators
  where
    f (i, doms) graph = foldr (flip connectTo i) graph doms
    dominators :: [Set.Set Int]
    dominators = fixpoint (==) calc initial
    initial = Set.singleton 0 : replicate (num - 1) (Set.fromList $ [0..num-1])
    calc dss = fmap g [0..num-1]
      where
        g i = Set.insert i $ fromMaybe mempty $ do
          prevs <- predOf i
          foldr1May Set.intersection $ mapMaybe (dss `atMay`) prevs


calcDominators :: Int -> (Int -> Maybe [Int]) -> [Set.Set Int]
calcDominators num predOf = fixpoint (==) calc initial
  where
    initial = Set.singleton 0 : replicate (num - 1) (Set.fromList $ [0..num-1])
    calc dss = fmap f [0..num-1]
      where
        f i = Set.insert i $ fromMaybe mempty $ do
          prevs <- predOf i
          foldr1May Set.intersection $ mapMaybe (dss `atMay`) prevs

calcDominated :: [Set.Set Int] -> Map.Map Int (Set.Set Int)
calcDominated dominators = foldr f mempty $ zip [0..] dominators
  where
    f (i, ds) table = foldr g table ds
      where
        g = Map.alter (Just . maybe (Set.singleton i) (Set.insert i))

buildDominanceTree :: DominanceGraph -> DominanceTree
buildDominanceTree graph
  = foldr (\(v, d) -> connectTo d v) empty
  $ mapMaybe (\v -> predSetOf v graph >>= f . (v, ))
  $ vertexOf graph
  where
    f :: (Int, Set.Set Int) -> Maybe (Int, Int)
    f (i, dominators) = fmap (i,) $ Set.lookupMin $ Set.filter g dominators
      where
        dominators' = Set.delete i dominators
        g d = maybe False Set.null $ do
          dominated <- succSetOf d graph
          return $ dominators' `Set.intersection` (Set.delete d dominated)

calcDominanceFrontier :: (Int -> Maybe [Int]) -> (Int -> Maybe (Set.Set Int)) -> DominanceTree -> Bucket Int Int
calcDominanceFrontier succOf idominated tree = snd $ calc 0 mempty
  where
    calc i (fs, table) = (frontier <> fs, Map.insert i frontier table')
      where
        frontier = local `Set.union` up        
        local = maybe mempty (Set.fromList . filter (not . flip isIDominatedBy i)) $ succOf i
        up = Set.filter (\cf -> cf == i || not (cf `isIDominatedBy` i)) cfs        
        (cfs, table') = foldr calc (mempty, table) $ fromMaybe mempty $ idominated i
        isIDominatedBy x = Set.member x . fromMaybe mempty . idominated 
        
  
