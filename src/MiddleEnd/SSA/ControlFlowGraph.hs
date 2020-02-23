{-# LANGUAGE TupleSections #-}

module MiddleEnd.SSA.ControlFlowGraph
  ( buildControlFlowGraph'
  , successorsOf'
  , predecessorsOf'
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes)

import MiddleEnd.SSA.Quadruple
import Graph
import Translation.Label

type ControlFlowGraph' = Graph Int

buildControlFlowGraph' :: [Quadruple] -> ControlFlowGraph'
buildControlFlowGraph' quads = Map.fromList $ zip [0..] $ zipWith f [0..] quads
  where
    qnum = length quads
    posOf label = Map.lookup label $ calcLabelMap quads
    f i quad = Set.fromList $ catMaybes $ case quad of
      JumpQ l -> [posOf l]
      CJumpQ _ _ _ l1 l2 -> [posOf l1, posOf l2]
      _ | i + 1 < qnum -> [Just $ i + 1]
      _ -> []

successorsOf' :: ControlFlowGraph' -> Int -> Maybe [Int]
successorsOf' cfg i = fmap Set.toList $ Map.lookup i cfg

predecessorsOf' :: ControlFlowGraph' -> Int -> Maybe [Int]
predecessorsOf' cfg = flip Map.lookup reversed
  where
    reversed = Map.foldrWithKey f mempty cfg
    f i xs table = foldr (Map.alter (Just . maybe [i] (i: ))) table xs 


calcLabelMap :: [Quadruple] -> Map.Map Label Int
calcLabelMap = Map.fromList . catMaybes . zipWith (\pos label-> fmap (, pos) label) [0..] . fmap labelOf
  where
    labelOf quad = case quad of
      LabelQ l -> Just l
      _ -> Nothing
