{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module ControlFlowGraph
  ( ControlFlowGraph
  , buildControlFlowGraph
  , successorsOf
  ) where

import qualified Data.Vector as Vec
import Data.Vector ((!?))
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import PseudoInst
import Label
import Temp

type ControlFlowGraph = Vec.Vector [Int]

successorsOf :: Int -> ControlFlowGraph -> Maybe [Int]
successorsOf i graph = graph !? i

buildControlFlowGraph :: [PseudoInst Temp] -> ControlFlowGraph
buildControlFlowGraph insts = Vec.fromList $ zipWith f [0..] insts
  where
    posOf label = Map.lookup label $ calcLabelMap insts
    f i pit@PseudoInst{..} = case branchDstOf pit of
      Just dst -> ($ maybe [] pure (posOf dst)) $ case _piOperator of
        Jmp -> id
        _ -> (succ i :)
      _ -> [i + 1]

calcLabelMap :: [PseudoInst Temp] -> Map.Map Label Int
calcLabelMap = Map.fromList . catMaybes . zipWith (\pos label-> fmap (, pos) label) [0..] . fmap labelOf 
