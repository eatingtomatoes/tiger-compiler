{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.ControlFlowGraph
  ( ControlFlowGraph
  , buildControlFlowGraph
  , successorsOf
  , calcLabelMap
  ) where

import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Arrow ((&&&))

import Backend.Instruction
import Translation.Label
import Translation.Temp
import Graph

type ControlFlowGraph = Graph Int

buildControlFlowGraph :: [Instruction Temp] -> ControlFlowGraph
buildControlFlowGraph insts = foldr f mempty $ zip [0..] insts
  where
    f (i, inst) = case branchDstOf &&& _piOperator $ inst of
      (Nothing, _) -> connectSucc
      (Just dst, Jmp) -> connectDst dst
      (Just dst, _) -> connectSucc . connectDst dst
      where
        connectDst dst = maybe id (connectTo i) (Map.lookup dst labelMap)
        connectSucc = if i + 1 < inum then connectTo i (i + 1) else id
    inum = length insts
    labelMap = calcLabelMap insts

successorsOf :: Int -> ControlFlowGraph -> Maybe (Set.Set Int)
successorsOf = neighborsOf 

calcLabelMap :: [Instruction Temp] -> Map.Map Label Int
calcLabelMap = Map.fromList . catMaybes . zipWith (\pos label-> fmap (, pos) label) [0..] . fmap labelOf 

