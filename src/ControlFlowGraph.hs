{-# LANGUAGE TupleSections #-}

module ControlFlowGraph
  ( ControlFlowGraph
  , buildControlFlowGraph
  , successorsOf
  ) where

import qualified Data.Vector as Vec
import Data.Vector ((!?))
import Data.Maybe (catMaybes)
import qualified Data.Map.Strict as Map

import Instruction
import Label

type ControlFlowGraph = Vec.Vector [Int]

successorsOf :: Int -> ControlFlowGraph -> Maybe [Int]
successorsOf i graph = graph !? i

buildControlFlowGraph :: [Instruction] -> ControlFlowGraph
buildControlFlowGraph insts = Vec.fromList $ zipWith f [0..] insts
  where
    posOf label = Map.lookup label $ calcLabelMap insts 
    f i (Jmp op dst) = (if op == "jmp" then id else (succ i :)) $ maybe [] pure (posOf dst) 
    f i _ = [i + 1]

calcLabelMap :: [Instruction] -> Map.Map Label Int
calcLabelMap = Map.fromList . catMaybes . zipWith (\pos label-> fmap (, pos) label) [0..] . fmap labelOf 
