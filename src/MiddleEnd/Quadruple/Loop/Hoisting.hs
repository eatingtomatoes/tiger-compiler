{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

module MiddleEnd.Quadruple.Loop.Hoisting
  ( hoist
  ) where

import Data.List (sort)
import Safe (headMay, atMay)
import Control.Monad.State
import Control.Monad.Except
import Data.Maybe (catMaybes)
import Control.Lens (zoom, _1)

import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.ControlFlowGraph
import MiddleEnd.Quadruple.Loop.LoopInvariant
import MiddleEnd.Quadruple.Loop.Dominance
import MiddleEnd.Quadruple.Loop.LoopNestForest

import MiddleEnd.Optimization
import Utility.Debug
import Utility.Debug.Trace

import Translation.Temp

hoist :: Monad m => PartialOptimizer Quadruple m
hoist trace = buildOptimizer $ zoom _1 . opt
  where
    opt quads = case mloop of
      Just (entry, invariants) -> hoist_ trace quads entry invariants
      _ -> return quads
      where
        mloop = headMay
          $ filter (not . null . snd)
          $ zip (fmap fst loops) (fmap (calcLoopInvariant quads) loops)
        loops = flattenLoopNestTree OutterFirst tree
        tree = buildLoopNestTree forest
        forest = buildLoopNestForest loopHeaders (flip succOf cfg) dominanceTree
        loopHeaders = calcLoopHeaders (flip succOf cfg) dominanceGraph
        dominanceTree = buildDominanceTree dominanceGraph
        dominanceGraph = buildDominanceGraph (length quads) (flip predOf cfg)           
        cfg = buildControlFlowGraphForQuadruple quads 

hoist_ :: Monad m => Trace -> [Quadruple] -> Int -> [Int] -> ExceptT String (StateT TempPool m) [Quadruple]
hoist_ trace quads entry invariants = do
  (qas, qbs) <- fmap unzip $ fmap catMaybes $ forM sortedInvariants $ \i -> do
    temp <- allocTemp
    return $ case quads `atMay` i of
      Nothing -> error $ "hoist_: invariant " <> quote i <> " points to a nonexistant quadruple"
      Just q -> fmap (trace "hoist" ("hoisting " <> quote q)) $ case q of
        BinQ t op v1 v2 -> Just (BinQ temp op v1 v2, MoveQ t (TempV temp))
        UnQ t op v1 -> Just (UnQ temp op v1, MoveQ t (TempV temp))
        -- MoveQ t v1 | not (isTempV v1) -> Just (MoveQ temp v1, MoveQ t (TempV temp))
        _ -> Nothing
  let quads' = foldr f quads $ zip sortedInvariants qbs
      (former, latter) = splitAt entry quads'
  return $ former <> qas <> latter
  where
    sortedInvariants = sort $ invariants
    f (i, qb) qs = case splitAt i qs of
      (as, _ : bs) -> as <> (qb : bs)
      _ -> error
           $ "hoist_: invariant "
           <> quote i
           <> " points to a nonexistant quadruple"
