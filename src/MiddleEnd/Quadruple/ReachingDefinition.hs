{-# LANGUAGE TupleSections #-}

module MiddleEnd.Quadruple.ReachingDefinition 
  ( calcInAndOut
  , calcDefs
  , destOf
  , srcOf
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Safe (atMay)
import Control.Arrow

import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.ControlFlowGraph
import Translation.Temp
import Utility.Fixpoint       

calcDefs :: [Quadruple] -> Map.Map Temp (Set.Set Int)
calcDefs = foldr h mempty . catMaybes . fmap f . zip [0..] 
  where
    f (i, quad) = fmap (i, ) $ case quad of
      BinQ t _ _ _ -> Just t
      UnQ t _ _ -> Just t
      LoadQ t _ -> Just t
      MoveCallQ t _ _ -> Just t
      MoveQ t _ -> Just t
      _ -> Nothing
    h (i, t) table = Map.alter g t table
      where
        g = Just . maybe (Set.singleton i) (Set.insert i)

calcGenAndKills :: [Quadruple] -> [(Set.Set Int, Set.Set Int)]
calcGenAndKills quads
  = fmap (maybe mempty g)
  $ fmap f
  $ zip [0..] quads
  where
    f (i, quad) = fmap (i, ) $ case quad of
      BinQ t _ _ _ -> Just t
      UnQ t _ _ -> Just t
      LoadQ t _ -> Just t
      MoveCallQ t _ _ -> Just t
      MoveQ t _ -> Just t
      _ -> Nothing
    g (i, t) = (Set.singleton i, Set.delete i $ defsOf t)
    defsOf t = maybe mempty id $ Map.lookup t (calcDefs quads) 

type PredecessorsMap = Int -> Maybe [Int]
type GenKillMap = Int -> Maybe (Set.Set Int, Set.Set Int)

calcInAndOut_ :: Int -> PredecessorsMap -> GenKillMap -> ([Set.Set Int], [Set.Set Int])
calcInAndOut_ qnum predsOf genKillsOf = fixpoint (==) calc mempty
  where
    calc (ins, outs) = fmap f &&& fmap g $ [0..qnum]
      where
        f i = Set.unions $ catMaybes $ fmap (outs `atMay`) $ maybe [] id $ predsOf i
        g i = gens `Set.union` (maybe mempty id (ins `atMay` i) `Set.difference` kills)
          where
            (gens, kills) = maybe mempty id $ genKillsOf i

calcInAndOut :: [Quadruple] -> ([Set.Set Int], [Set.Set Int])
calcInAndOut quads = calcInAndOut_ (length quads) (flip predOf cfg) (genAndKills `atMay`) 
  where
    (cfg, genAndKills) = buildControlFlowGraphForQuadruple &&& calcGenAndKills $ quads
  
destOf :: Quadruple -> Maybe Temp
destOf quad = case quad of
   BinQ t _ _ _ -> Just t
   UnQ t _ _ -> Just t
   LoadQ t _ -> Just t
   MoveCallQ t _ _ -> Just t
   MoveQ t _ -> Just t
   _ -> Nothing  

srcOf :: Quadruple -> Maybe Var
srcOf quad = case quad of
  MoveQ _ v -> Just v
  _ -> Nothing
