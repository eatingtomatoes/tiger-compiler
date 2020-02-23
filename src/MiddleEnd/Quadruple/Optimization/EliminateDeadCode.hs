{-# LANGUAGE TupleSections #-}

module MiddleEnd.Quadruple.Optimization.EliminateDeadCode
  ( eliminateDeadCode
  ) where

import qualified Data.Set as Set
import Data.Maybe (catMaybes)
import Safe (atMay)
import Control.Arrow

import Translation.Temp
import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.ControlFlowGraph
import Utility.Fixpoint
import MiddleEnd.Optimization
import Utility.Debug

calcGenAndKills :: [Quadruple] -> [(Set.Set Temp, Set.Set Temp)]
calcGenAndKills quads = fmap f quads
  where
    f quad = case quad of
      BinQ t _ v1 v2 -> (, Set.singleton t) $ case (v1, v2) of
        (TempV t1, TempV t2) -> Set.fromList [t1, t2]
        (TempV t1, _) -> Set.singleton t1
        (_, TempV t2) -> Set.singleton t2
        _ -> mempty
      UnQ t _ v1 -> (, Set.singleton t) $ case v1 of
        TempV t1 -> Set.singleton t1
        _ -> mempty
      CJumpQ _ v1 v2 _ _ -> (, mempty) $ case (v1, v2) of
        (TempV t1, TempV t2) -> Set.fromList [t1, t2]
        (TempV t1, _) -> Set.singleton t1
        (_, TempV t2) -> Set.singleton t2
        _ -> mempty
      LoadQ t v1 -> (, Set.singleton t) $ case v1 of
        TempV t1 -> Set.singleton t1
        _ -> mempty
      StoreQ v1 v2 -> (, mempty) $ case (v1, v2) of
        (TempV t1, TempV t2) -> Set.fromList [t1, t2]
        (TempV t1, _) -> Set.singleton t1
        (_, TempV t2) -> Set.singleton t2
        _ -> mempty
      CallQ _ args -> (Set.fromList $ catMaybes $ fmap g args, mempty)
      MoveCallQ t _ args -> (Set.fromList $ catMaybes $ fmap g args, Set.singleton t)
      MoveQ t v1 -> (, Set.singleton t) $  case v1 of
        TempV t1 -> Set.singleton t1
        _ -> mempty
      _ -> mempty
      where
        g (TempV t) = Just t
        g _ = Nothing

calcInAndOut :: [Quadruple] -> ([Set.Set Temp], [Set.Set Temp])
calcInAndOut quads = fixpoint (==) calc mempty
  where
    genAndKills = calcGenAndKills quads
    cfg = buildControlFlowGraphForQuadruple quads
    calc :: ([Set.Set Temp], [Set.Set Temp]) -> ([Set.Set Temp], [Set.Set Temp])
    calc (ins, outs) = fmap f &&& fmap g $ [0..length quads]
      where
        f i = gens `Set.union` (maybe mempty id (outs `atMay` i) `Set.difference` kills)
          where
           (gens, kills) = maybe mempty id $ genAndKills `atMay` i
        g i = Set.unions $ fmap h $ maybe mempty id $ (succOf i cfg :: Maybe [Int])
          where
            h k = maybe mempty id $ ins `atMay` k
              
eliminateDeadCode :: Monad m => PartialOptimizer Quadruple m
eliminateDeadCode trace = buildOptimizer $ return . opt
  where
    opt quads = fmap fst $ filter f $ zip quads outs
      where
        outs = snd $ calcInAndOut quads
        f (quad, out) = pr $ maybe True (\t -> isReg t || Set.member t out) $ case quad of
          BinQ t _ _ _ -> Just t
          UnQ t _ _ -> Just t
          LoadQ t _ -> Just t
          MoveQ t _ -> Just t
          _ -> Nothing
          where
            pr s = if s then s else trace "eliminateDeadCode" ("eliminating " <> quote quad) s
    
    
