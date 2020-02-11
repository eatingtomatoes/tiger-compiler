{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module InterferenceGraph
  ( InterferenceGraph
  , buildInterferenceGraph
  , coalesceTemp
  ) where

import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
import Control.Monad.Writer
import Data.Maybe (catMaybes)
import Data.Bifunctor (bimap)
import qualified Data.DList as DList
import Debug.Pretty.Simple

import Graph
import Temp
import PseudoInst
import Liveness

type InterferenceGraph = Graph Temp

buildInterferenceGraph :: [PseudoInst Temp] -> InterferenceGraph
buildInterferenceGraph insts = uncurry (buildInterferenceGraph_ insts) $ calcInsOuts insts

buildInterferenceGraph_ :: [PseudoInst Temp] -> [Set.Set Temp] -> [Set.Set Temp] -> InterferenceGraph
buildInterferenceGraph_ insts ins outs = addExtra $ foldr connectGroup mempty ins
  where
    addExtra g = foldr f g (zip insts outs) 
      where
        f (inst, out) g' = foldr (uncurry connect) g' $ Set.cartesianProduct dsts neighbors 
          where
            neighbors = (if isMove inst then flip Set.difference srcs else id) out
            (dsts, srcs) = defsAndUses inst

coalesceTemp :: Int -> [PseudoInst Temp] -> InterferenceGraph -> ([(Temp, Temp)], InterferenceGraph)
coalesceTemp usableRegisters insts graph = (Map.toList $ foldr f mempty replacements, coalesced)
  where
    (coalesced, replacements) = runWriter $ calc moveRelatedPairs graph
    f (x, y) table = Map.insert x (maybe y id $ Map.lookup y table) table      

    moveRelatedPairs = catMaybes $ fmap f' insts
      where
        f' PseudoInst{..} = case (_piOperator, _piAddrPattern) of
          (Mov, RR dst src) -> Just (dst, src)
          _ -> Nothing

    calc :: [(Temp, Temp)] -> InterferenceGraph -> Writer (DList.DList (Temp{-Removed-}, Temp)) InterferenceGraph
    calc [] g = return g
    calc ((x, y) : rest) g
      | isSafe x y = do
          tell $ DList.singleton (x, y)
          uncurry calc $ replace x y
      | isSafe y x = do
          tell $ DList.singleton (y, x)
          uncurry calc $ replace y x
      | otherwise = calc rest g
      where
        replace :: Temp -> Temp -> ([(Temp, Temp)], InterferenceGraph)
        replace a b = {-pTrace ("replace " <> show a <> "-->" <> show b)-} (pairs, mergeTo a b g) 
          where
            rep c = if c == a then b else c
            pairs = filter (\p -> fst p /= snd p) $ fmap (bimap rep rep) rest

        isSafe :: Temp{-to be replaced-} -> Temp{-replacement-} -> Bool
        isSafe a b = isTemp a && (not $ isNeighbors a b g) && sparse
          where
            sparse = maybe True (< usableRegisters) $ do
              neighbors <- Set.union <$> neighborsOf a g <*> neighborsOf b g
              return $ Set.size $ Set.filter (maybe False (>= usableRegisters) . flip neighborsNumOf g) neighbors
