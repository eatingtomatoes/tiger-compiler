{-# LANGUAGE LambdaCase #-}

module MiddleEnd.Quadruple.Loop.LoopNestForest
  ( FlattenStrategy (..)
  , calcLoopHeaders
  , buildLoopNestForest
  , buildLoopNestTree
  , flattenLoopNestTree
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Safe (headMay)
import Debug.Pretty.Simple
import Utility.Debug

import MiddleEnd.Quadruple.Loop.Dominance
 
type LoopNestForest = Map.Map Int (Set.Set Int{-exit-node-}, Set.Set Int{-nested loop-})
  
-- calcLoopHeaders :: (Int -> Maybe [Int]) -> Map.Map Int (Set.Set Int) -> Set.Set Int
-- calcLoopHeaders cfgSuccOf dominated = Set.fromList $ fmap fst $ filter f $ Map.toList dominated
--   where
--     f (i, doms) = any (maybe False (elem i) . cfgSuccOf) doms

calcLoopHeaders :: (Int -> Maybe [Int]) -> DominanceGraph -> Set.Set Int
calcLoopHeaders cfgSuccOf graph = Set.fromList $ fmap fst $ filter f $ flattenSucc graph
  where
    f (i, doms) = any (maybe False (elem i) . cfgSuccOf) doms


buildLoopNestForest :: Set.Set Int -> (Int -> Maybe (Set.Set Int))-> DominanceTree -> LoopNestForest
buildLoopNestForest loopHeaders cfgSuccOf tree = calc [-1] 0 (Map.singleton (-1) mempty)
  where
    calc :: [Int] -> Int -> LoopNestForest -> LoopNestForest
    calc ctx{-outer loop headers-} i{-instruction id-} forest
      = maybe forest' (foldr (calc ctx') forest') $ (succOf i tree :: Maybe (Set.Set Int))
      where 
        (forest', ctx') = case Set.member i loopHeaders of
          True -> (Map.insert i mempty $ maybe id (Map.alter $ f i) (headMay ctx) forest, i : ctx)
          _ -> case fmap (Set.lookupMin . Set.intersection loopHeaders) $ cfgSuccOf i of
            Just (Just h)
              | elem h ctx -> (Map.alter (g i) h forest, dropWhile (/= h) ctx)
              -- | otherwise -> error $ "buildLoopNestForest: encountered a loop tail "
              --   <> quote i
              --   <> " whose corresponding header "
              --   <> quote h
              --   <> " wasn't in the ctx"
            _ -> (forest, ctx)
            
    f i Nothing = error $ "unexpected loop header " <> quote i <> " in ctx but forest in buildLoopNestForest"
    f i (Just (exits, nested)) = Just (exits, Set.insert i nested)

    g i Nothing = error $ "unexpected loop header " <> quote i <> " in ctx but forest in buildLoopNestForest"
    g i (Just (exits, nested)) = Just (Set.insert i exits, nested)

data LoopNestTree
  = LoopNestTree (Maybe (Int, [Int])) [LoopNestTree]
  deriving (Show)

buildLoopNestTree :: LoopNestForest -> LoopNestTree
buildLoopNestTree forest = calc (-1)
  where
    calc i = case Map.lookup i forest of
      Just (exits, nested) -> LoopNestTree (desc $ Set.toList exits) $ fmap calc $ Set.toList nested
      _ -> error $ "loop header `" <> show i <> "' not found in forest in buildLoopNestTree"
      where
        desc exits = case exits of
          [] -> Nothing
          _ -> Just (i, exits)

data FlattenStrategy
  = InnerFirst
  | OutterFirst
  deriving (Show)

flattenLoopNestTree :: FlattenStrategy -> LoopNestTree -> [(Int, [Int])]
flattenLoopNestTree InnerFirst (LoopNestTree desc nested)
  = concatMap (flattenLoopNestTree InnerFirst) nested <> maybe mempty pure desc
flattenLoopNestTree OutterFirst (LoopNestTree desc nested)
  = maybe id (:) desc $ concatMap (flattenLoopNestTree InnerFirst) nested 
