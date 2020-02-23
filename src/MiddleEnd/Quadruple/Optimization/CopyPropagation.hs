{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module MiddleEnd.Quadruple.Optimization.CopyPropagation
  ( copyPropagation
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow
import Safe (atMay, foldr1May, headMay)
import Data.Maybe (catMaybes)

import Translation.Temp
import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.ControlFlowGraph
import MiddleEnd.Optimization
import Utility.Fixpoint
import Utility.Debug

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

calcUses :: [Quadruple] -> Map.Map Temp (Set.Set Int)
calcUses = foldr f mempty . zip [0..]
  where
    f (i, quad) = case quad of
      MoveQ _ (TempV t) -> Map.alter (Just . maybe (Set.singleton i) (Set.insert i)) t
      _ -> id

calcGenKill :: [Quadruple] -> ([Set.Set Int], [Set.Set Int])
calcGenKill quads = unzip $ zipWith f [0..] quads
  where
    f i quad = case quad of
      BinQ t _ _ _ -> (mempty, kill t)
      UnQ t _ _ -> (mempty, kill t)
      LoadQ t _ -> (mempty, kill t)
      MoveCallQ  t _ _ -> (mempty, kill t)
      MoveQ t _ -> (Set.singleton i, Set.delete i $ kill t)
      _ -> mempty
    kill t = Set.union (seek t defs) (seek t uses)
    seek t xs = maybe mempty id $ Map.lookup t xs
    (defs, uses) = calcDefs &&& calcUses $ quads    

calcInOut :: [Quadruple] -> ([Set.Set Int], [Set.Set Int])
calcInOut quads = fixpoint (==) calc mempty
  where
    calc (ins, outs) = fmap f &&& fmap g $ zipWith const [0..] quads
      where
        f i = maybe mempty id $ do
          prevs <- predOf i cfg
          foldr1May Set.intersection $ catMaybes $ fmap (atMay outs) prevs
        g i = Set.union (gens ! i) $ (ins ! i) `Set.difference` (kills ! i)
    (gens, kills) = calcGenKill quads
    cfg = buildControlFlowGraphForQuadruple quads
    xs ! i = maybe mempty id $ atMay xs i

copyPropagation :: Monad m => PartialOptimizer Quadruple m
copyPropagation trace = buildOptimizer $ \quads -> do 
  return $ zipWith (f (atMay quads)) quads $ fmap Set.toList $ fst $ calcInOut quads
  where
    f quadAt quad ins = case quad of
      BinQ t op v1 v2 -> case (rep v1, rep v2) of
        (Just r1, Just r2) -> pr v1 r1 $ pr v2 r2 $ BinQ t op r1 r2
        (Just r1, Nothing) -> pr v1 r1 $ BinQ t op r1 v2 
        (Nothing, Just r2) -> pr v2 r2 $ BinQ t op v1 r2
        _ -> quad
      UnQ t op v1 -> case rep v1 of
        Just r1 -> pr v1 r1 $ UnQ t op r1
        _ -> quad
      CJumpQ op v1 v2 l1 l2 -> case (rep v1, rep v2) of
        (Just r1, Just r2) -> pr v1 r1 $ pr v2 r2 $ CJumpQ op r1 r2 l1 l2
        (Just r1, Nothing) -> pr v1 r1 $ CJumpQ op r1 v2 l1 l2
        (Nothing, Just r2) -> pr v2 r2 $ CJumpQ op v1 r2 l1 l2
        _ -> quad
      LoadQ t v1 -> case rep v1 of
        Just r1 -> pr v1 r1 $ LoadQ t r1
        _ -> quad
      StoreQ v1 v2 -> case (rep v1, rep v2) of
        (Just r1, Just r2) -> pr v1 r1 $ pr v2 r2 $ StoreQ r1 r2
        (Just r1, Nothing) -> pr v1 r1 $ StoreQ r1 v2
        (Nothing, Just r2) -> pr v2 r2 $ StoreQ v1 r2
        _ -> quad
      CallQ l args -> CallQ l
        $ fmap (\arg -> maybe arg (\r -> pr arg r r) $ rep arg) args
      MoveCallQ t l args -> MoveCallQ t l
        $ fmap (\arg -> maybe arg (\r -> pr arg r r) $ rep arg) args
      MoveQ t v1 -> case rep v1 of
        Just r1 -> pr v1 r1 $ MoveQ t r1
        _ -> quad
      _ -> quad      
      where
        rep v = case v of
          TempV t -> headMay $ catMaybes $ fmap (g t) ins
          _ -> Nothing
          where
            g t i = case quadAt i of
              Just (MoveQ t1 v1) | t1 == t -> Just v1
              _ -> Nothing
        pr x y z = flip (trace "copyPropagation") z
          $ "replacing "
          <> quote x
          <> " with "
          <> quote y
          <> " in "
          <> quote quad

