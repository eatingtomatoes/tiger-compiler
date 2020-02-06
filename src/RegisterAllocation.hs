{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module RegisterAllocation
  ( RegisterDist
  , allocRegisters
  )where

import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Control.Monad.Writer
import Data.Maybe (catMaybes)
import Data.List (maximumBy, find, (\\))
import Data.Foldable (toList)
import Data.Ord (comparing)
import Data.Bifunctor
import Debug.Pretty.Simple
import Debug.Trace

import Temp
import Instruction
import Register
import InterferenceGraph
import Graph
import GraphHelper

import Util

type RegisterDist = Map.Map Temp Register

allocRegisters :: [Instruction] -> [Register] -> Either Temp RegisterDist
allocRegisters insts macRegisters = case simplify (length macRegisters) itfg of
  Left rest -> Left $ peek rest
  Right stack -> return $ flip (foldr addLost) (Map.toList table) $ foldl f mempty $ stack
  where
    
    -- itfg = buildInterferenceGraph insts
--
    (table, g) = simplify_ (length macRegisters) mvRelated $ buildInterferenceGraph {- $ pTraceShowId -} insts
    -- (table, g) = (mempty, buildInterferenceGraph $ pTraceShowId insts)
    itfg = {- dump "simplified" -} g

    mvRelated = {- pTraceShowId $ -} movePairs insts
    dump title g' = trace (toGraphviz title g) g'
--    
    peek graph = fst $ maximumBy (comparing snd) $ Map.toList $ Map.map Set.size graph

    f :: RegisterDist -> Temp -> RegisterDist
    f dist t = maybe dist (flip (Map.insert t) dist) $ find (match t) $ macRegisters \\ occupied
      where
        occupied = maybe [] (catMaybes . fmap (flip Map.lookup dist) . toList) $ neighborsOf t itfg

    addLost :: (Temp, Temp) -> RegisterDist -> RegisterDist
    addLost (temp, rep) dist = maybe (error msg) (flip (Map.insert temp) dist) $ Map.lookup rep dist
      where
        msg = "fatal error in register allocRegisters: no available register for " <> show temp <> ", which is replaced with " <> show rep

    match (Temp _) _ = True
    match (Reg "rax") RAX = True
    match (Reg "rdi") RDI = True
    match (Reg "rsi") RSI = True
    match (Reg "rdx") RDX = True
    match (Reg "rcx") RCX = True
    match (Reg "r8") R8 = True
    match (Reg "r9") R9 = True
    match (Reg "r10") R10 = True
    match (Reg "r11") R11 = True
    match (Reg "rsp") RSP = True
    match (Reg "rbp") RBP = True
    match _ _ = False          

simplify :: Int -> InterferenceGraph -> Either InterferenceGraph [Temp]
simplify k g = if Map.null simplified then Right stacked else Left simplified
  where
    (stacked, simplified) = fixpoint eq (uncurry calc) ([], g)
    eq (s1, _) (s2, _) = length s1 == length s2
    calc stack graph  = (stack <> trimed, foldr deleteNode graph trimed)
      where
        trimed = fmap fst $ filter (\tup -> Set.size (snd tup) < k) $ Map.toList graph

movePairs :: [Instruction] -> [(Temp, Temp)]
movePairs insts  = catMaybes $ flip fmap insts $ \case
  Oper True "mov %r, %r" [dst] [src] _ -> Just (dst, src)
  _ -> Nothing
                                 
isSafe :: Int -> Temp{-removed-} -> Temp -> InterferenceGraph -> Bool
isSafe k x@(Temp _) y g = apart && sparse
  where
    f n = maybe False (>= k) (neighborsNumOf n g)
    apart = not $ isNeighborOf x y g
    sparse = maybe True (< k) $ do
      xns <- neighborsOf x g
      yns <- neighborsOf y g
      let ns = Set.union xns yns
      return $ Set.size (Set.filter f ns)
isSafe _ _ _ _ = False


simplify_ :: Int -> [(Temp, Temp)] -> InterferenceGraph -> (Map.Map Temp{-removed-} Temp, InterferenceGraph)
simplify_ k pairs graph = (foldr f mempty replacements, g)
  where
    (g, replacements) = runWriter $ simplify' k pairs graph
    f (x, y) table = Map.insert x (maybe y id $ Map.lookup y table) table
  
simplify' :: Int -> [(Temp, Temp)] -> InterferenceGraph -> Writer [(Temp{-Removed-}, Temp)] InterferenceGraph
simplify' _ [] g = return g
simplify' k ((x, y) : rest) g
  | isSafe k x y g = do
      tell $ {- pTrace ("replace " <> show x <> " -> " <> show y) -} [(x, y)]
      uncurry (simplify' k) $ replace x y rest g
  | isSafe k y x g = do
      tell $ {- pTrace ("replace " <> show y <> " -> " <> show x) -} [(y, x)]
      uncurry (simplify' k) $ replace y x rest g      
  | otherwise = simplify' k rest g
  where
    replace :: Temp{-removed-} -> Temp -> [(Temp, Temp)] -> InterferenceGraph -> ([(Temp, Temp)], InterferenceGraph)
    replace x' y' pairs graph = (pairs', graph')
      where
        rep a = if a == x' then y' else a
        pairs' = filter (\p -> fst p /= snd p) $ fmap (bimap rep rep) pairs
        graph' = foldr f graph xns
          where
            xns = maybe mempty id (neighborsOf x' graph)
            f n gr = connect y' n $ deleteNode x' $ disconnect x' n gr

