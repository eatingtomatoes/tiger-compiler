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
import Data.Maybe (catMaybes)
import Data.List (maximumBy, find, (\\))
import Data.Foldable (toList)
import Data.Ord (comparing)
import Debug.Pretty.Simple
import Debug.Trace
import Data.List (sort)

import Temp
import Instruction
import Register
import InterferenceGraph
import Graph
import GraphHelper

import Util

type RegisterDist = Map.Map Temp Register

allocRegisters :: [Instruction] -> Either Temp RegisterDist 
allocRegisters insts = case simplify (length machineRegisters) $ traceGraph "simplified" itfg of
  Left rest -> Left $ peek $ traceGraph "rest" rest
  Right stack -> return $ flip (foldr addLost) table $ foldl f mempty $ pTraceShowId stack
  where
    (table, itfg) = coalesceTemp (length machineRegisters) insts $ buildInterferenceGraph $ pTraceShowId insts 
    peek graph = fst $ maximumBy (comparing snd) $ filter (isTemp . fst) $ Map.toList $ Map.map Set.size graph

    f :: RegisterDist -> Temp -> RegisterDist
    f dist t = case find (match t) $ machineRegisters \\ occupied of
      Just register -> Map.insert t register dist
      _ -> if isTemp t then dist else error $ "allocRegisters: no register available for " <> show t
      where
        occupied = maybe [] (catMaybes . fmap (flip Map.lookup dist) . toList) $ neighborsOf t itfg

    addLost :: (Temp, Temp) -> RegisterDist -> RegisterDist
    addLost (replaced, replacement) dist = case Map.lookup replacement dist of
      Just register -> Map.insert replaced register dist
      _ -> error msg
      where
        msg = "allocRegisters: no available register for " <> show replaced <> ", which is replaced with " <> show replacement


machineRegisters :: [Register]
machineRegisters =
  [ RAX
  , RBX
  , RCX
  , RDX
  , RDI  
  , RSI
  , RBP
  , RSP
  , R8
  , R9
  , R10
  , R11
  , R12
  , R13
  , R14
  , R15
  ]

match :: Temp -> Register -> Bool
match (Temp _) _ = True
match (Reg "rax") RAX = True
match (Reg "rbx") RBX = True
match (Reg "rcx") RCX = True
match (Reg "rdx") RDX = True
match (Reg "rdi") RDI = True
match (Reg "rsi") RSI = True
match (Reg "r8") R8 = True
match (Reg "r9") R9 = True
match (Reg "r10") R10 = True
match (Reg "r11") R11 = True
match (Reg "r12") R12 = True
match (Reg "r13") R13 = True
match (Reg "r14") R14 = True
match (Reg "r15") R15 = True
match (Reg "rsp") RSP = True
match (Reg "rbp") RBP = True
match _ _ = False          

-- simplify :: Int -> InterferenceGraph -> Either InterferenceGraph [Temp]
-- simplify k g = if Map.null simplified then Right stacked else Left simplified
--   where
--     (stacked, simplified) = fixpoint eq (uncurry calc) ([], g)
--     eq (s1, _) (s2, _) = length s1 == length s2
--     calc stack graph  = (sort trimed <> stack, foldr deleteNode graph trimed)
--       where
--         trimed = fmap fst $ filter (\tup -> Set.size (snd tup) < k) $ Map.toList graph


simplify :: Int -> InterferenceGraph -> Either InterferenceGraph [Temp]
simplify k g = if all isReg (nodesOf simplified) then Right (nodesOf simplified <> stacked) else Left simplified
  where
    (stacked, simplified) = fixpoint eq (uncurry calc) ([], g)
    eq (s1, _) (s2, _) = length s1 == length s2
    calc stack graph  = (sort trimed <> stack, foldr deleteNode graph trimed)
      where
        trimed = fmap fst $ filter (\(x, ns) -> isTemp x && Set.size ns < k) $ Map.toList graph
