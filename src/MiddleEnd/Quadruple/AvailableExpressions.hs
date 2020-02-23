{-# LANGUAGE FlexibleContexts #-}

module MiddleEnd.Quadruple.AvailableExpressions where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad.State
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Safe (atMay)
import Control.Arrow

import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.ControlFlowGraph
import MiddleEnd.Tree
import Translation.Temp

import Utility.Fixpoint

data Expression
  = BinE BinOp Var Var
  | UnE UnOp Var
  | MemE Var
  deriving (Eq, Ord, Show)

data Key
  = TempK Temp
  | MemK
  deriving (Eq, Ord, Show)

calcGenAndKills
  :: [Quadruple]
  -> [(Set.Set Expression, Set.Set Expression)]
calcGenAndKills quads = fmap f quads
  where
    f quad = g $ case quad of
      BinQ t op v1 v2 -> (Set.singleton (BinE op v1 v2), killsOf $ TempK t)
      UnQ t op v -> (Set.singleton (UnE op v), killsOf $ TempK t)
      LoadQ t v -> (Set.singleton (MemE v), killsOf $ TempK t)
      StoreQ _ _ -> (mempty, killsOf MemK)
      CallQ _ _ -> (mempty, killsOf MemK)
      MoveCallQ t _ _ -> (mempty, killsOf (TempK t) `Set.union`killsOf MemK)
      MoveQ t _ -> (mempty, killsOf $ TempK t)
      _ -> mempty
      where
        g (gens, kills) = (gens `Set.difference` kills, kills)
    killsOf k = fromMaybe mempty $ Map.lookup k kills
      where
        kills = execState (mapM g quads) Map.empty
        g quad = case quad of
          BinQ _ op v1 v2 -> do
            case v1 of
              TempV t -> note (TempK t) $ BinE op v1 v2
              _ -> return ()
            case v2 of
              TempV t -> note (TempK t) $ BinE op v1 v2
              _ -> return ()
          UnQ _ op v@(TempV t) -> note (TempK t) $ UnE op v
          LoadQ _ v@(TempV _) -> note MemK $ MemE v
          _ -> return ()
        note key e = modify $ Map.alter (Just . maybe (Set.singleton e) (Set.insert e)) key

calcInAndOut :: [Quadruple] -> ([Set.Set Expression], [Set.Set Expression])
calcInAndOut quads = fixpoint (==) calc initial
  where
    genAndKills = calcGenAndKills quads
    allExprs = Set.unions $ map fst genAndKills
    initial = (mempty : repeat allExprs, repeat allExprs)
    calc (ins, outs) = fmap f &&& fmap g $ [0..length quads]
      where
        f i = maybe mempty (foldr1 Set.intersection)
          $ maybeEmpty
          $ mapMaybe (outs `atMay`)
          $ fromMaybe [] $ predOf i cfg
        g i = gens `Set.union` (fromMaybe mempty (ins `atMay` i) `Set.difference` kills )
          where
            (gens, kills) = fromMaybe mempty $ genAndKills `atMay` i
        maybeEmpty [] = Nothing
        maybeEmpty xs = Just xs
    cfg = buildControlFlowGraphForQuadruple quads

exprOf :: Quadruple -> Maybe Expression
exprOf quad = case quad of
   BinQ _ op v1 v2 -> Just $ BinE op v1 v2
   UnQ _ op v -> Just $ UnE op v
   LoadQ _ v -> Just $ MemE v
   _ -> Nothing
