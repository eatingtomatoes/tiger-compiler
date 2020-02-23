{-# LANGUAGE TupleSections #-}

module MiddleEnd.Quadruple.Optimization.EliminateCommonSubexpression
  ( eliminateCommonSubexpression
  ) where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Control.Arrow
import Safe (atMay, foldr1May)
import Data.Maybe (catMaybes)
import Debug.Pretty.Simple

import Translation.Temp
import MiddleEnd.Quadruple
import MiddleEnd.Quadruple.ControlFlowGraph
import MiddleEnd.Quadruple.Optimization.Basic
import MiddleEnd.Tree (BinOp, UnOp)
import MiddleEnd.Optimization
import Utility.Fixpoint
import Utility.Debug

data Expr
  = BinE BinOp Var Var
  | UnE UnOp Var
  | MemE Var
  deriving (Eq, Ord, Show)   
 
data Key
  = TempK Temp
  | MemK 
  deriving (Eq, Ord, Show)

exprOf :: Quadruple -> Maybe Expr
exprOf quad = case quad of
   BinQ _ op v1 v2 -> Just $ BinE op v1 v2
   UnQ _ op v -> Just $ UnE op v
   LoadQ _ v -> Just $ MemE v
   _ -> Nothing

calcTempMemUsesInExpr :: [Quadruple] -> Map.Map Key (Set.Set Int)
calcTempMemUsesInExpr = foldr g mempty . zipWith f [0..]
  where
    f i quad = (i, ) $ case quad of
      BinQ _ _ v1 v2 -> catMaybes $ [toKey v1, toKey v2]
      UnQ _ _ v1 -> catMaybes $ [toKey v1]
      LoadQ _ _ -> [MemK]
      _ -> []
    g (i, ks) table = foldr (Map.alter $ Just . maybe (Set.singleton i) (Set.insert i)) table ks
    toKey var = case var of
      TempV t -> Just $ TempK t
      _ -> Nothing 

calcGenKill :: [Quadruple] -> [(Set.Set Int, Set.Set Int)]
calcGenKill quads = fmap g $ zipWith f [0..] quads
  where
    g (gens, ds, us) = (gens `Set.difference` us, ds `Set.union` us)
    f i quad = case quad of
      BinQ t _ _ _ -> (Set.singleton i, Set.delete i $ seek t defs, seek (TempK t) uses)
      UnQ t _ _ -> (Set.singleton i, Set.delete i $ seek t defs, seek (TempK t) uses)
      LoadQ t _ -> (Set.singleton i, Set.delete i $ seek t defs, seek (TempK t) uses)
      StoreQ _ _ -> (mempty, mempty, seek MemK uses)
      MoveCallQ  t _ _ -> (mempty, Set.delete i $ seek t defs, seek (TempK t) uses)
      MoveQ t _ -> (mempty, Set.delete i $ seek t defs, seek (TempK t) uses)
      _ -> mempty
    seek :: Ord k => k -> Map.Map k (Set.Set Int) -> Set.Set Int
    seek t xs = maybe mempty id $ Map.lookup t xs
    (defs, uses) = calcDefs &&& calcTempMemUsesInExpr $ quads    

calcInOut :: [Quadruple] -> ([Set.Set Int], [Set.Set Int])
calcInOut quads = fixpoint (==) calc mempty
  where
    calc (ins, outs) = (fmap (f . fst) $ zip [0..] quads, zipWith g genKills ins)
      where
        f i = maybe mempty id $ do
          prevs <- predOf i cfg 
          foldr1May Set.intersection $ catMaybes $ fmap (atMay outs) prevs
        g (gens, kills) ins_ = Set.union gens $ ins_ `Set.difference` kills
    genKills = calcGenKill quads
    cfg = buildControlFlowGraphForQuadruple quads 

eliminateCommonSubexpression :: Monad m => PartialOptimizer Quadruple m
eliminateCommonSubexpression trace = buildOptimizer $ return . opt
  where
    opt quads = zipWith f quads $ fmap (foldr g mempty) $ fst $ calcInOut quads
      where 
        g i table = maybe table (\(e, t) -> Map.insert e t table) $ do
          q <- quads `atMay` i
          (,) <$> exprOf q <*> dstOf q
        f q ins = maybe q id $ do
          t <- dstOf q
          e <- exprOf q
          t' <- Map.lookup e ins
          return $ report $ MoveQ t (TempV t')
          where
            report q' = trace "eliminateCommonSubexpression" ("replacing " <> quote q <> " with " <> quote q') q' 
    
