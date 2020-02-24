{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Translation.SSAToQuadruple
  ( transSSAToQuadruple
  ) where


import qualified Data.Map as Map
import qualified Data.Set as Set
import Debug.Pretty.Simple
import Data.Maybe (fromMaybe)
import Data.Foldable
import Control.Monad.Loops
import Control.Monad.State
import Control.Monad.Except
import Safe (headMay, lastMay)
import Control.Lens (makeLenses, modifying, over, _1, _2, view, uses, zoom)
import Data.List (sortOn)
import Text.Printf

import Translation.Temp
import MiddleEnd.SSA.Quadruple 
import MiddleEnd.SSA.Quadruple.BlockControlFlowGraph
import qualified MiddleEnd.Quadruple as Q
import Utility.Bucket
import Utility.Debug

transSSAToQuadruple :: Monad m => [Quadruple] -> ExceptT String (StateT TempPool m) [Q.Quadruple]
transSSAToQuadruple quads = do
  tempPool <- get
  let (qs, (tempPool', _)) = runState (transSSAToQuadruple' quads) (tempPool, mempty)
  put tempPool'
  return qs

transSSAToQuadruple' :: [Quadruple] -> State (TempPool, Map.Map TaggedTemp Temp) [Q.Quadruple]
transSSAToQuadruple' quads = mapM toQuadruple $ normalize quads
  where
    toQuadruple :: Quadruple -> State (TempPool, Map.Map TaggedTemp Temp) Q.Quadruple
    toQuadruple q = case q of
      BinQ t op v1 v2 -> do
        t' <- rept t
        v1' <- repv v1
        v2' <- repv v2
        return $ Q.BinQ t' op v1' v2'
      UnQ t op v1 -> do
        t' <- rept t
        v1' <- repv v1
        return $ Q.UnQ t' op v1'
      MoveCallQ t l args -> do
        t' <- rept t
        args' <- mapM repv args
        return $ Q.MoveCallQ t' l args'
      MoveQ t v1 -> do
        t' <- rept t
        v1' <- repv v1
        return $ Q.MoveQ t' v1'
      JumpQ l -> return $ Q.JumpQ l
      CJumpQ op v1 v2 l1 l2 -> do
        v1' <- repv v1
        v2' <- repv v2
        return $ Q.CJumpQ op v1' v2' l1 l2
      LabelQ l -> return $ Q.LabelQ l
      LoadQ t v1 -> do
        t' <- rept t
        v1' <- repv v1
        return $ Q.LoadQ t' v1'
      StoreQ v1 v2 -> do
        v1' <- repv v1
        v2' <- repv v2
        return $ Q.StoreQ v1' v2'
      CallQ l args -> do
        args' <- mapM repv args
        return $ Q.CallQ l args'
      _ -> error $ "transSSAToQuadruple:toQuadruple: unhandled quadruple " <> quote q
      where
        rept :: TaggedTemp -> State (TempPool, Map.Map TaggedTemp Temp) Temp
        rept t = do
          case t of
            TaggedTemp r@(Reg _) _ -> return r
            _ -> do
              uses _2 (Map.lookup t) >>= \case
                Just qt -> return qt
                _ -> do
                  qt <- zoom _1 allocTemp
                  modifying _2 $ Map.insert t qt
                  return qt
        repv :: Var -> State (TempPool, Map.Map TaggedTemp Temp) Q.Var
        repv v = case v of
          TempV t -> fmap Q.TempV $ rept t
          ConstV c -> return $ Q.ConstV c
          LabelV l -> return $ Q.LabelV l

normalize :: [Quadruple] -> [Quadruple]
normalize quads = qs
  where
    qs = filter (not . isPhiQ) $ concat $ fmap snd $ sortOn fst $ fmap f $ (flattenData bcfg :: [(Int, [Quadruple])])
      where
        f (blockId, blockQuads) = (blockId, ) $ case assocOf blockId assignments of
          Nothing -> blockQuads
          -- Just pairs -> blockQuads <> fmap g (Set.toList pairs)
          Just pairs -> case lastMay blockQuads of
            Just q@(JumpQ _ ) -> init blockQuads <> fmap g (Set.toList pairs) <> [q]
            -- !!Here might be a bug!!!
            Just q@(CJumpQ _ _ _ _ _) -> init blockQuads <> fmap g (Set.toList pairs) <> [q]
            _ -> blockQuads <> fmap g (Set.toList pairs)
        g (dst, src) = MoveQ dst (TempV src)
    bcfg = buildBlockControlFlowGraphForQuadruple quads
    
    assignments :: Bucket Int (TaggedTemp{-dst-}, TaggedTemp{-src-})
    assignments = flip execState mempty $ forM_ (flattenData bcfg :: [(Int, [Quadruple])]) $ \(sid, sqs) -> do 
      let predcessors = fromMaybe mempty $ predOf sid bcfg
      forM_ sqs $ \case
        PhiQ t args -> do
          forM_ (zip predcessors args) $ \(pid, arg) -> do
            modify $ insert pid (t, arg)
        _ -> return ()
