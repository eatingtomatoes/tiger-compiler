{-# LANGUAGE LambdaCase #-} 
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}

module Translation.QuadrupleToSSA
  ( transQuadrupleToSSA
  ) where

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe (catMaybes, mapMaybe, fromMaybe)
import Data.Foldable
import Control.Monad.Loops
import Control.Monad.State
import Safe (headMay)
import Control.Lens (makeLenses, makeLensesFor, modifying, over,  _2, view)
import Data.List (sortOn)
import Text.Printf 
  
import MiddleEnd.Quadruple
import Translation.Temp
import MiddleEnd.Quadruple.ControlFlowGraph 
import MiddleEnd.Quadruple.Loop.Dominance
import MiddleEnd.Quadruple.BlockControlFlowGraph
import MiddleEnd.Quadruple.ReachingDefinition
import qualified MiddleEnd.SSA.Quadruple as SQ

import qualified Utility.Bucket as Bucket

data IBlock
  = IBlock
  { _ibQuads :: [Either Quadruple SQ.Quadruple]
  , _ibPhis :: [(Either Temp SQ.TaggedTemp, Map.Map Int SQ.TaggedTemp)]
  } deriving (Show)

data RenamingContext
  = RenamingContext
  { _rcCounter :: Map.Map Temp (Int, [Int])
  , _rcBlocks :: Map.Map Int IBlock
  } deriving (Show)

makeLensesFor [("_ibPhis", "ibPhis")] ''IBlock
makeLenses ''RenamingContext

transQuadrupleToSSA :: [Quadruple] -> [SQ.Quadruple]
transQuadrupleToSSA quads = placePhiFunction dominanceTree bcfg phiVars
  where
    bcfg = buildBlockControlFlowGraphForQuadruple quads    
    dominanceTree = buildDominanceTree $ buildDominanceGraph (length quads) (flip predOf bcfg)
    phiVars = calcPhiVars bcfg $ calcDominanceFrontier (flip succOf bcfg) (flip succOf dominanceTree)

placePhiFunction :: DominanceTree -> BlockControlFlowGraph Quadruple -> Bucket.Bucket Int Temp -> [SQ.Quadruple]
placePhiFunction dominatorTree bcfg phiVars
  = concat
  $ fmap (blockToQuads . snd)
  $ sortOn fst
  $ Map.toList
  $ _rcBlocks
  $ execState (rename 0) RenamingContext
  { _rcCounter = mempty
  , _rcBlocks = toIBlocks bcfg phiVars
  }
  where
    blockToQuads IBlock{..} = case fmap g _ibQuads of
      l@(SQ.LabelQ _) : rest -> l : (fmap f _ibPhis <> rest)
      qs -> fmap f _ibPhis <> qs
      where
        f (et, ts) = case et of
          Left _ -> error $ "unhandled phi `" <> show (et, ts) <> "'in blockToQuads"
          Right pt -> SQ.PhiQ  pt $ fmap snd $ sortOn fst  $ Map.toList ts
        g eq = case eq of
          Left _ -> error $ printf "unhandled quadruple `%s'in blockToQuads" (show eq)
          Right q -> q
    
    rename :: Int -> State RenamingContext ()
    rename i = do
      gets (Map.lookup i . _rcBlocks) >>= \case
        Nothing -> error $ "encountered an unknown block `" <> show i <> "` in rename"
        Just IBlock{..} -> do
          (ts1, ps) <- fmap unzip $ mapM repPhi _ibPhis
          (ts2, qs) <- do
            (ts, qs) <- fmap unzip $ mapM repQuad _ibQuads             
            return $ (catMaybes ts, qs)
          modifying rcBlocks $ Map.insert i (IBlock qs ps) 
            
          forM_ (fromMaybe mempty $ succOf i bcfg :: Set.Set Int) $ \next -> do
            gets (Map.lookup next . _rcBlocks) >>= \case
              Nothing -> error $ "encountered an unknown block `" <> show next <> "`"
              Just iblock -> do
                let pos = case predOf next bcfg of
                      Nothing -> error $ printf "encountered a successor(`%s') of `%s' which is not in the block control flow graph in rename" (show next) (show i)
                      Just prevs ->do
                        case headMay $ filter ((== i) . snd) $ zip [(0 :: Int)..] prevs of
                          Nothing -> error $ printf "encountered a successor(`%s') of `%s' whose predecessors set doesn't include `%s' in rename" (show next) (show i) (show i)
                          Just (k, _) -> k
                ps' <- mapM (addVar pos) $ view ibPhis iblock 
                modifying rcBlocks $ Map.insert next $ iblock { _ibPhis = ps' }

          mapM_ rename (fromMaybe mempty $ succOf i dominatorTree :: Set.Set Int)

          mapM_ popt $ ts1 <> ts2
      where
        addVar pos (t, ts) = do
          case Map.lookup pos ts of
            Nothing -> do
              t' <- rept $ case t of
                Left t' -> t'
                Right (SQ.TaggedTemp t' _) -> t'
              return (t, Map.insert pos t' ts)
            Just _ -> error $ printf "the `%s'var has been replaced in addVar" (show t) 

        repPhi (et, ts) = case et of
          Left t -> do
            t' <- def t
            return $ (t, (Right t', ts))
          _ -> error $ "encountered an unexpected phi in repPhi"

        repQuad (Right _) = error $ "unexpected replaced quadruple in repQuad"
        repQuad (Left q) = fmap (over _2 Right) $ do
          case q of
            BinQ t op v1 v2 -> do
              v1' <- repv v1
              v2' <- repv v2
              t' <- def t
              return $ (Just t, SQ.BinQ t' op v1' v2')
            UnQ t op v1 -> do
              v1' <- repv v1
              t' <- def t
              return $ (Just t, SQ.UnQ t' op v1')
            JumpQ l -> return $ (Nothing, SQ.JumpQ l)
            CJumpQ op v1 v2 l1 l2 -> do
              v1' <- repv v1
              v2' <- repv v2
              return $ (Nothing, SQ.CJumpQ op v1' v2' l1 l2)
            LabelQ l -> return $ (Nothing, SQ.LabelQ l)
            LoadQ t v1 -> do
              v1' <- repv v1
              t' <- def t
              return $ (Just t, SQ.LoadQ t' v1')
            StoreQ v1 v2 -> do
              v1' <- repv v1
              v2' <- repv v2
              return $ (Nothing, SQ.StoreQ v1' v2')
            CallQ l vs -> do
              vs' <- mapM repv vs
              return $ (Nothing, SQ.CallQ l vs')
            MoveCallQ t l vs -> do
              vs' <- mapM repv vs
              t' <- def t
              return $ (Just t, SQ.MoveCallQ t' l vs')
            MoveQ t v1 -> do
              v1' <- repv v1
              t' <- def t
              return $ (Just t, SQ.MoveQ t' v1')
              
        repv v = case v of
            ConstV c -> return $ SQ.ConstV c
            LabelV l -> return $ SQ.LabelV l
            TempV t -> fmap SQ.TempV $ rept t

        rept :: Temp -> State RenamingContext SQ.TaggedTemp
        rept t = do
          (_, stack) <- counterOf t
          return $ SQ.TaggedTemp t $ maybe 0 id $ headMay stack 
                
        def t = do
          (counter, stack) <- counterOf t
          let c = counter + 1
          modifying rcCounter (Map.insert t (c, c : stack))
          return $ SQ.TaggedTemp t c          

        popt t = do
          (counter, stack) <- counterOf t
          modifying rcCounter $ Map.insert t (counter, tail stack)
 
        counterOf t = gets $ maybe (0, mempty) id . Map.lookup t . _rcCounter

toIBlocks :: BlockControlFlowGraph Quadruple -> Bucket.Bucket Int Temp -> Map.Map Int IBlock
toIBlocks bcfg phiVars = Map.fromList $ fmap f $ (flattenData bcfg :: [(Int, [Quadruple])])
  where
    f (i, qs) = (i, IBlock { _ibQuads = qs', _ibPhis = ps' })
      where
        qs' = fmap Left qs
        ps' :: [(Either Temp SQ.TaggedTemp, Map.Map Int SQ.TaggedTemp)]
        ps' = maybe mempty g $ Bucket.assocOf i phiVars
          where
            g vars = fmap (, mempty) $ fmap Left $ toList vars
    
-- calcPhiVars :: BlockControlFlowGraph Quadruple -> Bucket.Bucket Int Int -> Map.Map Int{-block-} (Set.Set Temp){-phi funs needs inserting-}
calcPhiVars :: BlockControlFlowGraph Quadruple -> Bucket.Bucket Int Int -> Bucket.Bucket Int Temp{-phi funs needs inserting-}
calcPhiVars bcfg frontiers = foldr f mempty $ Bucket.toList initialDefsites
  where
    -- f :: (Temp, Set.Set Int) -> Map.Map Int (Set.Set Temp) -> Map.Map Int (Set.Set Temp)
    f :: (Temp, Set.Set Int) -> Bucket.Bucket Int Temp -> Bucket.Bucket Int Temp 
    f (t, bs) table = flip execState table $ do
      flip (iterateUntilM Set.null) bs $ return . Set.minView >=> \case
        Nothing -> return mempty
        Just (b, bs') -> do
          foldrM h bs' $ maybe mempty id $ Bucket.assocOf b frontiers
      where
        h :: Int -> Set.Set Int -> State (Bucket.Bucket Int Temp) (Set.Set Int)
        h ft bs_ = do
          gets (fromMaybe False . Bucket.isAssocWith t ft) >>= \case            
            True -> return bs_ 
            _ -> do
              modify $ Bucket.insert ft t
              return $ if maybe False (Set.member t) $ Bucket.assocOf ft defs
                then bs_
                else Set.insert ft bs_
    
    defs :: Bucket.Bucket Int Temp
    defs= foldr g mempty $ (vertexOf bcfg :: Set.Set Int)
      where
        g v = maybe mempty (flip (foldr (Bucket.insert v))) $ do
          qs <- dataOf v bcfg
          return $ mapMaybe destOf qs 

    initialDefsites :: Bucket.Bucket Temp Int
    initialDefsites = Bucket.reverse defs
