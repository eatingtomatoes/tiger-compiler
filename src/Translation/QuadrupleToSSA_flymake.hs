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
import Debug.Pretty.Simple
import Data.Maybe (catMaybes)
import Data.Foldable
import Control.Monad.Loops
import Control.Monad.State
import Safe (headMay)
import Control.Lens (makeLenses, modifying, over,  _2, view)
import Data.List (sortOn)
import Text.Printf 
  
import MiddleEnd.Quadruple
import Translation.Temp
import MiddleEnd.Quadruple.ControlFlowGraph 
import MiddleEnd.Quadruple.Loop.Dominance
import MiddleEnd.Quadruple.BlockControlFlowGraph
import MiddleEnd.Quadruple.ReachingDefinition
import qualified MiddleEnd.SSA.Quadruple as SQ

       
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

makeLenses ''IBlock
makeLenses ''RenamingContext

placePhiFunction :: DominanceTree -> BlockControlFlowGraph -> Map.Map Int (Set.Set Temp) -> [SQ.Quadruple]
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
    -- blockToQuads IBlock{..} = {-pTrace (show "iblock: " <> show ib) $ -}fmap f _ibPhis <> fmap g _ibQuads
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
            
          forM_ (maybe mempty toList $ succOf i bcfg) $ \next -> do
            gets (Map.lookup next . _rcBlocks) >>= \case
              Nothing -> error $ "encountered an unknown block `" <> show next <> "`"
              Just iblock -> do
                let pos = case predOf next bcfg of
                      Nothing -> error $ printf "encountered a successor(`%s') of `%s' which is not in the block control flow graph in rename" (show next) (show i)
                      Just prevs ->do
                        case headMay $ filter ((== i) . snd) $ zip [(0 :: Int)..] $ toList prevs of
                          Nothing -> error $ printf "encountered a successor(`%s') of `%s' whose predecessors set doesn't include `%s' in rename" (show next) (show i) (show i)
                          Just (k, _) -> k
                ps' <- mapM (addVar pos) $ view ibPhis iblock 
                modifying rcBlocks $ Map.insert next $ iblock { _ibPhis = ps' }

          mapM_ rename (maybe mempty toList $ Map.lookup i dominatorTree)

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

toIBlocks :: BlockControlFlowGraph -> Map.Map Int (Set.Set Temp) -> Map.Map Int IBlock
toIBlocks bcfg phiVars = Map.mapWithKey f bcfg
      where
        f i (qs, _) = IBlock { _ibQuads = qs', _ibPhis = ps' }
          where
            qs' = fmap (Left . snd) qs
            -- ps' = maybe mempty (fmap ((, []) . Left) . Set.toList) $ Map.lookup i phiVars
            ps' :: [(Either Temp SQ.TaggedTemp, Map.Map Int SQ.TaggedTemp)]
            ps' = maybe mempty g $ Map.lookup i phiVars
              where
                g vars = fmap (, mempty) $ fmap Left $ toList vars
    
calcPhiVars :: BlockControlFlowGraph -> Graph Int -> Map.Map Int{-block-} (Set.Set Temp){-phi funs needs inserting-}
calcPhiVars bcfg frontiers = foldr f mempty $ Map.toList initialDefsites
  where
    f :: (Temp, Set.Set Int) -> Map.Map Int (Set.Set Temp) -> Map.Map Int (Set.Set Temp)
    f (t, bs) table = flip execState table $ do
      flip (iterateUntilM Set.null) bs $ return . Set.minView >=> \case
        Nothing -> return mempty
        Just (b, bs') -> do
          foldrM h bs' $ maybe mempty id $ Map.lookup b frontiers
      where
        h :: Int -> Set.Set Int -> State (Map.Map Int (Set.Set Temp)) (Set.Set Int)
        h ft bs_ = do
          gets (maybe False (Set.member t) . Map.lookup ft) >>= \case
            True -> return bs_
            _ -> do
              modify $ Map.alter (Just . maybe (Set.singleton t) (Set.insert t)) ft
              return $ if maybe False (Set.member t) $ Map.lookup ft defs
                then bs_
                else Set.insert ft bs_
    
    defs :: Map.Map Int{-block-} (Set.Set Temp){-defined variable in this block-}
    defs = fmap (Set.fromList . catMaybes . fmap (destOf . snd) . fst) bcfg

    initialDefsites :: Map.Map Temp (Set.Set Int)
    initialDefsites = foldr g mempty $ Map.toList defs
      where
        g (i, ts) table = foldr (Map.alter $ Just . maybe (Set.singleton i) (Set.insert i)) table ts
        

transQuadrupleToSSA :: [Quadruple] -> [SQ.Quadruple]
transQuadrupleToSSA quads = placePhiFunction dominatorTree bcfg phiVars
  where
    dominatorTree
      = id
      -- $ traceDirectedGraph "tree"
      -- $ (pTraceWith $ \tree -> "tree: " <> show tree)
      $ buildDominatorTree'
      -- $ (pTraceWith $ \ims -> "immediatedominator: " <> (show ims))
      $ calcImmediateDominator
      -- $ (pTraceWith $ \ims -> "dominatos: " <> (show ims))      
      $ calcDominators (Map.size bcfg) (fmap Set.toList . flip predOf bcfg)
    bcfg = id
      -- $ traceDirectedGraph_ "bcfg"
      $ buildBlockControlFlowGraph quads
    phiVars = calcPhiVars bcfg $ calcDominanceFrontier (fmap toList . flip succOf bcfg) dominatorTree
 
