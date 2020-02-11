{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Trace where

import Control.Monad.State
import Control.Monad.Except
import Data.Foldable
import Data.Maybe (catMaybes)
import qualified Data.List.NonEmpty as NonEmpty
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as Map

import Tree
import Label

data TraceError
  = LonelyBreak
  | NoEntryLabelInLastBlock (NonEmpty Stmt)
  | NoBlocks
  deriving (Show)

data Block
  = Block
  { _bckEntry :: Label
  , _bckFollowers :: NonEmpty Label
  , _bckStmts :: NonEmpty Stmt
  } deriving (Show)

type Trace m = (MonadState LabelPool m, MonadError TraceError m)

traceSchedule :: [Stmt] -> ExceptT TraceError (State LabelPool) [Stmt]
traceSchedule stmts = do
  (entry, stmts') <- addEntry stmts
  (blocks, done) <- complete $ splitStmts stmts'
  let stmts'' = joinTrace $ trace blocks
  stmts''' <- adjustCJump done stmts''
  let j = JumpStmt (NameExpr entry) [entry]
      f = LabelStmt done
  return $ j : (stmts''' <> [f])
  
addEntry :: Trace m => [Stmt] -> m (Label, [Stmt])
addEntry stmts = do
  entry <- allocLabelWithTag "entry"
  return (entry, LabelStmt entry : stmts)

splitStmts :: [Stmt] -> [[Stmt]]    
splitStmts stmts = let (bacc, sacc) = foldl f ([], []) stmts in bacc <> [sacc]
  where 
    f (bacc, sacc) s = case s of
      LabelStmt _ -> (bacc <> [sacc], [s])
      JumpStmt _ _ -> (bacc <> [sacc <> [s]], [])
      CJumpStmt _ _ _ _ _ -> (bacc <> [sacc <> [s]], [])      
      _ -> (bacc, sacc <> [s])

complete :: Trace m => [[Stmt]] -> m ([Block], Label)
complete bs =
  case fmap NonEmpty.fromList $ filter (Prelude.not . null) bs of
    [] -> throwError NoBlocks
    blocks -> do
      bs' <- zipWithM f blocks (tail blocks)
      (lastOne, done) <- handleTail $ last blocks
      return $ (bs' <> [lastOne], done)
      where 
        f :: Trace m => NonEmpty Stmt -> NonEmpty Stmt -> m Block
        f ls rs = do
          (entry, ls') <- case NonEmpty.head ls of
            LabelStmt label -> return (label, ls)
            _ -> do
              label <- allocLabel
              return $ (label, LabelStmt label `NonEmpty.cons` ls)
          (ls'', followings) <- case NonEmpty.last ls' of
            JumpStmt _ labels -> return (ls', NonEmpty.fromList labels)
            CJumpStmt _ _ _ l1 l2 -> return (ls', NonEmpty.fromList [l1, l2])
            _ -> case NonEmpty.head rs of
              LabelStmt l -> do
                let j = JumpStmt (NameExpr l) [l] 
                return (ls' <> NonEmpty.fromList [j],  NonEmpty.fromList [l])
              _ -> throwError LonelyBreak
          return Block
            { _bckEntry = entry
            , _bckFollowers = followings
            , _bckStmts = ls''
            }
        handleTail :: Trace m => NonEmpty Stmt -> m (Block, Label)
        handleTail stmts = do
          done <- allocLabelWithTag "done"
          entry <- case NonEmpty.head stmts of
            LabelStmt l -> return l
            _ -> throwError $ NoEntryLabelInLastBlock stmts
          let j = JumpStmt (NameExpr done) [done]
              block = Block
                { _bckEntry = entry
                , _bckFollowers = NonEmpty.fromList [done]
                , _bckStmts = stmts <> NonEmpty.fromList [j]
                }
          return (block, done)

trace :: [Block] -> [[Block]]
trace blocks = flip evalState table $ do
  foldlM f [] blocks
  where
    table = Map.fromList $ fmap (\b -> (_bckEntry b, b)) blocks
    f :: [[Block]] -> Block -> State (Map.Map Label Block) [[Block]]
    f tacc b = do
      gets (Map.lookup $ _bckEntry b) >>= \case
        Nothing -> return tacc
        Just _ -> do
          t <- collect [] b
          return $ tacc <> [t]
    collect :: [Block] -> Block -> State (Map.Map Label Block) [Block]
    collect bacc b = do
      modify $ Map.delete $ _bckEntry b
      usable <- fmap catMaybes $ mapM (gets . Map.lookup) $ NonEmpty.toList (_bckFollowers b)
      case usable of
        [] -> return $ bacc <> [b]
        (x : _) -> collect (bacc <> [b]) x

joinTrace :: [[Block]] -> [Stmt]
joinTrace traces = concatMap (concatMap $ NonEmpty.toList . _bckStmts) traces

adjustCJump :: Trace m => Label -> [Stmt] -> m [Stmt]
adjustCJump done stmts = do
  sss <- zipWithM f stmts (tail stmts)
  lastStmts <- f (last stmts) (LabelStmt done)
  return $ concat sss <> lastStmts
  where
    f s1 s2 = do
      case s1 of
        CJumpStmt op e1 e2 l1 l2 -> do
          case s2 of
            LabelStmt l3 -> do
              if | l3 == l2 -> return [s1]
                 | l3 == l1 -> return [CJumpStmt (opposite op) e1 e2 l2 l1]
                 | otherwise -> do
                     label <- allocLabel
                     return
                       [ CJumpStmt op e1 e2 l1 label
                       , LabelStmt label
                       , JumpStmt (NameExpr label) [label]
                       ]
            _ -> return [s1]           
        JumpStmt (NameExpr l1) _ -> do
          case s2 of
            LabelStmt l2 -> do
              if | l2 == l1 -> return []
                 | otherwise -> return [s1]
            _ -> return [s1]
        _ -> return [s1]
    opposite op = case op of
      Equal -> NotEqual
      NotEqual -> Equal
      Less -> NotLess
      LessOrEqual -> NotLessOrEqual
      Greater -> NotGreater
      GreaterOrEqual -> NotGreaterOrEqual
      NotLess -> Less
      NotLessOrEqual -> LessOrEqual
      NotGreater -> Greater
      NotGreaterOrEqual -> GreaterOrEqual
