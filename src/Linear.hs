{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}

module Linear where

import Control.Monad.State

import Temp
import Tree
import TreeHelper

type Linear m  = MonadState TempPool m
 
linearize :: Linear m => Stmt -> m [Stmt]
-- linearize :: Stmt -> State TempPool [Stmt]
linearize stmt = linearizeStmt <$> liftStmt stmt

linearizeStmt :: Stmt -> [Stmt]
linearizeStmt stmt = case stmt of
  SeqStmt s1 s2 -> linearizeStmt s1 <> linearizeStmt s2
  _ -> [stmt]

liftStmt :: Linear m => Stmt -> m Stmt
liftStmt stmt = do
  case stmt of
    SeqStmt s1 s2 -> do
      s1' <- liftStmt s1
      s2' <- liftStmt s2
      return $ SeqStmt s1' s2'
    LabelStmt _ -> return stmt
    JumpStmt (ESeqExpr s e) labels -> do
      s' <- liftStmt s
      e' <- liftExpr e
      j' <- returnSteadyStmt $ JumpStmt e' labels
      return $ SeqStmt s' j'
    JumpStmt e labels -> do
      e' <- liftExpr e
      returnSteadyStmt $ JumpStmt e' labels
    CJumpStmt op (ESeqExpr s e1) e2 l1 l2 -> do
      s' <- liftStmt s
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      cj' <- returnSteadyStmt $ CJumpStmt op e1' e2' l1 l2
      return $ SeqStmt s' cj'
    CJumpStmt op e1 (ESeqExpr s e2) l1 l2 -> do
      t <- TempExpr <$> allocTemp
      s' <- liftStmt s      
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      m' <- returnSteadyStmt $ MoveStmt t e1'
      cj' <- returnSteadyStmt $ CJumpStmt op t e2' l1 l2
      return $ SeqStmt (SeqStmt m' s') cj'
    CJumpStmt op e1 e2 l1 l2 -> do
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      returnSteadyStmt $ CJumpStmt op e1' e2' l1 l2
    MoveStmt t@(TempExpr _) (ESeqExpr s e) -> do
      s' <- liftStmt s
      e' <- liftExpr e
      m' <- returnSteadyStmt $ MoveStmt t e'
      return $ SeqStmt s' m'
    MoveStmt (MemExpr (ESeqExpr s e1)) e2 -> do
      s' <- liftStmt s
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      m' <- returnSteadyStmt $ MoveStmt (MemExpr e1') e2'
      return $ SeqStmt s' m'
    MoveStmt (MemExpr e1) (ESeqExpr s e2) -> do
      t <- TempExpr <$> allocTemp      
      s' <- liftStmt s      
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      m1' <- returnSteadyStmt $ MoveStmt t e1'
      m2' <- returnSteadyStmt $ MoveStmt (MemExpr t) e2'
      return $ SeqStmt (SeqStmt m1' s') m2'
    MoveStmt t@(TempExpr _) (CallExpr (ESeqExpr s e) args) -> do
      s' <- liftStmt s
      e' <- liftExpr e
      args' <- mapM liftExpr args
      m' <- returnSteadyStmt $ MoveStmt t (CallExpr e' args')
      return $ SeqStmt s' m'
    MoveStmt t@(TempExpr _) (CallExpr e args) -> do
      e' <- liftExpr e
      args' <- mapM liftExpr args
      return $ MoveStmt t (CallExpr e' args')
    MoveStmt e1 e2 -> do
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      returnSteadyStmt $ MoveStmt e1' e2'
    ExprStmt (ESeqExpr s e) -> do
      s' <- liftStmt s
      e' <- returnSteadyStmt $ ExprStmt e
      return $ SeqStmt s' e'
    ExprStmt (CallExpr (ESeqExpr s e) args) -> do
      s' <- liftStmt s
      e' <- liftExpr e
      args' <- mapM liftExpr args
      c' <- returnSteadyStmt $ (ExprStmt $ CallExpr e' args')
      return $ SeqStmt s' c'
    ExprStmt (CallExpr e args) -> do
      e' <- liftExpr e
      (s', args') <- liftExprList args
      c' <- returnSteadyStmt $ ExprStmt (CallExpr e' args')
      return $ SeqStmt s' c'
    ExprStmt e -> do
      e' <- liftExpr e
      returnSteadyStmt $ ExprStmt e' 

isSteadyStmt :: Stmt -> Bool
isSteadyStmt stmt = case stmt of
  JumpStmt (ESeqExpr _ _) _ -> False
  CJumpStmt _ (ESeqExpr _ _) _ _ _ -> False
  CJumpStmt _ _ (ESeqExpr _ _ ) _ _ -> False
  MoveStmt (TempExpr _) (ESeqExpr _ _) -> False
  MoveStmt (MemExpr (ESeqExpr _ _)) _ -> False
  MoveStmt (MemExpr _) (ESeqExpr _ _)  -> False
  MoveStmt (TempExpr _) (CallExpr (ESeqExpr _  _) _) -> False
  ExprStmt (ESeqExpr _ _) -> False
  ExprStmt (CallExpr (ESeqExpr _ _)  _) -> False
  _ -> True

returnSteadyStmt :: Linear m => Stmt -> m Stmt
returnSteadyStmt stmt
  | isSteadyStmt stmt = return stmt
  | otherwise = liftStmt stmt
  
liftExpr :: Linear m => Expr -> m Expr
liftExpr expr = do
  case expr of
    BinOpExpr op (ESeqExpr s e1) e2 -> do
      s' <- liftStmt s
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      b' <- returnSteadyExpr $ BinOpExpr op e1' e2'
      return $ ESeqExpr s' b'
    BinOpExpr op e1 (ESeqExpr s e2)  -> do
      t <- TempExpr <$> allocTemp
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      s' <- liftStmt s
      m' <- returnSteadyStmt $ MoveStmt t e1'      
      b' <- returnSteadyExpr $ BinOpExpr op t e2'
      return $ ESeqExpr (SeqStmt m' s') b'
    BinOpExpr op e1 e2 -> do
      e1' <- liftExpr e1
      e2' <- liftExpr e2
      returnSteadyExpr $ BinOpExpr op e1' e2'
    UnOpExpr op (ESeqExpr s e) -> do
      s' <- liftStmt s
      e' <-  liftExpr e
      u' <- returnSteadyExpr $ UnOpExpr op e'
      return $ ESeqExpr s' u'
    UnOpExpr op e -> do
      e' <- liftExpr e
      returnSteadyExpr $ UnOpExpr op e'
    MemExpr (ESeqExpr s e) -> do
      s' <- liftStmt s
      e' <- liftExpr e
      return $ ESeqExpr s' e'
    MemExpr e -> do
      e' <- liftExpr e
      returnSteadyExpr $ MemExpr e'
    TempExpr _ -> return expr
    ESeqExpr s1 (ESeqExpr s2 e) -> do
      s1' <- liftStmt s1
      s2' <- liftStmt s2
      e' <- liftExpr e
      return $ ESeqExpr (SeqStmt s1' s2') e'
    ESeqExpr s e -> do
      s' <- liftStmt s
      e' <- liftExpr e
      returnSteadyExpr $ ESeqExpr s' e'
    NameExpr _ -> return expr
    ConstExpr _ -> return expr
    CallExpr e args -> do
      t <- TempExpr <$> allocTemp
      e' <- liftExpr e
      -- args' <- mapM liftExpr args
      (s', args') <- liftExprList args
      m' <- returnSteadyStmt $ MoveStmt t (CallExpr e' args')
      return $ ESeqExpr (SeqStmt s' m') t
      
isSteadyExpr :: Expr -> Bool
isSteadyExpr expr = case expr of
  BinOpExpr _ (ESeqExpr _ _) _ -> False
  BinOpExpr _ _ (ESeqExpr _ _) -> False
  UnOpExpr _ (ESeqExpr _ _) -> False
  MemExpr (ESeqExpr _ _) -> False
  ESeqExpr _ (ESeqExpr _ _ ) -> False
  CallExpr (ESeqExpr _ _) _ -> False
  _ -> True

returnSteadyExpr :: Linear m => Expr -> m Expr
returnSteadyExpr expr
  | isSteadyExpr expr = return expr
  | otherwise = liftExpr expr

liftExprList :: Linear m => [Expr] -> m (Stmt, [Expr])
liftExprList exprs = do
  exprs' <- mapM liftExpr exprs
  return $ foldr f (dummyStmt, []) $ condense <$> exprs'
  where
    f expr (sacc, eacc) = case expr of
      ESeqExpr s e -> (SeqStmt s sacc, e : eacc)
      _  -> (sacc, expr : eacc)

condense :: Expr -> Expr
condense expr = case expr of
  ESeqExpr s1 (ESeqExpr s2 e) -> ESeqExpr (SeqStmt s1 s2) e
  _ -> expr

