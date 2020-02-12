module QuadrupleGen where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer

import Tree
import Quadruple
import Temp

type Context = WriterT [Quadruple] (ExceptT String (State TempPool))

transTreeToQuadruple :: Stmt -> ExceptT String (State TempPool) [Quadruple]
transTreeToQuadruple stmt = execWriterT $ transStmt stmt

transStmt :: Stmt -> Context ()
transStmt stmt = do
  case stmt of
    SeqStmt s1 s2 -> do
      transStmt s1
      transStmt s2
    LabelStmt l -> do
      emit $ LabelQ l
    JumpStmt _ dsts -> do
      emit $ JumpQ $ head dsts
    CJumpStmt op e1 e2 l1 l2 -> do
      v1 <- transExprToVar e1
      v2 <- transExprToVar e2
      emit $ CJumpQ op v1 v2 l1 l2
    MoveStmt (MemExpr e1) e2 -> do
      v1 <- transExprToVar e1
      v2 <- transExprToVar e2
      emit $ StoreQ v1 v2
    MoveStmt e1 e2 -> do
      t <- transExprToTemp e1
      v <- transExprToVar e2
      emit $ MoveQ t v
    ExprStmt e -> do
      void $ transExprToVar e

transExprToTemp :: Expr -> Context Temp
transExprToTemp expr = do
  case expr of
    BinOpExpr op e1 e2 -> do
      v1 <- transExprToVar e1 
      v2 <- transExprToVar e2
      temp <- allocTemp
      emit $ BinQ temp op v1 v2
      return temp
    UnOpExpr op e -> do
      v <- transExprToVar e
      temp <- allocTemp
      emit $ UnQ temp op v
      return temp
    MemExpr e -> do
      v <- transExprToVar e
      temp <- allocTemp
      emit $ LoadQ temp v
      return temp
    TempExpr t -> do
      return t
    ESeqExpr s e -> do
      transStmt s
      transExprToTemp e
    CallExpr e args -> do
      let (NameExpr l) = e
      args' <- mapM transExprToVar args
      temp <- allocTemp
      emit $ MoveCallQ temp l args'
      return temp
    _ -> error
      $ "unexpected expr `"
      <> show expr
      <>"'in transExprToTemp"
                  
transExprToVar :: Expr -> Context Var
transExprToVar expr = do
  case expr of
    ESeqExpr s e -> do
      transStmt s
      transExprToVar e
    NameExpr l -> do
      return $ LabelV l
    ConstExpr c -> do
      return $ ConstV c
    _ -> do
      TempV <$> transExprToTemp expr

emit :: Quadruple -> Context ()
emit = tell . pure
