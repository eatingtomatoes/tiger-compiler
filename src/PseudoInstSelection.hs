{-# LANGUAGE FlexibleContexts #-}    
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module PseudoInstSelection
  ( selectInstructions
  , removeRedundantJump
  ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import Control.Lens (zoom, _1, _2)
import Debug.Pretty.Simple
import Data.List (splitAt)

import Temp
import TempHelper
import Label
import qualified Tree as T
import PseudoInst 
import Frame
import Fragment

import Linear
import Trace

data SelectionError
  = UnexpectedStmt T.Stmt
  | UnexpectedExpr (Maybe T.Stmt) T.Expr
  deriving (Show, Eq)

type InstSel m = (MonadState TempPool m, MonadError SelectionError m)

type SelectionContext m =
  ( InstSel m
  , MonadReader (Maybe T.Stmt) m
  , MonadWriter [PseudoInst Temp] m
  )

selectInstructions
  :: ProcFrag
  -> ExceptT String (State (TempPool, LabelPool)) [PseudoInst Temp]
selectInstructions ProcFrag{..} = do
  let header = selectFunHeader _pfFrame
  let tail' = selectFunTail 
  stmts <- zoom _1 $ linearize _pfBody    
  scheduled <- zoom _2 $ do 
    withExceptT show (traceSchedule stmts) 
  body <- zoom _1 $ do
    withExceptT show $ selectFunBody scheduled
  return $ header <> body <> tail'

selectFunHeader :: Frame -> [PseudoInst Temp]
selectFunHeader Frame{..} = execWriter $ do
  emitPlain Push $ OnlyR rbp
  emitPlain Mov $ RR rbp rsp
  emit PseudoInst
    { _piOperator = Sub
    , _piAddrPattern = RC rsp (- _frPointer)
    , _piExtraDefs = []
    , _piExtraUses = []
    , _piMark = ReserveLocalSpace
    }
  emitPlain Mov $ MMinusCR rbp 8 r10
  zipWithM_ f _frFormalsList paramRegisters
  -- forM [ rbx, r12, r13, r14, r15 ] $ \temp -> do
  forM [ rbx ] $ \temp -> do
    emitPlain Push $ OnlyR temp
  where 
    f temp reg = do
      emitPlain Mov $ RR temp reg

    paramRegisters :: [Temp]
    paramRegisters = [ rdi, rsi, rdx, rcx, r8, r9 ]
  
selectFunTail :: [PseudoInst Temp]
selectFunTail = execWriter $ do
  -- forM_ (reverse [ rbx, r12, r13, r14, r15 ]) $ \temp -> do
  forM_ [ rbx ] $ \temp -> do
    emitPlain Pop $ OnlyR temp
  emitPlain Leave None
  emitPlain Ret None

selectFunBody :: InstSel m => [T.Stmt] -> m [PseudoInst Temp]
selectFunBody body = execWriterT $ runReaderT (mapM_ codeStmt body) Nothing

removeRedundantJump :: [PseudoInst Temp] -> [PseudoInst Temp]
removeRedundantJump (j : l : rest) = f $ removeRedundantJump (l : rest)
  where
    matched = maybe False id $ (==) <$> branchDstOf j <*> labelOf l
    f = if matched then id else (j :)
removeRedundantJump insts = insts

unexpectedStmt :: SelectionContext m => T.Stmt -> m a
unexpectedStmt = throwError . UnexpectedStmt

type Emit m = MonadWriter [PseudoInst Temp] m

emit :: Emit m => PseudoInst Temp -> m ()
emit x = tell [x]

emitPlain :: Emit m => Operator -> AddrPattern Temp -> m ()
emitPlain op pat = tell $ pure PseudoInst
  { _piOperator = op
  , _piAddrPattern = pat
  , _piExtraDefs = []
  , _piExtraUses = []
  , _piMark = Plain
  }

plainTreeOps :: [T.BinOp]
plainTreeOps = [T.Plus, T.Minus, T.And, T.Or, T.LeftShift, T.RightShift, T.Xor]

codeExpr :: SelectionContext m => T.Expr -> m Temp
codeExpr expr = do
  case expr of
    T.BinOpExpr op e1 e2 -> do
      let pop = case op of
            T.Plus -> Add
            T.Minus -> Sub
            T.And -> And
            T.Or -> Or
            T.LeftShift -> Shl
            T.RightShift -> Shr
            T.Xor -> Xor
            T.Times -> IMul
            T.Divide -> IDiv
      temp <- allocTemp            
      temp <$ case op of
        _ | elem op plainTreeOps-> do
              case (e1, e2) of
                (T.MemExpr e1', T.MemExpr e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RM temp r1
                  emitPlain pop $ RM temp r2
                (T.ConstExpr x, T.ConstExpr y) -> do
                  emitPlain Mov $ RC temp x
                  emitPlain pop $ RC temp y
                (T.ConstExpr x, T.MemExpr e2') -> do
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RC temp x
                  emitPlain pop $ RM temp r2
                (T.MemExpr (T.BinOpExpr T.Plus (T.ConstExpr x) e1'), T.ConstExpr y) -> do
                  r1 <- codeExpr e1'
                  if | x > 0 -> do
                         emitPlain Mov $ RMPlusC temp r1 x
                     | x == 0 -> do
                         emitPlain Mov $ RM temp r1
                     | otherwise -> do
                         emitPlain Mov $ RMMinusC temp r1 (- x)
                  emitPlain pop $ RC temp y
                (T.MemExpr (T.BinOpExpr T.Plus e1' (T.ConstExpr x)), T.ConstExpr y) -> do
                  r1 <- codeExpr e1'
                  if | x > 0 -> do
                         emitPlain Mov $ RMPlusC temp r1 x
                     | x == 0 -> do
                         emitPlain Mov $ RM temp r1
                     | otherwise -> do
                         emitPlain Mov $ RMMinusC temp r1 (- x)
                  emitPlain pop $ RC temp y
                (T.MemExpr e1', T.ConstExpr y) -> do
                  r1 <- codeExpr e1'
                  emitPlain Mov $ RM temp r1
                  emitPlain pop $ RC temp y
                (e1', T.MemExpr e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RR temp r1
                  emitPlain pop $ RM temp r2
                (T.MemExpr e1', e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RM temp r1
                  emitPlain pop $ RR temp r2
                (e1', T.ConstExpr y) -> do
                  r1 <- codeExpr e1'
                  emitPlain Mov $ RR temp r1
                  emitPlain pop $ RC temp y
                (T.ConstExpr x, e2') -> do
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RC temp x
                  emitPlain pop $ RR temp r2
                (e1', e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RR temp r1
                  emitPlain pop $ RR temp r2
        _ | elem op [T.Times, T.Divide] -> do
              emitPlain Xor $ RR rdx rdx
              case (e1, e2) of
                (T.MemExpr e1', T.MemExpr e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RM rax r1
                  emitPlain pop $ OnlyM r2
                  emitPlain Mov $ RR temp rax
                (e1', T.MemExpr e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RR rax r1
                  emitPlain pop $ OnlyM r2
                  emitPlain Mov $ RR temp rax
                (T.MemExpr e1', e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RM rax r1
                  emitPlain pop $ OnlyR r2
                  emitPlain Mov $ RR temp rax
                (e1', e2') -> do
                  r1 <- codeExpr e1'
                  r2 <- codeExpr e2'
                  emitPlain Mov $ RR rax r1
                  emitPlain pop $ OnlyR r2
                  emitPlain Mov $ RR temp rax
        _ -> error $ "unhandled binary operator: " <> show op
    T.UnOpExpr op e -> do
      let pop = case op of
            T.Not -> Not
            T.Negate -> Neg
      temp <- allocTemp
      temp <$ case e of
        T.MemExpr e' -> do
          r <- codeExpr e'
          emitPlain Mov $ RM temp r
          emitPlain pop $ OnlyR temp
        e' -> do
          r <- codeExpr e'
          emitPlain Mov $ RR temp r
          emitPlain pop $ OnlyR temp
    T.MemExpr e -> do
      r <- codeExpr e
      temp <- allocTemp
      emitPlain Mov $ RM temp r
      return temp
    T.TempExpr x -> do
      return x
    T.NameExpr label -> do
      temp <- allocTemp
      emitPlain Mov $ RL temp label
      return temp
    T.ConstExpr x -> do
      temp <- allocTemp
      emitPlain Mov $ RC temp x
      return temp
    _ -> error $ "unhandler expr in codeExpr: " <> show expr
      
codeStmt :: SelectionContext m => T.Stmt -> m ()
codeStmt stmt = local (const $ Just stmt) $ do
  temp <- allocTemp 
  case stmt of
    T.LabelStmt label -> do
      emitPlain Lab $ OnlyLabel label
    T.JumpStmt (T.NameExpr label) _ -> do
      emitPlain Jmp $ OnlyLabel label
    T.CJumpStmt op e1 e2 label _ -> do
      let pop = case op of
            T.Equal -> JE
            T.NotEqual -> JNE
            T.Less -> JL
            T.NotLess -> JNL
            T.LessOrEqual -> JLE
            T.NotLessOrEqual -> JNLE
            T.Greater -> JG
            T.NotGreater -> JNG
            T.GreaterOrEqual -> JGE
            T.NotGreaterOrEqual -> JNGE
      case (e1, e2) of
        (T.MemExpr e1', T.MemExpr e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ RM temp r1
          emitPlain Cmp $ RM temp r2
        (T.ConstExpr x, T.ConstExpr y) -> do
          emitPlain Mov $ RC temp x
          emitPlain Cmp $ RC temp y
        (T.ConstExpr x, T.MemExpr e2') -> do
          r2 <- codeExpr e2'
          emitPlain Mov $ RC temp x
          emitPlain Cmp $ RM temp r2
        (T.MemExpr e1', T.ConstExpr y) -> do
          r1 <- codeExpr e1'
          emitPlain Mov $ RM temp r1
          emitPlain Cmp $ RC temp y
        (e1', T.MemExpr e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Cmp $ RM r1 r2
        (T.MemExpr e1', e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ RM temp r1
          emitPlain Cmp $ RR temp r2
        (T.ConstExpr x, e2') -> do
          r2 <- codeExpr e2'
          emitPlain Mov $ RC temp x
          emitPlain Cmp $ RR temp r2
        (e1', T.ConstExpr y) -> do
          r1 <- codeExpr e1'
          emitPlain Cmp $ RC r1 y
        (e1', e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Cmp $ RR r1 r2
      emitPlain pop $ OnlyLabel label
    T.MoveStmt e1 e2 -> do
      case (e1, e2) of
        (T.TempExpr r1, T.CallExpr (T.NameExpr label) args) -> do
          withArgs args $ \dsts srcs -> do
            emit PseudoInst
              { _piOperator = Call
              , _piAddrPattern = OnlyLabel label
              , _piExtraDefs = dsts
              , _piExtraUses = srcs
              , _piMark = Plain
              }
            emitPlain Mov $ RR r1 rax
        (T.MemExpr (T.BinOpExpr T.Plus (T.ConstExpr x) e1'), T.ConstExpr y) -> do
          r1 <- codeExpr e1'
          if | x > 0 -> emitPlain Mov $ MPlusCC r1 x y
             | x == 0 -> emitPlain Mov $ MC r1 y
             | otherwise -> emitPlain Mov $ MMinusCC r1 (- x) y
        (T.MemExpr (T.BinOpExpr T.Plus e1' (T.ConstExpr x)), T.ConstExpr y) -> do
          r1 <- codeExpr e1'
          if | x > 0 -> emitPlain Mov $ MPlusCC r1 x y
             | x == 0 -> emitPlain Mov $ MC r1 y
             | otherwise -> emitPlain Mov $ MMinusCC r1 (- x) y          
        (T.MemExpr (T.BinOpExpr T.Plus (T.ConstExpr x) e1'), T.MemExpr e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ RM temp r2
          if | x > 0 -> emitPlain Mov $ MPlusCR r1 x temp
             | x == 0 -> emitPlain Mov $ MR r1 temp
             | otherwise -> emitPlain Mov $ MMinusCR r1 (- x) temp
        (T.MemExpr (T.BinOpExpr T.Plus e1' (T.ConstExpr x)), T.MemExpr e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ RM temp r2
          if | x > 0 -> emitPlain Mov $ MPlusCR r1 x temp
             | x == 0 -> emitPlain Mov $ MR r1 temp
             | otherwise -> emitPlain Mov $ MMinusCR r1 (- x) temp          
        (T.MemExpr (T.BinOpExpr T.Plus (T.ConstExpr x) e1'), e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Mov $ MPlusCR r1 x r2
             | x == 0 -> emitPlain Mov $ MR r1 r2
             | otherwise -> emitPlain Mov $ MMinusCR r1 (- x) r2
        (T.MemExpr (T.BinOpExpr T.Plus e1' (T.ConstExpr x)), e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Mov $ MPlusCR r1 x r2
             | x == 0 -> emitPlain Mov $ MR r1 r2
             | otherwise -> emitPlain Mov $ MMinusCR r1 (- x) r2
        (T.MemExpr e1', T.MemExpr (T.BinOpExpr T.Plus e2' (T.ConstExpr x))) -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Mov $ RMPlusC r1 r2 x
             | x == 0 -> emitPlain Mov $ RM r1 r2
             | otherwise -> emitPlain Mov $ RMMinusC r1 r2 (- x)
        (T.MemExpr e1', T.MemExpr (T.BinOpExpr T.Plus (T.ConstExpr x) e2')) -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Mov $ RMPlusC r1 r2 x
             | x == 0 -> emitPlain Mov $ RM r1 r2
             | otherwise -> emitPlain Mov $ RMMinusC r1 r2 (- x)             
        (T.MemExpr e1', T.MemExpr e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ RM temp r2
          emitPlain Mov $ MR r1 temp
        (T.ConstExpr _, _) -> do
          throwError $ UnexpectedStmt stmt
        (e1', T.MemExpr (T.BinOpExpr T.Plus e2' (T.ConstExpr x))) -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Mov $ RMPlusC r1 r2 x
             | x == 0 -> emitPlain Mov $ RM r1 r2
             | otherwise -> emitPlain Mov $ RMMinusC r1 r2 (- x)
        (e1', T.MemExpr (T.BinOpExpr T.Plus (T.ConstExpr x) e2')) -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Mov $ RMPlusC r1 r2 x
             | x == 0 -> emitPlain Mov $ RM r1 r2
             | otherwise -> emitPlain Mov $ RMMinusC r1 r2 (- x)             
        (e1', T.MemExpr e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ RM r1 r2
        (T.MemExpr e1', T.ConstExpr y) -> do
          r1 <- codeExpr e1'
          emitPlain Mov $ MC r1 y
        (T.MemExpr e1', e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ MR r1 r2
        (e1', T.ConstExpr y) -> do
          r1 <- codeExpr e1'
          emitPlain Mov $ RC r1 y
        (e1', T.BinOpExpr T.Plus e2' (T.ConstExpr x)) -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Lea $ RMPlusC r1 r2 x  
             | x == 0 -> emitPlain Mov $ RR r1 r2
             | otherwise -> emitPlain Lea $ RMMinusC r1 r2 (- x)
        (e1', T.BinOpExpr T.Plus (T.ConstExpr x) e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          if | x > 0 -> emitPlain Lea $ RMPlusC r1 r2 x
             | x == 0 -> emitPlain Mov $ RR r1 r2
             | otherwise -> emitPlain Lea $ RMMinusC r1 r2 (- x)                       
        (e1', e2') -> do
          r1 <- codeExpr e1'
          r2 <- codeExpr e2'
          emitPlain Mov $ RR r1 r2
    T.ExprStmt e -> do
      case e of
        T.CallExpr (T.NameExpr label) args -> do
          withArgs args $ \dsts srcs -> do
            emit PseudoInst
              { _piOperator = Call
              , _piAddrPattern = OnlyLabel label
              , _piExtraDefs = dsts
              , _piExtraUses = srcs
              , _piMark = Plain
              }
        T.ConstExpr _ -> return ()
        e' -> do
          void $ codeExpr e'
    _ -> unexpectedStmt stmt

withArgs :: SelectionContext m => [T.Expr] -> ([Temp] -> [Temp] -> m ()) -> m ()
withArgs args routine = do
  -- mapM_ save [rax, rcx, rdx, r11]
  
  let (inRegs, inStacks) = splitAt (length paramRegisters) args

  -- Pass arguments through register,
  forM_ (zip paramRegisters inRegs) $ \(temp, expr) -> do
    codeStmt $ T.MoveStmt (T.TempExpr temp) expr     

  -- Pass arguments through the stack.
  forM_ (reverse inStacks) $ \case
    T.ConstExpr x -> emitPlain Push $ OnlyC x
    T.NameExpr label -> emitPlain Push $ OnlyLabel label
    expr -> do
      temp <- codeExpr expr
      emitPlain Push $ OnlyR temp

  emitPlain Xor $ RR rax rax

  -- Execute the subroutine.
  let dsts = [ rax, rcx, rdx, r11 ] <> paramRegisters
      srcs = zipWith const paramRegisters inRegs
 
  routine dsts (rax : srcs)

  -- Restore the stack.
  let delta = length inStacks * 8
  when (delta > 0) $ do
    emitPlain Add $ RC rsp delta

  -- mapM_ restore [r11, rdx, rcx, rax]

  where
    paramRegisters = [ r10, rdi, rsi, rdx, rcx, r8, r9 ]
