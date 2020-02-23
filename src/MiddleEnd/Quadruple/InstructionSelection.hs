{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module MiddleEnd.Quadruple.InstructionSelection
  ( selectFunBody
  ) where

import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Except
import Debug.Pretty.Simple
import Data.List (splitAt)

import Translation.Temp
import Translation.TempHelper
import qualified MiddleEnd.Tree as T
import Backend.Instruction

import MiddleEnd.Quadruple

type SelectionContext m = (MonadState TempPool m, MonadWriter [Instruction Temp] m)

selectFunBody :: Monad m => [Quadruple] -> ExceptT String (StateT TempPool m) [Instruction Temp]
selectFunBody body = execWriterT (mapM_ codeQuadruple body)

codeQuadruple :: SelectionContext m => Quadruple -> m ()
codeQuadruple quad =
  case quad of
    BinQ t op v1 v2 -> do
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
      case op of
        _ | elem op plainTreeOps -> do
              emitPlain Mov $ case v1 of
                TempV t1 -> RR t t1
                ConstV c -> RC t c
                LabelV l -> RL t l
              emitPlain pop $ case v2 of
                TempV t2 -> RR t t2
                ConstV c -> RC t c
                LabelV l -> RL t l
        _ | elem op [T.Times, T.Divide] -> do
              emitPlain Xor $ RR rdx rdx
              emitPlain Mov $ case v1 of
                TempV t1 -> RR rax t1
                ConstV c -> RC rax c
                LabelV l -> RL rax l
              case v2 of
                TempV t2 -> emitPlain pop $ OnlyR t2
                ConstV c -> do
                  temp <- allocTemp
                  emitPlain Mov $ RC temp c
                  emitPlain pop $ OnlyR temp
                _ -> error $ "unhandled binary operator in codeQuadruple: " <> show op
              emitPlain Mov $ RR t rax
        _ -> error $ "unhandled binary operator in codeQuadruple: " <> show op
    UnQ t op v1 -> do
      let pop = case op of
            T.Not -> Not
            T.Negate -> Neg
      emitPlain Mov $ case v1 of
        TempV t1 -> RR t t1
        ConstV c -> RC t c
        LabelV l -> RL t l
      emitPlain pop $ OnlyR t
    JumpQ l -> emitPlain Jmp $ OnlyLabel l
    CJumpQ op v1 v2 l1 _ -> do
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
      case v1 of
        TempV t1 -> emitPlain Cmp $ case v2 of
          TempV t2 -> RR t1 t2
          ConstV c -> RC t1 c
          LabelV l -> RL t1 l
        ConstV c -> do
          temp <- allocTemp
          emitPlain Mov $ RC temp c
          emitPlain Cmp $ case v2 of
            TempV t2 -> RR temp t2
            ConstV c' -> RC temp c'
            LabelV l -> RL temp l
        LabelV l -> do
          temp <- allocTemp
          emitPlain Mov $ RL temp l
          emitPlain Cmp $ case v2 of
            TempV t2 -> RR temp t2
            ConstV c -> RC temp c
            LabelV l' -> RL temp l'
      emitPlain pop $ OnlyLabel l1
    LabelQ l -> emitPlain Lab $ OnlyLabel l
    LoadQ t v1 -> emitPlain Mov $ case v1 of
      TempV t1 -> RM t t1
      ConstV c -> RMOnlyC t c
      _ -> error $ "unhandled quad in codeQuadruple: " <> show quad
    StoreQ v1 v2 -> case v1 of
      TempV t1 -> emitPlain Mov $ case v2 of
        TempV t2 -> MR t1 t2
        ConstV c -> MC t1 c
        LabelV l -> ML t1 l
      ConstV c -> emitPlain Mov $ case v2 of
        TempV t2 -> MOnlyCR c t2
        ConstV d -> MOnlyCC c d
        LabelV l -> MOnlyCL c l
      _ -> error $ "unhandled quad in codeQuadruple: " <> show quad
    CallQ l args -> withArgs args $ \defs uses -> emit Instruction
      { _piOperator = Call
      , _piAddressPattern = OnlyLabel l
      , _piExtraDefs = defs
      , _piExtraUses = uses
      , _piMark = Plain
      }
    MoveCallQ t l args -> withArgs args $ \defs uses -> do
      emit Instruction
        { _piOperator = Call
        , _piAddressPattern = OnlyLabel l
        , _piExtraDefs = defs
        , _piExtraUses = uses
        , _piMark = Plain
        }
      emitPlain Mov $ RR t rax
    MoveQ t v1 -> emitPlain Mov $ case v1 of
      TempV t1 -> RR t t1
      ConstV c -> RC t c
      LabelV l -> RL t l

withArgs :: SelectionContext m => [Var] -> ([Temp] -> [Temp] -> m ()) -> m ()
withArgs args routine = do
  -- mapM_ save [rax, rcx, rdx, r11]

  let (inRegs, inStacks) = splitAt (length paramRegisters) args

  -- Pass arguments through register,
  forM_ (zip paramRegisters inRegs) $ \(temp, var) -> codeQuadruple $ MoveQ temp var

  -- Pass arguments through the stack.
  forM_ (reverse inStacks) $ \case
    TempV t -> emitPlain Push $ OnlyR t
    ConstV c -> emitPlain Push $ OnlyC c
    LabelV l -> emitPlain Push $ OnlyLabel l

  emitPlain Xor $ RR rax rax

  -- Execute the subroutine.
  let dsts = [ rax, rcx, rdx, r11 ] <> paramRegisters
      srcs = zipWith const paramRegisters inRegs

  routine dsts (rax : srcs)

  -- Restore the stack.
  let delta = length inStacks * 8
  when (delta > 0) $ emitPlain Add $ RC rsp delta

  -- mapM_ restore [r11, rdx, rcx, rax]

  where
    paramRegisters = [ r10, rdi, rsi, rdx, rcx, r8, r9 ]

type Emit m = MonadWriter [Instruction Temp] m

emit :: Emit m => Instruction Temp -> m ()
emit x = tell [x]

emitPlain :: Emit m => Operator -> AddressPattern Temp -> m ()
emitPlain op pat = tell $ pure Instruction
  { _piOperator = op
  , _piAddressPattern = pat
  , _piExtraDefs = []
  , _piExtraUses = []
  , _piMark = Plain
  }

plainTreeOps :: [T.BinOp]
plainTreeOps = [T.Plus, T.Minus, T.And, T.Or, T.LeftShift, T.RightShift, T.Xor]
