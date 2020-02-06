{-# LANGUAGE FlexibleContexts #-}   
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module InstructionSelection
  ( selectInstructions
  , spill
  , removeRedundantJump
  ) where

import Control.Monad.Writer
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Except
import qualified Data.ByteString.Lazy.Char8 as Char8
import Control.Lens (zoom, _1, _2)
-- import Debug.Pretty.Simple

import Temp
import TempHelper
import Label
import Tree
import Instruction
import Frame
import Fragment

import Linear
import Trace

data SelectionError
  = UnexpectedStmt Stmt
  | UnexpectedExpr (Maybe Stmt) Expr
  deriving (Show, Eq)

type InstSel m = (MonadState TempPool m, MonadError SelectionError m)

type SelectionContext m =
  ( InstSel m
  , MonadReader (Maybe Stmt) m
  , MonadWriter [Instruction] m
  )

selectInstructions :: ProcFrag -> ExceptT String (State (TempPool, LabelPool)) [Instruction]
selectInstructions ProcFrag{..} = do
  let header = selectFunHeader _pfFrame
  let tail' = selectFunTail 
  stmts <- zoom _1 $ linearize _pfBody    
  scheduled <- zoom _2 $ do 
    withExceptT show (traceSchedule stmts) 
  body <- zoom _1 $ do
    withExceptT show $ selectFunBody scheduled
  return $ header <> body <> tail'

selectFunHeader :: Frame -> [Instruction]
selectFunHeader Frame{..} = execWriter $ do
  emitPlain "push %r" [rsp] [rbp, rsp] $ One (Src 0)
  emitPlain "mov %r, %r" [rbp] [rsp] $ Two (Dst 0) (Src 0)
  emitPlain ("add %r, " <~ _frPointer) [rsp] [rsp] $ One (Dst 0)
  emitMove "mov [%r - 8], %r" [] [rbp, r10] $ Two (Src 0) (Src 1)
  zipWithM f _frFormalsList paramRegisters
  where
    f temp reg = do
      emitMove "mov %r, %r" [temp] [reg] $ Two (Dst 0) (Src 0)    

selectFunTail :: [Instruction]
selectFunTail = execWriter $ do
  emitPlain "leave" [rbp, rsp] [rbp] Zero
  emitPlain "ret" [rsp] [rsp] Zero

selectFunBody :: InstSel m => [Stmt] -> m [Instruction]
selectFunBody body = execWriterT $ runReaderT (mapM_ codeStmt body) Nothing

spill :: Temp -> ProcFrag -> ProcFrag 
spill temp ProcFrag{..} = ProcFrag
  { _pfFrame = frame
  , _pfBody = body
  }
  where
    (body, frame) = flip runState _pfFrame $ do
      fp <- framePointer
      buff <- flip toExpr fp <$> allocLocal
      return $ spillStmt buff _pfBody
    spillStmt buff stmt = case stmt of
      SeqStmt s1 s2 -> SeqStmt (trs s1) (trs s2)
      LabelStmt _ -> stmt
      JumpStmt e dst -> JumpStmt (tre e) dst
      CJumpStmt op e1 e2 l1 l2 -> CJumpStmt op (tre e1) (tre e2) l1 l2
      MoveStmt e1 e2 -> MoveStmt (tre e1) (tre e2)
      ExprStmt e -> ExprStmt (tre e)
      where
        trs = spillStmt buff
        tre = spillExpr buff
    spillExpr buff expr = case expr of
      BinOpExpr op e1 e2 -> BinOpExpr op (tre e1) (tre e2)
      UnOpExpr op e -> UnOpExpr op (tre e)
      MemExpr e -> MemExpr (tre e)
      TempExpr t -> if t == temp then buff else expr
      ESeqExpr s e -> ESeqExpr (trs s) (tre e)
      NameExpr _ -> expr
      ConstExpr  _ -> expr
      CallExpr e args -> CallExpr (tre e) (fmap tre args)
      where
        trs = spillStmt buff        
        tre = spillExpr buff

removeRedundantJump :: [Instruction] -> [Instruction]
removeRedundantJump (j@(Jmp "jmp" dst) : l@(Lab label) : rest)
  = (if dst == label then id else (\rs -> j : l : rs)) $ removeRedundantJump rest
removeRedundantJump (x : y : rest) = x : removeRedundantJump (y : rest)
removeRedundantJump insts = insts

unexpectedStmt :: SelectionContext m => Stmt -> m a
unexpectedStmt = throwError . UnexpectedStmt

unexpectedExpr :: SelectionContext m => Expr -> m a
unexpectedExpr expr = do
  stmt <- ask
  throwError $ UnexpectedExpr stmt expr


type Emit m = MonadWriter [Instruction] m

emit :: Emit m => Instruction -> m ()
emit x = tell [x]

emitPlain :: Emit m => String -> [Temp] -> [Temp] -> KeySet -> m ()
emitPlain fmt defs_ uses_ keys = tell [Oper False fmt defs_ uses_ keys] 

emitMove :: Emit m => String -> [Temp] -> [Temp] -> KeySet -> m ()
emitMove fmt defs_ uses_ keys = tell [Oper True fmt defs_ uses_ keys]

  
plainBinary :: SelectionContext m => String -> Expr -> Expr -> m Temp
plainBinary name e1 e2 = do
  case (e1, e2) of
    (MemExpr e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      temp <- allocTemp
      emitMove "mov %r, [%r]" [temp] [r1] $ Two (Dst 0) (Src 0)
      emitPlain (name ++ " %r, [%r]") [temp] [temp, r2] $ Two (Src 0) (Src 1)
      return temp

    (ConstExpr x, ConstExpr y) -> do
      temp <- allocTemp
      emitMove ("mov %r, " <~ x) [temp] [] $ One (Dst 0)
      emitPlain (name ++ " %r, " <~ y) [temp] [temp] $ One (Src 0)
      return temp            

    (ConstExpr x, MemExpr e2') -> do
      r2 <- codeExpr e2'
      temp <- allocTemp
      emitMove ("mov %r, " <~ x) [temp] [] $ One (Dst 0)
      emitPlain (name ++ " %r, [%r]") [temp] [temp, r2] $ Two (Src 0) (Src 1)
      return temp

    (MemExpr (BinOpExpr Plus (ConstExpr x) e1'), ConstExpr y) -> do
      r1 <- codeExpr e1'
      temp <- allocTemp
      emitPlain (name ++ " qword [%r + " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0)
      emitMove ("mov %r, qword [%r + " <~ x ++ "]") [temp] [r1] $ Two (Dst 0) (Src 0)
      return temp

    (MemExpr (BinOpExpr Plus e1' (ConstExpr x)), ConstExpr y) -> do
      r1 <- codeExpr e1'
      temp <- allocTemp
      emitPlain (name ++ " qword [%r + " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0)
      emitMove ("mov %r, qword [%r + " <~ x ++ "]") [temp] [r1] $ Two (Dst 0) (Src 0)      
      return temp

    (MemExpr e1', ConstExpr y) -> do
      r1 <- codeExpr e1'
      temp <- allocTemp 
      emitPlain (name ++ " [%r], " <~ y) [r1] [r1] $ One (Src 0)
      emitMove "mov %r, [%r]" [temp] [r1] $ Two (Dst 0) (Src 0)
      return temp

    (e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitPlain (name ++ " %r, [%r]") [r1] [r1, r2] $ Two (Src 0) (Src 1)
      return r1

    (MemExpr e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      temp <- allocTemp
      emitMove "mov %r, [%r]" [temp] [r1] $ Two (Dst 0) (Src 0)
      emitPlain (name ++ " %r, %r") [temp] [temp, r2] $ Two (Src 0) (Src 1)
      return temp
      
    (e1', ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitPlain (name ++ " %r, " ++ show y) [r1] [r1] $ One (Src 0)
      return r1
      
    (ConstExpr x, e2') -> do
      r2 <- codeExpr e2'
      temp <- allocTemp
      emitMove ("mov %r, " ++ show x) [temp] [] $ One (Dst 0)
      emitPlain (name ++ " %r, %r") [temp] [temp, r2] $ Two (Src 0) (Src 1)
      return temp
      
    (e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitPlain (name ++ " %r, %r") [r1] [r1, r2] $ Two (Src 0) (Src 1)
      return r1

itimes :: SelectionContext m => Expr -> Expr -> m Temp
itimes e1 e2 = do
  temp <- allocTemp  
  case (e1, e2) of
    (MemExpr e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, [%r]" [rax] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "imul %r" [rax, rdx] [r2] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp
    (e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, [%r]" [rax] [r2] $ Two (Dst 0) (Src 0)
      emitPlain "imul %r" [rax, rdx] [r1] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp
    (MemExpr e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, [%r]" [rax] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "imul %r" [rax, rdx] [r2] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp
    (e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, %r" [rax] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "imul %r" [rax, rdx] [r2] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp

idivide :: SelectionContext m => Expr -> Expr -> m Temp
idivide e1 e2 = do
  temp <- allocTemp  
  case (e1, e2) of
    (MemExpr e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, 0" [rdx] [] $ One (Dst 0)
      emitMove "mov %r, [%r]" [rax] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "idiv [%r]" [rax, rdx] [r2] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp
    (e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, 0" [rdx] [] $ One (Dst 0)
      emitMove "mov %r, %r" [rax] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "idiv [%r]" [rax, rdx] [r2] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp
    (MemExpr e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, 0" [rdx] [] $ One (Dst 0)
      emitMove "mov %r, [%r]" [rax] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "idiv %r" [rax, rdx] [r2] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp
    (e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, 0" [rdx] [] $ One (Dst 0)
      emitMove "mov %r, %r" [rax] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "idiv %r" [rax, rdx] [r2] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [rax] $ Two (Dst 0) (Src 0)
      return temp

plainUnary :: SelectionContext m => String -> Expr -> m Temp
plainUnary name e = do
  temp <- allocTemp
  case e of
    MemExpr e' -> do
      r <- codeExpr e'
      emitMove "mov %r, [%r]" [temp] [r] $ Two (Dst 0) (Src 0)
      emitPlain (name ++ " %r") [temp] [temp] $ One (Src 0)
      return temp
    e' -> do
      r <- codeExpr e'
      emitPlain (name ++ " %r") [r] [r] $ One (Src 0)
      emitMove "mov %r, %r" [temp] [r] $ Two (Dst 0) (Src 0)
      return temp
      
codeExpr :: SelectionContext m => Expr -> m Temp
codeExpr expr = do
  case expr of
    BinOpExpr op e1 e2 -> do
      case op of
        Plus -> plainBinary "add" e1 e2
        Minus -> plainBinary "sub" e1 e2
        Times -> itimes e1 e2
        Divide -> idivide e1 e2
        And -> plainBinary "and" e1 e2
        Or -> plainBinary "or" e1 e2
        LeftShift -> plainBinary "shl" e1 e2
        RightShift -> plainBinary "shr" e1 e2
        Xor -> plainBinary "xor" e1 e2
    UnOpExpr op e -> do
      case op of
        Not -> plainUnary "not" e
        Negate -> plainUnary "neg" e
    MemExpr e -> do
      r <- codeExpr e
      temp <- allocTemp
      emitMove "mov %r, [%r]" [temp] [r] $ Two (Dst 0) (Src 0)
      return temp
    TempExpr temp -> do
      return temp
    NameExpr label -> do
      temp <- allocTemp
      let str = Char8.unpack $ _lbString label
      emitMove ("mov %r, " ++ str) [temp] [] $ One (Dst 0)
      return temp
    _ -> unexpectedExpr expr

(<~) :: String -> Int -> String
x <~ y = x ++ show y

cmp :: SelectionContext m => Expr -> Expr -> m ()
cmp e1 e2 = do
  temp <- allocTemp
  case (e1, e2) of
    (MemExpr e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, [%r]" [temp] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "cmp %r, [%r]" [] [temp, r2] $ Two (Src 0) (Src 1)
    (ConstExpr x, ConstExpr y) -> do
      emitMove ("mov %r, " ++ show x) [temp] [] $ One (Dst 0)
      emitPlain ("cmp %r, " ++ show y) [] [temp] $ One (Src 0)
    (ConstExpr x, MemExpr e2') -> do
      r2 <- codeExpr e2'
      emitMove ("mov %r, " ++ show x) [temp] [] $ One (Dst 0)
      emitPlain "cmp %r, [%r]" [] [temp, r2] $ Two (Src 0) (Src 1)

    (MemExpr (BinOpExpr Plus (ConstExpr x) e1'), ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitPlain ("cmp qword [%r + " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0)

    (MemExpr (BinOpExpr Plus e1' (ConstExpr x)), ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitPlain ("cmp qword [%r + " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0)

    (MemExpr (BinOpExpr Minus (ConstExpr x) e1'), ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitPlain ("cmp qword [%r - " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0)

    (MemExpr (BinOpExpr Minus e1' (ConstExpr x)), ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitPlain ("cmp qword [%r - " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0)

    (MemExpr e1', ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitMove "mov %r, [%r]" [temp] [r1] $ Two (Dst 0) (Src 0)
      emitPlain ("cmp %r, " ++ show y) [] [temp] $ One (Src 0)      

    (e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitPlain "cmp %r, [%r]" [] [r1, r2] $ Two (Src 0) (Src 1)
    (MemExpr e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, [%r]" [temp] [r1] $ Two (Dst 0) (Src 0)
      emitPlain "cmp %r, [%r]" [] [temp, r2] $ Two (Src 0) (Src 1)
    (ConstExpr x, e2') -> do
      r2 <- codeExpr e2'
      emitMove ("mov %r, " ++ show x) [temp] [] $ One (Dst 0)
      emitPlain "cmp %r, %r" [] [temp, r2] $ Two (Src 0) (Src 1)
    (e1', ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitPlain ("cmp %r, " ++ show y) [] [r1] $ One (Src 0)       
    (e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitPlain "cmp %r, %r" [] [r1, r2] $ Two (Src 0) (Src 1)  

jmp :: SelectionContext m => Expr -> Expr -> Label -> String -> m ()
jmp e1 e2 dst name = do
  cmp e1 e2
  emit $ Jmp name dst

move :: SelectionContext m => Stmt -> Expr -> Expr -> m ()
move stmt e1 e2 = do
  temp <- allocTemp
  case (e1, e2) of
    (TempExpr r1, CallExpr (NameExpr label) args) -> do
      (rdst, rsrc) <- codeArgs args
      emitPlain ("call " ++ Char8.unpack (_lbString label)) (rv : rdst) rsrc Zero
      emitMove "mov %r, %r" [r1] [rv] $ Two (Dst 0) (Src 0) 

    (MemExpr (BinOpExpr Plus (ConstExpr x) e1'), ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitMove ("mov qword [%r + " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0) 

    (MemExpr (BinOpExpr Plus e1' (ConstExpr x)), ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitMove ("mov qword [%r + " <~ x ++ "], " <~ y) [] [r1] $ One (Src 0) 

    (MemExpr (BinOpExpr Plus (ConstExpr x) e1'), MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove ("mov [%r + " <~ x ++ "], %r") [] [r1, r2] $ Two (Src 0) (Src 1)

    (MemExpr (BinOpExpr Plus e1' (ConstExpr x)), MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove ("mov [%r + " <~ x ++ "], %r") [] [r1, r2] $ Two (Src 0) (Src 1)

    (MemExpr (BinOpExpr Plus (ConstExpr x) e1'), e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove ("mov [%r + " <~ x ++ "], %r") [] [r1, r2] $ Two (Src 0) (Src 1)

    (MemExpr (BinOpExpr Plus e1' (ConstExpr x)), e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove ("mov [%r + " <~ x ++ "], %r") [] [r1, r2] $ Two (Src 0) (Src 1)
        
    (MemExpr e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, [%r]" [temp] [r2] $ Two (Dst 0) (Src 0)
      emitMove "mov [%r], %r" [] [r1, temp] $ Two (Src 0) (Src 1)

    (ConstExpr _, _) -> do
      throwError $ UnexpectedStmt stmt
      
    (e1', MemExpr e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, [%r]" [r1] [r2] $ Two (Dst 0) (Src 0)
      
    (MemExpr e1', ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitMove ("mov qword [%r], " ++ show y) [] [r1] $ One (Src 0)
      
    (MemExpr e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov [%r], %r" [] [r1, r2] $ Two (Src 0) (Src 1)
    
    (e1', ConstExpr y) -> do
      r1 <- codeExpr e1'
      emitMove ("mov %r, " ++ show y) [r1] [] $ One (Dst 0)
      
    (e1', e2') -> do
      r1 <- codeExpr e1'
      r2 <- codeExpr e2'
      emitMove "mov %r, %r" [r1] [r2] $ Two (Dst 0) (Src 0)

exprStmt :: SelectionContext m => Expr -> m ()
exprStmt e = do
  case e of
    CallExpr (NameExpr label) args -> do
      (rdst, rsrc) <- codeArgs args
      emitPlain ("call " ++ Char8.unpack (_lbString label)) (rv : rdst) rsrc Zero
    ConstExpr _ -> return ()
    e' -> do
      void $ codeExpr e'
      return ()
      
codeStmt :: SelectionContext m => Stmt -> m ()
codeStmt stmt = local (const $ Just stmt) $ do
  case stmt of
    LabelStmt label -> do
      emit $ Lab label
    JumpStmt (NameExpr label) _ -> do
      emit $ Jmp "jmp" label
    CJumpStmt op e1 e2 label _ -> do
      jmp e1 e2 label $ case op of
        Equal -> "je"
        NotEqual -> "jne"
        Less -> "jl"
        LessOrEqual -> "jle"
        Greater -> "jg"
        GreaterOrEqual -> "jge"
        NotLess -> "jnl"
        NotLessOrEqual -> "jne"
        NotGreater -> "jng"
        NotGreaterOrEqual -> "jnge"        
    MoveStmt e1 e2 -> move stmt e1 e2
    ExprStmt e -> exprStmt e
    _ -> unexpectedStmt stmt
    
codeArgs :: SelectionContext m => [Expr] -> m ([Temp], [Temp])
codeArgs args = do
  mapM_ code (reverse $ zip [0..] args)
  return ([rbp, rsp], [rbp, rsp])
  where
    code :: SelectionContext m => (Int, Expr) -> m ()
    code (i, expr)
      | i == 0 = toReg r10
      | i == 1 = toReg rdi
      | i == 2 = toReg rsi
      | i == 3 = toReg rdx
      | i == 4 = toReg rcx
      | i == 5 = toReg r8
      | i == 6 = toReg r9    
      | otherwise = do
          case expr of
            ConstExpr x -> do
              emitPlain ("push " <~ x) [rsp] [rsp] Zero
            NameExpr label -> do
              emitPlain ("push " ++ show (_lbString label)) [rsp] [] Zero
            _ -> do
              r <- codeExpr expr              
              emitPlain "push %r" [] [r] $ One (Src 0)              
      where 
        toReg dst = void $ codeStmt $ MoveStmt (TempExpr dst) expr

paramRegisters :: [Temp]
paramRegisters =
  [ rdi
  , rsi
  , rdx
  , rcx
  , r8
  , r9
  ]
