{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module PseudoInst where

import Text.Printf
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Writer
import Control.Lens (makeLenses, _1, _2, over, zoom, use)
import Debug.Pretty.Simple

import Temp
import TempHelper
import Label

data PseudoInst r
  = PseudoInst
  { _piOperator :: Operator
  , _piAddrPattern :: AddrPattern r
  , _piExtraDefs :: [Temp]
  , _piExtraUses :: [Temp]
  , _piMark :: InstMark
  }

data InstMark
  = ReserveLocalSpace
  | Plain
  deriving (Eq)

data Operator
  = Push
  | Pop
  | Lea
  | Mov
  | Add
  | Sub
  | And
  | Or
  | Shl
  | Shr
  | Xor
  | Cmp  
  | Leave
  | Ret
  | Jmp
  | JE
  | JNE
  | JL
  | JNL  
  | JLE
  | JNLE
  | JG
  | JNG    
  | JGE
  | JNGE
  | IMul
  | IDiv
  | Not
  | Neg
  | Call
  | Lab
  deriving (Eq)

instance Show Operator where
  show op = case op of
    Push -> "push"
    Pop -> "pop"
    Lea -> "lea"
    Mov -> "mov"
    Add -> "add"
    Sub -> "sub"
    Leave -> "leave"
    Ret -> "ret"
    Jmp -> "jmp"
    JE -> "je"
    JNE -> "jne"
    JL -> "jl"
    JNL -> "jnl"
    JLE -> "jle"
    JNLE -> "jnle"
    JG -> "jg"
    JNG -> "jng"
    JGE -> "jge"
    JNGE -> "jnge"
    IMul -> "imul"
    IDiv -> "idiv"
    And -> "and"
    Or -> "or"
    Shl -> "shl"
    Shr -> "shr"
    Xor -> "xor"
    Not -> "not"
    Neg -> "neg"
    Cmp -> "cmp"
    Call -> "call"
    Lab -> ""

data AddrPattern r
  = RR r r -- op r, r
  | RC r Int -- op r, c
  | RL r Label -- op r, xxx
  | RM r r -- op r, [r]
  | RMPlusC r r Int -- op r, [r + c]
  | RMMinusC r r Int -- op r, [r - c]
  | RMOnlyC r Int -- op r, [c]
  | MR r r -- op [r], r
  | MPlusCR r Int r -- op [r + c], r
  | MMinusCR r Int r -- op [r - c], r
  | MOnlyCR Int r -- op [c], r
  | MC r Int -- op [r], c
  | MPlusCC r Int Int -- op [r + c], c
  | MMinusCC r Int Int -- op [r - c], c
  | MOnlyCC Int Int -- op [c], c
  | ML r Label -- op [r], xxx
  | MPlusCL r Int Label -- op [r + c], xxx
  | MMinusCL r Int Label -- op [r - c] xxx
  | MOnlyCL Int Label -- op [c] xxx
  | OnlyR r -- op r
  | OnlyC Int -- op c
  | OnlyM r -- op [r]
  | OnlyMPlusC r Int -- op [r + c]
  | OnlyMMinusC r Int -- op [r - c]
  | OnlyMOnlyC Int -- op [c]    
  | OnlyLabel Label
  | None
  deriving (Show)

makeLenses ''PseudoInst

defsAndUses :: PseudoInst Temp -> (Set.Set Temp, Set.Set Temp) 
defsAndUses pit@PseudoInst{..} = (ds', us')
  where
    ds' = Set.fromList $ _piExtraDefs <> ds
    us' = Set.fromList $ _piExtraUses <> us
    (ds, us) = case _piOperator of
      Push -> case _piAddrPattern of
        OnlyR x -> ([rsp], [rsp, x])
        OnlyC _ -> ([rsp], [rsp])
        OnlyLabel _ -> ([rsp], [rsp])
        OnlyM x -> ([rsp], [rsp, x])
        OnlyMPlusC x _ -> ([rsp], [rsp, x])
        OnlyMMinusC x _ -> ([rsp], [rsp, x])
        OnlyMOnlyC _ -> ([rsp], [rsp])
        _ -> reportInvalidInst "defsAndUses" pit

      Pop -> case _piAddrPattern of
        OnlyR x -> ([rsp, x], [rsp])
        OnlyM x -> ([rsp], [rsp, x])
        OnlyMPlusC x _ -> ([rsp], [rsp, x])
        OnlyMMinusC x _ -> ([rsp], [rsp, x])
        OnlyMOnlyC _ -> ([rsp], [rsp])
        _ -> reportInvalidInst "defsAndUses" pit

      Lea -> case _piAddrPattern of
        RMPlusC x y _ -> ([x], [y])
        RMMinusC x y _ -> ([x], [y])
        _ -> reportInvalidInst "defsAndUses" pit
        
      Mov -> case _piAddrPattern of
        RR x y -> ([x], [y])
        RC x _ -> ([x], [])
        RL x _ -> ([x], [])
        RM x y -> ([x], [y])
        RMPlusC x y _ -> ([x], [y])
        RMMinusC x y _ -> ([x], [y])
        RMOnlyC x _ -> ([x], [])
        MR x y -> ([], [x, y])
        MPlusCR x _ y -> ([], [x, y])
        MMinusCR x _ y -> ([], [x, y])
        MOnlyCR _ x -> ([], [x])
        MC x _ -> ([], [x])
        MPlusCC x _ _ -> ([], [x])
        MMinusCC x _ _ -> ([], [x])
        MOnlyCC _ _ -> ([], [])
        ML x _ -> ([], [x])
        MPlusCL x _ _-> ([], [x])
        MMinusCL x _ _ -> ([], [x])
        MOnlyCL _ _ -> ([], [])
        _ -> reportInvalidInst "defsAndUses" pit

      _ | _piOperator `elem` [Add, Sub, And, Or, Shl, Shr, Xor] -> case _piAddrPattern of
            RR x y -> ([x], [x, y])
            RC x _ -> ([x], [x])
            RM x _ -> ([x], [x])
            RMPlusC x y _ -> ([x], [x, y])
            RMMinusC x y _ -> ([x], [x, y])
            RMOnlyC x _ -> ([x], [x])
            MR x y -> ([], [x, y])
            MPlusCR x _ y -> ([], [x, y])
            MMinusCR x _ y -> ([], [x, y])
            MOnlyCR _ x -> ([], [x])
            MC x _ -> ([], [x])
            MPlusCC x _ _ -> ([], [x])
            MMinusCC x _ _ -> ([], [x])
            MOnlyCC _ _ -> ([], [])
            _ -> reportInvalidInst "defsAndUses" pit

      Cmp -> case _piAddrPattern of
        RR x y -> ([], [x, y])
        RC x _ -> ([], [x])
        RM x y -> ([], [x, y])
        RMPlusC x y _ -> ([], [x, y])
        RMMinusC x y _ -> ([], [x, y])
        RMOnlyC x _ -> ([], [x])
        _ -> reportInvalidInst "defsAndUses" pit

      Leave -> case _piAddrPattern of
        None -> ([rsp, rbp], [rbp])
        _ -> reportInvalidInst "defsAndUses" pit
    
      Ret -> case _piAddrPattern of
        None -> ([rsp], [rsp])
        _ -> reportInvalidInst "defsAndUses" pit

      _ | _piOperator `elem` [Jmp, JE, JNE, JL, JNL, JLE, JNLE, JG, JNG, JGE, JNGE] ->
          case _piAddrPattern of
            OnlyLabel _ -> ([], [])
            _ -> reportInvalidInst "defsAndUses" pit

      _ | _piOperator `elem` [IMul, IDiv] -> case _piAddrPattern of
            OnlyR r -> ([rax, rdx], [rax, r])
            OnlyC _ -> ([rax, rdx], [rax])
            OnlyM r -> ([rax, rdx], [rax, r])
            OnlyMPlusC r _ -> ([rax, rdx], [rax, r])
            OnlyMMinusC r _ -> ([rax, rdx], [rax, r])
            OnlyMOnlyC _ -> ([rax, rdx], [rax])
            _ -> reportInvalidInst "defsAndUses" pit
            
      _ | _piOperator `elem` [Not, Neg] -> case _piAddrPattern of
            OnlyR r -> ([r], [r])
            OnlyM r -> ([], [r])
            OnlyMPlusC r _ -> ([], [r])
            OnlyMMinusC r _ -> ([], [r])
            OnlyMOnlyC _ -> ([], [])
            _ -> reportInvalidInst "defsAndUses" pit

      Call -> case _piAddrPattern of
        OnlyLabel _ -> ([], [])
        _ -> reportInvalidInst "defsAndUses" pit
    
      Lab -> case _piAddrPattern of
        OnlyLabel _ -> ([], [])
        _ -> reportInvalidInst "defsAndUses" pit

      _ -> error $ "unhandled operator: " <> show _piOperator

branchOps :: [Operator]
branchOps = [Jmp, JE, JNE, JL, JNL, JLE, JNLE, JG, JNG, JGE, JNGE]
  
isMove :: PseudoInst r -> Bool
isMove PseudoInst{..} = case _piOperator of
  Mov -> True
  _ -> False

labelOf :: PseudoInst r -> Maybe Label
labelOf PseudoInst{..} = case (_piOperator, _piAddrPattern) of
  (Lab, OnlyLabel l) -> Just l
  _ -> Nothing

isBranch :: PseudoInst r -> Bool
isBranch pit = elem (_piOperator pit) branchOps
  
isLabel :: PseudoInst r -> Bool
isLabel pit = _piOperator pit == Lab

branchDstOf :: PseudoInst r -> Maybe Label
branchDstOf PseudoInst{..} = case _piAddrPattern of
  OnlyLabel l | elem _piOperator branchOps -> Just l
  _ -> Nothing


instance PrintfArg r => Show (PseudoInst r) where
  show PseudoInst{..} = case _piAddrPattern of
    RR x y -> printf "%s %r, %r" op x y
    RC x c -> printf "%s %r, %d" op x c
    RL x l -> printf "%s %r, %s" op x (toString l)
    RM x y -> printf "%s %r, [%r]" op x y
    RMPlusC x y c -> printf "%s %r, [%r + %d]" op x y c
    RMMinusC x y c -> printf "%s %r, [%r - %d]" op x y c
    RMOnlyC x c -> printf "%s %r, [%d]" op x c
    MR x y -> printf "%s [%r], %r" op x y
    MPlusCR x c y -> printf "%s [%r + %d], %r" op x c y
    MMinusCR x c y -> printf "%s [%r - %d], %r"  op x c y
    MOnlyCR c x -> printf "%s [%d], %r" op c x
    MC x c -> printf "%s qword [%r], %d" op x c
    MPlusCC x c d -> printf "%s qword [%r + %d], %d" op x c d
    MMinusCC x c d -> printf "%s qword [%r - %d], %d" op x c d 
    MOnlyCC c d -> printf "%s qword [%d], %d" op c d
    ML x l -> printf "%s qword [%r], %s" op x (toString l)
    MPlusCL x c l -> printf "% qword [%r + %d], %s" op x c (toString l)
    MMinusCL x c l -> printf "% qword [%r - %d], %s" op x c (toString l)
    MOnlyCL c l -> printf "%s qword [%d], %s" op c (toString l)
    OnlyR x -> printf "%s %r" op x
    OnlyC c -> printf "%s %d" op c
    OnlyM x -> printf "%s [%r]" op x
    OnlyMPlusC x c -> printf "%s [%r + %d]" op x c
    OnlyMMinusC x c -> printf "%s [%r - %d]" op x c
    OnlyMOnlyC c -> printf "%s [%d]" op c
    OnlyLabel l -> case _piOperator of
      Lab -> printf "%s:" (toString l)
      _ -> printf "%s %s" op (toString l)
    None -> printf "%s" op
    where
      op = show _piOperator

reportInvalidInst :: Show r => String -> PseudoInst r -> a
reportInvalidInst fun PseudoInst{..} = error
  $ "unexpected address pattern for `"
  <> show _piOperator
  <> "': "
  <> show _piAddrPattern
  <> " in func `"
  <> fun
  <> "'"

instance Functor PseudoInst where
  fmap f pit = pit { _piAddrPattern = pat }
    where
      pat = case _piAddrPattern pit of
        RR x y -> RR (f x) (f y)
        RC x c -> RC (f x) c
        RL x l -> RL (f x) l
        RM x y -> RM (f x) (f y)
        RMPlusC x y c -> RMPlusC (f x) (f y) c
        RMMinusC x y c -> RMMinusC (f x) (f y) c
        RMOnlyC x c -> RMOnlyC (f x) c
        MR x y -> MR (f x) (f y)
        MPlusCR x c y -> MPlusCR (f x) c (f y)
        MMinusCR x c y -> MMinusCR (f x) c (f y)
        MOnlyCR c x -> MOnlyCR c (f x)
        MC x c -> MC (f x) c
        MPlusCC x c d -> MPlusCC (f x) c d
        MMinusCC x c d -> MMinusCC (f x) c d
        MOnlyCC c d -> MOnlyCC c d
        ML x l -> ML (f x) l
        MPlusCL x c l -> MPlusCL (f x) c l
        MMinusCL x c l -> MMinusCL (f x) c l
        MOnlyCL c l -> MOnlyCL c l
        OnlyR x -> OnlyR (f x)
        OnlyC c -> OnlyC c
        OnlyM x -> OnlyM (f x)
        OnlyMPlusC x c -> OnlyMPlusC (f x) c
        OnlyMMinusC x c -> OnlyMMinusC (f x) c
        OnlyMOnlyC c -> OnlyMOnlyC c
        OnlyLabel l -> OnlyLabel l
        None -> None

-- type SpillContext = WriterT [PseudoInst Temp] (State (Maybe Int, TempPool))

type SpillContext = StateT (Maybe Int, TempPool) (Writer [PseudoInst Temp])

spill :: MonadState TempPool m => Temp -> [PseudoInst Temp] -> m [PseudoInst Temp]
spill target insts = pTrace ("to spill `" <> show target <> "' out") $ do
  pool <- get
  let (pool', insts') = spill' pool target insts
  put pool'
  return insts'
  
spill' :: TempPool -> Temp -> [PseudoInst Temp] -> (TempPool, [PseudoInst Temp])
spill' pool target insts
  = over _1 snd
  $ runWriter
  $ execStateT (mapM f insts) (Nothing, pool)
  where
    emit :: PseudoInst Temp -> SpillContext ()
    emit = tell . pure

    copyToTemp :: Int -> SpillContext Temp
    copyToTemp offset = do
      temp <- zoom _2 allocTemp
      emit PseudoInst
        { _piOperator = Mov
        , _piAddrPattern = RMMinusC temp rbp offset
        , _piExtraDefs = []
        , _piExtraUses = []
        , _piMark = Plain
        }
      return temp
      
    f :: PseudoInst Temp -> SpillContext ()
    f pit@PseudoInst{..} = use _1 >>= \case
      Nothing -> _piMark == ReserveLocalSpace $$ do
        case (_piOperator, _piAddrPattern) of
          (Add, RC x c) -> x == target $$ do
            let offset = -c + 8
            zoom _1 $ put $ Just offset
            emit pit { _piOperator = Sub, _piAddrPattern = RC x offset}
          (Sub, RC x c) -> x == rsp $$ do
            let offset = c + 8
            zoom _1 $ put $ Just offset
            emit pit {_piAddrPattern = RC x offset}
          _ -> reportInvalidInst "spill" pit
      Just offset -> do
        case _piOperator of
          Push -> case _piAddrPattern of
            OnlyR x -> x == target $$ do
              emit pit { _piAddrPattern = OnlyMMinusC rbp offset }
            OnlyC _ -> emit pit
            OnlyLabel _ -> emit pit
            OnlyM x -> x == target $$ do
              temp <- copyToTemp offset
              emit $ fmap (const temp) pit
            OnlyMPlusC x _ -> x == target $$ do
              temp <- copyToTemp offset
              emit $ fmap (const temp) pit
            OnlyMMinusC x _ -> x == target $$ do
              temp <- copyToTemp offset
              emit $ fmap (const temp) pit
            OnlyMOnlyC _ -> emit pit
            _ -> reportInvalidInst "spill" pit
            
          Pop -> case _piAddrPattern of
            OnlyR x -> x == target $$ do
              emit pit { _piAddrPattern = OnlyMMinusC rbp offset }
            OnlyM x -> x == target $$ do
              temp <- copyToTemp offset
              emit $ fmap (const temp) pit
            OnlyMPlusC x _ -> x == target $$ do
              temp <- copyToTemp offset              
              emit $ fmap (const temp) pit
            OnlyMMinusC _ _ -> do
              temp <- copyToTemp offset              
              emit $ fmap (const temp) pit
            OnlyMOnlyC _ -> emit pit
            _ -> reportInvalidInst "spill" pit

          Lea -> case _piAddrPattern of
            RMPlusC x y c -> case (x == target, y == target) of
              (True, True) -> do
                tempA <- copyToTemp offset
                tempB <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMPlusC tempB tempA c }
                emit pit { _piOperator = Mov, _piAddrPattern = MMinusCR rbp offset tempB }
              (True, False) -> do
                temp <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMPlusC temp y c }
                emit pit { _piOperator = Mov, _piAddrPattern = MMinusCR rbp offset temp }
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = RMPlusC x temp c }
              _ -> emit pit
            RMMinusC x y c -> case (x == target, y == target) of
              (True, True) -> do
                tempA <- copyToTemp offset
                tempB <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMMinusC tempB tempA c }
                emit pit { _piOperator = Mov, _piAddrPattern = MMinusCR rbp offset tempB }
              (True, False) -> do
                temp <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMMinusC temp y c }
                emit pit { _piOperator = Mov, _piAddrPattern = MMinusCR rbp offset temp }
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = RMMinusC x temp c }
              _ -> emit pit
            _ -> reportInvalidInst "spill" pit
            
          Mov -> case _piAddrPattern of
            RR x y -> case (x == target, y == target) of
              (True, True) -> return ()
              (True, False) -> emit pit { _piAddrPattern = MMinusCR rbp offset y  }
              (False, True) -> emit pit { _piAddrPattern = RMMinusC x rbp offset }
              _ -> emit pit
            RC x c -> x == target $$ do
              emit pit { _piAddrPattern = MMinusCC rbp offset c }
            RL x l -> x == target $$ do
              emit pit { _piAddrPattern = MMinusCL rbp offset l }              
            RM x y -> case (x == target, y == target) of
              (True, True) -> do
                tempA <- copyToTemp offset
                tempB <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RM tempB tempA }
                emit pit { _piAddrPattern = MMinusCR rbp offset tempB }
              (True, False) -> do
                temp <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RM temp y }
                emit pit { _piAddrPattern = MMinusCR rbp offset temp }
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = RM x temp }
              _ -> emit pit
            RMPlusC x y c -> case (x == target, y == target) of
              (True, True) -> do
                tempA <- copyToTemp offset
                tempB <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMPlusC tempB tempA c }
                emit pit { _piAddrPattern = MMinusCR rbp offset tempB }
              (True, False) -> do
                temp <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMPlusC temp y c }
                emit pit { _piAddrPattern = MMinusCR rbp offset temp }
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = RMPlusC x temp c }
              _ -> emit pit
            RMMinusC x y c -> case (x == target, y == target) of
              (True, True) -> do
                tempA <- copyToTemp offset
                tempB <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMMinusC tempB tempA c }
                emit pit { _piAddrPattern = MMinusCR rbp offset tempB }
              (True, False) -> do
                temp <- zoom _2 allocTemp
                emit pit { _piAddrPattern = RMMinusC temp y c }
                emit pit { _piAddrPattern = MMinusCR rbp offset temp }
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = RMMinusC x temp c }
              _ -> emit pit
            RMOnlyC x c -> x == target $$ do
              temp <- zoom _2 allocTemp
              emit pit { _piAddrPattern = RMOnlyC temp c }
              emit pit { _piAddrPattern = MMinusCR rbp offset temp }
            MR x y -> case (x == target, y == target) of
              (True, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MR temp temp }
              (True, False) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MR temp y}
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MR x temp }
              _ -> emit pit
            MPlusCR x c y -> case (x == target, y == target) of
              (True, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MPlusCR temp c temp }
              (True, False) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MPlusCR temp c y}
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MPlusCR x c temp }
              _ -> emit pit
            MMinusCR x c y -> case (x == target, y == target) of
              (True, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MMinusCR temp c temp }
              (True, False) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MMinusCR temp c y}
              (False, True) -> do
                temp <- copyToTemp offset
                emit pit { _piAddrPattern = MMinusCR x c temp }
              _ -> emit pit
            MOnlyCR c x -> x == target $$ do
              temp <- copyToTemp offset
              emit pit { _piAddrPattern = MOnlyCR c temp }
            MC x c -> x == target $$ do
              temp <- copyToTemp offset
              emit pit { _piAddrPattern = MC temp c }
            MPlusCC x c d -> x == target $$ do
              temp <- copyToTemp offset
              emit pit { _piAddrPattern = MPlusCC temp c d }
            MMinusCC x c d -> x == target $$ do
              temp <- copyToTemp offset
              emit pit { _piAddrPattern = MMinusCC temp c d }
            MOnlyCC _ _ -> emit pit
            ML x l -> x == target $$ do
              temp <- copyToTemp offset
              emit pit { _piAddrPattern = ML temp l }
            MPlusCL x c l -> x == target $$ do
              temp <- copyToTemp offset
              emit pit { _piAddrPattern = MPlusCL temp c l }
            MMinusCL x c l -> x == target $$ do
              temp <- copyToTemp offset
              emit pit { _piAddrPattern = MMinusCL temp c l }
            MOnlyCL _ _ -> emit pit            
            _ -> reportInvalidInst "spill" pit
            
          _ | _piOperator `elem` [Add, Sub, And, Or, Shl, Shr, Xor, Cmp] -> do
                let move pat = PseudoInst
                      { _piOperator = Mov
                      , _piAddrPattern = pat
                      , _piExtraDefs = []
                      , _piExtraUses = []
                      , _piMark = Plain
                      }
                case _piAddrPattern of
                  RR x y -> case (x == target, y == target) of
                    (True, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MMinusCR rbp offset temp }
                    (True, False) -> do
                      emit pit { _piAddrPattern = MMinusCR rbp offset y }
                    (False, True) -> do
                      emit pit { _piAddrPattern = RMMinusC x rbp offset }
                    _ -> emit pit
                  RC x c -> x == target $$ do
                    emit pit { _piAddrPattern = MMinusCC rbp offset c }
                  RM x y -> case (x == target, y == target) of
                    (True, True) -> do
                      tempA <- copyToTemp offset
                      tempB <- zoom _2 allocTemp
                      emit $ move $ RM tempB tempA
                      emit pit { _piAddrPattern = MMinusCR rbp offset tempB }
                    (True, False) -> do
                      temp <- zoom _2 allocTemp
                      emit $ move $ RM temp y
                      emit pit { _piAddrPattern = MMinusCR rbp offset temp }
                    (False, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = RM x temp }
                    _ -> emit pit
                  RMPlusC x y c -> case (x == target, y == target) of
                    (True, True) -> do
                      tempA <- copyToTemp offset
                      tempB <- zoom _2 allocTemp
                      emit $ move $ RMPlusC tempB tempA c
                      emit pit { _piAddrPattern = MMinusCR rbp offset tempB }
                    (True, False) -> do
                      temp <- zoom _2 allocTemp
                      emit $ move $ RMPlusC temp y c
                      emit pit { _piAddrPattern = MMinusCR rbp offset temp }
                    (False, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = RMPlusC x temp c }
                    _ -> emit pit
                  RMMinusC x y c -> case (x == target, y == target) of
                    (True, True) -> do
                      tempA <- copyToTemp offset
                      tempB <- zoom _2 allocTemp
                      emit $ move $ RMMinusC tempB tempA c
                      emit pit { _piAddrPattern = MMinusCR rbp offset tempB }
                    (True, False) -> do
                      temp <- zoom _2 allocTemp
                      emit $ move $ RMMinusC temp y c
                      emit pit { _piAddrPattern = MMinusCR rbp offset temp }
                    (False, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = RMMinusC x temp c }
                    _ -> emit pit
                  RMOnlyC x c -> x == target $$ do
                    temp <- zoom _2 allocTemp
                    emit $ move $ RMOnlyC temp c
                    emit pit { _piAddrPattern = MMinusCR rbp offset temp }
                  MR x y -> case (x == target, y == target) of
                    (True, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MR temp temp }
                    (True, False) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MR temp y }
                    (False, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MR x temp }
                    _ -> emit pit
                  MPlusCR x c y -> case (x == target, y == target) of
                    (True, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MPlusCR temp c temp }
                    (True, False) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MPlusCR temp c y }
                    (False, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MPlusCR x c temp }
                    _ -> emit pit
                  MMinusCR x c y -> case (x == target, y == target) of
                    (True, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MMinusCR temp c temp }
                    (True, False) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MMinusCR temp c y }
                    (False, True) -> do
                      temp <- copyToTemp offset
                      emit pit { _piAddrPattern = MMinusCR x c temp }
                    _ -> emit pit
                  MC x c -> x == target $$ do
                    temp <- copyToTemp offset
                    emit pit { _piAddrPattern = MC temp c }
                  MPlusCC x c d -> x == target $$ do
                    temp <- copyToTemp offset
                    emit pit { _piAddrPattern = MPlusCC temp c d }
                  MMinusCC x c d -> x == target $$ do
                    temp <- copyToTemp offset
                    emit pit { _piAddrPattern = MPlusCC temp c d }
                  MOnlyCC _ _ -> emit pit
                  _ -> reportInvalidInst "spill" pit
                
          Leave -> emit pit

          Ret -> emit pit

          _ | _piOperator `elem` [Jmp, JE, JNE, JL, JNL, JLE, JNLE, JG, JNG, JGE, JNGE] -> do
                emit pit

          _ | _piOperator `elem` [IMul, IDiv] -> case _piAddrPattern of
                OnlyR x -> x == target $$ do
                  emit pit { _piAddrPattern = OnlyMMinusC rbp offset }
                OnlyC _ -> emit pit
                OnlyM x -> x == target $$ do
                  temp <- copyToTemp offset
                  emit pit { _piAddrPattern = OnlyM temp }
                OnlyMPlusC x c -> x == target $$ do
                  temp <- copyToTemp offset
                  emit pit { _piAddrPattern = OnlyMPlusC temp c }
                OnlyMMinusC x c -> x == target $$ do
                  temp <- copyToTemp offset
                  emit pit { _piAddrPattern = OnlyMMinusC temp c }
                OnlyMOnlyC _ -> emit pit
                _ -> reportInvalidInst "spill" pit

          _ | _piOperator `elem` [Not, Neg] -> case _piAddrPattern of
                OnlyR x -> x == target $$ do
                  emit pit { _piAddrPattern = OnlyMMinusC rbp offset }
                OnlyMPlusC x c -> x == target $$ do
                  temp <- copyToTemp offset
                  emit pit { _piAddrPattern = OnlyMPlusC temp c }
                OnlyMMinusC x c -> x == target $$ do
                  temp <- copyToTemp offset
                  emit pit { _piAddrPattern = OnlyMMinusC temp c }
                OnlyMOnlyC _ -> emit pit
                _ -> reportInvalidInst "spill" pit

          Call -> emit pit

          Lab -> emit pit

          _ -> error
            $ "unhandled operator: "
            <> show _piOperator
            <> " in func `spill'"
      where
        infix 0 $$
        ($$) :: Bool -> SpillContext () -> SpillContext ()
        p $$ action = if p then action else emit pit
    
        
