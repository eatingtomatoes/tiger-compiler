{-# LANGUAGE FlexibleContexts #-}

module PseudoAsmGenSpec where

import Test.Hspec
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.Except
import Data.Default
import Text.Pretty.Simple

import Temp
import PseudoAsmGen
import PseudoAsm
import qualified Tree
import qualified TreeHelper as H

-- type InstSel m = (MonadState TempPool m, MonadError CodeError m)

-- type CodeContext m =
--   ( InstSel m
--   , MonadReader (Maybe Stmt) m
--   , MonadWriter [Inst] m
--   )

runGen
  :: ReaderT (Maybe Tree.Stmt) (StateT TempPool (WriterT [Inst] (Except CodeError))) ()
  -> Either CodeError [Inst]
runGen = runExcept . execWriterT . flip evalStateT def . flip runReaderT Nothing

stmt :: Tree.Stmt
stmt = H.move dst src 

dst :: Tree.Expr
dst = H.mem cell

src :: Tree.Expr
src = H.plus (H.mem cell) (H.const 1)

cell :: Tree.Expr
cell = H.plus (H.const (-8)) (H.temp FP)

pseudo :: [Inst]
pseudo = [ Oper "add qword [%r + -8], 1" [] [FP] (One (Src 0))
         , Mov "mov %r, qword [%r + -8]" [Temp {_tpIndex = 3}] [FP] (Two (Dst 0) (Src 0))
         , Mov "mov [%r + -8], %r" [] [FP,Temp {_tpIndex = 3}] (Two (Src 0) (Src 1))
         ]

spec :: Spec
spec = do
  describe "test move" $ do
    it "" $ do
      (runGen $ move stmt dst src) `shouldBe` Right pseudo
