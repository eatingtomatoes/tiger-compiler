{-# LANGUAGE QuasiQuotes #-} 
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-} 

module Compiler where

import qualified Data.Set as Set
import qualified Data.ByteString.Lazy.Char8 as Char8
import Data.ByteString.Lazy.Builder (Builder, toLazyByteString)
import Control.Monad.Except
import System.Exit

import Control.Monad.State
import Data.Default
import Data.Foldable
import Text.Pretty.Simple
import Debug.Pretty.Simple
import Control.Lens
import Control.Monad
import qualified Data.Map as Map

import FrontEnd.TigerParser
import Translation.AstToTree.SymbolTable
import Translation.Fragment
import Translation.Frame
import Backend.RegisterAllocation
import MiddleEnd.Tree.Linear
import MiddleEnd.Tree.Trace

import CompilerOpts
import Translation.Temp
import Translation.Label
import Translation.AstToTree.TransState
import Translation.AstToTree
import Translation.TreeToQuadruple
import Translation.QuadrupleToSSA
import Translation.SSAToQuadruple

import MiddleEnd.Tree.InstructionSelection
import qualified MiddleEnd.Quadruple.InstructionSelection as QInstSel
import MiddleEnd.Quadruple.Optimization.CopyPropagation
import MiddleEnd.Quadruple.Optimization.EliminateCommonSubexpression
import MiddleEnd.Quadruple.Optimization.EliminateDeadCode
import MiddleEnd.Quadruple.Loop.Dominance
import MiddleEnd.Quadruple.Loop.LoopNestForest
import MiddleEnd.Quadruple.Loop.LoopInvariant
import MiddleEnd.Quadruple.ControlFlowGraph
import MiddleEnd.Quadruple.BlockControlFlowGraph
import MiddleEnd.Quadruple.Loop.Hoisting
import qualified MiddleEnd.SSA.ControlFlowGraph as SSACFG
import MiddleEnd.Optimization

import Backend.AssemblerGen 
import Backend.Instruction
import Backend.Optimization

import Utility.Debug.Trace
import Utility.Debug
import Utility.Visualize

import GraphHelper

runCompiler :: IO ()
runCompiler = do
  CompilerOpts{..} <- getCompilerOpts
  source <- maybe Char8.getContents Char8.readFile _inPath

  ast <- access $ parseTiger source

  when _dumpAst $ do
    pPrint "ast:"
    pPrint ast  
  
  TransState{..} <- access $ transAstToTree def ast  
  
  funs <- (access =<<) $ flip evalStateT (_trsTempPool, _trsLabelPool) $ do
    runExceptT $ do
      forM (toList _trsProcFragSet) $ \frag -> do
        let funHeader = selectFunHeader $ _pfFrame frag 
            funTail = selectFunTail

        treeBody <- zoom _1 (linearize $ _pfBody frag) >>= zoom _2 . traceSchedule
 
        when _dumpTreeBody $ do
          pPrint "tree body:"
          pPrint treeBody
           
        funBody <- do
          case _useQuadruple of 
            False -> zoom _1 $ selectFunBody $ treeBody  
            _  -> do
              quads <- zoom _1 $ do
                fmap concat $ mapM transTreeToQuadruple treeBody
 
              when _dumpQuadruple $ do
                pPrint "unoptimized quadruple:"
                pPrint $ fmap show $ zip [0..] quads

              let optimizer = identity
                    `andThen` copyPropagation vocal
                    `andThen` eliminateCommonSubexpression vocal
                    `andThen` eliminateDeadCode vocal
                    `andThen` hoist vocal   

              quads <- case _optimizeQuadruple of
                False -> return quads
                _ -> do
                  quads <- runOptimizer optimizer quads
                  when _dumpOptimizedQuadruple $ do
                    pPrint "optimized quadruples:"
                    pPrint $ fmap show $ zip [0..] quads
                  return quads

              quads <- case _useSSA of
                False -> return quads
                _ -> do
                  let ssa = transQuadrupleToSSA quads

                  when _dumpSSA $ do
                    pPrint "ssa:"
                    pPrint $ fmap show $ zip [0..] ssa

                  quads <- zoom _1 $ transSSAToQuadruple ssa

                  when _dumpQuadrupleFromSSA $ do
                    pPrint "quadruples from ssa:"
                    pPrint $ fmap show $ zip [0..] quads

                  quads <- case _optimizeQuadrupleFromSSA of
                    False -> return quads
                    _ -> do
                      quads <- runOptimizer optimizer quads

                      when _dumpOptimizedQuadrupleFromSSA $ do
                        pPrint "optimized quadruples from ssa"
                        pPrint $ fmap show $ zip [0..] quads

                      return quads

                  return quads
                       
              zoom _1 $ QInstSel.selectFunBody quads
              
        insts <- zoom _1 $ do
          let fun = funHeader <> funBody <> funTail

          when _dumpPseudoInstruction $ do
            pPrint "pseudo instruction without register"
            pPrint $ fmap show fun
          
          allocRegisters 10 fun

        when _dumpPseudoInstructionWithRegister $ do
          pPrint "pseudo instruction with register"          
          pPrint $ fmap show insts

        return
          $ genFuntion (_frName $ _pfFrame frag)
          $ removeRedundantMove
          $ removeLonelyLabel
          $ removeRedundantJump
          $ insts   
      
  let strs = fmap encodeStrFrag $ toList _trsStrFragSet
      types = fmap encodeType $ describeTypes $ _stTypes _trsSymbolTable             
      dataSection = genDataSection $ types <> strs
      textSection = genTextSection _trsGlobal _trsExternal funs      
 
  let assembler = genAssemblerCode dataSection textSection  
  
  maybe Char8.putStrLn Char8.writeFile _outPath assembler

  where
    access :: Either String a -> IO a
    access x = case x of
      Left msg -> putStrLn msg >> exitFailure
      Right v -> return v

