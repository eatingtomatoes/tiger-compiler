module InstructionHelper
  ( plainInst
  , moveInst
  , labelInst
  , jmpInst
  ) where

import Instruction
import Temp
import Label

plainInst :: String -> [Temp] -> [Temp] -> KeySet -> Instruction
plainInst = Oper False 

moveInst :: String -> [Temp] -> [Temp] -> KeySet -> Instruction
moveInst = Oper True

labelInst :: Label -> Instruction
labelInst = Lab

jmpInst :: String -> Label -> Instruction
jmpInst = Jmp
