module TempHelper where

import Temp

rax :: Temp
rax = Reg "rax"

rdi :: Temp
rdi = Reg "rdi"

rsi :: Temp
rsi = Reg "rsi"

rdx :: Temp
rdx = Reg "rdx"

rcx :: Temp
rcx = Reg "rcx"

r8 :: Temp
r8 = Reg "r8"

r9 :: Temp
r9 = Reg "r9"

r10 :: Temp
r10 = Reg "r10"

r11 :: Temp
r11 = Reg "r11"

rsp :: Temp
rsp = Reg "rsp"

rbp :: Temp
rbp = Reg "rbp"

rv :: Temp
rv = rax

bp :: Temp
bp = rbp

sp :: Temp
sp = rsp
