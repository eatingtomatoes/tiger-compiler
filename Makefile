AS = nasm
ASFLAGS := -f elf64 -g
CC = gcc
CFLAGS = -no-pie -g
CXX = g++
CXXFLAGS = -std=c++17 -no-pie -g

CASE_NO = 6


TIGER_SOURCE_DIR = resource/tiger
CPP_SOURCE_DIR = resource/cpp
BUILD_DIR = test/manual

TIGER = $(TIGER_SOURCE_DIR)/case$(CASE_NO).tiger
TARGET = $(BUILD_DIR)/test_main
GC = $(BUILD_DIR)/gc
TEST_GC = $(BUILD_DIR)/test_gc

HASKELl = $(shell find src -name "*.hs")

PREPARE := $(shell mkdir -p $(BUILD_DIR))

.PHONY : all run cat dump edit view gdb test test_gc clean 

all : $(TARGET)

run : $(TARGET)
	rm -f core
	$<

$(TARGET) : $(TARGET).o $(GC).o
	$(CXX) $(CXXFLAGS) $^ -o $@

$(TEST_GC) : $(TEST_GC).o $(GC).o
	$(CXX) $(CXXFLAGS) $^ -o $@

$(TARGET).o : $(TARGET).asm
	$(AS) $(ASFLAGS) $^ -o $@

$(TARGET).asm : $(TIGER) tiger.cabal $(HASKELl) Makefile
	stack run -- -i $< -o $@ $(OPTS)	

$(GC).o : $(CPP_SOURCE_DIR)/gc.cpp $(CPP_SOURCE_DIR)/gc.hpp 
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(TEST_GC).o : $(CPP_SOURCE_DIR)/test_gc.cpp $(CPP_SOURCE_DIR)/gc.hpp 
	$(CXX) $(CXXFLAGS) -c $< -o $@

cat :
	cat -n $(TIGER)
	-@echo
	-@echo
	cat $(TARGET).asm | nasm-formatter-exe | cat -n
	-@echo
	-@echo

dump : 
	stack run -- -i $(TIGER) $(OPTS) | cat -n  

edit :
	emacs -nw $(TARGET).asm

view : 
	make test 2>&1 | ./scripts/view_diagram.sh 

gdb :
	gdb $(TARGET) core

test :
	make run OPTS="--use-quadruple --optimize-quadruple --dump-quadruple --dump-optimized-quadruple"

test_gc : $(TEST_GC)
	$< 

clean :
	rm -rf test/manual/ core 
