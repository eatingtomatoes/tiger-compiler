AS = nasm
ASFLAGS := -f elf64 -g
CC = gcc
CFLAGS = -no-pie -g
CXX = g++
CXXFLAGS = -std=c++17 -no-pie -g

CASE_NO = 5

TIGER_SOURCE_DIR = resource/code
CPP_SOURCE_DIR = resource/cpp
BUILD_DIR = test/manual

TIGER = $(TIGER_SOURCE_DIR)/case$(CASE_NO).tiger
TARGTE = $(BUILD_DIR)/test_main
GC = $(BUILD_DIR)/gc
TEST_GC = $(BUILD_DIR)/test_gc

PREPARE := $(shell mkdir -p $(BUILD_DIR))

.PHONY : all run cat dump edit view clean 

all : $(TARGTE)

run : $(TARGTE)
	rm -f core
	$< 

$(TARGTE) : $(TARGTE).o $(GC).o
	$(CXX) $(CXXFLAGS) $^ -o $@

$(TEST_GC) : $(TEST_GC).o $(GC).o
	$(CXX) $(CXXFLAGS) $^ -o $@

$(TARGTE).o : $(TARGTE).asm
	$(AS) $(ASFLAGS) $^ -o $@

$(TARGTE).asm : $(TIGER) tiger.cabal $(wildcard src/*.hs app/*.hs) 
	stack run -- -i $< -o $@ $(OPTS)

$(GC).o : $(CPP_SOURCE_DIR)/gc.cpp $(CPP_SOURCE_DIR)/gc.hpp 
	$(CXX) $(CXXFLAGS) -c $< -o $@

$(TEST_GC).o : $(CPP_SOURCE_DIR)/test_gc.cpp $(CPP_SOURCE_DIR)/gc.hpp 
	$(CXX) $(CXXFLAGS) -c $< -o $@

cat :
	cat -n $(TIGER)
	-@echo
	-@echo
	cat $(TARGTE).asm | nasm-formatter-exe | cat -n
	-@echo
	-@echo

dump : 
	stack run -- -i $(TIGER) $(OPTS) | cat -n  

edit :
	emacs -nw $(TARGTE).asm

view : 
	make dump 2>&1 | ./scripts/view_diagram.sh

gdb :
	gdb $(TARGTE) core

test_gc : $(TEST_GC)
	$<

clean :
	rm -rf test/manual/ core 
