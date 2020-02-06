AS = nasm
ASFLAGS := -f elf64
CC = gcc
CFLAGS = -no-pie 

CASE_NO = 2
SOURCE = resource/code/case$(CASE_NO).tiger
TARGTE = test/manual/test_main

.PHONY : all run cat dump edit view clean 

all : $(TARGTE)

run : $(TARGTE)
	$<

$(TARGTE) : $(TARGTE).o
	$(CC) $(CFLAGS) $^ -o $@

$(TARGTE).o : $(TARGTE).asm
	$(AS) $(ASFLAGS) $^ -o $@

$(TARGTE).asm : $(SOURCE) tiger.cabal $(wildcard src/*.hs) Makefile
	mkdir -p `dirname $@`
	stack run -- -i $< -o $@

cat :
	cat -n $(SOURCE)
	-@echo
	-@echo
	cat $(TARGTE).asm | nasm-formatter-exe | cat -n
	-@echo
	-@echo

dump : 
	stack run -- -i $(SOURCE) | cat -n

edit :
	emacs -nw $(TARGTE).asm

view : 
	make run 2>&1 | ./scripts/view_diagram.sh

clean :
	rm -rf test/manual/
