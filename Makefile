# Simple Makefile for building the Flappy Bird game
#

all: main.o
	ld -s -o main.out main.o -lX11 -lc --dynamic-linker /lib64/ld-linux-x86-64.so.2

main.o: main.asm
	nasm -f elf64 -g -F dwarf -o main.o main.asm

clean:
	rm -rf *.o *.out
