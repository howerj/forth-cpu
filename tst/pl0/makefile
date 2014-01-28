
CC=gcc
CCOPTS=-ansi -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2

all: compile

compile: compile.c
	$(CC) $(CCOPTS) compile.c  -o compile

clean:
	@rm -vf *.o compile
