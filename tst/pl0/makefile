
CC=gcc
CCOPTS=-ansi -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2

all: parse

parse: parse.c
	$(CC) $(CCOPTS) parse.c  -o parse

clean:
	@rm -vf *.o parse
