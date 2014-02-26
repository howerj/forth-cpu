CC=gcc
CCOPTS=-Wall -Wextra -Wno-write-strings -Wshadow -Wextra 

all: h2

lex.yy.c: compile.h compile.l
	flex compile.l

y.tab.X: compile.h compile.y
	bison -y -d compile.y

h2: compile.h lex.yy.c y.tab.X h2.c
	$(CC) $(CCOPTS) -c y.tab.c lex.yy.c
	$(CC) $(CCOPTS) y.tab.o lex.yy.o h2.c -o h2

clean:
	rm -f lex.yy.o y.tab.o
	rm -f lex.yy.c y.tab.c
	rm -f y.tab.h
	rm -f h2
