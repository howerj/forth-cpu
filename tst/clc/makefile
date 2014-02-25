CC=gcc

all: compile

compile: compile.y compile.l h2.c
	bison -y -d compile.y
	flex compile.l
	$(CC) -c y.tab.c lex.yy.c
	$(CC) y.tab.o lex.yy.o h2.c -o h2

clean:
	rm -f lex.yy.o y.tab.o
	rm -f lex.yy.c y.tab.c
	rm -f y.tab.h
	rm -f h2
