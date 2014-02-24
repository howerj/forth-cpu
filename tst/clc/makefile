
# bison -d calc1.y    produces calc1.tab.c
# bison -y -d calc1.y produces y.tab.c (the standard yacc output)

# for calc1 and calc2, you may have to
# include the following in the lex ".l" file:
#    extern int yylval;

CC=gcc

all: calc1 calc2 calc3

# calc1
calc1: calc1.y calc1.l
	bison -y -d calc1.y
	flex calc1.l
	$(CC) -c y.tab.c lex.yy.c
	$(CC) y.tab.o lex.yy.o -o calc1

# calc2
calc2: calc2.y calc2.l
	bison -y -d calc2.y
	flex calc2.l
	$(CC) -c y.tab.c lex.yy.c
	$(CC) y.tab.o lex.yy.o -o calc2

# calc3
calc3: calc3.y calc3.l calc3a.c calc3b.c calc3g.c
	bison -y -d calc3.y
	flex calc3.l
	$(CC) -c y.tab.c lex.yy.c
	$(CC) y.tab.o lex.yy.o calc3a.c -o calc3a
	$(CC) y.tab.o lex.yy.o calc3b.c -o calc3b
	$(CC) y.tab.o lex.yy.o calc3g.c -o calc3g

clean:
	rm -f calc3a calc3b calc3g calc1 calc2
	rm -f lex.yy.o y.tab.o
	rm -f lex.yy.c y.tab.c
	rm -f y.tab.h
