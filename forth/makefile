#
# Howe Forth: Makefile
# @author         Richard James Howe.
# @copyright      Copyright 2013 Richard James Howe.
# @license        LGPL      
# @email          howe.r.j.89@gmail.com
#
#


## Colors

BLUE=\e[1;34m
GREEN=\e[1;32m
RED=\e[1;31m
DEFAULT=\e[0m

## Compiler options

CC=gcc
CCOPTS=-ansi -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2 -save-temps
#CCOPTS=-ansi --coverage -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2

## Long strings passed to commands

RMSTR=bin/forth bin/*.o memory.txt *.log *.swo *.swp *.o lib/*~ *~ *.gcov *.gcda *.gcno doc/*.html doc/*.htm log/* *.i *.s log/*.i log/*.s

INDENTSTR=-v -linux -nut -i2 -l120 -lc120 lib/*.h lib/*.c main.c
SPLINTSTR=-forcehint main.c lib/*.h lib/*.c

## Help

all: banner bin/forth

banner:
	@/bin/echo -e "Howe Forth, GNU Makefile\n"
	@/bin/echo -e "Author:    $(BLUE)Richard James Howe$(DEFAULT)."
	@/bin/echo -e "Copyright: $(BLUE)Copyright 2013 Richard James Howe.$(DEFAULT)."
	@/bin/echo -e "License:   $(BLUE)LGPL$(DEFAULT)."
	@/bin/echo -e "Email:     $(BLUE)howe.r.j.89@gmail.com$(DEFAULT)."
	@/bin/echo -e "Add the following to \"CCOPT\" for different functionality"
	@/bin/echo -e "To compile with debug flags enable type $(BLUE)\"-DDEBUG_PRN\".$(DEFAULT)";
	@/bin/echo -e "To compile with debug cycle counter enabled $(BLUE)\"-DRUN4X\".$(DEFAULT)";
	@/bin/echo -e "To compile $(RED)without$(DEFAULT) bounds checking:$(BLUE) \"-DUNCHECK\".$(DEFAULT)";
	@/bin/echo -e "\n"

help:
	@/bin/echo "Options:"
	@/bin/echo "make"
	@/bin/echo "     Print out banner, this help message and compile program."
	@/bin/echo "make forth"
	@/bin/echo "     Compile Howe forth."
	@/bin/echo "make run"
	@/bin/echo "     Run Howe forth."
	@/bin/echo "make pretty"
	@/bin/echo "     Indent source, print out word count, clean up directory."
	@/bin/echo "make clean"
	@/bin/echo "     Clean up directory."
	@/bin/echo "make gcov"
	@/bin/echo "     Compile program with coverage options, run program, run gcov."
	@/bin/echo "make splint"
	@/bin/echo "     Run splint on all *.c and *.h source files."
	@/bin/echo "make html"
	@/bin/echo "     Compile the documentation."
	@/bin/echo "make valgrind"
	@/bin/echo "     Run program with valgrind on start up forth file only."

## Main forth program

# The Forth interpreter top level
bin/forth: main.c bin/forth.o bin/hosted.o
	$(CC) $(CCOPTS) main.c bin/hosted.o bin/forth.o -o bin/forth
	@mv *.i *.s log/

# Desktop interface
bin/hosted.o: lib/hosted.c lib/hosted.h
	$(CC) $(CCOPTS) -c lib/hosted.c -o bin/hosted.o

# The forth interpreter library
bin/forth.o: lib/forth.c lib/forth.h
	$(CC) $(CCOPTS) -c lib/forth.c -o bin/forth.o

## Optional extras, helper functions

# Run the interpreter

run: bin/forth
	@cd bin; ./forth

# Indent the files, clean up directory, word count.
pretty: 
	@/bin/echo -e "$(BLUE)"
	@/bin/echo -e "Indent files and clean up system.$(DEFAULT)"
	@/bin/echo -e "$(GREEN)"
	@/bin/echo "indent $(INDENTSTR)"
	@indent $(INDENTSTR);
	@/bin/echo -e "$(RED)"
	@rm -vf $(RMSTR);
	@/bin/echo -e "$(DEFAULT)"
	@wc lib/*.c lib/*.h *.c fth/*.4th makefile

# Clean up directory.
clean:
	@/bin/echo -e "$(RED)"
	@rm -vf $(RMSTR);
	@/bin/echo -e "$(DEFAULT)"

# Static checking.
splint:
	@/bin/echo "$(SPLINTSTR)";
	-splint $(SPLINTSTR);

html:
	@/bin/echo -e "Compiling markdown to html."
	for i in doc/*.md; do\
		/bin/echo "$$i > $$i.html";\
		markdown $$i > $$i.html;\
	done

valgrind: bin/forth
	@/bin/echo "Running valgrind on ./forth"
	@/bin/echo "  This command needs changing in the makefile"
	-valgrind bin/./forth << EOF

ctags:
	@ctags -R .

gcov: CCOPTS:=$(CCOPTS) --coverage
gcov: clean bin/forth
	@/bin/echo "Providing gcov statistics for forth program."
	@cd bin/; ./forth << EOF
	@mv bin/*.gcda bin/*.gcno .
	@gcov forth.c hosted.c main.c
	@if [ ! -d log/ ]; then mkdir log; fi
	@mv *.gcda *.gcno *.gcov log/
