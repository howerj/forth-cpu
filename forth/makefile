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
CCOPTS=-ansi -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2
#CCOPTS=-ansi --coverage -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2

## Help

all: banner forth assemble

banner:
	@/bin/echo -e "Howe Forth, GNU Makefile\n"
	@/bin/echo -e "Assembler/Virtual machine"
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
	@/bin/echo "     Compile howe forth."
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

## Run the interpreter
assemble:
	@/bin/echo "Running the assembler."
	@./forth

## Main forth program

forth: main.c forth.h forth.o
	$(CC) $(CCOPTS) main.c forth.o -o forth

forth.o: forth.c forth.h
	$(CC) $(CCOPTS) -c forth.c -o forth.o

## Optional extras, helper functions

# Indent the files, clean up directory, word count.
pretty: 
	@/bin/echo -e "$(BLUE)"
	@/bin/echo -e "Indent files and clean up system.$(DEFAULT)"
	@/bin/echo -e "$(GREEN)"
	@/bin/echo "indent -nut -linux *.h *.c";
	@indent -nut -linux *.h *c;
	@/bin/echo -e "$(RED)"
	@rm -vf forth memory.txt *.log *.swo *.swp *.o *~ *.gcov *.gcda *.gcno *.html *.htm;
	@/bin/echo -e "$(DEFAULT)"
	@wc *.c *.h *.fs makefile

# Clean up directory.
clean:
	@/bin/echo -e "$(RED)"
	@rm -vf forth memory.txt *.log *.swo *.swp *.o *~ *.gcov *.gcda *.gcno *.html *.htm;
	@/bin/echo -e "$(DEFAULT)"

# Static checking.
splint:
	@/bin/echo "Running \"splint *.c *.h &> splint.log\""
	-splint *.c *.h &> splint.log 

html:
	@/bin/echo -e "Compiling markdown to html."
	@for i in *.md; do /bin/echo "$$i > $$i.html"; markdown $$i > $$i.html; done

valgrind: forth
	@/bin/echo "Running valgrind on ./forth"
	@/bin/echo "  This command needs changing in the makefile"
	-valgrind ./forth &> valgrind.log << EOF

gcov: CCOPTS:=$(CCOPTS) --coverage
gcov: clean forth
	@/bin/echo "Providing gcov statistics for forth program."
	@./forth << EOF
	@gcov forth.c main.c


