#
# Howe H2 VM: Makefile
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

all: banner h2

banner:
	@echo -e "GNU Makefile\n"
	@echo -e "Virtual machine"
	@echo -e "Author:    $(BLUE)Richard James Howe$(DEFAULT)."
	@echo -e "Copyright: $(BLUE)Copyright 2013 Richard James Howe.$(DEFAULT)."
	@echo -e "License:   $(BLUE)LGPL$(DEFAULT)."
	@echo -e "Email:     $(BLUE)howe.r.j.89@gmail.com$(DEFAULT)."
	@echo -e "\n"


help:
	@echo "Options:"
	@echo "make"
	@echo "     Print out banner, this help message and compile program."
	@echo "make h2"
	@echo "     Compile h2 virtual machine."
	@echo "make pretty"
	@echo "     Indent source, print out word count, clean up directory."
	@echo "make clean"
	@echo "     Clean up directory."
	@echo "make gcov"
	@echo "     Compile program with coverage options, run program, run gcov."
	@echo "make splint"
	@echo "     Run splint on all *.c and *.h source files."
	@echo "make html"
	@echo "     Compile the documentation."
	@echo "make valgrind"
	@echo "     Run program with valgrind". 


## Main program

h2: main.c h2.h h2.o
	$(CC) $(CCOPTS) main.c h2.o -o vm

h2.o: h2.c h2.h
	$(CC) $(CCOPTS) -c h2.c -o h2.o

## Optional extras, helper functions

# Indent the files, clean up directory, word count.
pretty: 
	@echo -e "$(BLUE)"
	@echo -e "Indent files and clean up system.$(DEFAULT)"
	@echo -e "$(GREEN)"
	@echo "indent -nut -linux *.h *.c";
	@indent -nut -linux *.h *c;
	@echo -e "$(RED)"
	@rm -vf h2 vm memory.txt *.log *.swo *.swp *.o *~ *.gcov *.gcda *.gcno *.html *.htm;
	@echo -e "$(DEFAULT)"
	@wc *.c *.h makefile

# Clean up directory.
clean:
	@echo -e "$(RED)"
	@rm -vf h2 vm memory.txt *.log *.swo *.swp *.o *~ *.gcov *.gcda *.gcno *.html *.htm;
	@echo -e "$(DEFAULT)"

# Static checking.
splint:
	@echo "Running \"splint *.c *.h &> splint.log\""
	-splint *.c *.h &> splint.log 

html:
	@echo -e "Compiling markdown to html."
	@for i in *.md; do echo "$$i > $$i.html"; markdown $$i > $$i.html; done

valgrind: h2
	@echo "Running valgrind on ./vm"
	@echo "  This command needs changing in the makefile"
	-valgrind ./h2 &> valgrind.log << EOF

gcov: CCOPTS:=$(CCOPTS) --coverage
gcov: clean h2
	@echo "Providing gcov statistics for vm program."
	@./vm << EOF
	@gcov h2.c main.c


