#
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

all: filter.c
	$(CC) $(CCOPTS) filter.c -o filter


# Indent the files, clean up directory, word count.
pretty: 
	@/bin/echo -e "$(BLUE)"
	@/bin/echo -e "Indent files and clean up system.$(DEFAULT)"
	@/bin/echo -e "$(GREEN)"
	@/bin/echo "indent -nut -linux *.h *.c";
	@indent -nut -linux *.h *c;
	@/bin/echo -e "$(RED)"
	@rm -vf filter *.log *.swo *.swp *.o *~ *.gcov *.gcda *.gcno;
	@/bin/echo -e "$(DEFAULT)"
	@wc *.c *.h *.fs makefile

# Clean up directory.
clean:
	@/bin/echo -e "$(RED)"
	@rm -vf filter *.log *.swo *.swp *.o *~ *.gcov *.gcda *.gcno;
	@/bin/echo -e "$(DEFAULT)"
