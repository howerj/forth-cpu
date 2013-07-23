#!/bin/bash
#
# Richard James Howe
# Howe Forth.
#
# Compile interpreter.
#
# @author         Richard James Howe.
# @copyright      Copyright 2013 Richard James Howe.
# @license        LGPL      
# @email          howe.rj.89@googlemail.com
#
# Notes:
# -Wstrict-overflow=5
# -Wconversion

## Colors
BLUE="\e[1;34m";
GREEN="\e[1;32m";
RED="\e[1;31m";
DEFAULT="\e[0m";
CC=gcc
GCC_OPT="-ansi -g -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2"
TARGET=forth
echo -e "This will compile $BLUE\"Howe Forth\"$DEFAULT.";
echo -e "To run type $BLUE\"./$TARGET\"$DEFAULT with no arguments.";
echo -e "To compile with debug flags enable type $BLUE\"./compile -DDEBUG_PRN\"$DEFAULT.";
echo -e "To compile with debug cycle counter enabled $BLUE\"./compile -DRUN4X\"$DEFAULT.";
echo -e "To compile without bounds checking:$BLUE \"./compile -DUNCHECK\"$DEFAULT.";
echo -e "For code coverage with \"gcov\":$BLUE \"./compile --coverage\"$DEFAULT.";
echo -e "To compile documentation *only*:$BLUE \"./compile --docs\"$DEFAULT.";
echo -e "Compiling with:\n\t$BLUE\"$CC $GCC_OPT $1\"$DEFAULT";

if
  [ "$1" = "--docs" ];
then
  echo "Attempting to compile documentation *only*.";
  if hash markdown 2>/dev/null; then
    echo "\"markdown\" found";
    for i in *.md; do
      echo "markdown $i > $i.html";
      markdown $i > $i.html;
    done;
    echo -e "$GREEN > Done.$DEFAULT"
    exit 0;
  else
    echo -e "$BLUE\"markdown\"$RED not found. Unable to process documentation.$DEFAULT";
    exit 1;
  fi;
fi;


if
  $CC $GCC_OPT $1 -c forth.c -o forth.o && $CC $GCC_OPT $1 main.c forth.o -o $TARGET;
then
  echo -e "$GREEN";
	echo -e "Compilation Success.$DEFAULT";
  echo -e "$BLUE";
  WRDCNT=$(wc *.c *.h);
  echo -e "wc *.c *.h";
  echo -e "$WRDCNT$DEFAULT";
  exit 0;
else
  echo -e "$RED";
	echo -e "Compilation Failure.$DEFAULT";
  exit 1;
fi;
