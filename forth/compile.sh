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
CC=gcc
GCC_OPT="-ansi -Wall -Wno-write-strings -Wshadow -Wextra -pedantic -O2"
TARGET=forth
echo "This will compile \"Howe Forth\".";
echo "To run type \"./$TARGET\" with no arguments.";
echo "Compiling with: $CC $GCC_OPT."
if
    $CC $GCC_OPT -c forth.c -o forth.o && $CC $GCC_OPT main.c forth.o -o $TARGET;
then
	echo "Success"
    exit 0;
else
	echo "Failure"
    exit 1;
fi;
