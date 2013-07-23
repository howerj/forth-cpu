#/bin/bash
#
# Richard James Howe
# Howe Forth.
#
# Correctly indent files and clean up directory.
#
# @author         Richard James Howe.
# @copyright      Copyright 2013 Richard James Howe.
# @license        LGPL      
# @email          howe.rj.89@googlemail.com

## Colors
BLUE="\e[1;34m";
GREEN="\e[1;32m";
RED="\e[1;31m";
DEFAULT="\e[0m";

echo -e "$BLUE";
echo -e "Indent files and clean up system.$DEFAULT";

echo -e "$GREEN";
echo "indent -nut -linux *.h *.c";
indent -nut -linux *.h *c;

echo -e "$RED";
rm -vf forth memory.txt *.log *.swo *.swp *.o *~ *.gcov *.gcda *.gcno *.html *.htm;
echo -e "$DEFAULT";
