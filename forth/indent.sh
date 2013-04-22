#/bin/bash
#
# Richard James Howe
# Howe Forth.
#
# Correctly indent files.
#
# @author         Richard James Howe.
# @copyright      Copyright 2013 Richard James Howe.
# @license        LGPL      
# @email          howe.rj.89@googlemail.com

echo "indent -linux *.h *.c";
indent -linux *.h *c;
rm -v *~;
