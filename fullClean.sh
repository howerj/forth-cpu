#/bin/bash
#
# Richard James Howe
# Howe Forth.
#
# Clean up directory structure.
#
# @author         Richard James Howe.
# @copyright      Copyright 2013 Richard James Howe.
# @license        LGPL      
# @email          howe.rj.89@googlemail.com

echo "Cleaning directory tree of common temporary files.";
cd forth/;
./clean.sh;
cd ..;
cd vhdl/;
make clean;
cd ..;
rm -vr *.swp;
rm -vr *.swo;
rm -vr *.log;
rm -vr *.o;
echo "Done.";
