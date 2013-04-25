#!/bin/bash
# Richard James Howe.
# Run the test bench that acts on top_level.vhd
# @author         Richard James Howe.
# @copyright      Copyright 2013 Richard James Howe.
# @license        LGPL      
# @email          howe.rj.89@googlemail.com

TARGET="test_bench"
FILES="h2.vhd mem_h2.vhd mem_font.vhd mem_text.vhd uart.vhd ctrm.vhd losr.vhd vga80x40.vhd top_level.vhd test_bench.vhd"


if
	ghdl -a -g --ieee=synopsys $FILES;
then
	echo ".o files created."
	if
		ghdl -e $TARGET;
	then
		echo "Creating executable."
		if
			ghdl -r $TARGET --wave=$TARGET.ghw;
		then
			echo "Success. Cleaning up.";
            ghdl --clean;
            echo "Done.";
		else
			echo "Simulation failed!";
		fi
	else
		echo "Elaboration failed!";
	fi
else
	echo "Analysis failed!";
fi
