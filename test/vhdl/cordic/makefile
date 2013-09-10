#
# Makefile to simulate and synthesize VHDL designs
#
# @Author 		Richard Howe
# @Copyright	Copyright 2013 Marc Eberhard
# @License		LGPL
#

all: help simulation

help:
	@echo ""
	@echo "Simulation:"
	@echo ""
	@echo "make simulation     		- simulate design"
	@echo "make viewer         		- start waveform viewer for simulation results"
	@echo ""
	@echo ""
	@echo "make clean"						- Clean up working directory
	@echo ""

##
## Simulation ==============================================================

cordic.o: cordic.vhd 
	@echo "ghdl -a cordic.vhd"
	@ghdl -a cordic.vhd

simulation: cordic.o  cordic_test_bench.vhd
	ghdl -a cordic_test_bench.vhd
	ghdl -e cordic_test_bench
	ghdl -r cordic_test_bench --wave=cordic_test_bench.ghw 

## Simulation ==============================================================

viewer:
	gtkwave dac_test_bench.ghw &>/dev/null&

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw dac_test_bench 
