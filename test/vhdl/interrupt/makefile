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
	@echo "make clean"						- Clean up working directory
	@echo ""

##
## Simulation ==============================================================

interrupt.o: interrupt.vhd
	@echo "ghdl -a interrupt.vhd"
	@ghdl -a interrupt.vhd

simulation: interrupt.o interrupt_test_bench.vhd
	ghdl -a interrupt_test_bench.vhd
	ghdl -e interrupt_test_bench
	ghdl -r interrupt_test_bench --wave=interrupt_test_bench.ghw 
## Simulation ==============================================================

viewer:
	gtkwave interrupt_test_bench.ghw &>/dev/null&

## Second simulation of sub component

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw interrupt_test_bench 
