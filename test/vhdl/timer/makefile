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

gptimer.o: gptimer.vhd
	@echo "ghdl -a gptimer.vhd"
	@ghdl -a gptimer.vhd

simulation: gptimer.o gptimer_test_bench.vhd
	ghdl -a gptimer_test_bench.vhd
	ghdl -e gptimer_test_bench
	ghdl -r gptimer_test_bench --wave=gptimer_test_bench.ghw 
## Simulation ==============================================================

viewer:
	gtkwave gptimer_test_bench.ghw &>/dev/null&

## Second simulation of sub component

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw gptimer_test_bench 
