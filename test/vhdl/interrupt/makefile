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

queue.o: queue.vhd
	@echo "ghdl -a queue.vhd"
	@ghdl -a queue.vhd

simulation: queue.o queue_test_bench.vhd
	ghdl -a queue_test_bench.vhd
	ghdl -e queue_test_bench
	ghdl -r queue_test_bench --wave=queue_test_bench.ghw 
## Simulation ==============================================================

viewer:
	gtkwave queue_test_bench.ghw &>/dev/null&

## Second simulation of sub component

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw queue_test_bench 
