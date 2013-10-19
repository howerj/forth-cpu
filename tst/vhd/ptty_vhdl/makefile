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

uart.o: uart.vhd 
	@echo "ghdl -a uart.vhd"
	@ghdl -a uart.vhd

ptty.o: uart.o ptty.vhd 
	@echo "ghdl -a ptty.vhd"
	@ghdl -a ptty.vhd

loopback.o: loopback.vhd uart.o
	@echo "ghdl -a loopback.vhd"
	@ghdl -a loopback.vhd

simulation: ptty.o loopback.o  ptty_test_bench.vhd
	ghdl -a ptty_test_bench.vhd
	ghdl -e ptty_test_bench
	ghdl -r ptty_test_bench --wave=ptty_test_bench.ghw 

## Simulation ==============================================================

viewer:
	gtkwave ptty_test_bench.ghw 2&>/dev/null&

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw ptty_test_bench 
