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

mem_dac.o: mem_dac.vhd mem_dac.binary
	@echo "ghdl -a mem_dac.vhd"
	@ghdl -a mem_dac.vhd

shift_reg_16.o: shift_reg_16.vhd
	@echo "ghdl -a shift_reg_16.vhd"
	@ghdl -a shift_reg_16.vhd

spi_ad5641.o: shift_reg_16.o spi_ad5641.vhd
	@echo "ghdl -a spi_ad5641.vhd"
	@ghdl -a spi_ad5641.vhd

dac.o: spi_ad5641.o mem_dac.o dac.vhd 
	@echo "ghdl -a dac.vhd"
	@ghdl -a dac.vhd

simulation: dac.o  dac_test_bench.vhd
	ghdl -a dac_test_bench.vhd
	ghdl -e dac_test_bench
	ghdl -r dac_test_bench --wave=dac_test_bench.ghw 

## Simulation ==============================================================

viewer:
	gtkwave dac_test_bench.ghw &>/dev/null&

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw dac_test_bench 
