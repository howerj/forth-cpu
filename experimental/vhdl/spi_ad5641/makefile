#
# Makefile to simulate and synthesize VHDL designs
#
# @Author 		Marc Eberhard/Richard Howe
# @Copyright	Copyright 2013 Marc Eberhard
# @License		LGPL
#


## Remember to update the synthesis section as well
SOURCES = \
	mem_font.vhd \
	mem_text.vhd \
	uart.vhd \
	ctrm.vhd \
	losr.vhd \
	vga80x40.vhd \
	vga_top.vhd \
	h2.vhd \
	mem_h2.vhd \
	cpu.vhd \
#	io.vhd

all: help simulation

help:
	@echo ""
	@echo "Simulation:"
	@echo ""
	@echo "make simulation     		- simulate design"
	@echo "make viewer         		- start waveform viewer for simulation results"
	@echo ""
	@echo "make shift_reg_sim" 		- Simulate shift register sub section
	@echo "make shift_reg_viewer" - Waveform viewer for sub section
	@echo ""
	@echo "make clean"						- Clean up working directory
	@echo ""

##
## Simulation ==============================================================

shift_reg_16.o: shift_reg_16.vhd
	@echo "ghdl -a shift_reg_16.vhd"
	@ghdl -a shift_reg_16.vhd

spi_ad5641.o: shift_reg_16.o spi_ad5641.vhd
	@echo "ghdl -a spi_ad5641.vhd"
	@ghdl -a spi_ad5641.vhd

simulation: spi_ad5641.o spi_test_bench.vhd
	ghdl -a spi_test_bench.vhd
	ghdl -e spi_test_bench
	ghdl -r spi_test_bench --wave=spi_test_bench.ghw 
## Simulation ==============================================================

viewer:
	gtkwave spi_test_bench.ghw &>/dev/null&

## Second simulation of sub component

shift_reg_sim: shift_reg_16.o
	ghdl -a shift_reg_test_bench.vhd
	ghdl -e shift_reg_test_bench
	ghdl -r shift_reg_test_bench --wave=shift_reg_test_bench.ghw 

shift_reg_viewer:
	gtkwave shift_reg_test_bench.ghw &>/dev/null&

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw spi_test_bench shift_reg_test_bench
