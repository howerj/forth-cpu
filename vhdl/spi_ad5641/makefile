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
	@echo "make simulation     - simulate design"
	@echo "make viewer         - start waveform viewer for simulation results"
	@echo ""

##
## Simulation ==============================================================

edgeDff.o: edgeDff.vhd
	@echo "ghdl -a edgeDff.vhd"
	@ghdl -a edgeDff.vhd

shift_reg_16.o: edgeDff.o shift_reg_16.vhd
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
	gtkwave test_bench.ghw &>/dev/null&

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o
