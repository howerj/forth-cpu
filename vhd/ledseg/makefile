#
# Makefile to simulate VHDL designs
#
# @Author 		Richard Howe
# @Copyright	Copyright 2013 Richard Howe
# @License		LGPL
#

FILE=ledseg

all: help 

help:
	@echo ""
	@echo "Simulation: LED 8 - Segment display driver"
	@echo ""
	@echo "make simulation        - simulate design (VHDL)"
	@echo "make viewer            - start waveform viewer for simulation results"
	@echo ""
	@echo "make clean             - Clean up working directory"
	@echo ""


## Simulation ==============================================================

$(FILE).o: $(FILE).vhd 
	@echo "ghdl -a $(FILE).vhd"
	@ghdl -a $(FILE).vhd

simulation: $(FILE).o  $(FILE)_tb.vhd
	ghdl -a $(FILE)_tb.vhd
	ghdl -e $(FILE)
	ghdl -r $(FILE) --wave=$(FILE).ghw 

## Simulation ==============================================================

viewer:
	@gtkwave $(FILE).ghw

clean:
	@echo "Deleting temporary files and cleaning up directory..."
	@rm -vf *.o *.cf *.ghw $(FILE)
