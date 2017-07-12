OBJECTS := ucpu.o bram.o

all: tb.ghw

tb.o: bram.o ucpu.o

%.o: %.vhd
	ghdl -a $<

tb: ${OBJECTS} tb.o
	ghdl -e tb

%.ghw: % ucpu.bin
	ghdl -r $< --wave=$<.ghw

clean:
	rm -f tb *.o *.ghw

