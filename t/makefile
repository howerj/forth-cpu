SOURCES:=$(wildcard *.tst)
OBJFILES=$(SOURCES:%.tst=%.ansi)

.PHONY: all clean

all: ${OBJFILES}

%.ansi: %.tst ansi.fth eforth.fth 
	cat eforth.fth > $@
	gforth ansi.fth $< >> $@

upload: ${OBJFILES}
	cat $^ > /dev/ttyUSB0

clean:
	rm -fv *.ansi

