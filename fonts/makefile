SOURCES:=$(wildcard *.bin)
OBJFILES=$(SOURCES:%.bin=%.pbm)

.PHONY: all clean

all: ${OBJFILES}

%.pbm: %.bin
	echo "P1\n8" `wc $< | awk '{print $$1}'` > $@
	cat $< >> $@

clean:
	rm -fv ${OBJFILES}

