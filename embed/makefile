CFLAGS=-std=c99 -Wall -Wextra

all: embed embed.hex new.blk

embed: embed.o

new.blk: embed embed.blk embed.fth
	./embed embed.blk $@ embed.fth

embed.hex: hex new.blk
	./hex < new.blk > embed.hex
	cp $@ ..

clean:
	rm -f *.o embed new.blk embed.hex
