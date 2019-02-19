CFLAGS=-std=c99 -Wall -Wextra

all: embed

embed: embed.o

new.blk: embed embed.blk embed.fth
	./embed embed.blk $@ embed.fth

clean:
	rm *.o embed
