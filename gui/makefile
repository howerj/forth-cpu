LDFLAGS = -lglut -lGL -lm -lpthread
CFLAGS  = -std=c99 -Wall -Wextra -g -O2 -I..
TARGET  = gui

all: ${TARGET}

run: ${TARGET} ../h2.hex
	./${TARGET} ../h2.hex

h2.o: ../h2.c ../h2.h
	${CC} ${CFLAGS} -DNO_MAIN $< -c -o $@

gui: gui.c h2.o ../h2.h
	${CC} ${CFLAGS} -DNO_MAIN gui.c h2.o ${LDFLAGS} -o $@

clean:
	rm -fv ${TARGET}

