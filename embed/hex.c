#include <stdio.h>
#include <stdint.h>

int main(void) {
	size_t r = 0;
	uint8_t b[2];
	for (;(r = fread(b, 1, 2, stdin)); ) {
		const uint16_t o = ((unsigned)b[1] << 8) | ((unsigned)b[0] << 0);
		fprintf(stdout, "%04x\n", o);
		if (r != 2)
			return -1;
	}
	return 0;
}
