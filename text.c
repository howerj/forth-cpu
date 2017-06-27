/**@brief This is a simple utility for compiling a utility that
 * reads in up to 3200 characters and converts them into a text
 * that can be used with the VHDL SoC as the file "text.bin", which
 * expects a single line containing a 16-bit binary value as zero
 * and one characters */
#include <stdio.h>
#include <stdint.h>

#define MAX_READ (3200)

static int print_binary(FILE *o, uint16_t n)
{
	unsigned i;
	for(i = 15; i <= 15; i--)
		fputc(n & (1 << i) ? '1' : '0', o);
	fputc('\n', o);
}

int main(void)
{
	size_t i;
	for(i = 0; i < MAX_READ; i++) {
		int c = fgetc(stdin);
		if(c == EOF)
			return 0;
		if(print_binary(stdout, c) < 0)
			return -1;
	}
	return 0;
}

