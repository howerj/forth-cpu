#include <stdio.h>
#include <stdint.h>
#include <stdbool.h>

#define MAX_READ (3200)
#define DEFAULT_ATTRIBUTES (0x3800u)

static int printer(FILE *o, uint16_t n, bool binary_not_hex)
{
	n |= DEFAULT_ATTRIBUTES;
	if(binary_not_hex) {
		unsigned i;
		for(i = 15; i <= 15; i--)
			fputc(n & (1 << i) ? '1' : '0', o);
		fputc('\n', o);
	} else {
		fprintf(o, "%04x\n", (unsigned)n);
	}
	return 0;
}

static void usage(const char *arg0)
{
	static const char *help = "\
Generate initial VGA screen text for the H2 CPU project.\n\
Options:\n\n\
\t-h print this help message and exit\n\
\t-g turn on generation mode - generate file full of spaces\n\n\
This program turns a byte stream into a text file containing\n\
a single 16-bit (doubling up the input data) ASCII file encoded\n\
'1' and '0' characters.\n\n";
	fprintf(stderr, "usage: %s -h -g\n", arg0);
	fprintf(stderr, "%s", help);
	fprintf(stderr, "%d characters are read in and converted.\n\n", MAX_READ);
}

int main(int argc, char **argv)
{
	size_t i;
	int ia, gen = 0;
	for(ia = 1; ia < argc && argv[ia][0] == '-'; ia++) {
		switch(argv[ia][1]) {
		case '\0': goto done; /* stop processing options */
		case 'h':  usage(argv[0]);
			   return -1;
		case 'g':  gen = 1; break;
		default:
			   fprintf(stderr, "unknown option '%s'\n", argv[ia]);
			   return -1;
		}
	}
done:
	if(argc != ia) {
		fprintf(stderr, "too many arguments supplied\n");
		return -1;
	}
	for(i = 0; i < MAX_READ; i++) {
		int c = gen ? ' ' : fgetc(stdin);
		if(c == EOF)
			return 0;
		if(printer(stdout, c, false) < 0)
			return -1;
	}
	return 0;
}

