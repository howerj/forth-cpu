/**@brief Turn a text file into a Forth block file */

#include <stdio.h>
#include <string.h>

int main(void)
{
	size_t i;
	char line[80] = { 0 };

	memset(line, ' ', 64);
	for(i = 0;fgets(line, 66, stdin); memset(line, ' ', 64), i++) {
		if(strlen(line) > 65) {
			fprintf(stderr, "line %u too long:\n\t%s\n", (unsigned)i, line);
			return -1;
		}
		line[strlen(line)-1] = ' ';
		line[strlen(line)+0] = ' ';
		line[strlen(line)+1] = ' ';
		fprintf(stdout, "%.64s", line);
	}

	return 0;
}
