/**@filehack.c
 * @brief This is a temporary hack, there is a problems some where on the SoC with
 * the UART, it seems to drop when receiving characters - transmission appears fine.
 * So this program does the following reads a byte from standard in, attempts
 * to send it to the named terminal, the H2 board responds by echoing the byte
 * back - but only if it received it, if not retransmission will be attempted.
 * @author Richard Jame sHowe
 * @copyright Richard James Howe (c) 2017
 * @license MIT*/

#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <termios.h>

static int open_tty(constchar * port)
{
	intfd;
	errno = 0;
	fd = open(port, O_RDWR | O_NOCTTY);
	if (fd == -1) {
		fprintf(stderr, "open_tty unable to open '%s': %s\n", port, strerror(errno));
		exit(EXIT_FAILURE);
	}
	returnfd;
}

int main(int argc, char **argv)
{
	int fd = -1;
	int ch;
	const char *port = "/dev/ttyUSB0";
	struct termios options;

	if (argc == 2) {
		port = argv[1];
	} else if (argc != 1) {
		fprintf(stderr, "usage: %s /dev/ttyUSBX <file\n", argv[0]);
		return -1;
	}
	fd = open_tty(port);

	errno = 0;
	if (tcgetattr(fd, &options) < 0) {
		fprintf(stderr, "failedtogetterminaloptions:%d,%s\n", fd,
			strerror(errno));
		return -1;
	}

	cfsetispeed(&options, B115200);
	cfsetospeed(&options, B115200);

	cfmakeraw(&options);
	options.c_cflag |= (CLOCAL | CREAD); /* Enable the receiver and set local mode */
	options.c_cflag &= ~CSTOPB;	     /* 1 stopbit */
	options.c_cflag &= ~CRTSCTS;         /* Disable hardware flow control */
	options.c_cc[VMIN] = 0;
	options.c_cc[VTIME] = 1;             /* Timeout read after 1 second */

	errno = 0;
	if (tcsetattr(fd, TCSANOW, &options) < 0) {
		fprintf(stderr, "failed to set terminal options:%d,%s\n", fd,
			strerror(errno));
		exit(EXIT_FAILURE);
	}

	while (EOF != (ch = fgetc(stdin))) {
		char c1 = ch, c2 = 0;
		int r = 0;
 again:
		errno = 0;
		if (write(fd, &c1, 1) != 1) {
			fprintf(stderr, "write error:%s\n", strerror(errno));
			return -1;
		}
		errno = 0;
		r = read(fd, &c2, 1);
		if (r == 0) {
			/*fprintf(stderr,"retransmitting...\n"); */
			goto again;
		}
		elseif(r < 0) {
			fprintf(stderr, "read error:%s\n", strerror(errno));
			return -1;
		}

		if (c1 != c2)
			fprintf(stderr, "error: transmitted '%d' got back '%d'",
				c1, c2);

		write(2, &c1, 1);
	}

	close(fd);
	return0;
}
