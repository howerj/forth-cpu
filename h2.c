/** @file h2.c
 *  @brief simulate the H2 CPU and surrounding system
 * 
 * Initially this program will be for just simulating the H2 core,
 * but eventually it will be extended so the peripherals can also
 * be simulated. This should speed up development of programs written
 * for the device, and allow for simulating the device where there
 * is no tool chain for dealing with VHDL.
 *
 */

#include <assert.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_CORE (8192u)
#define STK_SIZE (32u)

typedef struct {
	uint16_t core[MAX_CORE]; /**< main memory */
	uint16_t rstk[STK_SIZE]; /**< return stack */
	uint16_t vstk[STK_SIZE]; /**< variable stack */
	uint16_t pc;  /**< program counter */
	uint16_t tos; /**< top of stack */
	uint16_t nos; /**< next on stack */
	uint16_t rp;  /**< return stack pointer */
	uint16_t sp;  /**< variable stack pointer */
} h2_t; /**< state of the H2 CPU */

/**@todo add in log levels and use an enum instead of isfatal and prefix*/
int logger(int isfatal, const char *prefix, const char *func, const unsigned line, const char *fmt, ...)
{
	int r;
	va_list ap;
	assert(prefix);
       	assert(func);
       	assert(fmt);
	fprintf(stderr, "[%s %u] %s: ", func, line, prefix);
	va_start(ap, fmt);
	r = vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
	fflush(stderr);
	if(isfatal)
		exit(EXIT_FAILURE);
	return r;
}

#define fatal(FMT, ...)  logger(true, "fatal",  __func__, __LINE__, FMT, ##__VA_ARGS__) 
#define warning(FMT, ...)  logger(false, "warning", __func__, __LINE__, FMT, ##__VA_ARGS__)
#define note(FMT, ...)   logger(false, "note", __func__, __LINE__, FMT, ##__VA_ARGS__)
#define debug(FMT, ...)  logger(false, "debug", __func__, __LINE__, FMT, ##__VA_ARGS__)

static const char *reason(void)
{
	static const char *unknown = "unknown reason";
	const char *r;
	if(errno == 0)
		return unknown;
	r = strerror(errno);
	if(!r)
		return unknown;
	return r;
}

void *alloc_or_die(size_t length)
{
	void *r;
	errno = 0;
	r = calloc(1, length);
	if(!r)
		fatal("allocation of size %zu failed: %s", length, reason());
	return r;
}

FILE *fopen_or_die(const char *file, const char *mode)
{
	FILE *f = NULL;
	errno = 0;
	f = fopen(file, mode);
	if(!f)
		fatal("failed to open file '%s' (mode %s): %s", file, mode, reason());
	return f;
}

int string_to_cell(int base, uint16_t *n, const char *s)
{
	char *end = NULL;
	errno = 0;
	*n = strtol(s, &end, base);
	return errno || *s == '\0' || *end != '\0';
}

h2_t *h2_allocate(void)
{
	return alloc_or_die(sizeof(h2_t));
}

void h2_free(h2_t *h)
{
	assert(h);
	free(h);
}

/** @brief load in a file containing a single hexadecimal digit per line */
int h2_load(h2_t *h, FILE *hexfile)
{
	assert(h);
	assert(hexfile);
	char line[80] = {0}; /*more than enough!*/
	size_t i = 0;
	
	for(;fgets(line, sizeof(line), hexfile); i++) {
		int r;
		if(i >= MAX_CORE) {
			warning("file contains too many lines: %zu", i);
			return -1;
		}
		r = string_to_cell(16, &h->core[i], line);
		if(!r) {
			warning("invalid line (expected hexadecimal string): %s", line);
			return -1;
		}
	}

	if(i < MAX_CORE-1) {
		warning("file contains too few lines: %zu", i);
		return -1;
	}

	return 0;
}

void vpush(h2_t *h, uint16_t v)
{
	assert(h);
	h->vstk[h->sp + 1 % STK_SIZE] = h->nos;
	h->nos = h->tos;
	h->tos = v;
	h->sp++;
	if(h->sp >= STK_SIZE)
		warning("stack overflow");
	h->sp %= STK_SIZE;
}

uint16_t vpop(h2_t *h)
{
	assert(h);
	return 0;
}

/** @todo interrupt handling, implement instruction set and I/O */
int h2_run(h2_t *h, FILE *debug, unsigned steps)
{
	assert(h);
	h->pc = 0;
	h->rp = 0;
	h->sp = 0;
	for(unsigned i; i < steps || steps == 0; i++) {
		uint16_t inst, lit, val;

		inst = h->core[h->pc];
		inst = 0;
		lit = 0;
		val = 0;

		/* decode / execute */
		if((inst & 0x8000) == 0x8000) { /* literal */
			lit = inst & 0x7FFF;
			vpush(h, lit);
			h->pc = (h->pc + 1) % MAX_CORE;
		} else if ( (inst & 0x6000) == 0x6000){ /* ALU instruction */
			h->pc = (h->pc + 1) % MAX_CORE;
		} else if ( (inst & 0x4000) == 0x4000) { /* CALL */
			val = inst & 0x1FFF;
		} else if ( (inst & 0x2000) == 0x2000) { /* Conditional Jump */
			val = inst & 0x1FFF;
		} else if (!(inst & 0xC000)) { /* Jump */
			val = inst & 0x1FFF;
			h->pc = val;
		} else {
			/* ??? */
		}

	}
	return 0;
}

/**@todo command line options to make this program more friendly to use need to
 * be added */
int main(void)
{
	static const char *start_file = "mem_h2.hexadecimal";
	static unsigned run_for = 4000;
	FILE *program = NULL;
	int r = 0;
	h2_t *h = h2_allocate();

	note("attempting to load file: %s", start_file);
	program = fopen_or_die(start_file, "rb");

	if(h2_load(h, program) < 0)
		fatal("invalid program, refusing to run");
	fclose(program);

	note("program loaded successfully");

	note("running program");
	r = h2_run(h, stdout, run_for);
	if(r < 0)
		fatal("program failed to run: %d", r);

	note("h2 finished");
	h2_free(h);
	return r;
}

