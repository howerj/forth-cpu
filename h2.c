/** @file      h2.c
 *  @brief     Simulate the H2 CPU and surrounding system
 *  @copyright Richard James Howe (2017)
 *  @license   MIT
 *
 * This file contains the toolchain for the H2, it is an assembler/compiler,
 * a simulator, a disassembler and a debugger. The H2 is written in VHDL and
 * is based on the J1 processor (see http://excamera.com/sphinx/fpga-j1.html).
 *
 * The processor has been tested on an FPGA and is working.
 * The project can be found at: https://github.com/howerj/forth-cpu */

/* ========================== Preamble: Types, Macros, Globals ============= */

#include "h2.h"
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>

#define UNUSED(VARIABLE) ((void)(VARIABLE))

#ifdef _WIN32 /* Making standard input streams on Windows binary */
#include <windows.h>
#include <io.h>
#include <fcntl.h>
extern int _fileno(FILE *stream);
static void binary(FILE *f) { _setmode(_fileno(f), _O_BINARY); }
#else
static inline void binary(FILE *f) { UNUSED(f); }
#endif

#define DEFAULT_STEPS (0) /*default is to run forever*/
#define MAX(X, Y)     ((X) > (Y) ? (X) : (Y))
#define MIN(X, Y)     ((X) > (Y) ? (Y) : (X))

#define NUMBER_OF_INTERRUPTS (8u)

#define OP_BRANCH        (0x0000)
#define OP_0BRANCH       (0x2000)
#define OP_CALL          (0x4000)
#define OP_ALU_OP        (0x6000)
#define OP_LITERAL       (0x8000)

#define IS_LITERAL(INST) (((INST) & 0x8000) == 0x8000)
#define IS_BRANCH(INST)  (((INST) & 0xE000) == 0x0000)
#define IS_0BRANCH(INST) (((INST) & 0xE000) == 0x2000)
#define IS_CALL(INST)    (((INST) & 0xE000) == 0x4000)
#define IS_ALU_OP(INST)  (((INST) & 0xE000) == 0x6000)

#define ALU_OP_LENGTH   (5u)
#define ALU_OP_START    (8u)
#define ALU_OP(INST)    (((INST) >> ALU_OP_START) & ((1 << ALU_OP_LENGTH) - 1))

#define DSTACK_LENGTH   (2u)
#define DSTACK_START    (0u)
#define DSTACK(INST)    (((INST) >> DSTACK_START) & ((1 << DSTACK_LENGTH) - 1))

#define RSTACK_LENGTH   (2u)
#define RSTACK_START    (2u)
#define RSTACK(INST)    (((INST) >> RSTACK_START) & ((1 << RSTACK_LENGTH) - 1))

#define R_TO_PC_BIT_INDEX     (4u)
#define N_TO_ADDR_T_BIT_INDEX (5u)
#define T_TO_R_BIT_INDEX      (6u)
#define T_TO_N_BIT_INDEX      (7u)

#define R_TO_PC         (1u << R_TO_PC_BIT_INDEX)
#define N_TO_ADDR_T     (1u << N_TO_ADDR_T_BIT_INDEX)
#define T_TO_R          (1u << T_TO_R_BIT_INDEX)
#define T_TO_N          (1u << T_TO_N_BIT_INDEX)

typedef enum {
	ALU_OP_T,                  /**< Top of Stack         */
	ALU_OP_N,                  /**< Copy T to N          */
	ALU_OP_T_PLUS_N,           /**< Addition             */
	ALU_OP_T_AND_N,            /**< Bitwise AND          */
	ALU_OP_T_OR_N,             /**< Bitwise OR           */
	ALU_OP_T_XOR_N,            /**< Bitwise XOR          */
	ALU_OP_T_INVERT,           /**< Bitwise Inversion    */
	ALU_OP_T_EQUAL_N,          /**< Equality test        */
	ALU_OP_N_LESS_T,           /**< Signed comparison    */
	ALU_OP_N_RSHIFT_T,         /**< Logical Right Shift  */
	ALU_OP_T_DECREMENT,        /**< Decrement            */
	ALU_OP_R,                  /**< Top of return stack  */
	ALU_OP_T_LOAD,             /**< Load from address    */
	ALU_OP_N_LSHIFT_T,         /**< Logical Left Shift   */
	ALU_OP_DEPTH,              /**< Depth of stack       */
	ALU_OP_N_ULESS_T,          /**< Unsigned comparison  */
	ALU_OP_ENABLE_INTERRUPTS,  /**< Enable interrupts    */

	ALU_OP_INTERRUPTS_ENABLED, /**< Are interrupts on?   */
	ALU_OP_RDEPTH,             /**< R Stack Depth        */
	ALU_OP_T_EQUAL_0,          /**< T == 0               */
	ALU_OP_CPU_ID              /**< CPU Identifier       */
} alu_code_e;

#define DELTA_0  (0)
#define DELTA_1  (1)
#define DELTA_N2 (2)
#define DELTA_N1 (3)

#define MK_DSTACK(DELTA) ((DELTA) << DSTACK_START)
#define MK_RSTACK(DELTA) ((DELTA) << RSTACK_START)
#define MK_CODE(CODE)    ((CODE)  << ALU_OP_START)

/**
 * @warning This table keeps most things synchronized when it comes
 * to instructions, the exception is in the lexer, which accepts
 * a range of tokens in one clause of the grammar from the first
 * instruction to the last. This must be manually updated
 * @note In the original J1 specification both r@ and r> both have
 * their T_TO_R bit set in their instruction description tables, this
 * appears to be incorrect */
#define X_MACRO_INSTRUCTIONS \
	X(DUP,    "dup",    true,  (OP_ALU_OP | MK_CODE(ALU_OP_T)        | T_TO_N  | MK_DSTACK(DELTA_1)))\
	X(OVER,   "over",   true,  (OP_ALU_OP | MK_CODE(ALU_OP_N)        | T_TO_N  | MK_DSTACK(DELTA_1)))\
	X(INVERT, "invert", true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_INVERT)))\
	X(ADD,    "+",      true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_PLUS_N)               | MK_DSTACK(DELTA_N1)))\
	X(SWAP,   "swap",   true,  (OP_ALU_OP | MK_CODE(ALU_OP_N)        | T_TO_N))\
	X(NIP,    "nip",    true,  (OP_ALU_OP | MK_CODE(ALU_OP_T)                      | MK_DSTACK(DELTA_N1)))\
	X(DROP,   "drop",   true,  (OP_ALU_OP | MK_CODE(ALU_OP_N)                      | MK_DSTACK(DELTA_N1)))\
	X(EXIT,   "exit",   true,  (OP_ALU_OP | MK_CODE(ALU_OP_T)        | R_TO_PC | MK_RSTACK(DELTA_N1)))\
	X(TOR,    ">r",     true,  (OP_ALU_OP | MK_CODE(ALU_OP_N)        | T_TO_R  | MK_DSTACK(DELTA_N1) | MK_RSTACK(DELTA_1)))\
	X(FROMR,  "r>",     true,  (OP_ALU_OP | MK_CODE(ALU_OP_R)        | T_TO_N  | MK_DSTACK(DELTA_1)  | MK_RSTACK(DELTA_N1)))\
	X(RAT,    "r@",     true,  (OP_ALU_OP | MK_CODE(ALU_OP_R)        | T_TO_N  | MK_DSTACK(DELTA_1)))\
	X(LOAD,   "@",      true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_LOAD)))\
	X(STORE,  "store",  false, (OP_ALU_OP | MK_CODE(ALU_OP_N)        | N_TO_ADDR_T | MK_DSTACK(DELTA_N1)))\
	X(RSHIFT, "rshift", true,  (OP_ALU_OP | MK_CODE(ALU_OP_N_RSHIFT_T)             | MK_DSTACK(DELTA_N1)))\
	X(LSHIFT, "lshift", true,  (OP_ALU_OP | MK_CODE(ALU_OP_N_LSHIFT_T)             | MK_DSTACK(DELTA_N1)))\
	X(EQUAL,  "=",      true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_EQUAL_N)              | MK_DSTACK(DELTA_N1)))\
	X(ULESS,  "u<",     true,  (OP_ALU_OP | MK_CODE(ALU_OP_N_ULESS_T)              | MK_DSTACK(DELTA_N1)))\
	X(LESS,   "<",      true,  (OP_ALU_OP | MK_CODE(ALU_OP_N_LESS_T)               | MK_DSTACK(DELTA_N1)))\
	X(AND,    "and",    true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_AND_N)                | MK_DSTACK(DELTA_N1)))\
	X(XOR,    "xor",    true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_XOR_N)                | MK_DSTACK(DELTA_N1)))\
	X(OR,     "or",     true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_OR_N)                 | MK_DSTACK(DELTA_N1)))\
	X(DEPTH,  "sp@",    true,  (OP_ALU_OP | MK_CODE(ALU_OP_DEPTH)   | T_TO_N       | MK_DSTACK(DELTA_1)))\
	X(T_N1,   "1-",     true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_DECREMENT)))\
	X(IEN,    "ien",    true,  (OP_ALU_OP | MK_CODE(ALU_OP_ENABLE_INTERRUPTS)     /* | MK_DSTACK(DELTA_N1) */))\
	X(ISIEN,  "ien?",   true,  (OP_ALU_OP | MK_CODE(ALU_OP_INTERRUPTS_ENABLED) | T_TO_N  | MK_DSTACK(DELTA_1)))\
	X(RDEPTH, "rp@",    true,  (OP_ALU_OP | MK_CODE(ALU_OP_RDEPTH)  | T_TO_N       | MK_DSTACK(DELTA_1)))\
	X(TE0,    "0=",     true,  (OP_ALU_OP | MK_CODE(ALU_OP_T_EQUAL_0)))\
	X(NOP,    "nop",    false, (OP_ALU_OP | MK_CODE(ALU_OP_T)))\
	X(CPU_ID, "cpu-id", true,  (OP_ALU_OP | MK_CODE(ALU_OP_CPU_ID))                | MK_DSTACK(DELTA_1))\
	X(RUP,    "rup",    false, (OP_ALU_OP | MK_CODE(ALU_OP_T))                     | MK_RSTACK(DELTA_1))\
	X(RDROP,  "rdrop",  true,  (OP_ALU_OP | MK_CODE(ALU_OP_T) | MK_RSTACK(DELTA_N1)))


typedef enum {
#define X(NAME, STRING, DEFINE, INSTRUCTION) CODE_ ## NAME = INSTRUCTION,
	X_MACRO_INSTRUCTIONS
#undef X
} forth_word_codes_e;

static const char *log_levels[] =
{
#define X(ENUM, NAME) [ENUM] = NAME,
	X_MACRO_LOGGING
#undef X
};

log_level_e log_level = LOG_WARNING;

typedef struct {
	int error;
	int jmp_buf_valid;
	jmp_buf j;
} error_t;

/* ========================== Preamble: Types, Macros, Globals ============= */

/* ========================== Utilities ==================================== */

int logger(log_level_e level, const char *func,
		const unsigned line, const char *fmt, ...)
{
	int r = 0;
	va_list ap;
       	assert(func);
       	assert(fmt);
	assert(level <= LOG_ALL_MESSAGES);
	if(level <= log_level) {
		fprintf(stderr, "[%s %u] %s: ", func, line, log_levels[level]);
		va_start(ap, fmt);
		r = vfprintf(stderr, fmt, ap);
		va_end(ap);
		fputc('\n', stderr);
		fflush(stderr);
	}
	if(level == LOG_FATAL)
		exit(EXIT_FAILURE);
	return r;
}

static const char *reason(void)
{
	static const char *unknown = "unknown reason";
	if(errno == 0)
		return unknown;
	const char *r = strerror(errno);
	if(!r)
		return unknown;
	return r;
}

void *allocate_or_die(size_t length)
{
	errno = 0;
	void *r = calloc(1, length);
	if(!r)
		fatal("allocation of size %zu failed: %s",
				length, reason());
	return r;
}

FILE *fopen_or_die(const char *file, const char *mode)
{
	assert(file);
	assert(mode);
	errno = 0;
	FILE *f = fopen(file, mode);
	if(!f)
		fatal("failed to open file '%s' (mode %s): %s",
				file, mode, reason());
	return f;
}

static int indent(FILE *output, char c, unsigned i)
{
	assert(output);
	while(i--)
		if(fputc(c, output) != c)
			return -1;
	return 0;
}

static int string_to_long(int base, long *n, const char *s)
{
	char *end = NULL;
	assert(base >= 0);
	assert(base != 1);
	assert(base <= 36);
	assert(n);
	assert(s);
	errno = 0;
	*n = strtol(s, &end, base);
	return errno || *s == '\0' || *end != '\0';
}

static int string_to_cell(int base, uint16_t *n, const char *s)
{
	long n1 = 0;
	int r = string_to_long(base, &n1, s);
	*n = n1;
	return r;
}

static char *duplicate(const char *str)
{
	assert(str);
	size_t length = strlen(str);
	assert((length + 1) > length);
	errno = 0;
	char *r = malloc(length + 1);
	if(!r)
		fatal("duplicate of '%s' failed: %s", str, reason());
	strcpy(r, str);
	return r;
}

static void ethrow(error_t *e)
{
	if(e && e->jmp_buf_valid) {
		e->jmp_buf_valid = 0;
		e->error = 1;
		longjmp(e->j, 1);
	}
	exit(EXIT_FAILURE);
}

h2_t *h2_new(uint16_t start_address)
{
	h2_t *h = allocate_or_die(sizeof(h2_t));
	h->pc = start_address;
	for(uint16_t i = 0; i < start_address; i++)
		h->core[i] = OP_BRANCH | start_address;
	return h;
}

void h2_free(h2_t *h)
{
	if(!h)
		return;
	free(h->bp.points);
	memset(h, 0, sizeof(*h));
	free(h);
}

int binary_memory_load(FILE *input, uint16_t *p, size_t length)
{
	assert(input);
	assert(p);
	for(size_t i = 0; i < length; i++) {
		errno = 0;
		const int r1 = fgetc(input);
		const int r2 = fgetc(input);
		if(r1 < 0 || r2 < 0) {
			debug("memory read failed: %s", strerror(errno));
			return -1;
		}
		p[i] = (((unsigned)r1 & 0xffu)) | (((unsigned)r2 & 0xffu) << 8u);
	}
	return 0;
}

int binary_memory_save(FILE *output, uint16_t *p, size_t length)
{
	assert(output);
	assert(p);
	for(size_t i = 0; i < length; i++) {
		errno = 0;
		const int r1 = fputc((p[i])     & 0xff,output);
		const int r2 = fputc((p[i]>>8u) & 0xff, output);
		if(r1 < 0 || r2 < 0) {
			debug("memory write failed: %s", strerror(errno));
			return -1;
		}
	}
	return 0;
}

int nvram_load_and_transfer(h2_io_t *io, const char *name, bool transfer_to_sram)
{
	assert(io);
	assert(name);
	FILE *input = NULL;
	int r = 0;
	errno = 0;
	if((input = fopen(name, "rb"))) {
		r = binary_memory_load(input, io->soc->flash.nvram, CHIP_MEMORY_SIZE);
		if(transfer_to_sram)
			memcpy(io->soc->vram, io->soc->flash.nvram, CHIP_MEMORY_SIZE);
		fclose(input);
	} else {
		error("nvram file read (from %s) failed: %s", name, strerror(errno));
		r = -1;
	}
	return r;
}

int nvram_save(h2_io_t *io, const char *name)
{
	FILE *output = NULL;
	int r = 0;
	assert(io);
	assert(name);
	errno = 0;
	if((output = fopen(name, "wb"))) {
		r = binary_memory_save(output, io->soc->flash.nvram, CHIP_MEMORY_SIZE);
		fclose(output);
	} else {
		error("nvram file write (to %s) failed: %s", name, strerror(errno));
		r = -1;
	}
	return r;
}

int memory_load(FILE *input, uint16_t *p, size_t length)
{
	assert(input);
	assert(p);
	char line[80] = {0}; /*more than enough!*/
	size_t i = 0;

	for(;fgets(line, sizeof(line), input); i++) {
		int r;
		if(i >= length) {
			error("file contains too many lines: %zu", i);
			return -1;
		}
		r = string_to_cell(16, &p[i], line);
		if(!r) {
			error("invalid line - expected hex string: %s", line);
			return -1;
		}
		debug("%zu %u", i, (unsigned)p[i]);
	}

	return 0;
}

int memory_save(FILE *output, uint16_t *p, size_t length)
{
	assert(output);
	assert(p);
	for(size_t i = 0; i < length; i++)
		if(fprintf(output, "%04"PRIx16"\n", p[i]) < 0) {
			error("failed to write line: %"PRId16, i);
			return -1;
		}
	return 0;
}

int h2_load(h2_t *h, FILE *hexfile)
{
	assert(h);
	assert(hexfile);
	return memory_load(hexfile, h->core, MAX_CORE);
}

int h2_save(h2_t *h, FILE *output, bool full)
{
	assert(h);
	assert(output);
	return memory_save(output, h->core, full ? MAX_CORE : h->pc);
}

/* From: https://stackoverflow.com/questions/215557/how-do-i-implement-a-circular-list-ring-buffer-in-c */

fifo_t *fifo_new(size_t size)
{
	assert(size >= 2); /* It does not make sense to have a FIFO less than this size */
	fifo_data_t *buffer = allocate_or_die(size * sizeof(buffer[0]));
	fifo_t *fifo = allocate_or_die(sizeof(fifo_t));

	fifo->buffer = buffer;
	fifo->head   = 0;
	fifo->tail   = 0;
	fifo->size   = size;

	return fifo;
}

void fifo_free(fifo_t *fifo)
{
	if(!fifo)
		return;
	free(fifo->buffer);
	free(fifo);
}

bool fifo_is_full(fifo_t * fifo)
{
	assert(fifo);
	return (fifo->head == (fifo->size - 1) && fifo->tail == 0)
	    || (fifo->head == (fifo->tail - 1));
}

bool fifo_is_empty(fifo_t * fifo)
{
	assert(fifo);
	return fifo->head == fifo->tail;
}

size_t fifo_count(fifo_t * fifo)
{
	assert(fifo);
	if (fifo_is_empty(fifo))
		return 0;
	else if (fifo_is_full(fifo))
		return fifo->size;
	else if (fifo->head < fifo->tail)
		return fifo->head + (fifo->size - fifo->tail);
	else
		return fifo->head - fifo->tail;
}

size_t fifo_push(fifo_t * fifo, fifo_data_t data)
{
	assert(fifo);

	if (fifo_is_full(fifo))
		return 0;

	fifo->buffer[fifo->head] = data;

	fifo->head++;
	if (fifo->head == fifo->size)
		fifo->head = 0;

	return 1;
}

size_t fifo_pop(fifo_t * fifo, fifo_data_t * data)
{
	assert(fifo);
	assert(data);

	if (fifo_is_empty(fifo))
		return 0;

	*data = fifo->buffer[fifo->tail];

	fifo->tail++;
	if (fifo->tail == fifo->size)
		fifo->tail = 0;

	return 1;
}

#ifdef __unix__
#include <unistd.h>
#include <termios.h>
static int getch(void)
{
	struct termios oldattr, newattr;
	tcgetattr(STDIN_FILENO, &oldattr);
	newattr = oldattr;
	newattr.c_iflag &= ~(ICRNL);
	newattr.c_lflag &= ~(ICANON | ECHO);

	tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
	const int ch = getchar();

	tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);

	return ch;
}

static int putch(int c)
{
	int res = putchar(c);
	fflush(stdout);
	return res;
}
#else
#ifdef _WIN32

extern int getch(void);
extern int putch(int c);

#else
static int getch(void)
{
	return getchar();
}

static int putch(int c)
{
	return putchar(c);
}
#endif
#endif /** __unix__ **/

static int wrap_getch(bool *debug_on)
{
	const int ch = getch();
	assert(debug_on);
	if(ch == EOF) {
		note("End Of Input - exiting", ESCAPE);
		exit(EXIT_SUCCESS);
	}
	if(ch == ESCAPE && debug_on)
		*debug_on = true;

	return ch == DELETE ? BACKSPACE : ch;
}

/* ========================== Utilities ==================================== */

/* ========================== Symbol Table ================================= */

static const char *symbol_names[] =
{
	[SYMBOL_TYPE_LABEL]       = "label",
	[SYMBOL_TYPE_CALL]        = "call",
	[SYMBOL_TYPE_CONSTANT]    = "constant",
	[SYMBOL_TYPE_VARIABLE]    = "variable",
	NULL
};

static symbol_t *symbol_new(symbol_type_e type, const char *id, uint16_t value)
{
	symbol_t *s = allocate_or_die(sizeof(*s));
	assert(id);
	s->id = duplicate(id);
	s->value = value;
	s->type = type;
	return s;
}

static void symbol_free(symbol_t *s)
{
	if(!s)
		return;
	free(s->id);
	memset(s, 0, sizeof(*s));
	free(s);
}

static symbol_table_t *symbol_table_new(void)
{
	symbol_table_t *t = allocate_or_die(sizeof(*t));
	return t;
}

static void symbol_table_free(symbol_table_t *t)
{
	if(!t)
		return;
	for(size_t i = 0; i < t->length; i++)
		symbol_free(t->symbols[i]);
	free(t->symbols);
	memset(t, 0, sizeof(*t));
	free(t);
}

static symbol_t *symbol_table_lookup(symbol_table_t *t, const char *id)
{
	for(size_t i = 0; i < t->length; i++)
		if(!strcmp(t->symbols[i]->id, id))
			return t->symbols[i];
	return NULL;
}

/** @note There can be multiple symbols with the same value of the same type */
static symbol_t *symbol_table_reverse_lookup(symbol_table_t *t, symbol_type_e type, uint16_t value)
{
	for(size_t i = 0; i < t->length; i++)
		if(t->symbols[i]->type == type && t->symbols[i]->value == value)
			return t->symbols[i];
	return NULL;
}

static int symbol_table_add(symbol_table_t *t, symbol_type_e type, const char *id, uint16_t value, error_t *e, bool hidden)
{
	symbol_t *s = symbol_new(type, id, value);
	symbol_t **xs = NULL;
	assert(t);

	if(symbol_table_lookup(t, id)) {
		symbol_free(s);
		error("redefinition of symbol: %s", id);
		if(e)
			ethrow(e);
		else
			return -1;
	}
	s->hidden = hidden;

	t->length++;
	errno = 0;
	xs = realloc(t->symbols, sizeof(*t->symbols) * t->length);
	if(!xs)
		fatal("reallocate of size %zu failed: %s", t->length, reason());
	t->symbols = xs;
	t->symbols[t->length - 1] = s;
	return 0;
}

static int symbol_table_print(symbol_table_t *t, FILE *output)
{

	assert(t);
	for(size_t i = 0; i < t->length; i++) {
		symbol_t *s = t->symbols[i];
		char *visibility = s->hidden ? "hidden" : "visible";
		if(fprintf(output, "%s %s %"PRId16" %s\n", symbol_names[s->type], s->id, s->value, visibility) < 0)
			return -1;
	}
	return 0;
}

symbol_table_t *symbol_table_load(FILE *input)
{
	symbol_table_t *t = symbol_table_new();
	assert(input);
	char symbol[80];
	char id[256];
	char visibility[80];
	uint16_t value;

	while(!feof(input)) {
		int r = 0;
		memset(symbol,     0, sizeof(symbol));
		memset(id,         0, sizeof(id));
		memset(visibility, 0, sizeof(visibility));
		value = 0;
		r = fscanf(input, "%79s%255s%"SCNd16"%79s", symbol, id, &value, visibility);
		if(r != 4 && r > 0) {
			error("invalid symbol table: %d", r);
			goto fail;
		}
		if(r == 4) {
			size_t i = 0;
			bool hidden = false;
			if(!strcmp(visibility, "hidden")) {
				hidden = true;
			}else if(!strcmp(visibility, "visible")) {
				error("invalid visibility value: %s", visibility);
				goto fail;
			}

			for(i = 0; symbol_names[i] && strcmp(symbol_names[i], symbol); i++)
				/*do nothing*/;
			if(symbol_names[i]) {
				if(symbol_table_add(t, i, id, value, NULL, hidden) < 0)
					goto fail;
			} else {
				error("invalid symbol: %s", symbol);
				goto fail;
			}
		}
	}
	if(log_level >= LOG_DEBUG)
		symbol_table_print(t, stderr);
	return t;
fail:
	symbol_table_free(t);
	return NULL;
}
/* ========================== Symbol Table ================================= */

/* ========================== Disassembler ================================= */

static const char *instruction_to_string(uint16_t i)
{
	switch(i) {
#define X(NAME, STRING, DEFINE, INSTRUCTION) case CODE_ ## NAME : return STRING ;
	X_MACRO_INSTRUCTIONS
#undef X
	default:          break;
	}
	return NULL;
}

static const char *alu_op_to_string(uint16_t instruction)
{
	switch(ALU_OP(instruction)) {
	case ALU_OP_T:                  return "T";
	case ALU_OP_N:                  return "N";
	case ALU_OP_T_PLUS_N:           return "T+N";
	case ALU_OP_T_AND_N:            return "T&N";
	case ALU_OP_T_OR_N:             return "T|N";
	case ALU_OP_T_XOR_N:            return "T^N";
	case ALU_OP_T_INVERT:           return "~T";
	case ALU_OP_T_EQUAL_N:          return "N=T";
	case ALU_OP_N_LESS_T:           return "T>N";
	case ALU_OP_N_RSHIFT_T:         return "N>>T";
	case ALU_OP_T_DECREMENT:        return "T-1";
	case ALU_OP_R:                  return "R";
	case ALU_OP_T_LOAD:             return "[T]";
	case ALU_OP_N_LSHIFT_T:         return "N<<T";
	case ALU_OP_DEPTH:              return "depth";
	case ALU_OP_N_ULESS_T:          return "Tu>N";
	case ALU_OP_ENABLE_INTERRUPTS:  return "ien";
	case ALU_OP_INTERRUPTS_ENABLED: return "ien?";
	case ALU_OP_RDEPTH:             return "rdepth";
	case ALU_OP_T_EQUAL_0:          return "0=";
	case ALU_OP_CPU_ID:             return "cpu-id";
	default:                        return "unknown";
	}
}

static char *disassembler_alu(uint16_t instruction)
{
	char buf[256] = {0};
	const char *r = instruction_to_string(OP_ALU_OP | instruction);
	if(r)
		return duplicate(r);
	sprintf(buf, "%04x:%s:%s:%s:%s:%s:%u:%u",
			(unsigned)instruction,
			alu_op_to_string(instruction),
			instruction & T_TO_N ? "T->N" : "",
			instruction & T_TO_R ? "T->R" : "",
			instruction & N_TO_ADDR_T ? "N->[T]" : "",
			instruction & R_TO_PC ? "R->PC" : "",
			(unsigned)(instruction & 0x000C),
			(unsigned)(instruction & 0x0003));
	return duplicate(buf);
}

static const char *disassemble_jump(symbol_table_t *symbols, symbol_type_e type, uint16_t address)
{
	static const char *r = "";
	symbol_t *found = NULL;
	if(!symbols)
		return r;
	if((found = symbol_table_reverse_lookup(symbols, type, address)))
		return found->id;
	return r;
}


#define CSI "\033["
#define ANSI_RESET   (CSI "0m")
#define ANSI_BLACK   (CSI "30m")
#define ANSI_RED     (CSI "31m")
#define ANSI_GREEN   (CSI "32m")
#define ANSI_YELLOW  (CSI "33m")
#define ANSI_BLUE    (CSI "34m")
#define ANSI_MAGENTA (CSI "35m")
#define ANSI_CYAN    (CSI "36m")
#define ANSI_WHITE   (CSI "37m")

typedef enum {
	DCM_NONE,
	DCM_X11,
	DCM_ANSI,
	DCM_MAX_DCM
} disassemble_color_method_e;

typedef enum {
	DC_LITERAL,
	DC_ALU,
	DC_CALL,
	DC_0BRANCH,
	DC_BRANCH,
	DC_ERROR,  /* Invalid instruction */
	DC_RESET,  /* Reset color */
} decompilation_color_e;

static int disassemble_instruction(uint16_t instruction, FILE *output, symbol_table_t *symbols, disassemble_color_method_e dcm)
{
	int r = 0;
	unsigned short literal, address;
	char *s = NULL;
	assert(output);
	assert(dcm < DCM_MAX_DCM);

	static const char *colors[3][7] = { /* for colorizing decompilation stream with in-band signalling */
		/* LITERAL     ALU            CALL        0BRANCH      BRANCH    ERROR      RESET */
		[DCM_NONE] = { "",           "",           "",         "",          "",        "",       "" },         /* No-Color */
		[DCM_X11]  = { "?HotPink?",  "?SkyBlue?",  "?GreenYellow?",  "?Khaki?",  "?MediumTurquoise?",  "?FireBrick?",  "" },         /* X11/GTKWave */
		[DCM_ANSI] = { ANSI_MAGENTA, ANSI_BLUE,    ANSI_GREEN, ANSI_YELLOW, ANSI_CYAN, ANSI_RED, ANSI_RESET }, /* ANSI Escape Sequences */
	};

	const char **color = colors[dcm];

	literal = instruction & 0x7FFF;
	address = instruction & 0x1FFF;

	if (IS_LITERAL(instruction))
		r = fprintf(output, "%s%hx%s", color[DC_LITERAL], literal, color[DC_RESET]);
	else if (IS_ALU_OP(instruction))
		r = fprintf(output, "%s%s%s", color[DC_ALU], s = disassembler_alu(instruction), color[DC_RESET]);
	else if (IS_CALL(instruction))
		r = fprintf(output, "%scall%s %hx %s",  color[DC_CALL], color[DC_RESET], address, disassemble_jump(symbols, SYMBOL_TYPE_CALL, address));
	else if (IS_0BRANCH(instruction))
		r = fprintf(output, "%s0branch%s %hx %s", color[DC_0BRANCH], color[DC_RESET], address, disassemble_jump(symbols, SYMBOL_TYPE_LABEL, address));
	else if (IS_BRANCH(instruction))
		r = fprintf(output, "%sbranch%s %hx %s",  color[DC_BRANCH], color[DC_RESET], address, disassemble_jump(symbols, SYMBOL_TYPE_LABEL, address));
	else
		r = fprintf(output, "%s?(%hx)%s", color[DC_ERROR], instruction, color[DC_RESET]);
	free(s);
	return r < 0 ? -1 : 0;
}

int h2_disassemble(disassemble_color_method_e dcm, FILE *input, FILE *output, symbol_table_t *symbols)
{
	assert(input);
	assert(output);
	assert(dcm < DCM_MAX_DCM);
	char line[80] = {0};
	while(!feof(input)) {
		memset(line, 0, sizeof(line));
		fscanf(input, "%79s", line);
		if(line[0]) {
			uint16_t instruction;
			if(string_to_cell(16, &instruction, line)) {
				error("invalid input to disassembler: %s", line);
				return -1;
			}
			if(disassemble_instruction(instruction, output, symbols, dcm) < 0) {
				error("disassembly failed");
				return -1;
			}
			if(fputc('\n', output) != '\n') {
				error("disassembly failed");
				return -1;
			}
			fflush(output);
		}
	}
	return 0;
}

/* ========================== Disassembler ================================= */

/* ========================== Simulation And Debugger ====================== */

/* @note At the moment I/O is not timing accurate, the UART behaves as if reads
 * and writes happened instantly, along with the PS/2 keyboard. Also the UART
 * has a FIFO which is not simulated. It should be easy enough to delay for
 * the roughly the right number of cycles, but not to get exact cycle
 * accurate timing */

static char to_char(uint8_t c)
{
	return isprint(c) ? c : '.';
}

static void memory_print(FILE *out, uint16_t start, uint16_t *p, uint16_t length, bool chars)
{
	const uint16_t line_length = 16;
	assert(out);
	assert(p);
	for(uint16_t i = 0; i < length; i += line_length) {
		fprintf(out, "%04"PRIx16 ": ", i + start);
		for(uint16_t j = 0; j < line_length && j + i < length; j++)
			fprintf(out, "%04"PRIx16 " ", p[j + i]);
		fputc('\t', out);
		if(chars) /* correct endianess? */
			for(uint16_t j = 0; j < line_length && j + i < length; j++)
				fprintf(out, "%c%c", to_char(p[j + i] >> 8), to_char(p[j + i]));

		putc('\n', out);
	}
	putc('\n', out);
}

static bool break_point_find(break_point_t *bp, uint16_t find_me)
{
	assert(bp);
	for(size_t i = 0; i < bp->length; i++)
		if(bp->points[i] == find_me)
			return true;
	return false;
}

static void break_point_add(break_point_t *bp, uint16_t point)
{
	assert(bp);
	if(break_point_find(bp, point))
		return;
	const size_t a = (bp->length + 1) * sizeof(bp->points[0]);
	uint16_t *r = realloc(bp->points, a);
	if(!r || a < bp->length)
		fatal("realloc of size %zu failed", a);
	r[bp->length] = point;
	bp->length++;
	bp->points = r;
}

static int break_point_print(FILE *out, break_point_t *bp)
{
	assert(out);
	assert(bp);
	for(size_t i = 0; i < bp->length; i++)
		if(fprintf(out, "\t0x%04"PRIx16 "\n", bp->points[i]) < 0)
			return -1;
	return 0;
}

#define LED_7_SEGMENT_DISPLAY_CHARSET_HEX  "0123456789AbCdEF"
#define LED_7_SEGMENT_DISPLAY_CHARSET_BCD  "0123456789 .-   "

static char l7seg(uint8_t c)
{
	static const char *v = LED_7_SEGMENT_DISPLAY_CHARSET_HEX;
	return v[c & 0xf];
}

void soc_print(FILE *out, h2_soc_state_t *soc)
{
	assert(out);
	assert(soc);
	const unsigned char led0 = l7seg(soc->led_7_segments >> 12);
	const unsigned char led1 = l7seg(soc->led_7_segments >>  8);
	const unsigned char led2 = l7seg(soc->led_7_segments >>  4);
	const unsigned char led3 = l7seg(soc->led_7_segments);

	fprintf(out, "LEDS:             %02"PRIx8"\n",  soc->leds);
	/*fprintf(out, "VGA Cursor:       %04"PRIx16"\n", soc->vga_cursor);
	fprintf(out, "VGA Control:      %04"PRIx16"\n", soc->vga_control);*/
	fprintf(out, "Timer Control:    %04"PRIx16"\n", soc->timer_control);
	fprintf(out, "Timer:            %04"PRIx16"\n", soc->timer);
	fprintf(out, "IRC Mask:         %04"PRIx16"\n", soc->irc_mask);
	fprintf(out, "UART Input:       %02"PRIx8"\n",  soc->uart_getchar_register);
	fprintf(out, "LED 7 segment:    %c%c%c%c\n",    led0, led1, led2, led3);
	fprintf(out, "Switches:         %04"PRIx16"\n", soc->switches);
	fprintf(out, "Waiting:          %s\n",          soc->wait ? "true" : "false");
	fprintf(out, "Flash Control:    %04"PRIx16"\n", soc->mem_control);
	fprintf(out, "Flash Address Lo: %04"PRIx16"\n", soc->mem_addr_low);
	fprintf(out, "Flash Data Out:   %04"PRIx16"\n", soc->mem_dout);
}

static void terminal_default_command_sequence(vt100_t *t)
{
	assert(t);
	t->n1 = 1;
	t->n2 = 1;
	t->command_index = 0;
}

static void terminal_at_xy(vt100_t *t, unsigned x, unsigned y, bool limit_not_wrap)
{
	assert(t);
	if(limit_not_wrap) {
		x = MAX(x, 0);
		y = MAX(y, 0);
		x = MIN(x, t->width - 1);
		y = MIN(y, t->height - 1);
	} else {
		x %= t->width;
		y %= t->height;
	}
	t->cursor = (y * t->width) + x;
}

static int terminal_x_current(vt100_t *t)
{
	assert(t);
	return t->cursor % t->width;
}

static int terminal_y_current(vt100_t *t)
{
	assert(t);
	return t->cursor / t->width;
}

static void terminal_at_xy_relative(vt100_t *t, int x, int y, bool limit_not_wrap)
{
	assert(t);
	const int x_current = terminal_x_current(t);
	const int y_current = terminal_y_current(t);
	terminal_at_xy(t, x_current + x, y_current + y, limit_not_wrap);
}

static void terminal_parse_attribute(vt100_attribute_t *a, unsigned v)
{
	assert(a);
	switch(v) {
	case 0:
		memset(a, 0, sizeof(*a));
		a->foreground_color = WHITE;
		a->background_color = BLACK;
		return;
	case 1: a->bold          = true; return;
	case 4: a->under_score   = true; return;
	case 5: a->blink         = true; return;
	case 7: a->reverse_video = true; return;
	case 8: a->conceal       = true; return;
	default:
		if(v >= 30 && v <= 37)
			a->foreground_color = v - 30;
		if(v >= 40 && v <= 47)
			a->background_color = v - 40;
	}
}

static const vt100_attribute_t vt100_default_attribute = {
	.foreground_color = WHITE,
	.background_color = BLACK,
};

static void terminal_attribute_block_set(vt100_t *t, size_t size, const vt100_attribute_t *a)
{
	assert(t);
	assert(a);
	for(size_t i = 0; i < size; i++)
		memcpy(&t->attributes[i], a, sizeof(*a));
}

static int terminal_escape_sequences(vt100_t *t, uint8_t c)
{
	assert(t);
	assert(t->state != TERMINAL_NORMAL_MODE);
	switch(t->state) {
	case TERMINAL_CSI:
		if(c == '[')
			t->state = TERMINAL_COMMAND;
		else
			goto fail;
		break;
	case TERMINAL_COMMAND:
		switch(c) {
		case 's':
			t->cursor_saved = t->cursor;
			goto success;
		case 'n':
			t->cursor = t->cursor_saved;
			goto success;
		case '?':
			terminal_default_command_sequence(t);
			t->state = TERMINAL_DECTCEM;
			break;
		case ';':
			terminal_default_command_sequence(t);
			t->state = TERMINAL_NUMBER_2;
			break;
		default:
			if(isdigit(c)) {
				terminal_default_command_sequence(t);
				t->command_index++;
				t->n1 = c - '0';
				t->state = TERMINAL_NUMBER_1;
			} else {
				goto fail;
			}
		}
		break;
	case TERMINAL_NUMBER_1:
		if(isdigit(c)) {
			if(t->command_index > 3)
				goto fail;
			t->n1 = (t->n1 * (t->command_index ? 10 : 0)) + (c - '0');
			t->command_index++;
			break;
		}

		switch(c) {
		case 'A': terminal_at_xy_relative(t,  0,     -t->n1, true); goto success;/* relative cursor up */
		case 'B': terminal_at_xy_relative(t,  0,      t->n1, true); goto success;/* relative cursor down */
		case 'C': terminal_at_xy_relative(t,  t->n1,  0,     true); goto success;/* relative cursor forward */
		case 'D': terminal_at_xy_relative(t, -t->n1,  0,     true); goto success;/* relative cursor back */
		case 'E': terminal_at_xy(t, 0,  t->n1, false); goto success; /* relative cursor down, beginning of line */
		case 'F': terminal_at_xy(t, 0, -t->n1, false); goto success; /* relative cursor up, beginning of line */
		case 'G': terminal_at_xy(t, t->n1, terminal_y_current(t), true); goto success; /* move the cursor to column n */
		case 'm': /* set attribute, CSI number m */
			terminal_parse_attribute(&t->attribute, t->n1);
			t->attributes[t->cursor] = t->attribute;
			goto success;
		case 'i': /* AUX Port On == 5, AUX Port Off == 4 */
			if(t->n1 == 5 || t->n1 == 4)
				goto success;
			goto fail;
		case 'n': /* Device Status Report */
			/** @note This should transmit to the H2 system the
			 * following "ESC[n;mR", where n is the row and m is the column,
			 * we're not going to do this, although fifo_push() on
			 * uart_rx_fifo could be called to do this */
			if(t->n1 == 6)
				goto success;
			goto fail;
		case 'J': /* reset */
			switch(t->n1) {
			case 3: /* fall-through */
			case 2: t->cursor = 0; /* with cursor */ /* fall-through */
			case 1:
				if(t->command_index) {
					memset(t->m, ' ', t->size);
					terminal_attribute_block_set(t, t->size, &vt100_default_attribute);
					goto success;
				} /* fall through if number not supplied */ /* fall-through */
			case 0:
				memset(t->m, ' ', t->cursor);
				terminal_attribute_block_set(t, t->cursor, &vt100_default_attribute);
				goto success;
			}
			goto fail;
		case ';':
			t->command_index = 0;
			t->state = TERMINAL_NUMBER_2;
			break;
		default:
			goto fail;
		}
		break;
	case TERMINAL_NUMBER_2:
		if(isdigit(c)) {
			if(t->command_index > 3)
				goto fail;
			t->n2 = (t->n2 * (t->command_index ? 10 : 0)) + (c - '0');
			t->command_index++;
		} else {
			switch(c) {
			case 'm':
				terminal_parse_attribute(&t->attribute, t->n1);
				terminal_parse_attribute(&t->attribute, t->n2);
				t->attributes[t->cursor] = t->attribute;
				goto success;
			case 'H':
			case 'f':
				terminal_at_xy(t, MIN(t->n2-1,t->n2), MIN(t->n1-1,t->n1), true);
				goto success;
			}
			goto fail;
		}
		break;
	case TERMINAL_DECTCEM:
		if(isdigit(c)) {
			if(t->command_index > 1)
				goto fail;
			t->n1 = (t->n1 * (t->command_index ? 10 : 0)) + (c - '0');
			t->command_index++;
			break;
		}

		if(t->n1 != 25)
			goto fail;
		switch(c) {
		case 'l': t->cursor_on = false; goto success;
		case 'h': t->cursor_on = true;  goto success;
		default:
			goto fail;
		}
	case TERMINAL_STATE_END:
		t->state = TERMINAL_NORMAL_MODE;
		break;
	default:
		fatal("invalid terminal state: %u", (unsigned)t->state);
	}

	return 0;
success:
	t->state = TERMINAL_NORMAL_MODE;
	return 0;
fail:
	t->state = TERMINAL_NORMAL_MODE;
	return -1;
}

void vt100_update(vt100_t *t, uint8_t c)
{
	assert(t);
	assert(t->size <= VT100_MAX_SIZE);
	assert((t->width * t->height) <= VT100_MAX_SIZE);

	if(t->state != TERMINAL_NORMAL_MODE) {
		if(terminal_escape_sequences(t, c)) {
			t->state = TERMINAL_NORMAL_MODE;
			/*warning("invalid ANSI command sequence");*/
		}
	} else {
		switch(c) {
		case ESCAPE:
			t->state = TERMINAL_CSI;
			break;
		case '\t':
			t->cursor += 8;
			t->cursor &= ~0x7;
			break;
		case '\n':
			t->cursor += t->width;
			t->cursor = (t->cursor / t->width) * t->width;
			break;
		case '\r':
			break;
		case BACKSPACE:
			terminal_at_xy_relative(t, -1, 0, true);
			break;
		default:
			assert(t->cursor < t->size);
			t->m[t->cursor] = c;
			memcpy(&t->attributes[t->cursor], &t->attribute, sizeof(t->attribute));
			t->cursor++;
		}
		if(t->cursor >= t->size) {
			terminal_attribute_block_set(t, t->size, &vt100_default_attribute);
			memset(t->m, ' ', t->size);
		}
		t->cursor %= t->size;
	}
}

#define FLASH_WRITE_CYCLES (20)  /* x10ns */
#define FLASH_ERASE_CYCLES (200) /* x10ns */

typedef enum {
	FLASH_STATUS_RESERVED         = 1u << 0,
	FLASH_STATUS_BLOCK_LOCKED     = 1u << 1,
	FLASH_STATUS_PROGRAM_SUSPEND  = 1u << 2,
	FLASH_STATUS_VPP              = 1u << 3,
	FLASH_STATUS_PROGRAM          = 1u << 4,
	FLASH_STATUS_ERASE_BLANK      = 1u << 5,
	FLASH_STATUS_ERASE_SUSPEND    = 1u << 6,
	FLASH_STATUS_DEVICE_READY     = 1u << 7,
} flash_status_register_t;

typedef enum {
	FLASH_READ_ARRAY,
	FLASH_QUERY,
	FLASH_READ_DEVICE_IDENTIFIER,
	FLASH_READ_STATUS_REGISTER,
	FLASH_WORD_PROGRAM,
	FLASH_WORD_PROGRAMMING,
	FLASH_LOCK_OPERATION,
	FLASH_LOCK_OPERATING,
	FLASH_BLOCK_ERASE,
	FLASH_BLOCK_ERASING,
	FLASH_BUFFERED_PROGRAM,
	FLASH_BUFFERED_PROGRAMMING,
} flash_state_t;

/** @note read the PC28F128P33BF60 datasheet to decode this
 * information, this table was actually acquired from reading
 * the data from the actual device. */
static const uint16_t PC28F128P33BF60_CFI_Query_Table[0x200] = {
0x0089, 0x881E, 0x0000, 0xFFFF, 0x0089, 0xBFCF, 0x0000, 0xFFFF,
0x0089, 0x881E, 0x0000, 0x0000, 0x0089, 0xBFCF, 0x0000, 0xFFFF,
0x0051, 0x0052, 0x0059, 0x0001, 0x0000, 0x000A, 0x0001, 0x0000,
0x0000, 0x0000, 0x0000, 0x0023, 0x0036, 0x0085, 0x0095, 0x0006,
0x0009, 0x0009, 0x0000, 0x0002, 0x0002, 0x0003, 0x0000, 0x0018,
0x0001, 0x0000, 0x0009, 0x0000, 0x0002, 0x007E, 0x0000, 0x0000,
0x0002, 0x0003, 0x0000, 0x0080, 0x0000, 0x0000, 0x0000, 0x0000,
0x0000, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFBE, 0x0396, 0x66A2, 0xA600, 0x395A, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0x0050, 0x0052, 0x0049, 0x0031, 0x0035, 0x00E6,
0x0001, 0x0000, 0x0000, 0x0001, 0x0003, 0x0000, 0x0030, 0x0090,
0x0002, 0x0080, 0x0000, 0x0003, 0x0003, 0x0089, 0x0000, 0x0000,
0x0000, 0x0000, 0x0000, 0x0000, 0x0010, 0x0000, 0x0004, 0x0004,
0x0004, 0x0001, 0x0002, 0x0003, 0x0007, 0x0001, 0x0024, 0x0000,
0x0001, 0x0000, 0x0011, 0x0000, 0x0000, 0x0002, 0x007E, 0x0000,
0x0000, 0x0002, 0x0064, 0x0000, 0x0002, 0x0003, 0x0000, 0x0080,
0x0000, 0x0000, 0x0000, 0x0080, 0x0003, 0x0000, 0x0080, 0x0000,
0x0064, 0x0000, 0x0002, 0x0003, 0x0000, 0x0080, 0x0000, 0x0000,
0x0000, 0x0080, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xF020, 0x4DBF, 0x838C, 0xFC08, 0x638F, 0x20E3, 0xFF03, 0xD8D7,
0xC838, 0xFFFF, 0xFFFF, 0xAFFF, 0x3352, 0xB333, 0x3004, 0x1353,
0x0003, 0xA000, 0x80D5, 0x8A03, 0xFF4A, 0xFFFF, 0xFFFF, 0x0FFF,
0x2000, 0x0000, 0x0004, 0x0080, 0x1000, 0x0000, 0x0002, 0x0040,
0x0000, 0x0008, 0x0000, 0x0001, 0x2000, 0x0000, 0x0400, 0x0000,
0x0080, 0x0000, 0x0010, 0x0000, 0x0002, 0x4000, 0x0000, 0x0800,
0x0000, 0x0100, 0x0000, 0x0020, 0x0000, 0x0004, 0x8000, 0x0000,
0x1000, 0x0000, 0x0200, 0x0000, 0x0040, 0x0000, 0x0008, 0x0000,
0x0001, 0x2000, 0x0000, 0x0800, 0x0000, 0x0200, 0x0000, 0x0040,
0x0000, 0x0008, 0x0000, 0x0001, 0x2000, 0x0000, 0x0400, 0x0000,
0x0080, 0x0000, 0x0010, 0x0000, 0x0002, 0x4000, 0x0000, 0x0800,
0x0000, 0x0100, 0x0000, 0x0020, 0x0000, 0x0004, 0x8000, 0x0000,
0x1000, 0x0000, 0x0200, 0x0000, 0x0040, 0x0000, 0x0008, 0x0000,
0x0001, 0x4000, 0x0000, 0x1000, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF, 0xFFFF,
};

uint16_t PC28F128P33BF60_CFI_Query_Read(uint32_t addr)
{
	addr &= 0x3ff;
	if(addr > 0x1ff) {
		addr &= 0x7;
		static const uint16_t r[] = {
			0x0089, 0x881E, 0x0000, 0x0000,
			0x0089, 0xBFCF, 0x0000, 0xFFFF
		};
		return r[addr];
	}
	return PC28F128P33BF60_CFI_Query_Table[addr];
}

static uint16_t h2_io_flash_read(flash_t *f, uint32_t addr, bool oe, bool we, bool rst)
{
	if(rst)
		return 0;

	if(oe && we) {
		warning("OE and WE set at the same time");
		return 0;
	}

	if(!oe) {
		warning("flash read with OE not selected");
		return 0;
	}

	switch(f->mode) {
	case FLASH_READ_ARRAY:             return f->nvram[0x7ffffff & addr];
	case FLASH_READ_DEVICE_IDENTIFIER:
	case FLASH_QUERY:                  return PC28F128P33BF60_CFI_Query_Read(addr);
	case FLASH_READ_STATUS_REGISTER:   return f->status;
	case FLASH_WORD_PROGRAMMING:
	case FLASH_WORD_PROGRAM:           return f->status;
	case FLASH_BLOCK_ERASING:
	case FLASH_BLOCK_ERASE:            return f->status;
	case FLASH_LOCK_OPERATING:
	case FLASH_LOCK_OPERATION:         return f->status; /* return what? */
	default:
		fatal("invalid flash state: %u", f->mode);
	}

	return 0;
}

static unsigned addr_to_block(uint32_t addr)
{
	uint32_t lower_64k_blocks_highest_address = 127u * 64u * 1024u; /* 0x7F000 */
	/*assert(addr < 0x7ffffff);*/
	if(addr < lower_64k_blocks_highest_address)
		return addr / (64u * 1024u);
	addr -= lower_64k_blocks_highest_address;
	addr /= (16u * 1024u);
	return addr + 127u;
}

static unsigned block_size(unsigned block)
{
	if(block >= 127u)
		return 16u * 1024u;
	return 64u * 1024u;
}

static bool block_locked(flash_t *f, unsigned block)
{
	assert(f);
	assert(block < FLASH_BLOCK_MAX);
	/**@todo The locks block would probably be best be represented as a bit
	 * vector, the functions to manipulate a bit vector can quite easily be
	 * turned into a useful header only library */
	return !!(f->locks[block]);
}

static bool address_protected(flash_t *f, uint32_t addr)
{
	assert(f);
	return block_locked(f, addr_to_block(addr));
}

/**@todo implement the full standard for the Common Flash Memory Interface, and
 * make the timing based on a simulated calculated time instead multiples of
 * 10us see:
 * <https://en.wikipedia.org/wiki/Common_Flash_Memory_Interface> with the
 * devices PC28F128P33BF60 and NP8P128A13T1760E. The lock status of a register
 * should be read as well as checking f->arg1_address == f->arg2_address for
 * commands which require this.*/
static void h2_io_flash_update(flash_t *f, uint32_t addr, uint16_t data, bool oe, bool we, bool rst, bool cs)
{
	assert(f);
	if(oe && we)
		warning("OE and WE set at the same time");

	if(rst) {
		f->mode = FLASH_READ_ARRAY;
		return;
	}

	switch(f->mode) {
	case FLASH_READ_ARRAY:
	case FLASH_READ_STATUS_REGISTER:
	case FLASH_QUERY:
	case FLASH_READ_DEVICE_IDENTIFIER:
		f->arg1_address = addr;
		f->cycle = 0;
		f->status |= FLASH_STATUS_DEVICE_READY;

		if(!we && f->we && cs) {
			switch(f->data) {
			case 0x00: break;
			case 0xff: f->mode = FLASH_READ_ARRAY;             break;
			case 0x90: f->mode = FLASH_READ_DEVICE_IDENTIFIER; break;
			case 0x98: f->mode = FLASH_QUERY;                  break;
			case 0x70: f->mode = FLASH_READ_STATUS_REGISTER;   break;
			case 0x50: f->status = FLASH_STATUS_DEVICE_READY;  break; /* changes state? */
			case 0x10:
			case 0x40: f->mode = FLASH_WORD_PROGRAM;           break;
			case 0xE8: f->mode = FLASH_BUFFERED_PROGRAM;       break;
			case 0x20: f->mode = FLASH_BLOCK_ERASE;            break;
			/*case 0xB0: SUSPEND NOT IMPLEMENTED;              break; */
			/*case 0xD0: RESUME NOT IMPLEMENTED;               break; */
			case 0x60: f->mode = FLASH_LOCK_OPERATION;         break;
			/*case 0xC0: PROTECTION PROGRAM NOT IMPLEMENTED;     break; */
			default:
				warning("Common Flash Interface command not implemented: %x", (unsigned)(f->data));
				f->mode = FLASH_READ_ARRAY;
			}
		}
		break;
	case FLASH_WORD_PROGRAM:
		if(!we && f->we && cs) {
			f->cycle   = 0;
			if(address_protected(f, f->arg1_address)) {
				warning("address locked: %u", (unsigned)f->arg1_address);
				f->status |= FLASH_STATUS_BLOCK_LOCKED;
				f->status |= FLASH_STATUS_PROGRAM;
				f->mode    = FLASH_READ_STATUS_REGISTER;
			} else {
				f->status &= ~FLASH_STATUS_DEVICE_READY;
				f->mode    = FLASH_WORD_PROGRAMMING;
			}
		} else if(we && cs) {
			f->arg2_address = addr;
		}
		break;
	case FLASH_WORD_PROGRAMMING:
		if(f->cycle++ > FLASH_WRITE_CYCLES) {
			f->nvram[f->arg1_address] &= f->data;
			f->mode         = FLASH_READ_STATUS_REGISTER;
			f->cycle        = 0;
			f->status |= FLASH_STATUS_DEVICE_READY;
		}
		break;
	case FLASH_LOCK_OPERATION:
		if(!we && f->we && cs) {
			f->mode = FLASH_LOCK_OPERATING;
		} else if(we && cs) {
			f->arg2_address = addr;
		}
		break;
	case FLASH_LOCK_OPERATING:
		if(f->arg1_address > FLASH_BLOCK_MAX) {
			warning("block address invalid: %u", (unsigned)f->arg1_address);
			f->mode = FLASH_READ_STATUS_REGISTER;
			break;
		}

		switch(f->data) {
		case 0xD0:
			if(f->locks[f->arg1_address] != FLASH_LOCKED_DOWN)
				f->locks[f->arg1_address] = FLASH_UNLOCKED;
			else
				warning("block locked down: %u", (unsigned)f->arg1_address);
			break;
		case 0x01:
			if(f->locks[f->arg1_address] != FLASH_LOCKED_DOWN)
				f->locks[f->arg1_address] = FLASH_LOCKED;
			else
				warning("block locked down: %u", (unsigned)f->arg1_address);
			break;
		case 0x2F:
			f->locks[f->arg1_address] = FLASH_LOCKED_DOWN;
			break;
		default:
			warning("Unknown/Unimplemented Common Flash Interface Lock Operation: %x", (unsigned)(f->data));
		}
		f->mode = FLASH_READ_STATUS_REGISTER;
		break;
	case FLASH_BLOCK_ERASE:
		/*f->status &= ~FLASH_STATUS_DEVICE_READY;*/
		if(!we && f->we && cs) {
			if(addr != f->arg1_address)
				warning("block addresses differ: 1(%u) 2(%u)", f->arg1_address, addr);
			if(f->data != 0xD0) /* erase confirm */
				f->mode = FLASH_READ_STATUS_REGISTER;
			else
				f->mode = FLASH_BLOCK_ERASING;

			if(f->mode == FLASH_BLOCK_ERASING && address_protected(f, f->arg1_address)) {
				warning("address locked: %u", (unsigned)f->arg1_address);
				f->status |= FLASH_STATUS_BLOCK_LOCKED;
				f->status |= FLASH_STATUS_ERASE_BLANK;
				f->mode    = FLASH_READ_STATUS_REGISTER;
			}
		} else if(we && cs) {
			f->arg2_address = addr;
		}
		f->cycle = 0;
		break;
	case FLASH_BLOCK_ERASING:
		f->status &= ~FLASH_STATUS_DEVICE_READY;
		if(f->cycle++ > FLASH_ERASE_CYCLES) {
			const unsigned block = f->arg1_address;
			const unsigned size  = block_size(block);
			if(block >= FLASH_BLOCK_MAX) {
				warning("block operation out of range: %u", block);
				f->status |= FLASH_STATUS_ERASE_BLANK;
			} else {
				memset(f->nvram+block*size, 0xff, sizeof(f->nvram[0])*size);
			}
			f->cycle = 0;
			f->mode = FLASH_READ_STATUS_REGISTER;
			f->status |= FLASH_STATUS_DEVICE_READY;
		}
		break;
	case FLASH_BUFFERED_PROGRAM:
	case FLASH_BUFFERED_PROGRAMMING:
		warning("block programming not implemented");
		f->status |= FLASH_STATUS_PROGRAM;
		f->mode = FLASH_READ_STATUS_REGISTER;
		break;
	default:
		fatal("invalid flash state: %u", f->mode);
		return;
	}
	if(we && !oe)
		f->data = data;
	f->we = we;
	f->cs = cs;
}

uint16_t h2_io_memory_read_operation(h2_soc_state_t *soc)
{
	assert(soc);
	const uint32_t flash_addr = ((uint32_t)(soc->mem_control & FLASH_MASK_ADDR_UPPER_MASK) << 16) | soc->mem_addr_low;
	const bool flash_rst = soc->mem_control & FLASH_MEMORY_RESET;
	const bool flash_cs  = soc->mem_control & FLASH_CHIP_SELECT;
	const bool sram_cs   = soc->mem_control & SRAM_CHIP_SELECT;
	const bool oe        = soc->mem_control & FLASH_MEMORY_OE;
	const bool we        = soc->mem_control & FLASH_MEMORY_WE;

	if(oe && we)
		return 0;

	if(flash_cs && sram_cs)
		warning("SRAM and Flash Chip selects both high");

	if(flash_cs)
		return h2_io_flash_read(&soc->flash, flash_addr >> 1, oe, we, flash_rst);

	if(sram_cs && oe && !we)
		return soc->vram[flash_addr >> 1];
	return 0;
}

static uint16_t h2_io_get_default(h2_soc_state_t *soc, uint16_t addr, bool *debug_on)
{
	assert(soc);
	debug("IO read addr: %"PRIx16, addr);
	(void)debug_on;
	switch(addr) {
	case iUart:         return UART_TX_FIFO_EMPTY | soc->uart_getchar_register;
	case iVT100:        return UART_TX_FIFO_EMPTY | soc->ps2_getchar_register;
	case iSwitches:     return soc->switches;
	case iTimerDin:     return soc->timer;
	case iMemDin:       return h2_io_memory_read_operation(soc);
	default:
		warning("invalid read from %04"PRIx16, addr);
	}
	return 0;
}

static void h2_io_set_default(h2_soc_state_t *soc, uint16_t addr, uint16_t value, bool *debug_on)
{
	assert(soc);
	debug("IO write addr/value: %"PRIx16"/%"PRIx16, addr, value);

	switch(addr) {
	case oUart:
			if(value & UART_TX_WE)
				putch(0xFF & value);
			if(value & UART_RX_RE)
				soc->uart_getchar_register = wrap_getch(debug_on);
			break;
	case oLeds:       soc->leds           = value; break;
	case oTimerCtrl:  soc->timer_control  = value; break;
	case oVT100:
		if(value & UART_TX_WE)
			vt100_update(&soc->vt100, value);
		if(value & UART_RX_RE)
			soc->ps2_getchar_register = wrap_getch(debug_on);
		break;
	case o7SegLED:    soc->led_7_segments = value; break;
	case oIrcMask:    soc->irc_mask       = value; break;
	case oMemControl:
	{
		soc->mem_control   = value;
		const bool sram_cs = soc->mem_control & SRAM_CHIP_SELECT;
		const bool oe      = soc->mem_control & FLASH_MEMORY_OE;
		const bool we      = soc->mem_control & FLASH_MEMORY_WE;

		if(sram_cs && !oe && we)
			soc->vram[(((uint32_t)(soc->mem_control & FLASH_MASK_ADDR_UPPER_MASK) << 16) | soc->mem_addr_low) >> 1] = soc->mem_dout;
		break;
	}
	case oMemAddrLow: soc->mem_addr_low   = value; break;
	case oMemDout:    soc->mem_dout       = value; break;
	default:
		warning("invalid write to %04"PRIx16 ":%04"PRIx16, addr, value);
	}
}

static void h2_io_update_default(h2_soc_state_t *soc)
{
	assert(soc);

	if(soc->timer_control & TIMER_ENABLE) {
		if(soc->timer_control & TIMER_RESET) {
			soc->timer = 0;
			soc->timer_control &= ~TIMER_RESET;
		} else {
			soc->timer++;
			if((soc->timer > (soc->timer_control & 0x1FFF))) {
				if(soc->timer_control & TIMER_INTERRUPT_ENABLE) {
					soc->interrupt           = soc->irc_mask & (1 << isrTimer);
					soc->interrupt_selector |= soc->irc_mask & (1 << isrTimer);
				}
				soc->timer = 0;
			}
		}
	}

	{ /* DPAD interrupt on change state */
		const uint16_t prev = soc->switches_previous;
		const uint16_t cur  = soc->switches;
		if((prev & 0xff00) != (cur & 0xff00)) {
			soc->interrupt           = soc->irc_mask & (1 << isrDPadButton);
			soc->interrupt_selector |= soc->irc_mask & (1 << isrDPadButton);
		}
		soc->switches_previous = soc->switches;
	}

	{
		const uint32_t flash_addr = ((uint32_t)(soc->mem_control & FLASH_MASK_ADDR_UPPER_MASK) << 16) | soc->mem_addr_low;
		const bool flash_rst = soc->mem_control & FLASH_MEMORY_RESET;
		const bool flash_cs  = soc->mem_control & FLASH_CHIP_SELECT;
		const bool oe        = soc->mem_control & FLASH_MEMORY_OE;
		const bool we        = soc->mem_control & FLASH_MEMORY_WE;
		h2_io_flash_update(&soc->flash, flash_addr >> 1, soc->mem_dout, oe, we, flash_rst, flash_cs);
	}
}

h2_soc_state_t *h2_soc_state_new(void)
{
	h2_soc_state_t *r = allocate_or_die(sizeof(h2_soc_state_t));
	vt100_t *v = &r->vt100;
	memset(r->flash.nvram, 0xff, sizeof(r->flash.nvram[0])*FLASH_BLOCK_MAX);
	memset(r->flash.locks, FLASH_LOCKED, FLASH_BLOCK_MAX);

	v->width        = VGA_WIDTH;
	v->height       = VGA_HEIGHT;
	v->size         = VGA_WIDTH * VGA_HEIGHT;
	v->state        = TERMINAL_NORMAL_MODE;
	v->cursor_on    = true;
	v->blinks       = false;
	v->n1           = 1;
	v->n2           = 1;
	v->attribute.foreground_color = WHITE;
	v->attribute.background_color = BLACK;
	for(size_t i = 0; i < v->size; i++)
		v->attributes[i] = v->attribute;
	return r;
}

void h2_soc_state_free(h2_soc_state_t *soc)
{
	if(!soc)
		return;
	memset(soc, 0, sizeof(*soc));
	free(soc);
}

h2_io_t *h2_io_new(void)
{
	h2_io_t *io = allocate_or_die(sizeof(*io));
	io->in      = h2_io_get_default;
	io->out     = h2_io_set_default;
	io->update  = h2_io_update_default;
	io->soc     = h2_soc_state_new();
	return io;
}

void h2_io_free(h2_io_t *io)
{
	if(!io)
		return;
	h2_soc_state_free(io->soc);
	memset(io, 0, sizeof(*io));
	free(io);
}

static void dpush(h2_t *h, uint16_t v)
{
	assert(h);
	h->sp++;
	h->dstk[h->sp % STK_SIZE] = h->tos;
	h->tos = v;
	if(h->sp >= STK_SIZE)
		warning("data stack overflow");
	h->sp %= STK_SIZE;
}

static uint16_t dpop(h2_t *h)
{
	assert(h);
	const uint16_t r = h->tos;
	h->tos = h->dstk[h->sp % STK_SIZE];
	h->sp--;
	if(h->sp >= STK_SIZE)
		warning("data stack underflow");
	h->sp %= STK_SIZE;
	return r;
}

static void rpush(h2_t *h, uint16_t r)
{
	assert(h);
	h->rp++;
	h->rstk[(h->rp) % STK_SIZE] = r;
	if(h->rp >= STK_SIZE)
		warning("return stack overflow");
	h->rp %= STK_SIZE;
}

static uint16_t stack_delta(uint16_t d)
{
	static const uint16_t i[4] = { 0x0000, 0x0001, 0xFFFE, 0xFFFF };
	assert((d & 0xFFFC) == 0);
	return i[d];
}

static inline void reverse(char *r, size_t length)
{
	const size_t last = length - 1;
	for(size_t i = 0; i < length/2; i++) {
		const size_t t = r[i];
		r[i] = r[last - i];
		r[last - i] = t;
	}
}

static inline void unsigned_to_csv(char b[64], unsigned u, char delimiter)
{
	unsigned i = 0;
	do {
		const unsigned base = 10; /* bases 2-10 allowed */
		const unsigned q = u % base;
		const unsigned r = u / base;
		b[i++] = q + '0';
		u = r;
	} while(u);
	b[i] = delimiter;
	b[i+1] = '\0';
	reverse(b, i);
}

static inline void csv_value(FILE *o, unsigned u)
{
	char b[64] = { 0 };
	unsigned_to_csv(b, u, ',');
	fputs(b, o);
}

/* This is a fairly fast trace/CSV generation routine which avoids the use of fprintf,
 * speeding up this routine would greatly improve the speed of the interpreter when
 * tracing is on. The symbol table lookup can be disabled by passing in NULL, this
 * also greatly speeds things up. It can be used in a roundabout way to generate
 * a file viewable by GTKWave, its output can be feed into 'csv2vcd' after some
 * minimal processing with AWK, turning it into a VCD file, which is viewable in
 * the wave form viewer, see:
 * <http://www.ic.unicamp.br/~ducatte/mc542/Docs/gtkwave.pdf> and
 * <https://github.com/carlos-jenkins/csv2vcd> for more details.  */
static int h2_log_csv(FILE *o, h2_t *h, symbol_table_t *symbols, bool header)
{
	if(!o)
		return 0;
	assert(h);
	if(header) {
		fputs("\"pc[15:0]\",", o);
		fputs("\"tos[15:0]\",", o);
		fputs("\"rp[7:0]\",", o);
		fputs("\"sp[7:0]\",", o);
		fputs("\"ie\",", o);
		fputs("\"instruction[15:0]\",", o);
		if(symbols)
			fputs("\"disassembled\",", o);
		fputs("\"Time\"", o);
		if(fputc('\n', o) != '\n')
			return -1;
		return 0;
	}

	csv_value(o, h->pc);
	csv_value(o, h->tos);
	csv_value(o, h->rp);
	csv_value(o, h->sp);
	csv_value(o, h->ie);
	csv_value(o, h->core[h->pc]);
	if(symbols) {
		fputc('"', o);
		disassemble_instruction(h->core[h->pc], o, symbols, DCM_NONE);
		fputs("\",", o);
	}
	csv_value(o, h->time*10);

	if(fputc('\n', o) != '\n')
		return -1;
	return 0;
}

typedef struct {
	FILE *input;
	FILE *output;
	bool step;
	bool trace_on;
} debug_state_t;

static const char *debug_prompt = "debug> ";

static int number(char *s, uint16_t *o, size_t length);

static void h2_print(FILE *out, h2_t *h)
{
	fputs("Return Stack:\n", out);
	memory_print(out, 0, h->rstk, STK_SIZE, false);
	fputs("Variable Stack:\n", out);
	fprintf(out, "tos:  %04"PRIx16"\n", h->tos);
	memory_print(out, 1, h->dstk, STK_SIZE, false);

	fprintf(out, "pc:   %04"PRIx16"\n", h->pc);
	fprintf(out, "rp:   %04"PRIx16" (max %04"PRIx16")\n", h->rp, h->rpm);
	fprintf(out, "dp:   %04"PRIx16" (max %04"PRIx16")\n", h->sp, h->spm);
	fprintf(out, "ie:   %s\n", h->ie ? "true" : "false");
}

typedef enum {
	DBG_CMD_NO_ARG,
	DBG_CMD_NUMBER,
	DBG_CMD_STRING,
	DBG_CMD_EITHER,
} debug_command_type_e;

typedef struct
{
	int cmd;
	int argc;
	debug_command_type_e arg1;
	debug_command_type_e arg2;
	char *description;
} debug_command_t;

static const debug_command_t debug_commands[] = {
	{ .cmd = 'a', .argc = 1, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NO_ARG, .description = "assemble               " },
	{ .cmd = 'b', .argc = 1, .arg1 = DBG_CMD_EITHER, .arg2 = DBG_CMD_NO_ARG, .description = "set break point        " },
	{ .cmd = 'c', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "continue               " },
	{ .cmd = 'd', .argc = 2, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NUMBER, .description = "dump                   " },
	{ .cmd = 'f', .argc = 1, .arg1 = DBG_CMD_EITHER, .arg2 = DBG_CMD_NO_ARG, .description = "save to file           " },
	{ .cmd = 'g', .argc = 1, .arg1 = DBG_CMD_EITHER, .arg2 = DBG_CMD_NO_ARG, .description = "goto address           " },
	{ .cmd = 'h', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "help                   " },
	{ .cmd = 'i', .argc = 1, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NO_ARG, .description = "input (port)           " },
	{ .cmd = 'k', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "list all breakpoints   " },
	{ .cmd = 'l', .argc = 1, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NO_ARG, .description = "set debug level        " },
	{ .cmd = 'o', .argc = 2, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NUMBER, .description = "output (port value)    " },
	{ .cmd = 'p', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "print IO state         " },
	{ .cmd = 'q', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "quit                   " },
	{ .cmd = 'r', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "remove all break points" },
	{ .cmd = 's', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "step                   " },
	{ .cmd = 't', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "toggle tracing         " },
	{ .cmd = 'u', .argc = 2, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NUMBER, .description = "unassemble             " },
	{ .cmd = 'y', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "list symbols           " },
	{ .cmd = 'v', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "print VGA display      " },
	{ .cmd = 'P', .argc = 1, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "push value             " },
	{ .cmd = 'D', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "pop value              " },
	{ .cmd = 'G', .argc = 1, .arg1 = DBG_CMD_EITHER, .arg2 = DBG_CMD_NO_ARG, .description = "call function/location " },
	{ .cmd = '!', .argc = 2, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NUMBER, .description = "set value              " },
	{ .cmd = '.', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "print H2 CPU state     " },
	{ .cmd = -1,  .argc = 0, .arg1 = DBG_CMD_EITHER, .arg2 = DBG_CMD_NO_ARG, .description = NULL },
};

static void debug_command_print_help(FILE *out, const debug_command_t *dc)
{
	assert(out);
	assert(dc);

	static const char *debug_help = "\
Debugger Help: \n\n\
Hit 'Escape' when the simulation is and is reading from input to exit \n\
into the back debugger.\n\n\
Command list:\n\n";

	static const char *arg_type[] = {
		[DBG_CMD_NO_ARG] = "             ",
		[DBG_CMD_NUMBER] = "number       ",
		[DBG_CMD_STRING] = "string       ",
		[DBG_CMD_EITHER] = "number/string"
	};
	fputs(debug_help, out);
	for(unsigned i = 0; dc[i].cmd != -1; i++)
		fprintf(out, " %c %s\t%d\t%s %s\n", dc[i].cmd, dc[i].description, dc[i].argc, arg_type[dc[i].arg1], arg_type[dc[i].arg2]);
}

static int debug_command_check(FILE *out, const debug_command_t *dc, int cmd, int argc, bool is_numeric1, bool is_numeric2)
{
	assert(out);
	assert(dc);
	for(unsigned i = 0; dc[i].cmd != -1 ; i++) {
		if(dc[i].cmd == cmd) {
			if(dc[i].argc != argc) {
				fprintf(out, "command '%c' expects %d arguments, got %d\n", cmd, dc[i].argc, argc);
				return -1;
			}

			if(dc[i].argc == 0)
				return 0;

			if(dc[i].arg1 == DBG_CMD_NUMBER && !is_numeric1) {
				fprintf(out, "command '%c' expects arguments one to be numeric\n", cmd);
				return -1;
			}

			if(dc[i].argc == 1)
				return 0;

			if(dc[i].arg2 == DBG_CMD_NUMBER && !is_numeric2) {
				fprintf(out, "command '%c' expects arguments two to be numeric\n", cmd);
				return -1;
			}

			return 0;
		}
	}
	fprintf(out, "unrecognized command '%c'\n", cmd);
	return -1;
}

static int debug_resolve_symbol(FILE *out, char *symbol, symbol_table_t *symbols, uint16_t *value)
{
	assert(out);
	assert(symbol);
	assert(symbols);
	assert(value);
	symbol_t *sym = NULL;
	*value = 0;
	if(!(sym = symbol_table_lookup(symbols, symbol))) {
		fprintf(out, "symbol '%s' not found\n", symbol);
		return -1;
	}
	if(sym->type != SYMBOL_TYPE_LABEL && sym->type != SYMBOL_TYPE_CALL) {
		fprintf(out, "symbol is not call or label\n");
		return -1;
	}
	*value = sym->value;
	return 0;
}

static int h2_debugger(debug_state_t *ds, h2_t *h, h2_io_t *io, symbol_table_t *symbols, uint16_t point)
{
	bool breaks = false;
	assert(h);
	assert(ds);

	breaks = break_point_find(&h->bp, point);
	if(breaks)
		fprintf(ds->output, "\n === BREAK(0x%04"PRIx16") ===\n", h->pc);

	if(ds->step || breaks) {
		char line[256];
		char op[256], arg1[256], arg2[256];
		int argc;
		bool is_numeric1, is_numeric2;
		uint16_t num1, num2;

		ds->step = true;

again:
		memset(line, 0, sizeof(line));
		memset(op,   0, sizeof(op));
		memset(arg1, 0, sizeof(arg1));
		memset(arg2, 0, sizeof(arg2));

		fputs(debug_prompt, ds->output);
		if(!fgets(line, sizeof(line), ds->input)) {
			fputs("End Of Input - exiting\n", ds->output);
			return -1;
		}

		argc = sscanf(line, "%255s %255s %255s", op, arg1, arg2);
		if(argc < 1)
			goto again;

		is_numeric1 = number(arg1, &num1, strlen(arg1));
		is_numeric2 = number(arg2, &num2, strlen(arg2));

		if(!(strlen(op) == 1)) {
			fprintf(ds->output, "invalid command '%s'\n", op);
			goto again;
		}

		if(debug_command_check(ds->output, debug_commands, op[0], argc-1, is_numeric1, is_numeric2) < 0)
			goto again;

		switch(op[0]) {
		case ' ':
		case '\t':
		case '\r':
		case '\n':
			break;
		case 'a':
			fprintf(ds->output, "command '%c' not implemented yet!\n", op[0]);
			break;
		case 'f':
		{
			FILE *o = fopen(arg1, "wb");
			if(!o) {
				fprintf(ds->output, "could not open file '%s 'for writing: %s", arg1, strerror(errno));
				break;
			}
			h2_save(h, o, true);
			fclose(o);
			break;
		}
		case 'd':
			if(((long)num1 + (long)num2) > MAX_CORE)
				fprintf(ds->output, "overflow in RAM dump\n");
			else
				memory_print(ds->output, num1, h->core + num1, num2, true);
			break;
		case 'l':
			if(!is_numeric1) {
				fprintf(ds->output, "set log level expects one numeric argument\n");
				break;
			}
			log_level = num1;
			break;
		case 'b':
			if(!is_numeric1) {
				if(debug_resolve_symbol(ds->output, arg1, symbols, &num1))
					break;
			}
			break_point_add(&h->bp, num1);
			break;

		case 'g':
			if(!is_numeric1) {
				if(debug_resolve_symbol(ds->output, arg1, symbols, &num1))
					break;
			}
			h->pc = num1;
			break;
		case 'G':
			if(!is_numeric1) {
				if(debug_resolve_symbol(ds->output, arg1, symbols, &num1))
					break;
			}
			rpush(h, h->pc);
			h->pc = num1;
			break;
		case '.':
			h2_print(ds->output, h);
			break;

		case '!':
			if(num1 >= MAX_CORE) {
				fprintf(ds->output, "invalid write\n");
				break;
			}
			h->core[num1] = num2;
			break;
		case 'P':
			dpush(h, num1);
			break;
		case 'D':
			fprintf(ds->output, "popped: %04u\n", (unsigned)dpop(h));
			break;

		case 'r':
			free(h->bp.points);
			h->bp.points = NULL;
			h->bp.length = 0;
			break;
		case 'u':
			if(num2 >= MAX_CORE || num1 > num2) {
				fprintf(ds->output, "invalid range\n");
				break;
			}
			for(uint16_t i = num1; i < num2; i++) {
				fprintf(ds->output, "%04"PRIx16 ":\t", i);
				disassemble_instruction(h->core[i], ds->output, symbols, DCM_NONE);
				fputc('\n', ds->output);
			}
			break;

		case 'o':
			if(!io) {
				fprintf(ds->output, "I/O unavailable\n");
				break;
			}
			io->out(io->soc, num1, num2, NULL);

			break;

		case 'i':
			if(!io) {
				fprintf(ds->output, "I/O unavailable\n");
				break;
			}
			fprintf(ds->output, "read: %"PRIx16"\n", io->in(io->soc, num1, NULL));
			break;

		case 'k':
			break_point_print(ds->output, &h->bp);
			break;
		case 'h':
			debug_command_print_help(ds->output, debug_commands);
			break;
		case 's':
			return 0;
		case 'c':
			ds->step = false;
			return 0;
		case 't':
			ds->trace_on = !ds->trace_on;
			fprintf(ds->output, "trace %s\n", ds->trace_on ? "on" : "off");
			break;
		case 'y':
			if(symbols)
				symbol_table_print(symbols, ds->output);
			else
				fprintf(ds->output, "symbol table unavailable\n");
			break;
		case 'v':
			if(!io) {
				fprintf(ds->output, "I/O unavailable\n");
				break;
			}
			for(size_t i = 0; i < VGA_HEIGHT; i++) {
				for(size_t j = 0; j < VGA_WIDTH; j++) {
					unsigned char c = io->soc->vt100.m[i*VGA_WIDTH + j];
					fputc(c < 32 || c > 127 ? '?' : c, ds->output);
				}
				fputc('\n', ds->output);
			}

			break;
		case 'p':
			if(io)
				soc_print(ds->output, io->soc);
			else
				fprintf(ds->output, "I/O unavailable\n");
			break;
		case 'q':
			fprintf(ds->output, "Quiting simulator\n");
			return -1;
		default:
			fprintf(ds->output, "unknown command '%c'\n", op[0]);
		}
		goto again;
	}
	return 0;
}

static uint16_t interrupt_decode(uint8_t *vector)
{
	for(unsigned i = 0; i < NUMBER_OF_INTERRUPTS; i++)
		if(*vector & (1 << i)) {
			*vector ^= 1 << i;
			return i;
		}
	return 0;
}

int h2_run(h2_t *h, h2_io_t *io, FILE *output, unsigned steps, symbol_table_t *symbols, bool run_debugger, FILE *trace)
{
	bool turn_debug_on = false;
	assert(h);
	debug_state_t ds = { .input = stdin, .output = stderr, .step = run_debugger, .trace_on = false /*run_debugger*/ };

	if(trace)
		h2_log_csv(trace, h, NULL, true);

	if(run_debugger)
		fputs("Debugger running, type 'h' for a list of command\n", ds.output);

	for(unsigned i = 0; i < steps || steps == 0 || run_debugger; i++) {
		uint16_t instruction,
			 literal,
			 address,
			 pc_plus_one;
		if(log_level >= LOG_DEBUG || ds.trace_on)
		       h2_log_csv(output, h, symbols, false);
		if(trace)
		       h2_log_csv(trace, h, NULL, false);

		if(run_debugger)
			if(h2_debugger(&ds, h, io, symbols, h->pc))
				return 0;

		h->time++;

		if(io) {
			io->update(io->soc);
			if(io->soc->wait)
				continue; /* wait only applies to the H2 core not the rest of the SoC */
		}

		if(h->pc >= MAX_CORE) {
			error("invalid program counter: %04x > %04x", (unsigned)h->pc, MAX_CORE);
			return -1;
		}
		instruction = h->core[h->pc];

		literal = instruction & 0x7FFF;
		address = instruction & 0x1FFF; /* NB. also used for ALU OP */

		if(h->ie && io && io->soc->interrupt) {
			rpush(h, h->pc << 1);
			io->soc->interrupt = false;
			h->pc = interrupt_decode(&io->soc->interrupt_selector);
			continue;
		}

		pc_plus_one = (h->pc + 1) % MAX_CORE;

		/* decode / execute */
		if(IS_LITERAL(instruction)) {
			dpush(h, literal);
			h->pc = pc_plus_one;
		} else if (IS_ALU_OP(instruction)) {
			const uint16_t rd  = stack_delta(RSTACK(instruction));
			const uint16_t dd  = stack_delta(DSTACK(instruction));
			uint16_t nos = h->dstk[h->sp % STK_SIZE];
			uint16_t tos = h->tos;
			uint16_t npc = pc_plus_one;

			if(instruction & R_TO_PC)
				npc = h->rstk[h->rp % STK_SIZE] >> 1;

			switch(ALU_OP(instruction)) {
			case ALU_OP_T:        /* tos = tos; */ break;
			case ALU_OP_N:           tos = nos;    break;
			case ALU_OP_T_PLUS_N:    tos += nos;   break;
			case ALU_OP_T_AND_N:     tos &= nos;   break;
			case ALU_OP_T_OR_N:      tos |= nos;   break;
			case ALU_OP_T_XOR_N:     tos ^= nos;   break;
			case ALU_OP_T_INVERT:    tos = ~tos;   break;
			case ALU_OP_T_EQUAL_N:   tos = -(tos == nos); break;
			case ALU_OP_N_LESS_T:    tos = -((int16_t)nos < (int16_t)tos); break;
			case ALU_OP_N_RSHIFT_T:  tos = nos >> tos; break;
			case ALU_OP_T_DECREMENT: tos--; break;
			case ALU_OP_R:           tos = h->rstk[h->rp % STK_SIZE]; break;
			case ALU_OP_T_LOAD:
				if(h->tos & 0x4000) {
					if(io) {
						if(h->tos & 0x1)
							warning("unaligned register read: %04x", (unsigned)h->tos);
						tos = io->in(io->soc, h->tos & ~0x1, &turn_debug_on);
						if(turn_debug_on) {
							ds.step = true;
							run_debugger = true;
							turn_debug_on = false;
						}
					} else {
						warning("I/O read attempted on addr: %"PRIx16, h->tos);
					}
				} else {
					tos = h->core[(h->tos >> 1) % MAX_CORE];
				}
				break;
			case ALU_OP_N_LSHIFT_T: tos = nos << tos;           break;
			case ALU_OP_DEPTH:      tos = h->sp;                break;
			case ALU_OP_N_ULESS_T:  tos = -(nos < tos);         break;
			case ALU_OP_ENABLE_INTERRUPTS: h->ie = tos & 1; /*tos = nos;*/ break;
			case ALU_OP_INTERRUPTS_ENABLED: tos = -h->ie;       break;
			case ALU_OP_RDEPTH:     tos = h->rp;                break;
			case ALU_OP_T_EQUAL_0:  tos = -(tos == 0);          break;
			case ALU_OP_CPU_ID:     tos = H2_CPU_ID_SIMULATION; break;
			default:
				warning("unknown ALU operation: %u", (unsigned)ALU_OP(instruction));
			}

			h->sp += dd;
			if(h->sp >= STK_SIZE)
				warning("data stack overflow");
			h->sp %= STK_SIZE;

			h->rp += rd;
			if(h->rp >= STK_SIZE)
				warning("return stack overflow");
			h->rp %= STK_SIZE;

			if(instruction & T_TO_R)
				h->rstk[h->rp % STK_SIZE] = h->tos;

			if(instruction & T_TO_N)
				h->dstk[h->sp % STK_SIZE] = h->tos;

			if(instruction & N_TO_ADDR_T) {
				if((h->tos & 0x4000) && ALU_OP(instruction) != ALU_OP_T_LOAD) {
					if(io) {
						if(h->tos & 0x1)
							warning("unaligned register write: %04x <- %04x", (unsigned)h->tos, (unsigned)nos);
						io->out(io->soc, h->tos & ~0x1, nos, &turn_debug_on);
						if(turn_debug_on) {
							ds.step = true;
							run_debugger = true;
							turn_debug_on = false;
						}
					} else {
						warning("I/O write attempted with addr/value: %"PRIx16 "/%"PRIx16, tos, nos);
					}
				} else {
					h->core[(h->tos >> 1) % MAX_CORE] = nos;
				}
			}

			h->tos = tos;
			h->pc = npc;
		} else if (IS_CALL(instruction)) {
			rpush(h, pc_plus_one << 1);
			h->pc = address;
		} else if (IS_0BRANCH(instruction)) {
			if(!dpop(h))
				h->pc = address % MAX_CORE;
			else
				h->pc = pc_plus_one;
		} else if (IS_BRANCH(instruction)) {
			h->pc = address;
		} else {
			error("invalid instruction: %"PRId16, instruction);
		}

		h->rpm = MAX(h->rpm, h->rp);
		h->spm = MAX(h->spm, h->sp);
	}
	return 0;
}

/* ========================== Simulation And Debugger ====================== */

/* ========================== Assembler ==================================== */
/* This section is the most complex, it implements a lexer, parser and code
 * compiler for a simple pseudo Forth like language, whilst it looks like
 * Forth it is not Forth. */

#define MAX_ID_LENGTH (256u)

/**@warning The ordering of the following enumerations matters a lot */
typedef enum {
	LEX_LITERAL,
	LEX_IDENTIFIER,
	LEX_LABEL,
	LEX_STRING,

	LEX_CONSTANT, /* start of named tokens */
	LEX_CALL,
	LEX_BRANCH,
	LEX_0BRANCH,
	LEX_BEGIN,
	LEX_WHILE,
	LEX_REPEAT,
	LEX_AGAIN,
	LEX_UNTIL,
	LEX_FOR,
	LEX_AFT,
	LEX_NEXT,
	LEX_IF,
	LEX_ELSE,
	LEX_THEN,
	LEX_DEFINE,
	LEX_ENDDEFINE,
	LEX_CHAR,
	LEX_VARIABLE,
	LEX_LOCATION,
	LEX_IMMEDIATE,
	LEX_HIDDEN,
	LEX_INLINE,
	LEX_QUOTE,

	LEX_PWD,
	LEX_SET,
	LEX_PC,
	LEX_BREAK,
	LEX_MODE,
	LEX_ALLOCATE,
	LEX_BUILT_IN,

	/* start of instructions */
#define X(NAME, STRING, DEFINE, INSTRUCTION) LEX_ ## NAME,
	X_MACRO_INSTRUCTIONS
#undef X
	/* end of named tokens and instructions */

	LEX_ERROR, /* error token: this needs to be after the named tokens */

	LEX_EOI = EOF
} token_e;

static const char *keywords[] =
{
	[LEX_LITERAL]    = "literal",
	[LEX_IDENTIFIER] = "identifier",
	[LEX_LABEL]      = "label",
	[LEX_STRING]     = "string",
	[LEX_CONSTANT]   = "constant",
	[LEX_CALL]       = "call",
	[LEX_BRANCH]     = "branch",
	[LEX_0BRANCH]    = "0branch",
	[LEX_BEGIN]      = "begin",
	[LEX_WHILE]      = "while",
	[LEX_REPEAT]     = "repeat",
	[LEX_AGAIN]      = "again",
	[LEX_UNTIL]      = "until",
	[LEX_FOR]        = "for",
	[LEX_AFT]        = "aft",
	[LEX_NEXT]       = "next",
	[LEX_IF]         = "if",
	[LEX_ELSE]       = "else",
	[LEX_THEN]       = "then",
	[LEX_DEFINE]     = ":",
	[LEX_ENDDEFINE]  = ";",
	[LEX_CHAR]       = "[char]",
	[LEX_VARIABLE]   = "variable",
	[LEX_LOCATION]   = "location",
	[LEX_IMMEDIATE]  = "immediate",
	[LEX_HIDDEN]     = "hidden",
	[LEX_INLINE]     = "inline",
	[LEX_QUOTE]      = "'",
	[LEX_PWD]        = ".pwd",
	[LEX_SET]        = ".set",
	[LEX_PC]         = ".pc",
	[LEX_BREAK]      = ".break",
	[LEX_MODE]       = ".mode",
	[LEX_ALLOCATE]   = ".allocate",
	[LEX_BUILT_IN]   = ".built-in",

	/* start of instructions */
#define X(NAME, STRING, DEFINE, INSTRUCTION) [ LEX_ ## NAME ] = STRING,
	X_MACRO_INSTRUCTIONS
#undef X
	/* end of named tokens and instructions */

	[LEX_ERROR]      = NULL,
	NULL
};

typedef struct {
	union {
		char *id;
		uint16_t number;
	} p;
	unsigned location;
	unsigned line;
	token_e type;
} token_t;

typedef struct {
	error_t error;
	FILE *input;
	unsigned line;
	int c;
	char id[MAX_ID_LENGTH];
	token_t *token;
	token_t *accepted;
	bool in_definition;
} lexer_t;

/********* LEXER *********/

/**@note it would be possible to add a very small amount of state to the
 * lexer, so when keywords like 'hex' and 'decimal' are encountered, the
 * base is changed. */

static token_t *token_new(token_e type, unsigned line)
{
	token_t *r = allocate_or_die(sizeof(*r));
	r->type = type;
	r->line = line;
	return r;
}

void token_free(token_t *t)
{
	if(!t)
		return;
	if(t->type == LEX_IDENTIFIER || t->type == LEX_STRING || t->type == LEX_LABEL)
		free(t->p.id);
	memset(t, 0, sizeof(*t));
	free(t);
}

static int next_char(lexer_t *l)
{
	assert(l);
	return fgetc(l->input);
}

static int unget_char(lexer_t *l, int c)
{
	assert(l);
	return ungetc(c, l->input);
}

static lexer_t* lexer_new(FILE *input)
{
	lexer_t *l = allocate_or_die(sizeof(lexer_t));
	l->input = input;
	return l;
}

static void lexer_free(lexer_t *l)
{
	assert(l);
	token_free(l->token);
	memset(l, 0, sizeof(*l));
	free(l);
}

static int token_print(token_t *t, FILE *output, unsigned depth)
{
	if(!t)
		return 0;
	indent(output, ' ', depth);
	token_e type = t->type;
	int r = 0;
	if(type == LEX_LITERAL) {
		r = fprintf(output, "number: %"PRId16, t->p.number);
	} else if(type == LEX_LABEL) {
		r = fprintf(output, "label: %s", t->p.id);
	} else if(type == LEX_IDENTIFIER) {
		r = fprintf(output, "id: %s", t->p.id);
	} else if(type == LEX_ERROR) {
		r = fputs("error", output);
	} else if(type == LEX_EOI) {
		r = fputs("EOI", output);
	} else {
		r = fprintf(output, "keyword: %s", keywords[type]);
	}
	return r < 0 ? -1 : 0;
}

static int _syntax_error(lexer_t *l,
		const char *func, unsigned line, const char *fmt, ...)
{
	va_list ap;
	assert(l);
	assert(func);
	assert(fmt);
	fprintf(stderr, "%s:%u\n", func, line);
	fprintf(stderr, "  syntax error on line %u of input\n", l->line);
	va_start(ap, fmt);
	vfprintf(stderr, fmt, ap);
	va_end(ap);
	fputc('\n', stderr);
	token_print(l->token, stderr, 2);
	fputc('\n', stderr);
	ethrow(&l->error);
	return 0;
}

#define syntax_error(LEXER, ...) _syntax_error(LEXER, __func__, __LINE__, ## __VA_ARGS__)

static uint16_t map_char_to_number(int c)
{
	if(c >= '0' && c <= '9')
		return c - '0';
	c = tolower(c);
	if(c >= 'a' && c <= 'z')
		return c + 10 - 'a';
	fatal("invalid numeric character: %c", c);
	return 0;
}

static bool numeric(int c, int base)
{
	assert(base == 10 || base == 16);
	if(base == 10)
		return isdigit(c);
	return isxdigit(c);
}

static int number(char *s, uint16_t *o, size_t length)
{
	size_t i = 0, start = 0;
	uint32_t out = 0;
	int base = 10;
	bool negate = false;
	assert(o);
	if(s[i] == '\0')
		return 0;

	if(s[i] == '-') {
		if(s[i+1] == '\0')
			return 0;
		negate = true;
		start = ++i;
	}

	if(s[i] == '$') {
		base = 16;
		if(s[i+1] == '\0')
			return 0;
		start = i + 1;
	}

	for(i = start; i < length; i++)
		if(!numeric(s[i], base))
			return 0;

	for(i = start; i < length; i++)
		out = out * base + map_char_to_number(s[i]);

	*o = negate ? out * -1 : out;
	return 1;
}

static void lexer(lexer_t *l)
{
	assert(l);
	size_t i     = 0;
	token_e sym  = LEX_ERROR;
	uint16_t lit = 0;
	int ch       = next_char(l);
	l->token     = token_new(LEX_ERROR, l->line);
again:
	switch(ch) {
	case '\n':
		l->line++; /* fall-through */
	case ' ':
	case '\t':
	case '\r':
	case '\v':
		ch = next_char(l);
		goto again;
	case EOF:
		l->token->type = LEX_EOI;
		return;
	case '\\':
		for(; '\n' != (ch = next_char(l));)
			if(ch == EOF)
				syntax_error(l, "'\\' commented terminated by EOF");
		ch = next_char(l);
		l->line++;
		goto again;
	case '(':
		ch = next_char(l);
		if(!isspace(ch)) {
			unget_char(l, ch);
			ch = '(';
			goto graph;
		}
		for(; ')' != (ch = next_char(l));)
			if(ch == EOF)
				syntax_error(l, "'(' comment terminated by EOF");
			else if(ch == '\n')
				l->line++;
		ch = next_char(l);
		goto again;
	case '"':
		for(i = 0; '"' != (ch = next_char(l));) {
			if(ch == EOF)
				syntax_error(l, "string terminated by EOF");
			if(i >= MAX_ID_LENGTH - 1)
				syntax_error(l, "identifier too large: %s", l->id);
			l->id[i++] = ch;
		}
		l->id[i] = '\0';
		l->token->type = LEX_STRING;
		l->token->p.id = duplicate(l->id);
		ch = next_char(l);
		break;
	default:
		i = 0;
	graph:
		if(isgraph(ch)) {
			while(isgraph(ch)) {
				if(i >= MAX_ID_LENGTH - 1)
					syntax_error(l, "identifier too large: %s", l->id);
				l->id[i++] = ch;
				ch = next_char(l);
			}
			l->id[i] = '\0';
		} else {
			syntax_error(l, "invalid character: %c", ch);
		}

		if(number(l->id, &lit, i)) {
			l->token->type = LEX_LITERAL;
			l->token->p.number = lit;
			break;
		}

		for(sym = LEX_CONSTANT; sym != LEX_ERROR && keywords[sym] && strcmp(keywords[sym], l->id); sym++)
			/*do nothing*/;
		if(!keywords[sym]) {
			if(i > 1 && l->id[i - 1] == ':') {
				l->id[strlen(l->id) - 1] = '\0';
				l->token->type = LEX_LABEL;
			} else { /* IDENTIFIER */
				l->token->type = LEX_IDENTIFIER;
			}
			l->token->p.id = duplicate(l->id);
		} else {
			l->token->type = sym;

			if(sym == LEX_DEFINE) {
				if(l->in_definition)
					syntax_error(l, "Nested definitions are not allowed");
				l->in_definition = true;
			}
			if(sym == LEX_ENDDEFINE) {
				if(!(l->in_definition))
					syntax_error(l, "Use of ';' not terminating word definition");
				l->in_definition = false;
			}
		}
		break;
	}
	unget_char(l, ch);
}

/********* PARSER *********/

#define X_MACRO_PARSE\
	X(SYM_PROGRAM,             "program")\
	X(SYM_STATEMENTS,          "statements")\
	X(SYM_LABEL,               "label")\
	X(SYM_BRANCH,              "branch")\
	X(SYM_0BRANCH,             "0branch")\
	X(SYM_CALL,                "call")\
	X(SYM_CONSTANT,            "constant")\
	X(SYM_VARIABLE,            "variable")\
	X(SYM_LOCATION,            "location")\
	X(SYM_LITERAL,             "literal")\
	X(SYM_STRING,              "string")\
	X(SYM_INSTRUCTION,         "instruction")\
	X(SYM_BEGIN_UNTIL,         "begin...until")\
	X(SYM_BEGIN_AGAIN,         "begin...again")\
	X(SYM_BEGIN_WHILE_REPEAT,  "begin...while...repeat")\
	X(SYM_FOR_NEXT,            "for...next")\
	X(SYM_FOR_AFT_THEN_NEXT,   "for...aft...then...next")\
	X(SYM_IF1,                 "if1")\
	X(SYM_DEFINITION,          "definition")\
	X(SYM_CHAR,                "[char]")\
	X(SYM_QUOTE,               "'")\
	X(SYM_PWD,                 "pwd")\
	X(SYM_SET,                 "set")\
	X(SYM_PC,                  "pc")\
	X(SYM_BREAK,               "break")\
	X(SYM_BUILT_IN,            "built-in")\
	X(SYM_MODE,                "mode")\
	X(SYM_ALLOCATE,            "allocate")\
	X(SYM_CALL_DEFINITION,     "call-definition")

typedef enum {
#define X(ENUM, NAME) ENUM,
	X_MACRO_PARSE
#undef X
} parse_e;

static const char *names[] = {
#define X(ENUM, NAME) [ENUM] = NAME,
	X_MACRO_PARSE
#undef X
	NULL
};

typedef struct node_t  {
	parse_e type;
	size_t length;
	uint16_t bits; /*general use bits*/
	token_t *token, *value;
	struct node_t *o[];
} node_t;

static node_t *node_new(parse_e type, size_t size)
{
	node_t *r = allocate_or_die(sizeof(*r) + sizeof(r->o[0]) * size);
	if(log_level >= LOG_DEBUG)
		fprintf(stderr, "node> %s\n", names[type]);
	r->length = size;
	r->type = type;
	return r;
}

static node_t *node_grow(node_t *n)
{
	assert(n);
	errno = 0;
	node_t *r = realloc(n, sizeof(*n) + (sizeof(n->o[0]) * (n->length + 1)));
	if(!r)
		fatal("reallocate of size %zu failed: %s", n->length + 1, reason());
	r->o[r->length++] = 0;
	return r;
}

static void node_free(node_t *n)
{
	if(!n)
		return;
	for(unsigned i = 0; i < n->length; i++)
		node_free(n->o[i]);
	token_free(n->token);
	token_free(n->value);
	free(n);
}

static int accept_token(lexer_t *l, token_e sym)
{
	assert(l);
	if(sym == l->token->type) {
		token_free(l->accepted); /* free token owned by lexer */
		l->accepted = l->token;
		if(sym != LEX_EOI)
			lexer(l);
		return 1;
	}
	return 0;
}

static int accept_range(lexer_t *l, token_e low, token_e high)
{
	assert(l);
	assert(low <= high);
	for(token_e i = low; i <= high; i++)
		if(accept_token(l, i))
			return 1;
	return 0;
}

static void use(lexer_t *l, node_t *n)
{ /* move ownership of token from lexer to parse tree */
	assert(l);
	assert(n);
	if(n->token)
		n->value = l->accepted;
	else
		n->token = l->accepted;
	l->accepted = NULL;
}

static int token_enum_print(token_e sym, FILE *output)
{
	assert(output);
	assert(sym < LEX_ERROR);
	const char *s = keywords[sym];
	return fprintf(output, "%s(%u)", s ? s : "???", sym);
}

static void node_print(FILE *output, node_t *n, bool shallow, unsigned depth)
{
	if(!n)
		return;
	assert(output);
	indent(output, ' ', depth);
	fprintf(output, "node(%d): %s\n", n->type, names[n->type]);
	token_print(n->token, output, depth);
	if(n->token)
		fputc('\n', output);
	if(shallow)
		return;
	for(size_t i = 0; i < n->length; i++)
		node_print(output, n->o[i], shallow, depth+1);
}

static int _expect(lexer_t *l, token_e token, const char *file, const char *func, unsigned line)
{
	assert(l);
	assert(file);
	assert(func);
	if(accept_token(l, token))
		return 1;
	fprintf(stderr, "%s:%s:%u\n", file, func, line);
	fprintf(stderr, "  Syntax error: unexpected token\n  Got:          ");
	token_print(l->token, stderr, 0);
	fputs("  Expected:     ", stderr);
	token_enum_print(token, stderr);
	fprintf(stderr, "\n  On line: %u\n", l->line);
	ethrow(&l->error);
	return 0;
}

#define expect(L, TOKEN) _expect((L), (TOKEN), __FILE__, __func__, __LINE__)

/* for rules in the BNF tree defined entirely by their token */
static node_t *defined_by_token(lexer_t *l, parse_e type)
{
	assert(l);
	node_t *r = node_new(type, 0);
	use(l, r);
	return r;
}

typedef enum {
	DEFINE_HIDDEN    = 1 << 0,
	DEFINE_IMMEDIATE = 1 << 1,
	DEFINE_INLINE    = 1 << 2,
} define_type_e;

/** @note LEX_LOCATION handled by modifying return node in statement() */
static node_t *variable_or_constant(lexer_t *l, bool variable)
{
	assert(l);
	node_t *r = node_new(variable ? SYM_VARIABLE : SYM_CONSTANT, 1);
	expect(l, LEX_IDENTIFIER);
	use(l, r);
	if(accept_token(l, LEX_LITERAL)) {
		r->o[0] = defined_by_token(l, SYM_LITERAL);
	} else {
		expect(l, LEX_STRING);
		r->o[0] = defined_by_token(l, SYM_STRING);
	}
	if(accept_token(l, LEX_HIDDEN)) {
		if(r->bits & DEFINE_HIDDEN)
			syntax_error(l, "hidden bit already set on latest word definition");
		r->bits |= DEFINE_HIDDEN;
	}
	return r;
}

static node_t *jump(lexer_t *l, parse_e type)
{
	assert(l);
	node_t *r = node_new(type, 0);
	(void)(accept_token(l, LEX_LITERAL) || accept_token(l, LEX_STRING) || expect(l, LEX_IDENTIFIER));
	use(l, r);
	return r;
}

static node_t *statements(lexer_t *l);

static node_t *for_next(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_FOR_NEXT, 1);
	r->o[0] = statements(l);
	if(accept_token(l, LEX_AFT)) {
		r->type = SYM_FOR_AFT_THEN_NEXT;
		r = node_grow(r);
		r->o[1] = statements(l);
		r = node_grow(r);
		expect(l, LEX_THEN);
		r->o[2] = statements(l);
	}
	expect(l, LEX_NEXT);
	return r;
}

static node_t *begin(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_BEGIN_UNTIL, 1);
	r->o[0] = statements(l);
	if(accept_token(l, LEX_AGAIN)) {
		r->type = SYM_BEGIN_AGAIN;
	} else if(accept_token(l, LEX_WHILE)) {
		r->type = SYM_BEGIN_WHILE_REPEAT;
		r = node_grow(r);
		r->o[1] = statements(l);
		expect(l, LEX_REPEAT);
	} else {
		expect(l, LEX_UNTIL);
	}
	return r;
}

static node_t *if1(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_IF1, 2);
	r->o[0] = statements(l);
	if(accept_token(l, LEX_ELSE))
		r->o[1] = statements(l);
	expect(l, LEX_THEN);
	return r;
}

static node_t *define(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_DEFINITION, 1);
	if(accept_token(l, LEX_IDENTIFIER))
		/*DO NOTHING*/;
	else
		expect(l, LEX_STRING);
	use(l, r);
	r->o[0] = statements(l);
	expect(l, LEX_ENDDEFINE);
again:
	if(accept_token(l, LEX_IMMEDIATE)) {
		if(r->bits & DEFINE_IMMEDIATE)
			syntax_error(l, "immediate bit already set on latest word definition");
		r->bits |= DEFINE_IMMEDIATE;
		goto again;
	}
	if(accept_token(l, LEX_HIDDEN)) {
		if(r->bits & DEFINE_HIDDEN)
			syntax_error(l, "hidden bit already set on latest word definition");
		r->bits |= DEFINE_HIDDEN;
		goto again;
	}
	if(accept_token(l, LEX_INLINE)) {
		if(r->bits & DEFINE_INLINE)
			syntax_error(l, "inline bit already set on latest word definition");
		r->bits |= DEFINE_INLINE;
		goto again;
	}
	return r;
}

static node_t *char_compile(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_CHAR, 0);
	expect(l, LEX_IDENTIFIER);
	use(l, r);
	if(strlen(r->token->p.id) > 1)
		syntax_error(l, "expected single character, got identifier: %s", r->token->p.id);
	return r;
}

static node_t *mode(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_MODE, 0);
	expect(l, LEX_LITERAL);
	use(l, r);
	return r;
}

static node_t *pc(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_PC, 0);
	if(!accept_token(l, LEX_LITERAL))
		expect(l, LEX_IDENTIFIER);
	use(l, r);
	return r;
}

static node_t *pwd(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_PWD, 0);
	if(!accept_token(l, LEX_LITERAL))
		expect(l, LEX_IDENTIFIER);
	use(l, r);
	return r;
}

static node_t *set(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_SET, 0);
	if(!accept_token(l, LEX_IDENTIFIER) && !accept_token(l, LEX_STRING))
		expect(l, LEX_LITERAL);
	use(l, r);
	if(!accept_token(l, LEX_IDENTIFIER) && !accept_token(l, LEX_STRING))
		expect(l, LEX_LITERAL);
	use(l, r);
	return r;
}

static node_t *allocate(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_ALLOCATE, 0);
	if(!accept_token(l, LEX_IDENTIFIER))
		expect(l, LEX_LITERAL);
	use(l, r);
	return r;
}

static node_t *quote(lexer_t *l)
{
	assert(l);
	node_t *r = node_new(SYM_QUOTE, 0);
	if(!accept_token(l, LEX_IDENTIFIER))
		expect(l, LEX_STRING);
	use(l, r);
	return r;
}

static node_t *statements(lexer_t *l)
{
	size_t i = 0;
	assert(l);
	node_t *r = node_new(SYM_STATEMENTS, 2);
again:
	r = node_grow(r);
	if(accept_token(l, LEX_CALL)) {
		r->o[i++] = jump(l, SYM_CALL);
		goto again;
	} else if(accept_token(l, LEX_BRANCH)) {
		r->o[i++] = jump(l, SYM_BRANCH);
		goto again;
	} else if(accept_token(l, LEX_0BRANCH)) {
		r->o[i++] = jump(l, SYM_0BRANCH);
		goto again;
	} else if(accept_token(l, LEX_LITERAL)) {
		r->o[i++] = defined_by_token(l, SYM_LITERAL);
		goto again;
	} else if(accept_token(l, LEX_LABEL)) {
		r->o[i++] = defined_by_token(l, SYM_LABEL);
		goto again;
	} else if(accept_token(l, LEX_CONSTANT)) {
		r->o[i++] = variable_or_constant(l, false);
		goto again;
	} else if(accept_token(l, LEX_VARIABLE)) {
		r->o[i++] = variable_or_constant(l, true);
		goto again;
	} else if(accept_token(l, LEX_LOCATION)) {
		r->o[i]   = variable_or_constant(l, true);
		r->o[i++]->type = SYM_LOCATION;
		goto again;
	} else if(accept_token(l, LEX_IF)) {
		r->o[i++] = if1(l);
		goto again;
	} else if(accept_token(l, LEX_DEFINE)) {
		r->o[i++] = define(l);
		goto again;
	} else if(accept_token(l, LEX_CHAR)) {
		r->o[i++] = char_compile(l);
		goto again;
	} else if(accept_token(l, LEX_BEGIN)) {
		r->o[i++] = begin(l);
		goto again;
	} else if(accept_token(l, LEX_FOR)) {
		r->o[i++] = for_next(l);
		goto again;
	} else if(accept_token(l, LEX_QUOTE)) {
		r->o[i++] = quote(l);
		goto again;
	} else if(accept_token(l, LEX_IDENTIFIER)) {
		r->o[i++] = defined_by_token(l, SYM_CALL_DEFINITION);
		goto again;
	} else if(accept_token(l, LEX_PWD)) {
		r->o[i++] = pwd(l);
		goto again;
	} else if(accept_token(l, LEX_SET)) {
		r->o[i++] = set(l);
		goto again;
	} else if(accept_token(l, LEX_PC)) {
		r->o[i++] = pc(l);
		goto again;
	} else if(accept_token(l, LEX_BREAK)) {
		r->o[i++] = defined_by_token(l, SYM_BREAK);
		goto again;
	} else if(accept_token(l, LEX_MODE)) {
		r->o[i++] = mode(l);
		goto again;
	} else if(accept_token(l, LEX_ALLOCATE)) {
		r->o[i++] = allocate(l);
		goto again;
	} else if(accept_token(l, LEX_BUILT_IN)) {
		r->o[i++] = defined_by_token(l, SYM_BUILT_IN);
		goto again;
	/**@warning This is a token range from the first instruction to the
	 * last instruction */
	} else if(accept_range(l, LEX_DUP, LEX_RDROP)) {
		r->o[i++] = defined_by_token(l, SYM_INSTRUCTION);
		goto again;
	}
	return r;
}

static node_t *program(lexer_t *l) /* block ( "." | EOF ) */
{
	node_t *r;
	assert(l);
	r = node_new(SYM_PROGRAM, 1);
	lexer(l);
	r->o[0] = statements(l);
	expect(l, LEX_EOI);
	return r;
}

static node_t *parse(FILE *input)
{
	lexer_t *l;
	assert(input);
	l = lexer_new(input);
	l->error.jmp_buf_valid = 1;
	if(setjmp(l->error.j)) {
		lexer_free(l);
		return NULL;
	}
	node_t *n = program(l);
	lexer_free(l);
	return n;
}

/********* CODE ***********/

typedef enum {
	MODE_NORMAL              = 0 << 0,
	MODE_COMPILE_WORD_HEADER = 1 << 0,
	MODE_OPTIMIZATION_ON     = 1 << 1,
} assembler_mode_e;

typedef struct {
	bool in_definition;
	bool start_defined;
	bool built_in_words_defined;
	uint16_t start;
	uint16_t mode;
	uint16_t pwd; /* previous word register */
	uint16_t fence; /* mark a boundary before which optimization cannot take place */
	symbol_t *do_r_minus_one;
	symbol_t *do_next;
	symbol_t *do_var;
	symbol_t *do_const;
} assembler_t;

static void update_fence(assembler_t *a, uint16_t pc)
{
	assert(a);
	a->fence = MAX(a->fence, pc);
}

static void generate(h2_t *h, assembler_t *a, uint16_t instruction)
{
	assert(h);
	assert(a);
	debug("%"PRIx16":\t%"PRIx16, h->pc, instruction);

	if(IS_CALL(instruction) || IS_LITERAL(instruction) || IS_0BRANCH(instruction) || IS_BRANCH(instruction))
		update_fence(a, h->pc);

	/** @note This implements two ad-hoc optimizations, both related to
	 * CODE_EXIT, they should be replaced by a generic peep hole optimizer */
	if(a->mode & MODE_OPTIMIZATION_ON && h->pc) {
		uint16_t previous = h->core[h->pc - 1];
		if(((h->pc - 1) > a->fence) && IS_ALU_OP(previous) && (instruction == CODE_EXIT)) {
			/* merge the CODE_EXIT instruction with the previous instruction if it is possible to do so */
			if(!(previous & R_TO_PC) && !(previous & MK_RSTACK(DELTA_N1))) {
				debug("optimization EXIT MERGE pc(%04"PRIx16 ") [%04"PRIx16 " -> %04"PRIx16"]", h->pc, previous, previous|instruction);
				previous |= instruction;
				h->core[h->pc - 1] = previous;
				update_fence(a, h->pc - 1);
				return;
			}
		} else if(h->pc > a->fence && IS_CALL(previous) && (instruction == CODE_EXIT)) {
			/* do not emit CODE_EXIT if last instruction in a word
			 * definition is a call, instead replace that call with
			 * a jump */
			debug("optimization TAIL CALL pc(%04"PRIx16 ") [%04"PRIx16 " -> %04"PRIx16"]", h->pc, previous, OP_BRANCH | (previous & 0x1FFF));
			h->core[h->pc - 1] = (OP_BRANCH | (previous & 0x1FFF));
			update_fence(a, h->pc - 1);
			return;
		}
	}

	h->core[h->pc++] = instruction;
}

static uint16_t here(h2_t *h, assembler_t *a)
{
	assert(h);
	assert(h->pc < MAX_CORE);
	update_fence(a, h->pc);
	return h->pc;
}

static uint16_t hole(h2_t *h, assembler_t *a)
{
	assert(h);
	assert(h->pc < MAX_CORE);
	here(h, a);
	return h->pc++;
}

static void fix(h2_t *h, uint16_t hole, uint16_t patch)
{
	assert(h);
	assert(hole < MAX_CORE);
	h->core[hole] = patch;
}

#define assembly_error(ERROR, FMT, ...) do{ error(FMT, ##__VA_ARGS__); ethrow(e); }while(0)

static void generate_jump(h2_t *h, assembler_t *a, symbol_table_t *t, token_t *tok, parse_e type, error_t *e)
{
	uint16_t or = 0;
	uint16_t addr = 0;
	symbol_t *s;
	assert(h);
	assert(t);
	assert(a);

	if(tok->type == LEX_IDENTIFIER || tok->type == LEX_STRING) {
		s = symbol_table_lookup(t, tok->p.id);
		if(!s)
			assembly_error(e, "undefined symbol: %s", tok->p.id);
		addr = s->value;

		if(s->type == SYMBOL_TYPE_CALL && type != SYM_CALL)
			assembly_error(e, "cannot branch/0branch to call: %s", tok->p.id);

	} else if (tok->type == LEX_LITERAL) {
		addr = tok->p.number;
	} else {
		fatal("invalid jump target token type");
	}

	if(addr > MAX_CORE)
		assembly_error(e, "invalid jump address: %"PRId16, addr);

	switch(type) {
	case SYM_BRANCH:  or = OP_BRANCH ; break;
	case SYM_0BRANCH: or = OP_0BRANCH; break;
	case SYM_CALL:    or = OP_CALL;    break;
	default:
		fatal("invalid call type: %u", type);
	}
	generate(h, a, or | addr);
}

static void generate_literal(h2_t *h, assembler_t *a, uint16_t number)
{
	if(number & OP_LITERAL) {
		number = ~number;
		generate(h, a, OP_LITERAL | number);
		generate(h, a, CODE_INVERT);
	} else {
		generate(h, a, OP_LITERAL | number);
	}
}

static uint16_t lexer_to_alu_op(token_e t)
{
	assert(t >= LEX_DUP && t <= LEX_RDROP);
	switch(t) {
#define X(NAME, STRING, DEFINE, INSTRUCTION) case LEX_ ## NAME : return CODE_ ## NAME ;
	X_MACRO_INSTRUCTIONS
#undef X
	default: fatal("invalid ALU operation: %u", t);
	}
	return 0;
}

static uint16_t literal_or_symbol_lookup(token_t *token, symbol_table_t *t, error_t *e)
{
	symbol_t *s = NULL;
	assert(token);
	assert(t);
	if(token->type == LEX_LITERAL)
		return token->p.number;

	assert(token->type == LEX_IDENTIFIER);

	if(!(s = symbol_table_lookup(t, token->p.id)))
		assembly_error(e, "symbol not found: %s", token->p.id);
	return s->value;
}

static uint16_t pack_16(const char lb, const char hb)
{
	return (((uint16_t)hb) << 8) | (uint16_t)lb;
}

static uint16_t pack_string(h2_t *h, assembler_t *a, const char *s, error_t *e)
{
	assert(h);
	assert(s);
	size_t l = strlen(s);
	size_t i = 0;
	uint16_t r = h->pc;
	if(l > 255)
		assembly_error(e, "string \"%s\" is too large (%zu > 255)", s, l);
	h->core[hole(h, a)] = pack_16(l, s[0]);
	for(i = 1; i < l; i += 2)
		h->core[hole(h, a)] = pack_16(s[i], s[i+1]);
	if(i < l)
		h->core[hole(h, a)] = pack_16(s[i], 0);
	here(h, a);
	return r;
}

static uint16_t symbol_special(h2_t *h, assembler_t *a, const char *id, error_t *e)
{
	static const char *special[] = {
		"$pc",
		"$pwd",
		NULL
	};

	enum special_e {
		SPECIAL_VARIABLE_PC,
		SPECIAL_VARIABLE_PWD
	};

	size_t i;
	assert(h);
	assert(id);
	assert(a);

	for(i = 0; special[i]; i++)
		if(!strcmp(id, special[i]))
			break;
	if(!special[i])
		assembly_error(e, "'%s' is not a symbol", id);

	switch(i) {
	case SPECIAL_VARIABLE_PC:   return h->pc << 1;
	case SPECIAL_VARIABLE_PWD:  return a->pwd; /**@note already as a character address */
	default: fatal("reached the unreachable: %zu", i);
	}

	return 0;
}

typedef struct {
	char *name;
	size_t len;
	bool inline_bit;
	bool hidden;
	bool compile;
	uint16_t code[32];
} built_in_words_t;

static built_in_words_t built_in_words[] = {
#define X(NAME, STRING, DEFINE, INSTRUCTION) \
	{\
		.name = STRING,\
		.compile = DEFINE,\
		.len = 1,\
		.inline_bit = true,\
		.hidden = false,\
		.code = { INSTRUCTION }\
	},
	X_MACRO_INSTRUCTIONS
#undef X
	/**@note We might want to compile these words, even if we are not
	 * compiling the other in-line-able, so the compiler can use them for
	 * variable declaration and for...next loops */
	{ .name = "doVar",   .compile = true, .inline_bit = false, .hidden = true, .len = 1, .code = {CODE_FROMR} },
	{ .name = "doConst", .compile = true, .inline_bit = false, .hidden = true, .len = 2, .code = {CODE_FROMR, CODE_LOAD} },
	{ .name = "r1-",     .compile = true, .inline_bit = false, .hidden = true, .len = 5, .code = {CODE_FROMR, CODE_FROMR, CODE_T_N1, CODE_TOR, CODE_TOR} },
	{ .name = NULL,      .compile = true, .inline_bit = false, .hidden = true, .len = 0, .code = {0} }
};

static void generate_loop_decrement(h2_t *h, assembler_t *a, symbol_table_t *t)
{
	a->do_r_minus_one = a->do_r_minus_one ? a->do_r_minus_one : symbol_table_lookup(t, "r1-");
	if(a->do_r_minus_one && a->mode & MODE_OPTIMIZATION_ON) {
		generate(h, a, OP_CALL | a->do_r_minus_one->value);
	} else {
		generate(h, a, CODE_FROMR);
		generate(h, a, CODE_T_N1);
		generate(h, a, CODE_TOR);
	}
}

static void assemble(h2_t *h, assembler_t *a, node_t *n, symbol_table_t *t, error_t *e)
{
	uint16_t hole1 = 0, hole2 = 0;
	assert(h);
	assert(t);
	assert(e);

	if(!n)
		return;

	if(h->pc > MAX_CORE)
		assembly_error(e, "PC/Dictionary overflow: %"PRId16, h->pc);

	switch(n->type) {
	case SYM_PROGRAM:
		assemble(h, a, n->o[0], t, e);
		break;
	case SYM_STATEMENTS:
		for(size_t i = 0; i < n->length; i++)
			assemble(h, a, n->o[i], t, e);
		break;
	case SYM_LABEL:
		symbol_table_add(t, SYMBOL_TYPE_LABEL, n->token->p.id, here(h, a), e, false);
		break;
	case SYM_BRANCH:
	case SYM_0BRANCH:
	case SYM_CALL:
		generate_jump(h, a, t, n->token, n->type, e);
		break;
	case SYM_CONSTANT:
		if(a->mode & MODE_COMPILE_WORD_HEADER && a->built_in_words_defined && (!(n->bits & DEFINE_HIDDEN))) {
			a->do_const = a->do_const ? a->do_const : symbol_table_lookup(t, "doConst");
			assert(a->do_const);
			hole1 = hole(h, a);
			fix(h, hole1, a->pwd);
			a->pwd = hole1 << 1;
			pack_string(h, a, n->token->p.id, e);
			generate(h, a, OP_CALL | a->do_const->value);
			hole1 = hole(h, a);
			fix(h, hole1, n->o[0]->token->p.number);
		}
		symbol_table_add(t, SYMBOL_TYPE_CONSTANT, n->token->p.id, n->o[0]->token->p.number, e, false);
		break;
	case SYM_VARIABLE:
		if(a->mode & MODE_COMPILE_WORD_HEADER && a->built_in_words_defined && (!(n->bits & DEFINE_HIDDEN))) {
			a->do_var = a->do_var ? a->do_var : symbol_table_lookup(t, "doVar");
			assert(a->do_var);
			hole1 = hole(h, a);
			fix(h, hole1, a->pwd);
			a->pwd = hole1 << 1;
			pack_string(h, a, n->token->p.id, e);
			generate(h, a, OP_CALL | a->do_var->value);
		} else if (!(n->bits & DEFINE_HIDDEN)) {
			assembly_error(e, "variable used but doVar not defined, use location");
		}
		/* fall through */
	case SYM_LOCATION:
		here(h, a);

		if(n->o[0]->token->type == LEX_LITERAL) {
			hole1 = hole(h, a);
			fix(h, hole1, n->o[0]->token->p.number);
		} else {
			assert(n->o[0]->token->type == LEX_STRING);
			hole1 = pack_string(h, a, n->o[0]->token->p.id, e);
		}

		/**@note The lowest bit of the address for memory loads is
		 * discarded. */
		symbol_table_add(t, SYMBOL_TYPE_VARIABLE, n->token->p.id, hole1 << 1, e, n->type == SYM_LOCATION ? true : false);
		break;
	case SYM_QUOTE:
	{
		symbol_t *s = symbol_table_lookup(t, n->token->p.id);
		if(!s || (s->type != SYMBOL_TYPE_CALL && s->type != SYMBOL_TYPE_LABEL))
			assembly_error(e, "not a defined procedure: %s", n->token->p.id);
		generate_literal(h, a, s->value << 1);
		break;
	}
	case SYM_LITERAL:
		generate_literal(h, a, n->token->p.number);
		break;
	case SYM_INSTRUCTION:
		generate(h, a, lexer_to_alu_op(n->token->type));
		break;
	case SYM_BEGIN_AGAIN: /* fall through */
	case SYM_BEGIN_UNTIL:
		hole1 = here(h, a);
		assemble(h, a, n->o[0], t, e);
		generate(h, a, (n->type == SYM_BEGIN_AGAIN ? OP_BRANCH : OP_0BRANCH) | hole1);
		break;

	case SYM_FOR_NEXT:
	{
		symbol_t *s = a->do_next ? a->do_next : symbol_table_lookup(t, "doNext");
		if(s && a->mode & MODE_OPTIMIZATION_ON) {
			generate(h, a, CODE_TOR);
			hole1 = here(h, a);
			assemble(h, a, n->o[0], t, e);
			generate(h, a, OP_CALL | s->value);
			generate(h, a, hole1 << 1);
		} else {
			generate(h, a, CODE_TOR);
			hole1 = here(h, a);
			assemble(h, a, n->o[0], t, e);
			generate(h, a, CODE_RAT);
			hole2 = hole(h, a);
			generate_loop_decrement(h, a, t);
			generate(h, a, OP_BRANCH | hole1);
			fix(h, hole2, OP_0BRANCH | here(h, a));
			generate(h, a, CODE_RDROP);
		}
		break;
	}
	case SYM_FOR_AFT_THEN_NEXT:
	{
		symbol_t *s = a->do_next ? a->do_next : symbol_table_lookup(t, "doNext");
		if(s && a->mode & MODE_OPTIMIZATION_ON) {
			generate(h, a, CODE_TOR);
			assemble(h, a, n->o[0], t, e);
			hole1 = hole(h, a);
			hole2 = here(h, a);
			assemble(h, a, n->o[1], t, e);
			fix(h, hole1, OP_BRANCH | here(h, a));
			assemble(h, a, n->o[2], t, e);
			generate(h, a, OP_CALL | s->value);
			generate(h, a, hole2 << 1);
		} else {
			generate(h, a, CODE_TOR);
			assemble(h, a, n->o[0], t, e);
			hole1 = hole(h, a);
			generate(h, a, CODE_RAT);
			generate_loop_decrement(h, a, t);
			hole2 = hole(h, a);
			assemble(h, a, n->o[1], t, e);
			fix(h, hole1, OP_BRANCH | (here(h, a)));
			assemble(h, a, n->o[2], t, e);
			generate(h, a, OP_BRANCH | (hole1 + 1));
			fix(h, hole2, OP_0BRANCH | (here(h, a)));
			generate(h, a, CODE_RDROP);
		}
		break;
	}
	case SYM_BEGIN_WHILE_REPEAT:
		hole1 = here(h, a);
		assemble(h, a, n->o[0], t, e);
		hole2 = hole(h, a);
		assemble(h, a, n->o[1], t, e);
		generate(h, a, OP_BRANCH  | hole1);
		fix(h, hole2, OP_0BRANCH | here(h, a));
		break;
	case SYM_IF1:
		hole1 = hole(h, a);
		assemble(h, a, n->o[0], t, e);
		if(n->o[1]) { /* if ... else .. then */
			hole2 = hole(h, a);
			fix(h, hole1, OP_0BRANCH | (hole2 + 1));
			assemble(h, a, n->o[1], t, e);
			fix(h, hole2, OP_BRANCH  | here(h, a));
		} else { /* if ... then */
			fix(h, hole1, OP_0BRANCH | here(h, a));
		}
		break;
	case SYM_CALL_DEFINITION:
	{
		symbol_t *s = symbol_table_lookup(t, n->token->p.id);
		if(!s)
			assembly_error(e, "not a constant or a defined procedure: %s", n->token->p.id);
		if(s->type == SYMBOL_TYPE_CALL) {
			generate(h, a, OP_CALL | s->value);
		} else if(s->type == SYMBOL_TYPE_CONSTANT || s->type == SYMBOL_TYPE_VARIABLE) {
			generate_literal(h, a, s->value);
		} else {
			error("can only call or push literal: %s", s->id);
			ethrow(e);
		}
		break;
	}
	case SYM_DEFINITION:
		if(n->bits && !(a->mode & MODE_COMPILE_WORD_HEADER))
			assembly_error(e, "cannot modify word bits (immediate/hidden/inline) if not in compile mode");
		if(a->mode & MODE_COMPILE_WORD_HEADER && !(n->bits & DEFINE_HIDDEN)) {
			hole1 = hole(h, a);
			n->bits &= (DEFINE_IMMEDIATE | DEFINE_INLINE);
			fix(h, hole1, a->pwd | (n->bits << 13)); /* shift in word bits into PWD field */
			a->pwd = hole1 << 1;
			pack_string(h, a, n->token->p.id, e);
		}
		symbol_table_add(t, SYMBOL_TYPE_CALL, n->token->p.id, here(h, a), e, n->bits & DEFINE_HIDDEN);
		if(a->in_definition)
			assembly_error(e, "nested word definition is not allowed");
		a->in_definition = true;
		assemble(h, a, n->o[0], t, e);
		generate(h, a, CODE_EXIT);
		a->in_definition = false;
		break;
	case SYM_CHAR: /* [char] A  */
		generate(h, a, OP_LITERAL | n->token->p.id[0]);
		break;
	case SYM_SET:
	{
		uint16_t value = 0;
		const uint16_t location = literal_or_symbol_lookup(n->token, t, e);

		if(n->value->type == LEX_LITERAL) {
			value = n->value->p.number;
		} else {
			symbol_t *l = symbol_table_lookup(t, n->value->p.id);
			if(l) {
				value = l->value;
				if(l->type == SYMBOL_TYPE_CALL) // || l->type == SYMBOL_TYPE_LABEL)
					value <<= 1;
			} else {
				value = symbol_special(h, a, n->value->p.id, e);
			}
		}
		fix(h, location >> 1, value);
		break;
	}
	case SYM_PWD:
		a->pwd = literal_or_symbol_lookup(n->token, t, e);
		break;
	case SYM_PC:
		h->pc = literal_or_symbol_lookup(n->token, t, e);
		update_fence(a, h->pc);
		break;
	case SYM_MODE:
		a->mode = n->token->p.number;
		break;
	case SYM_ALLOCATE:
		h->pc += literal_or_symbol_lookup(n->token, t, e) >> 1;
		update_fence(a, h->pc);
		break;
	case SYM_BREAK:
		break_point_add(&h->bp, h->pc);
		update_fence(a, h->pc);
		break;
	case SYM_BUILT_IN:
		if(!(a->mode & MODE_COMPILE_WORD_HEADER))
			break;

		if(a->built_in_words_defined)
			assembly_error(e, "built in words already defined");
		a->built_in_words_defined = true;

		for(unsigned i = 0; built_in_words[i].name; i++) {
			if(!(built_in_words[i].compile))
				continue;

			if(!built_in_words[i].hidden) {
				uint16_t pwd = a->pwd;
				hole1 = hole(h, a);
				if(built_in_words[i].inline_bit)
					pwd |= (DEFINE_INLINE << 13);
				fix(h, hole1, pwd);
				a->pwd = hole1 << 1;
				pack_string(h, a, built_in_words[i].name, e);
			}
			symbol_table_add(t, SYMBOL_TYPE_CALL, built_in_words[i].name, here(h, a), e, built_in_words[i].hidden);
			for(size_t j = 0; j < built_in_words[i].len; j++)
				generate(h, a, built_in_words[i].code[j]);
			generate(h, a, CODE_EXIT);
		}
		break;
	default:
		fatal("Invalid or unknown type: %u", n->type);
	}
}

static bool assembler(h2_t *h, assembler_t *a, node_t *n, symbol_table_t *t, error_t *e)
{
	assert(h && a && n && t && e);
	if(setjmp(e->j))
		return false;
	assemble(h, a, n, t, e);
	return true;
}

static h2_t *code(node_t *n, symbol_table_t *symbols)
{
	error_t e;
	h2_t *h;
	symbol_table_t *t = NULL;
	assembler_t a;
	assert(n);
	memset(&a, 0, sizeof a);

	t = symbols ? symbols : symbol_table_new();
	h = h2_new(START_ADDR);
	a.fence = h->pc;

	e.jmp_buf_valid = 1;
	if(!assembler(h, &a, n, t, &e)) {
		h2_free(h);
		if(!symbols)
			symbol_table_free(t);
		return NULL;
	}

	if(log_level >= LOG_DEBUG)
		symbol_table_print(t, stderr);
	if(!symbols)
		symbol_table_free(t);
	return h;
}

int h2_assemble_file(FILE *input, FILE *output, symbol_table_t *symbols)
{
	assert(input);
	assert(output);
	int r = 0;
	node_t *n = parse(input);

	if(log_level >= LOG_DEBUG)
		node_print(stderr, n, false, 0);
	if(n) {
		h2_t *h = code(n, symbols);
		if(h)
			r = h2_save(h, output, false);
		else
			r = -1;
		h2_free(h);
	} else {
		r = -1;
	}
	node_free(n);
	return r;
}

h2_t *h2_assemble_core(FILE *input, symbol_table_t *symbols)
{
	assert(input);
	h2_t *h = NULL;
	node_t *n = parse(input);
	if(log_level >= LOG_DEBUG)
		node_print(stderr, n, false, 0);
	if(n)
		h = code(n, symbols);
	node_free(n);
	return h;
}

/* ========================== Assembler ==================================== */

/* ========================== Main ========================================= */

#ifndef NO_MAIN
typedef enum {
	DEFAULT_COMMAND,
	DISASSEMBLE_COMMAND,
	ASSEMBLE_COMMAND,
	RUN_COMMAND,
	ASSEMBLE_RUN_COMMAND
} command_e;

typedef struct {
	command_e cmd;
	long steps;
	bool full_disassembly;
	bool debug_mode;
	bool hacks;
	disassemble_color_method_e dcm;
	const char *nvram;
} command_args_t;

static const char *help = "\
usage ./h2 [-hvdDarRTH] [-sc number] [-L symbol.file] [-S symbol.file] [-e file.fth] (file.hex|file.fth)\n\n\
Brief:     A H2 CPU Assembler, disassembler and Simulator.\n\
Author:    Richard James Howe\n\
Site:      https://github.com/howerj/forth-cpu\n\
License:   MIT\n\
Copyright: Richard James Howe (2017,2018)\n\
Options:\n\n\
\t-\tstop processing options, following arguments are files\n\
\t-h\tprint this help message and exit\n\
\t-v\tincrease logging level\n\
\t-d\tdisassemble input files (default)\n\
\t-D\tfull disassembly of input files\n\
\t-T\tEnter debug mode when running simulation\n\
\t-a\tassemble file\n\
\t-H\tenable hacks to make the simulation easier to use\n\
\t-r\trun hex file\n\
\t-R\tassemble file then run it\n\
\t-L #\tload symbol file\n\
\t-S #\tsave symbols to file\n\
\t-s #\tnumber of steps to run simulation (0 = forever)\n\
\t-n #\tspecify nvram file\n\
\t-H #\tenable certain hacks for simulation purposes\n\
\t-c #\tset colorization method for disassembly\n\
\tfile\thex or forth file to process\n\n\
Options must precede any files given, if a file has not been\n\
given as arguments input is taken from stdin. Output is to\n\
stdout. Program returns zero on success, non zero on failure.\n\n\
";

static void debug_note(command_args_t *cmd)
{
	if(cmd->debug_mode)
		note("entering debug mode");
	else
		note("running for %u cycles (0 = forever)", (unsigned)cmd->steps);
}

static int assemble_run_command(command_args_t *cmd, FILE *input, FILE *output, symbol_table_t *symbols, bool assemble, uint16_t *vga_initial_contents)
{
	assert(input);
	assert(output);
	assert(cmd);
	assert(cmd->nvram);
	h2_t *h = NULL;
	h2_io_t *io = NULL;
	int r = 0;

	if(assemble) {
		h = h2_assemble_core(input, symbols);
	} else {
		h = h2_new(START_ADDR);
		if(h2_load(h, input) < 0)
			return -1;
	}

	if(!h)
		return -1;

	io = h2_io_new();
	assert(VGA_BUFFER_LENGTH <= VT100_MAX_SIZE);
	for(size_t i = 0; i < VGA_BUFFER_LENGTH; i++) {
		vt100_attribute_t attr;
		memset(&attr, 0, sizeof(attr));
		io->soc->vt100.m[i]   =  vga_initial_contents[i] & 0xff;
		attr.background_color = (vga_initial_contents[i] >> 8)  & 0x7;
		attr.foreground_color = (vga_initial_contents[i] >> 11) & 0x7;
		memcpy(&io->soc->vt100.attributes[i], &attr, sizeof(attr));
	}

	nvram_load_and_transfer(io, cmd->nvram, cmd->hacks);
	h->pc = START_ADDR;
	debug_note(cmd);
	r = h2_run(h, io, output, cmd->steps, symbols, cmd->debug_mode, NULL);
	nvram_save(io, cmd->nvram);

	h2_free(h);
	h2_io_free(io);
	return r;
}

int command(command_args_t *cmd, FILE *input, FILE *output, symbol_table_t *symbols, uint16_t *vga_initial_contents)
{
	assert(input);
	assert(output);
	assert(cmd);
	switch(cmd->cmd) {
	case DEFAULT_COMMAND:      /* fall through */
	case DISASSEMBLE_COMMAND:  return h2_disassemble(cmd->dcm, input, output, symbols);
	case ASSEMBLE_COMMAND:     return h2_assemble_file(input, output, symbols);
	case RUN_COMMAND:          return assemble_run_command(cmd, input, output, symbols, false, vga_initial_contents);
	case ASSEMBLE_RUN_COMMAND: return assemble_run_command(cmd, input, output, symbols, true,  vga_initial_contents);
	default:                   fatal("invalid command: %d", cmd->cmd);
	}
	return -1;
}

static const char *nvram_file = FLASH_INIT_FILE;

int h2_main(int argc, char **argv)
{
	int i;
	const char *optarg = NULL;
	command_args_t cmd;
	symbol_table_t *symbols = NULL;
	FILE *symfile = NULL;
	FILE *newsymfile = NULL;
	FILE *input = NULL;
	memset(&cmd, 0, sizeof(cmd));
	cmd.steps = DEFAULT_STEPS;
	cmd.nvram = nvram_file;
	cmd.dcm   = DCM_X11;

	static uint16_t vga_initial_contents[VGA_BUFFER_LENGTH] = { 0 };

	binary(stdin);
	binary(stdout);
	binary(stderr);

	{ /* attempt to load initial contents of VGA memory */
		errno = 0;
		FILE *vga_init = fopen(VGA_INIT_FILE, "rb");
		if(vga_init) {
			memory_load(vga_init, vga_initial_contents, VGA_BUFFER_LENGTH);
			fclose(vga_init);
		} else {
			warning("could not load initial VGA memory file %s: %s", VGA_INIT_FILE, strerror(errno));
		}
	}

	for(i = 1; i < argc && argv[i][0] == '-'; i++) {

		if(strlen(argv[i]) > 2) {
			error("Only one option allowed at a time (got %s)", argv[i]);
			goto fail;
		}

		switch(argv[i][1]) {
		case '\0':
			goto done; /* stop processing options */
		case 'h':
			fprintf(stderr, "%s\n", help);
			return -1;
		case 'v':  /* increase verbosity */
			log_level += log_level < LOG_ALL_MESSAGES ? 1 : 0;
			break;
		case 'D':
			cmd.full_disassembly = true;
			/* fall through */
		case 'd':
			if(cmd.cmd)
				goto fail;
			cmd.cmd = DISASSEMBLE_COMMAND;
			break;
		case 'a':
			if(cmd.cmd)
				goto fail;
			cmd.cmd = ASSEMBLE_COMMAND;
			break;
		case 'r':
			if(cmd.cmd)
				goto fail;
			cmd.cmd = RUN_COMMAND;
			break;
		case 'T':
			cmd.debug_mode = true;
			break;
		case 'R':
			if(cmd.cmd)
				goto fail;
			cmd.cmd = ASSEMBLE_RUN_COMMAND;
			break;
		case 'L':
			if(i >= (argc - 1) || symfile)
				goto fail;
			optarg = argv[++i];
			/* NB. Cannot merge symbol tables */
			symfile = fopen_or_die(optarg, "rb");
			symbols = symbol_table_load(symfile);
			break;
		case 'S':
			if(i >= (argc - 1) || newsymfile)
				goto fail;
			optarg = argv[++i];
			newsymfile = fopen_or_die(optarg, "wb");
			break;
		case 'c':
		{
			long dcm = DCM_NONE;
			if(i >= (argc - 1))
				goto fail;
			optarg = argv[++i];
			if(string_to_long(0, &dcm, optarg))
				goto fail;
			cmd.dcm = dcm;
			if(cmd.dcm >= DCM_MAX_DCM) {
				fprintf(stderr, "Invalid Colorization Method: %u\n", cmd.dcm);
				goto fail;
			}
			break;
		}
		case 's':
			if(i >= (argc - 1))
				goto fail;
			optarg = argv[++i];
			if(string_to_long(0, &cmd.steps, optarg))
				goto fail;
			break;
		case 'n':
			if(i >= (argc - 1))
				goto fail;
			cmd.nvram = argv[++i];
			note("nvram file %s", cmd.nvram);
			break;
		case 'H':
			cmd.hacks = true;
			break;
		default:
		fail:
			fatal("invalid argument '%s'\n%s\n", argv[i], help);
		}
	}
	if(!symbols)
		symbols = symbol_table_new();

done:
	if(i == argc) {
		if(command(&cmd, stdin, stdout, symbols, vga_initial_contents) < 0)
			fatal("failed to process standard input");
		return 0;
	}

	if(i < (argc - 1))
		fatal("more than one file argument given");

	input = fopen_or_die(argv[i], "rb");
	if(command(&cmd, input, stdout, symbols, vga_initial_contents) < 0)
		fatal("failed to process file: %s", argv[i]);
	/**@note keeping "input" open until the command exits locks the
	 * file for longer than is necessary under Windows */
	fclose(input);

	if(newsymfile) {
		symbol_table_print(symbols, newsymfile);
		fclose(newsymfile);
	}
	symbol_table_free(symbols);
	if(symfile)
		fclose(symfile);
	return 0;
}

int main(int argc, char **argv)
{
	return h2_main(argc, argv);
}
#endif

/* ========================== Main ========================================= */
