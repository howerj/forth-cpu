/** @file      h2.c
 *  @brief     Simulate the H2 CPU and surrounding system
 *  @copyright Richard James Howe (2017)
 *  @license   MIT
 *
 * Initially this program will be for just simulating the H2 core,
 * but eventually it will be extended so the peripherals can also
 * be simulated. This should speed up development of programs written
 * for the device, and allow for simulating the device where there
 * is no tool chain for dealing with VHDL.
 *
 * @todo Turn this into a library, and into a literate program, and possibly
 * make a nice and shiny OpenGL version which simulates the VGA output, LEDs
 * and Switches.
 * @todo make a peephole optimizer for the assembler and a super optimizer
 * utility.
 * @todo Turn the diagrams in this file into help strings which can
 * be printed out
 *
 * @note Given a sufficiently developed H2 application, it should be possible
 * to feed the same inputs into h2_run and except the same outputs as the
 * VHDL based CPU. This could be used as an advanced test bench. This could
 * be done instruction by instruction, or the data could be read in from a
 * file.
 *
 * The H2 CPU is a rewrite of the J1 Forth CPU in VHDL with some extensions,
 *
 * It is a stack based CPU with minimal state; a program counter, a top
 * of the data stack register and two stacks 32 deep each, the return
 * and the data stacks. It is a 16-bit CPU.
 *
 * The program is stored in RAM and is 8192 cells long, which is the
 * maximum number of cells that the program can be. Any read or write
 * to an address larger than this can be used for memory mapped peripherals.
 *
 * Instruction layout:
 *
 *	*---------------------------------------------------------------*
 *	| F | E | D | C | B | A | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *	*---------------------------------------------------------------*
 *	| 1 |                    LITERAL VALUE                          |
 *	*---------------------------------------------------------------*
 *	| 0 | 0 | 0 |            BRANCH TARGET ADDRESS                  |
 *	*---------------------------------------------------------------*
 *	| 0 | 0 | 1 |            CONDITIONAL BRANCH TARGET ADDRESS      |
 *	*---------------------------------------------------------------*
 *	| 0 | 1 | 0 |            CALL TARGET ADDRESS                    |
 *	*---------------------------------------------------------------*
 *	| 0 | 1 | 1 |   ALU OPERATION   |T2N|T2R|N2A|R2P| RSTACK| DSTACK|
 *	*---------------------------------------------------------------*
 *	| F | E | D | C | B | A | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *	*---------------------------------------------------------------*
 *
 *      T   : Top of data stack
 *	N   : Next on data stack
 *	PC  : Program Counter
 *
 *      LITERAL VALUES : push a value onto the data stack
 *      CONDITIONAL    : BRANCHS pop and test the T
 *      CALLS          : PC+1 onto the return stack
 *
 *	T2N : Move T to N
 *	T2R : Move T to top of return stack
 *	N2A : STORE T to memory location addressed by N
 *	R2P : Move top of return stack to PC
 *
 *	RSTACK and DSTACK are signed values (twos compliment) that are
 *	the stack delta (the amount to increment or decrement the stack
 *	by for their respective stacks: return and data)
 *
 * ALU OPERATIONS
 *
 * All ALU operations replace T:
 *
 *	*-------*----------------*-----------------------*
 *	| Value |   Operation    |     Description       |
 *	*-------*----------------*-----------------------*
 *	|   0   |       T        |  Top of Stack         |
 *	|   1   |       N        |  Copy T to N          |
 *	|   2   |     T + N      |  Addition             |
 *	|   3   |     T & N      |  Bitwise AND          |
 *	|   4   |     T | N      |  Bitwise OR           |
 *	|   5   |     T ^ N      |  Bitwise XOR          |
 *	|   6   |      ~T        |  Bitwise Inversion    |
 *	|   7   |     T = N      |  Equality test        |
 *	|   8   |     N < T      |  Signed comparison    |
 *	|   9   |     N >> T     |  Logical Right Shift  |
 *	|  10   |     T - 1      |  Decrement            |
 *	|  11   |       R        |  Top of return stack  |
 *	|  12   |      [T]       |  Load from address    |
 *	|  13   |     N << T     |  Logical Left Shift   |
 *	|  14   |     depth      |  Depth of stack       |
 *	|  15   |     N u< T     |  Unsigned comparison  |
 *	|  16   | set interrupts |  Enable interrupts    |
 *	|  17   | interrupts on? |  Are interrupts on?   |
 *	|  18   |     rdepth     |  Depth of return stk  |
 *	|  19   |      0=        |  T == 0?              |
 *	*-------*----------------*-----------------------*
 *
 * More information about the original J1 CPU can be found at:
 *
 * 	http://excamera.com/sphinx/fpga-j1.html
 */

/* ========================== Preamble: Types, Macros, Globals ============= */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <setjmp.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifdef _WIN32
#include <io.h>
#include <fcntl.h>
extern int _fileno(FILE *stream);
#endif

/**@note STK_SIZE is fixed to 32, but h2.vhd allows for the instantiation of
 * CPUs with different stack sizes (so long as they are a power of 2) */

#define MAX_CORE      (8192u)
#define STK_SIZE      (32u)
#define START_ADDR    (8u)
#define DEFAULT_STEPS (512)
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
#define IS_0BRANCH(INST) (((INST) & 0x2000) == 0x2000)
#define IS_CALL(INST)    (((INST) & 0x4000) == 0x4000)
#define IS_ALU_OP(INST)  (((INST) & 0x6000) == 0x6000)

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
	ALU_OP_T_EQUAL_0           /**< T == 0               */
} alu_code_e;

#define DELTA_0  (0)
#define DELTA_1  (1)
#define DELTA_N2 (2)
#define DELTA_N1 (3)

#define MK_DSTACK(DELTA) ((DELTA) << DSTACK_START)
#define MK_RSTACK(DELTA) ((DELTA) << RSTACK_START)
#define MK_CODE(CODE)    ((CODE)  << ALU_OP_START)

typedef enum {
	CODE_DUP    = (OP_ALU_OP | MK_CODE(ALU_OP_T)        | T_TO_N  | MK_DSTACK(DELTA_1)),
	CODE_OVER   = (OP_ALU_OP | MK_CODE(ALU_OP_N)        | T_TO_N  | MK_DSTACK(DELTA_1)),
	CODE_INVERT = (OP_ALU_OP | MK_CODE(ALU_OP_T_INVERT)),
	CODE_ADD    = (OP_ALU_OP | MK_CODE(ALU_OP_T_PLUS_N)               | MK_DSTACK(DELTA_N1)),
	CODE_SWAP   = (OP_ALU_OP | MK_CODE(ALU_OP_N)        | T_TO_N),
	CODE_NIP    = (OP_ALU_OP | MK_CODE(ALU_OP_T)                      | MK_DSTACK(DELTA_N1)),
	CODE_DROP   = (OP_ALU_OP | MK_CODE(ALU_OP_N)                      | MK_DSTACK(DELTA_N1)),
	CODE_EXIT   = (OP_ALU_OP | MK_CODE(ALU_OP_T)        | R_TO_PC | MK_RSTACK(DELTA_N1)),
	CODE_TOR    = (OP_ALU_OP | MK_CODE(ALU_OP_N)        | T_TO_R  | MK_DSTACK(DELTA_N1) | MK_RSTACK(DELTA_1)),
	/**@note In the original J1 specification both r@ and r> both have
	 * their T_TO_R bit set in their instruction description tables, this
	 * appears to be incorrect */
	CODE_FROMR  = (OP_ALU_OP | MK_CODE(ALU_OP_R)        | T_TO_N  | MK_DSTACK(DELTA_1) | MK_RSTACK(DELTA_N1)),
	CODE_RAT    = (OP_ALU_OP | MK_CODE(ALU_OP_R)        | T_TO_N  | MK_DSTACK(DELTA_1)),
	CODE_LOAD   = (OP_ALU_OP | MK_CODE(ALU_OP_T_LOAD)),
	CODE_STORE  = (OP_ALU_OP | MK_CODE(ALU_OP_N)        | N_TO_ADDR_T | MK_DSTACK(DELTA_N1)),
	CODE_RSHIFT = (OP_ALU_OP | MK_CODE(ALU_OP_N_RSHIFT_T)             | MK_DSTACK(DELTA_N1)),
	CODE_LSHIFT = (OP_ALU_OP | MK_CODE(ALU_OP_N_LSHIFT_T)             | MK_DSTACK(DELTA_N1)),
	CODE_EQUAL  = (OP_ALU_OP | MK_CODE(ALU_OP_T_EQUAL_N)              | MK_DSTACK(DELTA_N1)),
	CODE_ULESS  = (OP_ALU_OP | MK_CODE(ALU_OP_N_ULESS_T)              | MK_DSTACK(DELTA_N1)),
	CODE_LESS   = (OP_ALU_OP | MK_CODE(ALU_OP_N_LESS_T)               | MK_DSTACK(DELTA_N1)),
	CODE_AND    = (OP_ALU_OP | MK_CODE(ALU_OP_T_AND_N)                | MK_DSTACK(DELTA_N1)),
	CODE_XOR    = (OP_ALU_OP | MK_CODE(ALU_OP_T_XOR_N)                | MK_DSTACK(DELTA_N1)),
	CODE_OR     = (OP_ALU_OP | MK_CODE(ALU_OP_T_OR_N)                 | MK_DSTACK(DELTA_N1)),
	CODE_DEPTH  = (OP_ALU_OP | MK_CODE(ALU_OP_DEPTH)                  | MK_DSTACK(DELTA_1)),
	CODE_T_N1   = (OP_ALU_OP | MK_CODE(ALU_OP_T_DECREMENT)),

	CODE_IEN    = (OP_ALU_OP | MK_CODE(ALU_OP_ENABLE_INTERRUPTS)      | MK_DSTACK(DELTA_N1)),
	CODE_ISIEN  = (OP_ALU_OP | MK_CODE(ALU_OP_INTERRUPTS_ENABLED)     | MK_DSTACK(DELTA_1)),
	CODE_RDEPTH = (OP_ALU_OP | MK_CODE(ALU_OP_RDEPTH)                 | MK_DSTACK(DELTA_1)),
	CODE_TE0    = (OP_ALU_OP | MK_CODE(ALU_OP_T_EQUAL_0))

} forth_alu_words_e;

typedef struct {
	size_t length;
	uint16_t *points;
} break_point_t;

typedef struct {
	uint16_t core[MAX_CORE]; /**< main memory */
	uint16_t rstk[STK_SIZE]; /**< return stack */
	uint16_t dstk[STK_SIZE]; /**< variable stack */
	uint16_t pc;  /**< program counter */
	uint16_t tos; /**< top of stack */
	uint16_t rp;  /**< return stack pointer */
	uint16_t sp;  /**< variable stack pointer */
	bool     ie;  /**< interrupt enable */

	break_point_t bp;
} h2_t; /**< state of the H2 CPU */

/** @warning LOG_FATAL level kills the program */
#define X_MACRO_LOGGING\
 X(LOG_MESSAGE_OFF,  "")\
 X(LOG_FATAL,        "fatal")\
 X(LOG_ERROR,        "error")\
 X(LOG_WARNING,      "warning")\
 X(LOG_NOTE,         "note")\
 X(LOG_DEBUG,        "debug")\
 X(LOG_ALL_MESSAGES, "any")

typedef enum {
#define X(ENUM, NAME) ENUM,
	X_MACRO_LOGGING
#undef X
} log_level_e;

static const char *log_levels[] =
{
#define X(ENUM, NAME) [ENUM] = NAME,
	X_MACRO_LOGGING
#undef X
};

static log_level_e log_level = LOG_WARNING;

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

#define fatal(FMT, ...)   logger(LOG_FATAL,   __func__, __LINE__, FMT, ##__VA_ARGS__)
#define error(FMT, ...)   logger(LOG_ERROR,   __func__, __LINE__, FMT, ##__VA_ARGS__)
#define warning(FMT, ...) logger(LOG_WARNING, __func__, __LINE__, FMT, ##__VA_ARGS__)
#define note(FMT, ...)    logger(LOG_NOTE,    __func__, __LINE__, FMT, ##__VA_ARGS__)
#define debug(FMT, ...)   logger(LOG_DEBUG,   __func__, __LINE__, FMT, ##__VA_ARGS__)

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

static void *allocate_or_die(size_t length)
{
	void *r;
	errno = 0;
	r = calloc(1, length);
	if(!r)
		fatal("allocation of size %zu failed: %s",
				length, reason());
	return r;
}

static FILE *fopen_or_die(const char *file, const char *mode)
{
	FILE *f = NULL;
	assert(file);
	assert(mode);
	errno = 0;
	f = fopen(file, mode);
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
	char *r;
	assert(str);
	errno = 0;
	r = malloc(strlen(str) + 1);
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
			error("file contains too many lines: %zu", i);
			return -1;
		}
		r = string_to_cell(16, &h->core[i], line);
		if(!r) {
			error("invalid line, expected hex string: %s", line);
			return -1;
		}
		debug("%zu %u", i, (unsigned)h->core[i]);
	}

	return 0;
}

int h2_save(h2_t *h, FILE *output, bool full)
{
	assert(h);
	assert(output);

	uint16_t max = full ? MAX_CORE : h->pc;
	for(uint16_t i = 0; i < max; i++)
		if(fprintf(output, "%04"PRIx16"\n", h->core[i]) < 0) {
			error("failed to write line: %"PRId16, i);
			return -1;
		}
	return 0;
}

#define CHAR_BACKSPACE (8)    /* ASCII backspace */
#define CHAR_ESCAPE    (27)   /* ASCII escape character value */
#define CHAR_DELETE    (127)  /* ASCII delete */

/**@todo move non-portable I/O stuff to external program that calls h2_run */
#ifdef __unix__
#include <unistd.h>
#include <termios.h>
static int getch(void)
{
	struct termios oldattr, newattr;
	int ch;
	tcgetattr(STDIN_FILENO, &oldattr);
	newattr = oldattr;
	newattr.c_iflag &= ~(ICRNL);
	newattr.c_lflag &= ~(ICANON | ECHO);

	tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
	ch = getchar();

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

static int wrap_getch(void)
{
	int ch = getch();
	/**@todo Exit to debugger if escape is hit */
	if(ch == CHAR_ESCAPE) {
		note("escape character (%d) read in - exiting", CHAR_ESCAPE);
		exit(EXIT_SUCCESS);
	}

	return ch == CHAR_DELETE ? CHAR_BACKSPACE : ch;
}

/* ========================== Utilities ==================================== */

/* ========================== Symbol Table ================================= */

typedef enum {
	SYMBOL_TYPE_LABEL,
	SYMBOL_TYPE_CALL,
	SYMBOL_TYPE_CONSTANT,
	SYMBOL_TYPE_VARIABLE,
} symbol_type_e;

typedef struct {
	symbol_type_e type;
	char *id;
	uint16_t value;
} symbol_t;

typedef struct {
	size_t length;
	symbol_t **symbols;
} symbol_table_t;

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

/**@note If this becomes too slow, the first optimization would be
 * to use a frequency sorted list */
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

static int symbol_table_add(symbol_table_t *t, symbol_type_e type, const char *id, uint16_t value, error_t *e)
{
	symbol_t *s = symbol_new(type, id, value);
	symbol_t **xs = NULL;
	assert(t);

	if(symbol_table_lookup(t, id)) {
		error("redefinition of symbol: %s", id);
		if(e)
			ethrow(e);
		else
			return -1;
	}

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
		if(fprintf(output, "%s %s %"PRId16"\n", symbol_names[s->type], s->id, s->value) < 0)
			return -1;
	}
	return 0;
}

static symbol_table_t *symbol_table_load(FILE *input)
{
	symbol_table_t *t = symbol_table_new();
	assert(input);
	char symbol[80];
	char id[256];
	uint16_t value;

	while(!feof(input)) {
		int r = 0;
		memset(symbol, 0, sizeof(symbol));
		memset(id,     0, sizeof(id));
		value = 0;
		r = fscanf(input, "%79s%255s%"SCNd16, symbol, id, &value);
		if(r != 3 && r > 0) {
			error("invalid symbol table: %d", r);
			goto fail;
		}
		if(r == 3) {
			size_t i = 0;
			for(i = 0; symbol_names[i] && strcmp(symbol_names[i], symbol); i++)
				/*do nothing*/;
			if(symbol_names[i]) {
				if(symbol_table_add(t, i, id, value, NULL) < 0)
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
	case CODE_DUP:    return "dup";
	case CODE_OVER:   return "over";
	case CODE_INVERT: return "invert";
	case CODE_ADD:    return "+";
	case CODE_SWAP:   return "swap";
	case CODE_NIP:    return "nip";
	case CODE_DROP:   return "drop";
	case CODE_EXIT:   return "exit";
	case CODE_TOR:    return ">r";
	case CODE_FROMR:  return "r>";
	case CODE_RAT:    return "r@";
	case CODE_LOAD:   return "@";
	case CODE_STORE:  return "store";
	case CODE_RSHIFT: return "rshift";
	case CODE_LSHIFT: return "lshift";
	case CODE_EQUAL:  return "equal";
	case CODE_ULESS:  return "u<";
	case CODE_LESS:   return "<";
	case CODE_AND:    return "and";
	case CODE_XOR:    return "xor";
	case CODE_OR:     return "or";
	case CODE_DEPTH:  return "depth";
	case CODE_T_N1:   return "1-";
	case CODE_IEN:    return "seti";
	case CODE_ISIEN:  return "iset?";
	case CODE_RDEPTH: return "rdepth";
	case CODE_TE0:    return "0=";
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
	case ALU_OP_ENABLE_INTERRUPTS:  return "seti";
	case ALU_OP_INTERRUPTS_ENABLED: return "iset?";
	case ALU_OP_RDEPTH:             return "rdepth";
	case ALU_OP_T_EQUAL_0:          return "0=";
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

static int disassembler_instruction(uint16_t instruction, FILE *output, symbol_table_t *symbols)
{
	int r = 0;
	unsigned short literal, address;
	char *s = NULL;
	assert(output);

	literal = instruction & 0x7FFF;
	address = instruction & 0x1FFF;

	if (IS_LITERAL(instruction))
		r = fprintf(output, "%hx", literal);
	else if (IS_ALU_OP(instruction))
		r = fprintf(output, "%s", s = disassembler_alu(instruction));
	else if (IS_CALL(instruction))
		r = fprintf(output, "call %hx %s",    address, disassemble_jump(symbols, SYMBOL_TYPE_CALL, address));
	else if (IS_0BRANCH(instruction))
		r = fprintf(output, "0branch %hx %s", address, disassemble_jump(symbols, SYMBOL_TYPE_LABEL, address));
	else if (IS_BRANCH(instruction))
		r = fprintf(output, "branch %hx %s",  address, disassemble_jump(symbols, SYMBOL_TYPE_LABEL, address));
	else
		r = fprintf(output, "?(%hx)", instruction);
	free(s);
	return r < 0 ? -1 : 0;
}

/**@todo The disassembler should decompile word headers */
int h2_disassemble(FILE *input, FILE *output, symbol_table_t *symbols)
{
	assert(input);
	assert(output);
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
			if(disassembler_instruction(instruction, output, symbols) < 0) {
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

#define CLOCK_SPEED_HZ             (100000000ULL)
#define VGA_BUFFER_LENGTH          (1 << 13)

#define TIMER_ENABLE_BIT           (15)
#define TIMER_RESET_BIT            (14)
#define TIMER_INTERRUPT_ENABLE_BIT (13)
#define TIMER_ENABLE               (1 << TIMER_ENABLE_BIT)
#define TIMER_RESET                (1 << TIMER_RESET_BIT)
#define TIMER_INTERRUPT_ENABLE     (1 << TIMER_INTERRUPT_ENABLE_BIT)

#define UART_RX_FIFO_EMPTY_BIT     (8)
#define UART_RX_FIFO_FULL_BIT      (9)
#define UART_RX_RE_BIT             (10)
#define UART_TX_FIFO_EMPTY_BIT     (11)
#define UART_TX_FIFO_FULL_BIT      (12)
#define UART_TX_WE_BIT             (13)

#define UART_RX_FIFO_EMPTY         (1 << UART_RX_FIFO_EMPTY_BIT)
#define UART_RX_FIFO_FULL          (1 << UART_RX_FIFO_FULL_BIT)
#define UART_RX_RE                 (1 << UART_RX_RE_BIT)
#define UART_TX_FIFO_EMPTY         (1 << UART_TX_FIFO_EMPTY_BIT)
#define UART_TX_FIFO_FULL          (1 << UART_TX_FIFO_FULL_BIT)
#define UART_TX_WE                 (1 << UART_TX_WE_BIT)

#define PS2_NEW_CHAR_BIT           (8)
#define PS2_NEW_CHAR               (1 << PS2_NEW_CHAR_BIT)

typedef struct {
	uint8_t leds;
	uint16_t vga_cursor;
	uint16_t vga_control;
	uint16_t vga[VGA_BUFFER_LENGTH];

	uint16_t timer_control;
	uint16_t timer;

	uint16_t irc_mask;

	uint8_t uart_getchar_register;

	uint8_t led_8_segments;

	uint8_t switches;

	bool wait;
} h2_soc_state_t;

typedef uint16_t (*h2_io_get)(h2_soc_state_t *soc, uint16_t addr);
typedef void     (*h2_io_set)(h2_soc_state_t *soc, uint16_t addr, uint16_t value);
typedef void     (*h2_io_update)(h2_soc_state_t *soc);

typedef struct {
	h2_io_get in;
	h2_io_set out;
	h2_io_update update;
	h2_soc_state_t *soc;
} h2_io_t;

/**@todo used the enumerations h2_input_addr_t and h2_output_addr_t to
 * generate constants for use with the assembler */

typedef enum {
	iUart         = 0x6000,
	iSwitches     = 0x6001,
	iTimerCtrl    = 0x6002,
	iTimerDin     = 0x6003,
	iVgaTxtDout   = 0x6004,
	iPs2          = 0x6005,
} h2_input_addr_t;

typedef enum {
	oUart         = 0x6000,
	oLeds         = 0x6001,
	oTimerCtrl    = 0x6002,
	oVgaCursor    = 0x6003,
	oVgaCtrl      = 0x6004,
	o8SegLED      = 0x6005,
	oIrcMask      = 0x6006,
} h2_output_addr_t;

static void memory_print(FILE *out, uint16_t start, uint16_t *p, uint16_t length)
{
	const uint16_t line_length = 16;
	assert(out);
	assert(p);
	for(uint16_t i = 0; i < length; i += line_length) {
		fprintf(out, "%04"PRIx16 ": ", i + start);
		for(uint16_t j = 0; j < line_length && j + i < length; j++)
			fprintf(out, "%04"PRIx16 " ", p[j + i]);
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
	size_t a;
	uint16_t *r;
	if(break_point_find(bp, point))
		return;

	a = (bp->length + 1) * sizeof(bp->points[0]);
	r = realloc(bp->points, a);
	if(!r || a < bp->length)
		fatal("realloc of size %zu failed", a);
	r[bp->length] = point;
	bp->length++;
	bp->points = r;
}

static int break_point_print(FILE *out, break_point_t *bp)
{
	for(size_t i = 0; i < bp->length; i++)
		if(fprintf(out, "\t0x%04"PRIx16 "\n", bp->points[i]) < 0)
			return -1;
	return 0;
}

#define LED_8_SEGMENT_DISPLAY_CHARSET_HEX  "0123456789AbCdEF"
#define LED_8_SEGMENT_DISPLAY_CHARSET_BCD  "0123456789 .-   "

static char l8seg(uint8_t c)
{
	static const char *v = LED_8_SEGMENT_DISPLAY_CHARSET_HEX;
	return v[c & 0xf];
}

static void soc_print(FILE *out, h2_soc_state_t *soc, bool verbose)
{
	assert(out);
	assert(soc);
	unsigned char led0 = l8seg(soc->led_8_segments >> 12);
	unsigned char led1 = l8seg(soc->led_8_segments >>  8);
	unsigned char led2 = l8seg(soc->led_8_segments >>  4);
	unsigned char led3 = l8seg(soc->led_8_segments);

	fprintf(out, "LEDS:          %02"PRIx8"\n",  soc->leds);
	fprintf(out, "VGA Cursor:    %04"PRIx16"\n", soc->vga_cursor);
	fprintf(out, "VGA Control:   %04"PRIx16"\n", soc->vga_control);
	fprintf(out, "Timer Control: %04"PRIx16"\n", soc->timer_control);
	fprintf(out, "Timer:         %04"PRIx16"\n", soc->timer);
	fprintf(out, "IRC Mask:      %04"PRIx16"\n", soc->irc_mask);
	fprintf(out, "UART Input:    %02"PRIx8"\n",  soc->uart_getchar_register);
	fprintf(out, "LED 7seg:      %c%c%c%c\n",    led0, led1, led2, led3);
	fprintf(out, "Switches:      %02"PRIx8"\n",  soc->switches);
	fprintf(out, "Waiting:       %s\n",          soc->wait ? "true" : "false");
	
	if(verbose) {
		fputs("VGA Memory:\n", out);
		memory_print(out, 0, soc->vga, VGA_BUFFER_LENGTH);
	}
}

static uint16_t h2_io_get_default(h2_soc_state_t *soc, uint16_t addr)
{
	assert(soc);
	if(log_level >= LOG_DEBUG)
		fprintf(stderr, "IO read addr: %"PRIx16"\n", addr);

	switch(addr) {
	case iUart:         return UART_TX_FIFO_EMPTY | soc->uart_getchar_register; /** @bug This does not reflect accurate timing */
	case iSwitches:     return soc->switches;
	case iTimerCtrl:    return soc->timer_control;
	case iTimerDin:     return soc->timer;
	/** @bug reading from VGA memory is broken for the moment */
	case iVgaTxtDout:   return 0; 
	case iPs2:          return PS2_NEW_CHAR | wrap_getch();
	default:
		warning("invalid read from %04"PRIx16, addr);
	}
	return 0;
}

static void h2_io_set_default(h2_soc_state_t *soc, uint16_t addr, uint16_t value)
{
	assert(soc);
	if(log_level >= LOG_DEBUG)
		fprintf(stderr, "IO write addr/value: %"PRIx16"/%"PRIx16"\n", addr, value);

	if(addr & 0x8000) {
		soc->vga[addr & 0x1FFF] = value;
		return;
	}

	switch(addr) {
	case oUart:
			if(value & UART_TX_WE)
				putch(0xFF & value);
			if(value & UART_RX_RE)
				soc->uart_getchar_register = wrap_getch();
			break;
	case oLeds:       soc->leds           = value; break;
	case oTimerCtrl:  soc->timer_control  = value; break;
	case oVgaCtrl:    soc->vga_control    = value; break;
	case oVgaCursor:  soc->vga_cursor     = value; break;
	case o8SegLED:    soc->led_8_segments = value; break;
	case oIrcMask:    soc->irc_mask       = value; break;
	default:
		warning("invalid write to %04"PRIx16 ":%04"PRIx16, addr, value);
	}
}

/**@todo allow for realistic timing of things like the UART, debounce time outs
 * and other events */
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
				/**@todo generate interrupt*/
				soc->timer = 0;
			}	
		}
	}
}

static h2_soc_state_t *h2_soc_state_new(void)
{
	return allocate_or_die(sizeof(h2_soc_state_t));
}

static void h2_soc_state_free(h2_soc_state_t *soc)
{
	if(!soc)
		return;
	memset(soc, 0, sizeof(*soc));
	free(soc);
}

static h2_io_t *h2_io_new(void)
{
	h2_io_t *io =  allocate_or_die(sizeof(*io));
	io->in      = h2_io_get_default;
	io->out     = h2_io_set_default;
	io->update  = h2_io_update_default;
	io->soc     = h2_soc_state_new();
	return io;
}

static void h2_io_free(h2_io_t *io)
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
	uint16_t r;
	assert(h);
	r = h->tos;
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

static int trace(FILE *output, uint16_t instruction, symbol_table_t *symbols, const char *fmt, ...)
{
	int r = 0;
	va_list ap;
	assert(output);
	if(!output)
		return r;
	assert(fmt);
	va_start(ap, fmt);
	r = vfprintf(output, fmt, ap);
	va_end(ap);
	if(r < 0)
		return r;
	if(fputc('\t', output) != '\t')
		return -1;
	r = disassembler_instruction(instruction, output, symbols);
	if(r < 0)
		return r;
	if(fputc('\n', output) != '\n')
		return -1;
	if(fflush(output) == EOF)
		return -1;
	return r;
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
	memory_print(out, 0, h->rstk, STK_SIZE);
	fputs("Variable Stack:\n", out);
	fprintf(out, "tos:  %04"PRIx16"\n", h->tos);
	memory_print(out, 1, h->dstk, STK_SIZE);
	
	fprintf(out, "pc:   %04"PRIx16"\n", h->pc);
	fprintf(out, "rp:   %04"PRIx16"\n", h->rp);
	fprintf(out, "dp:   %04"PRIx16"\n", h->sp);
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

/** @todo implement all these instructions and more from
 * http://thestarman.pcministry.com/asm/debug/debug.htm as
 * well as push/pop numbers, and call (not just jump) */
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
	{ .cmd = 'w', .argc = 2, .arg1 = DBG_CMD_NUMBER, .arg2 = DBG_CMD_NUMBER, .description = "set value              " },
	{ .cmd = 'y', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "list symbols           " },
	{ .cmd = '.', .argc = 0, .arg1 = DBG_CMD_NO_ARG, .arg2 = DBG_CMD_NO_ARG, .description = "print H2 CPU state     " },
	{ .cmd = -1,  .argc = 0, .arg1 = DBG_CMD_EITHER, .arg2 = DBG_CMD_NO_ARG, .description = NULL },
};

static void debug_command_print_help(FILE *out, const debug_command_t *dc)
{
	static const char *arg_type[] = { 
		[DBG_CMD_NO_ARG] = "             ",
		[DBG_CMD_NUMBER] = "number       ",
		[DBG_CMD_STRING] = "string       ",
		[DBG_CMD_EITHER] = "number/string"
	};
	for(unsigned i = 0; dc[i].cmd != -1; i++)
		fprintf(out, " %c %s\t%d\t%s %s\n", dc[i].cmd, dc[i].description, dc[i].argc, arg_type[dc[i].arg1], arg_type[dc[i].arg2]);
}

static int debug_command_check(FILE *out, const debug_command_t *dc, int cmd, int argc, bool is_numeric1, bool is_numeric2)
{
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
	symbol_t *sym;
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

/**@todo If a breakpoint is set on a location a load or store could to
 * that location should also trigger a break */
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
			fputs("End of Input\n", ds->output);
			return -1;
		}

		argc = sscanf(line, "%256s %256s %256s", op, arg1, arg2);
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
		case 'f':
			fprintf(ds->output, "command '%c' not implemented yet!\n", op[0]);
			break;

		case 'd':
			if(num1 & 0x8000) { /* VGA memory */
				if((long)(num1 & 0x1FFF) + (long)num2 > VGA_BUFFER_LENGTH) {
					fprintf(ds->output, "overflow in VGA dump\n");
					break;
				}

				if(io)
					memory_print(ds->output, num1, io->soc->vga + (num1 & 0x1FFF), num2);
				else
					fprintf(ds->output, "I/O unavailable\n");
			} else { /* RAM */

				if(((long)num1 + (long)num2) > MAX_CORE)
					fprintf(ds->output, "overflow in RAM dump\n");
				else
					memory_print(ds->output, num1, h->core + num1, num2);

			}
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
		case '.':
			h2_print(ds->output, h);
			break;

		case 'w':
			if(num1 >= MAX_CORE) {
				fprintf(ds->output, "invalid write\n");
				break;
			}
			h->core[num1] = num2;
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
				/**@todo reverse lookup on all labels */
				fprintf(ds->output, "%04"PRIx16 ":\t", i);
				disassembler_instruction(h->core[i], ds->output, symbols);
				fputc('\n', ds->output);
			}
			break;

		case 'o':
			if(!io) {
				fprintf(ds->output, "I/O unavailable\n");
				break;
			}
			io->out(io->soc, num1, num2);

			break;

		case 'i':
			if(!io) {
				fprintf(ds->output, "I/O unavailable\n");
				break;
			}
			fprintf(ds->output, "read: %"PRIx16"\n", io->in(io->soc, num1));
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
		case 'p':
			if(io)
				soc_print(ds->output, io->soc, log_level >= LOG_DEBUG);
			else
				fprintf(ds->output, "I/O unavailable\n");
			break;
		case EOF:
			/* fall through */
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

/**@todo rewrite so variables used within here mirror the VHDL */
int h2_run(h2_t *h, h2_io_t *io, FILE *output, unsigned steps, symbol_table_t *symbols, bool run_debugger)
{
	assert(h);
	debug_state_t ds = { .input = stdin, .output = stderr, .step = run_debugger, .trace_on = run_debugger };

	if(run_debugger)
		fputs("Debugger running, type 'h' for a list of command\n", ds.output);

	for(unsigned i = 0; i < steps || steps == 0 || run_debugger; i++) {
		uint16_t instruction,
			 literal,
			 address,
			 pc_plus_one;

		if(run_debugger)
			if(h2_debugger(&ds, h, io, symbols, h->pc))
				return 0;

		if(io)
			io->update(io->soc);

		if(io && io->soc->wait) /* wait only applies to the H2 core not the rest of the SoC */
			continue;

		if(h->pc >= MAX_CORE) {
			error("invalid program counter: %04x > %04x", (unsigned)h->pc, MAX_CORE);
			return -1;
		}
		instruction = h->core[h->pc];

		literal = instruction & 0x7FFF;
		address = instruction & 0x1FFF; /* NB. also used for ALU OP */

		/**@todo interrupt handling */
		pc_plus_one = (h->pc + 1) % MAX_CORE;

		if(log_level >= LOG_DEBUG || ds.trace_on)
			trace(output, instruction, symbols,
				"%04u: pc(%04x) inst(%04x) sp(%x) rp(%x) tos(%04x) r(%04x)",
				i,
				(unsigned)h->pc,
				(unsigned)instruction,
				(unsigned)h->sp,
				(unsigned)h->rp,
				(unsigned)h->tos,
				(unsigned)h->rstk[h->rp % STK_SIZE]);


		/* decode / execute */
		if(IS_LITERAL(instruction)) {
			dpush(h, literal);
			h->pc = pc_plus_one;
		} else if (IS_ALU_OP(instruction)) {
			uint16_t rd  = stack_delta(RSTACK(instruction));
			uint16_t dd  = stack_delta(DSTACK(instruction));
			uint16_t nos = h->dstk[h->sp % STK_SIZE];
			uint16_t tos = h->tos;
			uint16_t npc = pc_plus_one;

			if(instruction & R_TO_PC)
				npc = h->rstk[h->rp % STK_SIZE];

			switch(ALU_OP(instruction)) {
			case ALU_OP_T:           /* tos = tos; */ break;
			case ALU_OP_N:           tos = nos; break;
			case ALU_OP_T_PLUS_N:    tos += nos; break;
			case ALU_OP_T_AND_N:     tos &= nos; break;
			case ALU_OP_T_OR_N:      tos |= nos; break;
			case ALU_OP_T_XOR_N:     tos ^= nos; break;
			case ALU_OP_T_INVERT:    tos = ~tos; break;
			case ALU_OP_T_EQUAL_N:   tos = -(tos == nos); break;
			case ALU_OP_N_LESS_T:    tos = -((int16_t)nos < (int16_t)tos); break;
			case ALU_OP_N_RSHIFT_T:  tos = nos >> tos; break;
			case ALU_OP_T_DECREMENT: tos--; break;
			case ALU_OP_R:           tos = h->rstk[h->rp % STK_SIZE]; break;
			case ALU_OP_T_LOAD:

				/* if(run_debugger)
					if(h2_debugger(&ds, h, io, symbols, h->tos))
						return 0; */

				if(h->tos & 0x6000) {
					if(io)
						tos = io->in(io->soc, h->tos);
					else
						warning("I/O read attempted on addr: %"PRIx16, h->tos);
				} else {
					/**@note The lowest bit is not used in the address for memory reads */
					tos = h->core[(h->tos >> 1) % MAX_CORE];
				}
				break;
			case ALU_OP_N_LSHIFT_T: tos = nos << tos; break;
			case ALU_OP_DEPTH:      tos = h->sp; break;
			case ALU_OP_N_ULESS_T:  tos = -(nos < tos); break;
			case ALU_OP_ENABLE_INTERRUPTS: h->ie = tos & 1; break;
			case ALU_OP_INTERRUPTS_ENABLED: tos = -h->ie; break;
			case ALU_OP_RDEPTH:     tos = h->rp; break;
			case ALU_OP_T_EQUAL_0:  tos = -(tos == 0); break;
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

			/** @bug A read operation with ALU_OP_T_LOAD should
			 * stop a write from happening - except for VGA memory
			 * writes, for the moment. */
			if(instruction & N_TO_ADDR_T) {
				/* if(run_debugger)
					if(h2_debugger(&ds, h, io, symbols, h->tos))
						return 0; */

				if(h->tos & 0x6000) {
					if(io)
						io->out(io->soc, h->tos, nos);
					else
						warning("I/O write attempted with addr/value: %"PRIx16 "/%"PRIx16, tos, nos);
				} else {
					/**@note The lowest bit is not used in the address for memory writes */
					h->core[(h->tos >> 1) % MAX_CORE] = nos;
				}
			}

			h->tos = tos;
			h->pc = npc;
		} else if (IS_CALL(instruction)) {
			rpush(h, pc_plus_one);
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
	LEX_COMPILE,
	LEX_VARIABLE,

	LEX_ISR,
	LEX_SET,
	LEX_PC,
	LEX_BREAK,
	LEX_MODE,
	LEX_ALLOCATE,

	LEX_DUP,   /* start of instructions */
	LEX_OVER,
	LEX_INVERT,
	LEX_ADD,
	LEX_SWAP,
	LEX_NIP,
	LEX_DROP,
	LEX_EXIT,
	LEX_TOR,
	LEX_FROMR,
	LEX_RAT,
	LEX_LOAD,
	LEX_STORE,
	LEX_RSHIFT,
	LEX_LSHIFT,
	LEX_EQUAL,
	LEX_ULESS,
	LEX_LESS,
	LEX_AND,
	LEX_XOR,
	LEX_OR,
	LEX_DEPTH,
	LEX_T_N1,
	LEX_IEN,
	LEX_ISIEN,
	LEX_RDEPTH,
	LEX_TE0,
	/* end of named tokens and instructions */

	LEX_ERROR, /* error token: this needs to be after the named tokens */

	LEX_EOI = EOF
} token_e;

static const char *keywords[] =
{
	[LEX_CONSTANT]  =  "constant",
	[LEX_CALL]      =  "call",
	[LEX_BRANCH]    =  "branch",
	[LEX_0BRANCH]   =  "0branch",
	[LEX_BEGIN]     =  "begin",
	[LEX_WHILE]     =  "while",
	[LEX_REPEAT]    =  "repeat",
	[LEX_AGAIN]     =  "again",
	[LEX_UNTIL]     =  "until",
	[LEX_FOR]       =  "for",
	[LEX_AFT]       =  "aft",
	[LEX_NEXT]      =  "next",
	[LEX_IF]        =  "if",
	[LEX_ELSE]      =  "else",
	[LEX_THEN]      =  "then",
	[LEX_DEFINE]    =  ":",
	[LEX_ENDDEFINE] =  ";",
	[LEX_CHAR]      =  "[char]",
	[LEX_COMPILE]   =  ",",
	[LEX_VARIABLE]  =  "variable",
	[LEX_ISR]       =  ".isr",
	[LEX_SET]       =  ".set",
	[LEX_PC]        =  ".pc",
	[LEX_BREAK]     =  ".break",
	[LEX_MODE]      =  ".mode",
	[LEX_ALLOCATE]  =  ".allocate",
	[LEX_DUP]       =  "dup",
	[LEX_OVER]      =  "over",
	[LEX_INVERT]    =  "invert",
	[LEX_ADD]       =  "+",
	[LEX_SWAP]      =  "swap",
	[LEX_NIP]       =  "nip",
	[LEX_DROP]      =  "drop",
	[LEX_EXIT]      =  "exit",
	[LEX_TOR]       =  ">r",
	[LEX_FROMR]     =  "r>",
	[LEX_RAT]       =  "r@",
	[LEX_LOAD]      =  "@",
	[LEX_STORE]     =  "store",
	[LEX_RSHIFT]    =  "rshift",
	[LEX_LSHIFT]    =  "lshift",
	[LEX_EQUAL]     =  "=",
	[LEX_ULESS]     =  "u<",
	[LEX_LESS]      =  "<",
	[LEX_AND]       =  "and",
	[LEX_XOR]       =  "xor",
	[LEX_OR]        =  "or",
	[LEX_DEPTH]     =  "depth",
	[LEX_T_N1]      =  "1-",
	[LEX_IEN]       =  "seti",
	[LEX_ISIEN]     =  "iset?",
	[LEX_RDEPTH]    =  "rdepth",
	[LEX_TE0]       =  "0=",
	[LEX_ERROR]     =  NULL,
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
	if(t->type == LEX_IDENTIFIER || t->type == LEX_STRING)
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
	memset(l, 0, sizeof(*l));
	free(l);
}

static int token_print(token_t *t, FILE *output, unsigned depth)
{
	token_e type;
	int r = 0;
	if(!t)
		return 0;
	indent(output, ' ', depth);
	type = t->type;
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
	assert(base == 8 || base == 10 || base == 16);
	if(base == 8)
		return isdigit(c) && c < '8';
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

	/**@bug this does not quite work correctly, it accepts
	 * things as numbers that is should not */
	if(s[i] == '0') {
		base = 8;
		/* if(!numeric(s[i+1], base))
			return 0; */
		if(s[i+1] == 'x' || s[i+1] == 'X') {
			base = 16;
			if(s[i+2] == '\0')
				return 0;
			start = i+2;
		}
	}

	for(i = start; i < length; i++)
		if(!numeric(s[i], base))
			return 0;

	for(i = start; i < length; i++)
		out = out * base + map_char_to_number(s[i]);

	*o = negate ? out * -1 : out;
	return 1;
}

/** @bug There is a minor memory leak in the lexer: tokens are lost somewhere */
static void lexer(lexer_t *l)
{
	size_t i;
	int ch;
	token_e sym;
	uint16_t lit = 0;
	assert(l);
	ch = next_char(l);
	l->token = token_new(LEX_ERROR, l->line);

again:
	switch(ch) {
	case '\n':
		l->line++;
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

/** @todo Each statement here could be turned into a string and printed out
 * when an error occurs, or all of them printed out as a help message.
 *
 * Grammar:
 *
 * Program     := Statement* EOF
 * Statement   :=   Label | Branch | 0Branch | Call | Literal | Instruction
 *                | Identifier | Constant | Variable | Definition | If
 *                | Begin | Char | Set | Pc | Break | Mode | String
 * Label       := Identifier ";"
 * Branch      := "branch"  ( Identifier | Literal )
 * 0Branch     := "0branch" ( Identifier | Literal )
 * Call        := "call"    ( Identifier | Literal )
 * Set         := ".set"    ( Identifier | Literal ) ( Identifier | Literal )
 * Pc          := ".pc"     ( Identifier | Literal )
 * Break       := ".break"
 * Mode        := ".mode"      Literal
 * Allocate    := ".allocate" ( Identifier | Literal )
 * Constant    := "constant" Identifier Literal
 * Variable    := "variable" Identifier ( Literal | String )
 * Instruction := "@" | "store" | "exit" | ...
 * Definition  := ":" Statement* ";"
 * If          := "if" Statement* [ "else" ] Statement* "then"
 * Begin       := "begin" Statement* ("until" | "again" | "while" Statement* "repeat")
 * For         := "for"   Statement* ("aft" Statement* "then" Statement* | "next")
 * Isr         := "isr" Identifier (Identifier | Literal)
 * Literal     := [ "-" ] Number
 * String      := '"' SChar* '"'
 * Char        := "char" ASCII ","
 * Number      := Octal | Hex | Decimal
 * Octal       := "0" ... "7"
 * Decimal     := "1" ... "9" ( "0" ... "9" )*
 * Hex         := ( "x" | "X" ) HexDigit HexDigit*
 * HexDigit    := ( "a" ... "f" | "A" ... "F" )
 * SChar       := Any character except quote
 *
 * NB. Literals have higher priority than Identifiers, and comments are '\'
 * until a new line is encountered, or '(' until a ')' is encountered.
 *
 * The following looping constructs need to be implemented:
 *
 * Begin ... While ... Repeat
 * For   ... Next
 * For   ... Aft   ... Then ... Next
 * :     ... Immediate?
 *
 * The grammar allows for nested word definitions, however state is held in the
 * lexer to prevent this.
 *
 * Add words:
 *
 * 	_start    - set start routine
 * 	include   - include a file
 * 	[char]    - compile a character literal
 * 	:compile  - compile values into dictionary
 * 	begin...again
 * 	never...then
 *
 **/


#define XMACRO_PARSE\
	X(SYM_PROGRAM,             "program")\
	X(SYM_STATEMENTS,          "statements")\
	X(SYM_LABEL,               "label")\
	X(SYM_BRANCH,              "branch")\
	X(SYM_0BRANCH,             "0branch")\
	X(SYM_CALL,                "call")\
	X(SYM_CONSTANT,            "constant")\
	X(SYM_VARIABLE,            "variable")\
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
	X(SYM_CHAR,                "char")\
	X(SYM_ISR,                 "isr")\
	X(SYM_SET,                 "set")\
	X(SYM_PC,                  "pc")\
	X(SYM_BREAK,               "break")\
	X(SYM_MODE,                "mode")\
	X(SYM_ALLOCATE,            "allocate")\
	X(SYM_CALL_DEFINITION,     "call-definition")

typedef enum {
#define X(ENUM, NAME) ENUM,
	XMACRO_PARSE
#undef X
} parse_e;

static const char *names[] = {
#define X(ENUM, NAME) [ENUM] = NAME,
	XMACRO_PARSE
#undef X
	NULL
};

typedef struct node_t  {
	parse_e type;
	size_t length;
	token_t *token, *value;
	struct node_t *o[];
} node_t;

static node_t *node_new(lexer_t *l, parse_e type, size_t size)
{
	node_t *r = allocate_or_die(sizeof(*r) + sizeof(r->o[0]) * size);
	assert(l);
	if(log_level >= LOG_DEBUG)
		fprintf(stderr, "node> %s\n", names[type]);
	r->length = size;
	r->type = type;
	return r;
}

static node_t *node_grow(lexer_t *l, node_t *n)
{
	node_t *r = NULL;
	assert(l);
	assert(n);
	errno = 0;
	r = realloc(n, sizeof(*n) + (sizeof(n->o[0]) * (n->length + 1)));
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

static int accept(lexer_t *l, token_e sym)
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
		if(accept(l, i))
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
{ /**@todo improve this function */
	assert(output);
	return fprintf(stderr, "%u", sym);
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
	if(accept(l, token))
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
	node_t *r;
	assert(l);
	r = node_new(l, type, 0);
	use(l, r);
	return r;
}

static node_t *variable_or_constant(lexer_t *l, bool variable)
{
	node_t *r;
	assert(l);
	r = node_new(l, variable ? SYM_VARIABLE : SYM_CONSTANT, 1);
	expect(l, LEX_IDENTIFIER);
	use(l, r);
	if(accept(l, LEX_LITERAL)) {
		r->o[0] = defined_by_token(l, SYM_LITERAL);
	} else {
		expect(l, LEX_STRING);
		r->o[0] = defined_by_token(l, SYM_STRING);
	}
	return r;
}

static node_t *jump(lexer_t *l, parse_e type)
{
	node_t *r;
	assert(l);
	r = node_new(l, type, 0);
	(void)(accept(l, LEX_LITERAL) || expect(l, LEX_IDENTIFIER));
	use(l, r);
	return r;
}

static node_t *statements(lexer_t *l);

static node_t *for_next(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_FOR_NEXT, 1);
	r->o[0] = statements(l);
	if(accept(l, LEX_AFT)) {
		r->type = SYM_FOR_AFT_THEN_NEXT;
		r = node_grow(l, r);
		r->o[1] = statements(l);
		r = node_grow(l, r);
		expect(l, LEX_THEN);
		r->o[2] = statements(l);
	} 
	expect(l, LEX_NEXT);
	return r;
}

static node_t *begin(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_BEGIN_UNTIL, 1);
	r->o[0] = statements(l);
	if(accept(l, LEX_AGAIN)) {
		r->type = SYM_BEGIN_AGAIN;
	} else if(accept(l, LEX_WHILE)) {
		r->type = SYM_BEGIN_WHILE_REPEAT;
		r = node_grow(l, r);
		r->o[1] = statements(l);
		expect(l, LEX_REPEAT);
	} else {
		expect(l, LEX_UNTIL);
	}
	return r;
}

static node_t *if1(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_IF1, 2);
	r->o[0] = statements(l);
	if(accept(l, LEX_ELSE))
		r->o[1] = statements(l);
	expect(l, LEX_THEN);
	return r;
}

static node_t *define(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_DEFINITION, 1);
	expect(l, LEX_IDENTIFIER);
	use(l, r);
	r->o[0] = statements(l);
	expect(l, LEX_ENDDEFINE);
	return r;
}

static node_t *char_compile(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_CHAR, 0);
	expect(l, LEX_IDENTIFIER);
	use(l, r);
	if(strlen(r->token->p.id) > 1)
		syntax_error(l, "expected single character, got identifier: %s", r->token->p.id);
	expect(l, LEX_COMPILE);
	return r;
}

static node_t *isr(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_ISR, 0);
	expect(l, LEX_IDENTIFIER);
	use(l, r);
	if(!accept(l, LEX_IDENTIFIER))
		expect(l, LEX_LITERAL);
	use(l, r);
	return r;
}

static node_t *mode(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_MODE, 0);
	expect(l, LEX_LITERAL);
	use(l, r);
	return r;
}

static node_t *pc(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_PC, 0);
	if(!accept(l, LEX_LITERAL))
		expect(l, LEX_IDENTIFIER);
	use(l, r);
	return r;
}

static node_t *set(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_SET, 0);
	if(!accept(l, LEX_IDENTIFIER))
		expect(l, LEX_LITERAL);
	use(l, r);
	if(!accept(l, LEX_IDENTIFIER))
		expect(l, LEX_LITERAL);
	use(l, r);
	return r;
}

static node_t *allocate(lexer_t *l)
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_ALLOCATE, 0);
	if(!accept(l, LEX_IDENTIFIER))
		expect(l, LEX_LITERAL);
	use(l, r);
	return r;
}

static node_t *statements(lexer_t *l)
{
	node_t *r;
	size_t i = 0;
	assert(l);
	r = node_new(l, SYM_STATEMENTS, 2);
again:
	r = node_grow(l, r);
	if(accept(l, LEX_CALL)) {
		r->o[i++] = jump(l, SYM_CALL);
		goto again;
	} else if(accept(l, LEX_BRANCH)) {
		r->o[i++] = jump(l, SYM_BRANCH);
		goto again;
	} else if(accept(l, LEX_0BRANCH)) {
		r->o[i++] = jump(l, SYM_0BRANCH);
		goto again;
	} else if(accept(l, LEX_LITERAL)) {
		r->o[i++] = defined_by_token(l, SYM_LITERAL);
		goto again;
	} else if(accept(l, LEX_LABEL)) {
		r->o[i++] = defined_by_token(l, SYM_LABEL);
		goto again;
	} else if(accept(l, LEX_CONSTANT)) {
		r->o[i++] = variable_or_constant(l, false);
		goto again;
	} else if(accept(l, LEX_VARIABLE)) {
		r->o[i++] = variable_or_constant(l, true);
		goto again;
	} else if(accept(l, LEX_IF)) {
		r->o[i++] = if1(l);
		goto again;
	} else if(accept(l, LEX_DEFINE)) {
		r->o[i++] = define(l);
		goto again;
	} else if(accept(l, LEX_CHAR)) {
		r->o[i++] = char_compile(l);
		goto again;
	} else if(accept(l, LEX_BEGIN)) {
		r->o[i++] = begin(l);
		goto again;
	} else if(accept(l, LEX_FOR)) {
		r->o[i++] = for_next(l);
		goto again;
	} else if(accept(l, LEX_IDENTIFIER)) {
		r->o[i++] = defined_by_token(l, SYM_CALL_DEFINITION);
		goto again;
	} else if(accept(l, LEX_ISR)) {
		r->o[i++] = isr(l);
		goto again;
	} else if(accept(l, LEX_SET)) {
		r->o[i++] = set(l);
		goto again;
	} else if(accept(l, LEX_PC)) {
		r->o[i++] = pc(l);
		goto again;
	} else if(accept(l, LEX_BREAK)) {
		r->o[i++] = defined_by_token(l, SYM_BREAK);
		goto again;
	} else if(accept(l, LEX_MODE)) {
		r->o[i++] = mode(l);
		goto again;
	} else if(accept(l, LEX_ALLOCATE)) {
		r->o[i++] = allocate(l);
		goto again;
	} else if(accept_range(l, LEX_DUP, LEX_TE0)) {
		r->o[i++] = defined_by_token(l, SYM_INSTRUCTION);
		goto again;
	}
	return r;
}

static node_t *program(lexer_t *l) /* block ( "." | EOF ) */
{
	node_t *r;
	assert(l);
	r = node_new(l, SYM_PROGRAM, 1);
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
		/**@bug leaks parsed nodes */
		return NULL;
	}
	node_t *n = program(l);
	lexer_free(l);
	return n;
}

/********* CODE ***********/

typedef enum {
	MODE_NORMAL = 0,
	MODE_COMPILE_WORD_HEADER = 1,
} assembler_mode_e;

typedef struct {
	bool in_definition;
	bool start_defined;
	uint16_t start;
	uint16_t mode;
	uint16_t pwd; /* previous word register */
} assembler_t;

static void generate(h2_t *h, uint16_t instruction)
{
	assert(h);
	if(log_level >= LOG_DEBUG)
		fprintf(stderr, "%"PRIx16":\t%"PRIx16"\n", h->pc, instruction);
	h->core[h->pc++] = instruction;
}

static uint16_t here(h2_t *h)
{
	assert(h);
	assert(h->pc < MAX_CORE);
	return h->pc;
}

static uint16_t hole(h2_t *h)
{
	assert(h);
	assert(h->pc < MAX_CORE);
	return h->pc++;
}

static void fix(h2_t *h, uint16_t hole, uint16_t patch)
{
	assert(h);
	assert(hole < MAX_CORE);
	h->core[hole] = patch;
}

/**@todo print line number of error */
#define assembly_error(ERROR, FMT, ...) do{ error(FMT, ##__VA_ARGS__); ethrow(e); }while(0)

static void generate_jump(h2_t *h, symbol_table_t *t, token_t *tok, parse_e type, error_t *e)
{
	uint16_t or = 0;
	uint16_t addr = 0;
	symbol_t *s;
	assert(h);
	assert(t);

	if(tok->type == LEX_IDENTIFIER) {
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
	generate(h, or | addr);
}

static void generate_literal(h2_t *h, uint16_t number)
{
	if(number & OP_LITERAL) {
		number = ~number;
		generate(h, OP_LITERAL | number);
		generate(h, CODE_INVERT);
	} else {
		generate(h, OP_LITERAL | number);
	}
}

static uint16_t lexer_to_alu_op(token_e t)
{
	assert(t >= LEX_DUP && t <= LEX_TE0);
	switch(t) {
	case LEX_DUP:    return CODE_DUP;
	case LEX_OVER:   return CODE_OVER;
	case LEX_INVERT: return CODE_INVERT;
	case LEX_ADD:    return CODE_ADD;
	case LEX_SWAP:   return CODE_SWAP;
	case LEX_NIP:    return CODE_NIP;
	case LEX_DROP:   return CODE_DROP;
	case LEX_EXIT:   return CODE_EXIT;
	case LEX_TOR:    return CODE_TOR;
	case LEX_FROMR:  return CODE_FROMR;
	case LEX_RAT:    return CODE_RAT;
	case LEX_LOAD:   return CODE_LOAD;
	case LEX_STORE:  return CODE_STORE;
	case LEX_RSHIFT: return CODE_RSHIFT;
	case LEX_LSHIFT: return CODE_LSHIFT;
	case LEX_EQUAL:  return CODE_EQUAL;
	case LEX_ULESS:  return CODE_ULESS;
	case LEX_LESS:   return CODE_LESS;
	case LEX_AND:    return CODE_AND;
	case LEX_XOR:    return CODE_XOR;
	case LEX_OR:     return CODE_OR;
	case LEX_DEPTH:  return CODE_DEPTH;
	case LEX_T_N1:   return CODE_T_N1;
	case LEX_IEN:    return CODE_IEN;
	case LEX_ISIEN:  return CODE_ISIEN;
	case LEX_RDEPTH: return CODE_RDEPTH;
	case LEX_TE0:    return CODE_TE0;

	default: fatal("invalid ALU operation: %u", t);
	}
	return 0;
}

static uint16_t literal_or_symbol_lookup(error_t *e, token_t *token, symbol_table_t *t)
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

static uint16_t pack_string(h2_t *h, const char *s, error_t *e)
{
	assert(h);
	assert(s);
	size_t l = strlen(s);
	size_t i = 0;
	uint16_t r = h->pc;
	if(l > 255)
		assembly_error(e, "string \"%s\" is too large (%zu > 255)", s, l);
	h->core[hole(h)] = pack_16(l, s[0]);
	for(i = 1; i < l; i += 2)
		h->core[hole(h)] = pack_16(s[i], s[i+1]);
	if(i < l)
		h->core[hole(h)] = pack_16(s[i], 0);
	return r;
}

/**@todo define various special variables that the programmer can use:
 * such as $pwd for the Previous Word Register and $pc for the program
 * counter. "$pwd" will require word header compilation to be used. 
 * @todo Make constructs aware of they are in a Forth definition or
 * not */
static void assemble(h2_t *h, assembler_t *a, node_t *n, symbol_table_t *t, error_t *e)
{
	uint16_t hole1, hole2;
	assert(h);
	assert(t);
	assert(e);
	static const char *start_symbol = "start";

	if(!n)
		return;

	if(h->pc > MAX_CORE)
		assembly_error(e, "PC/Dictionary overflow: %"PRId16, h->pc);

	switch(n->type) {
	case SYM_PROGRAM:
	{
		/**@todo Remove special cased start symbol and replace with
		 * assembler directives */
		symbol_t *s;
		uint16_t start = hole(h);
		assemble(h, a, n->o[0], t, e);
		if(!(s = symbol_table_lookup(t, start_symbol)))
			assembly_error(e, "unable to locate start symbol: %s", start_symbol);
		if(s->type != SYMBOL_TYPE_LABEL)
			assembly_error(e, "start symbol '%s' can only be a label", start_symbol);
		fix(h, start, OP_BRANCH | s->value);
		break;
	}
	case SYM_STATEMENTS:
		for(size_t i = 0; i < n->length; i++)
			assemble(h, a, n->o[i], t, e);
		break;
	case SYM_LABEL:
		symbol_table_add(t, SYMBOL_TYPE_LABEL, n->token->p.id, here(h), e);
		break;
	case SYM_BRANCH:
	case SYM_0BRANCH:
	case SYM_CALL:
		generate_jump(h, t, n->token, n->type, e);
		break;
	case SYM_CONSTANT:
		symbol_table_add(t, SYMBOL_TYPE_CONSTANT, n->token->p.id, n->o[0]->token->p.number, e);
		break;
	case SYM_VARIABLE:
		if(n->o[0]->token->type == LEX_LITERAL) {
			hole1 = hole(h);
			fix(h, hole1, n->o[0]->token->p.number);
		} else {
			assert(n->o[0]->token->type == LEX_STRING);
			hole1 = pack_string(h, n->o[0]->token->p.id, e);
		}
		/**@note The lowest bit of the address for memory loads is
		 * discarded. */
		symbol_table_add(t, SYMBOL_TYPE_VARIABLE, n->token->p.id, hole1 << 1, e);
		break;
	case SYM_LITERAL:
		generate_literal(h, n->token->p.number);
		break;
	case SYM_INSTRUCTION:
		generate(h, lexer_to_alu_op(n->token->type));
		break;
	case SYM_BEGIN_AGAIN: /* fall through */
	case SYM_BEGIN_UNTIL:
		hole1 = here(h);
		assemble(h, a, n->o[0], t, e);
		generate(h, (n->type == SYM_BEGIN_AGAIN ? OP_BRANCH : OP_0BRANCH) | hole1);
		break;
	
	case SYM_FOR_NEXT:
	/* The code for for...next loops can be reduced in size with by generating
	    complex words that mess with the stack, this is an optimization that should
	    be considered. Also of note is the fact that the for...next loop is nearly
	    identical to the begin...while...repeat loop, just with inserted instructions
	    to perform the task */
		generate(h, CODE_TOR);
		hole1 = here(h);
		assemble(h, a, n->o[0], t, e);
		generate(h, CODE_RAT);
		hole2 = hole(h);
		generate(h, CODE_FROMR);
		generate(h, CODE_T_N1);
		generate(h, CODE_TOR);
		generate(h, OP_BRANCH | hole1);
		fix(h, hole2, OP_0BRANCH | here(h));
		generate(h, CODE_FROMR);
		generate(h, CODE_DROP);
		break;
	case SYM_FOR_AFT_THEN_NEXT:
		assembly_error(e, "not implemented");
	case SYM_BEGIN_WHILE_REPEAT:
		hole1 = here(h);
		assemble(h, a, n->o[0], t, e);
		hole2 = hole(h);
		assemble(h, a, n->o[1], t, e);
		generate(h, OP_BRANCH  | hole1);
		fix(h, hole2, OP_0BRANCH | here(h));
		break;
	case SYM_IF1:
		hole1 = hole(h);
		assemble(h, a, n->o[0], t, e);
		if(n->o[1]) { /* if ... else .. then */
			hole2 = hole(h);
			fix(h, hole1, OP_0BRANCH | (hole2 + 1));
			assemble(h, a, n->o[1], t, e);
			fix(h, hole2, OP_BRANCH  | here(h));
		} else { /* if ... then */
			fix(h, hole1, OP_0BRANCH | here(h));
		}
		break;
	case SYM_CALL_DEFINITION:
	{
		symbol_t *s = symbol_table_lookup(t, n->token->p.id);
		if(!s)
			assembly_error(e, "not a constant or a defined procedure: %s", n->token->p.id);
		if(s->type == SYMBOL_TYPE_CALL) {
			generate(h, OP_CALL | s->value);
		} else if(s->type == SYMBOL_TYPE_CONSTANT || s->type == SYMBOL_TYPE_VARIABLE) {
			generate_literal(h, s->value);
		} else {
			error("can only call or push literal: %s", s->id);
			ethrow(e);
		}
		break;
	}
	case SYM_DEFINITION:
		/**@todo Add mode bits field to word header (for immediate and
		 * hidden words: They could be added into the PWD field as
		 * the highest bits are not a valid RAM address */
		if(a->mode & MODE_COMPILE_WORD_HEADER) {
			hole1 = hole(h);
			fix(h, hole1, a->pwd);
			a->pwd = hole1 << 1;
			pack_string(h, n->token->p.id, e);
		}

		symbol_table_add(t, SYMBOL_TYPE_CALL, n->token->p.id, here(h), e);
		if(a->in_definition)
			assembly_error(e, "nested word definition is not allowed");
		a->in_definition = true;
		assemble(h, a, n->o[0], t, e);
		generate(h, CODE_EXIT); /**@todo smush this with the previous instruction if possible*/
		a->in_definition = false;
		break;
	case SYM_CHAR: /* char A , */
		generate(h, OP_LITERAL | n->token->p.id[0]);
		break;
	case SYM_ISR:
	{
		symbol_t *s = symbol_table_lookup(t, n->token->p.id);
		uint16_t fixme = 0, fixval = 0;

		if(!s || s->type != SYMBOL_TYPE_CALL)
			assembly_error(e, "not a function call/defined symbol: %s", n->token->p.id);
		if(n->value->type == LEX_IDENTIFIER) {
			symbol_t *l = symbol_table_lookup(t, n->value->p.id);
			if(!l || l->type != SYMBOL_TYPE_CONSTANT)
				assembly_error(e, "identifier is not a constant: %s", n->value->p.id);
			fixme  = l->value;
			fixval = s->value;
		} else {
			assert(n->value->type == LEX_LITERAL);
			fixme  = n->value->p.number;
			fixval = s->value;
		}
		if(fixme >= NUMBER_OF_INTERRUPTS)
			assembly_error(e, "invalid interrupt number: %"PRId16, fixme);
		fix(h, fixme, fixval);
		break;
	}
	case SYM_SET:
	{
		/**@todo Only allow constants or literal */
		uint16_t location, value;
		location = literal_or_symbol_lookup(e, n->token, t);
		value    = literal_or_symbol_lookup(e, n->value, t);
		fix(h, location, value);
		break;
	}
	case SYM_PC:
		h->pc = literal_or_symbol_lookup(e, n->token, t);
		break;
	case SYM_MODE:
		a->mode = n->token->p.number;
		break;
	case SYM_ALLOCATE:
		/**@todo Only allow constants or literal */
		h->pc += literal_or_symbol_lookup(e, n->token, t);
		break;
	case SYM_BREAK:
		break_point_add(&h->bp, h->pc);
		break;
	default:
		fatal("Invalid or unknown type: %u", n->type);
	}
}

static h2_t *code(node_t *n, symbol_table_t *symbols)
{
	error_t e;
	h2_t *h;
	symbol_table_t *t = NULL;
	assembler_t a = { false, false, 0, 0, 0 };
	assert(n);

	t = symbols ? symbols : symbol_table_new();
	h = h2_new(START_ADDR);

	e.jmp_buf_valid = 1;
	if(setjmp(e.j)) {
		h2_free(h);
		if(!symbols)
			symbol_table_free(t);
		return NULL;
	}

	assemble(h, &a, n, t, &e);

	if(log_level >= LOG_DEBUG)
		symbol_table_print(t, stderr);
	if(!symbols)
		symbol_table_free(t);
	return h;
}

int h2_assemble_file(FILE *input, FILE *output, symbol_table_t *symbols)
{
	int r = 0;
	node_t *n;
	assert(input);
	assert(output);

	n = parse(input);

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
} command_args_t;

static const char *help = "\
usage ./h2 [-hvdDarRT] [-s number] [-L symbol.file] [-S symbol.file] files*\n\n\
Brief:     A H2 CPU Assembler, disassembler and Simulator.\n\
Author:    Richard James Howe\n\
Site:      https://github.com/howerj/forth-cpu\n\
License:   MIT\n\
Copyright: Richard James Howe (2017)\n\
Options:\n\n\
\t-\tstop processing options, following arguments are files\n\
\t-h\tprint this help message and exit\n\
\t-v\tincrease logging level\n\
\t-d\tdisassemble input files (default)\n\
\t-D\tfull disassembly of input files\n\
\t-T\tEnter debug mode when running simulation\n\
\t-a\tassemble file\n\
\t-r\trun hex file\n\
\t-R\tassemble file then run it\n\
\t-L #\tload symbol file\n\
\t-S #\tsave symbols to file\n\
\t-s #\tnumber of steps to run simulation (0 = forever)\n\
\tfile*\tfile to process\n\n\
Options must precede any files given, if no files have been\n\
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

static int run_command(command_args_t *cmd, FILE *input, FILE *output, symbol_table_t *symbols)
{
	h2_t *h = NULL;
	h2_io_t *io = NULL;
	int r = 0;
	h = h2_new(START_ADDR);
	if(h2_load(h, input) < 0)
		return -1;
	debug_note(cmd);
	io = h2_io_new();
	r = h2_run(h, io, output, cmd->steps, symbols, cmd->debug_mode);

	if(log_level >= LOG_NOTE)
		soc_print(output, io->soc, log_level >= LOG_DEBUG);

	h2_free(h);
	h2_io_free(io);
	return r;
}

static int assemble_run_command(command_args_t *cmd, FILE *input, FILE *output, symbol_table_t *symbols)
{
	assert(input);
	assert(output);
	h2_t *h = h2_assemble_core(input, symbols);
	h2_io_t *io = h2_io_new();
	int r = 0;
	if(!h)
		return -1;

	h->pc = START_ADDR;
	debug_note(cmd);
	r = h2_run(h, io, output, cmd->steps, symbols, cmd->debug_mode);

	h2_free(h);
	h2_io_free(io);
	return r;
}

int command(command_args_t *cmd, FILE *input, FILE *output, symbol_table_t *symbols)
{
	assert(input);
	assert(output);
	assert(cmd);
	switch(cmd->cmd) {
	case DEFAULT_COMMAND:      /* fall through */
	case DISASSEMBLE_COMMAND:  return h2_disassemble(input, output, symbols);
	case ASSEMBLE_COMMAND:     return h2_assemble_file(input, output, symbols);
	case RUN_COMMAND:          return run_command(cmd, input, output, symbols);
	case ASSEMBLE_RUN_COMMAND: return assemble_run_command(cmd, input, output, symbols);
	default:                   fatal("invalid command: %d", cmd->cmd);
	}
	return -1;
}

/**@todo limit number of input files to one? */
int main(int argc, char **argv)
{
	int i;
	const char *optarg = NULL;
	command_args_t cmd;
	symbol_table_t *symbols = NULL;
	FILE *symfile = NULL;
	FILE *newsymfile = NULL;
	memset(&cmd, 0, sizeof(cmd));
	cmd.steps = DEFAULT_STEPS;

#ifdef _WIN32
	/* Windows Only: Put the used standard streams into binary mode. 
	 * Text mode sucks. */
	_setmode(_fileno(stdin),  _O_BINARY);
	_setmode(_fileno(stdout), _O_BINARY);
	_setmode(_fileno(stderr), _O_BINARY);
#endif

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
		case 's':
			if(i >= (argc - 1))
				goto fail;
			optarg = argv[++i];
			if(string_to_long(0, &cmd.steps, optarg))
				goto fail;
			break;
		default:
		fail:
			fatal("invalid argument '%s'\n%s\n", argv[i], help);
		}
	}
	if(!symbols)
		symbols = symbol_table_new();
done:
	if(i == argc)
		if(command(&cmd, stdin, stdout, symbols) < 0) {
			fatal("failed to process standard input");
			return 0;
		}

	for(; i < argc; i++) { /* process all files on command line */
		FILE *input = fopen_or_die(argv[i], "rb");
		if(command(&cmd, input, stdout, symbols) < 0)
			fatal("failed to process file: %s", argv[i]);
		fclose(input);
	}

	if(newsymfile) {
		symbol_table_print(symbols, newsymfile);
		fclose(newsymfile);
	}
	symbol_table_free(symbols);
	if(symfile)
		fclose(symfile);
	return 0;
}

/* ========================== Main ========================================= */
