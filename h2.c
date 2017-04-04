/** @file      h2.c
 *  @brief     Simulate the H2 CPU and surrounding system
 *  @author    Richard James Howe
 *  @copyright Richard James Howe (2017)
 *  @license   MIT
 * 
 * Initially this program will be for just simulating the H2 core,
 * but eventually it will be extended so the peripherals can also
 * be simulated. This should speed up development of programs written
 * for the device, and allow for simulating the device where there
 * is no tool chain for dealing with VHDL.
 *
 * @todo implement this
 * @todo generate data that could be used with GTK wave
 * @todo integrate Assembler and Disassembler into this file
 * @todo turn this into a library
 * @todo turn this into a literate program
 * @todo make common macros for extracting instruction information
 *
 * @note Given a sufficiently developed H2 application, it should be possible
 * to feed the same inputs into h2_run and except the same outputs as the
 * VHDL based CPU. This could be used as an advanced test bench. This could
 * be done instruction by instruction, or the data could be read in from a
 * file.
 */

/* ========================== Preamble: Types, Macros, Globals ============= */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
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
#endif

#define MAX_CORE      (8192u)
#define STK_SIZE      (32u)
#define START_ADDR    (8u)
#define DEFAULT_STEPS (64)
#define MAX(X, Y)  ((X) > (Y) ? (X) : (Y))
#define MIN(X, Y)  ((X) > (Y) ? (Y) : (X))

/* The H2 CPU is a rewrite of the J1 Forth CPU in VHDL with some extensions,
 *
 * It is a stack based CPU with minimal state; a program counter, a top
 * of the variable stack register and two stacks 32 deep each, the return
 * and the variable stacks. It is a 16-bit CPU.
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
 *	| 0 | 0 | 0 |            JUMP TARGET ADDRESS                    |
 *	*---------------------------------------------------------------*
 *	| 0 | 0 | 1 |            CONDITIONAL JUMP TARGET ADDRESS        |
 *	*---------------------------------------------------------------*
 *	| 0 | 1 | 0 |            CALL TARGET ADDRESS                    |
 *	*---------------------------------------------------------------*
 *	| 0 | 1 | 0 |   ALU OPERATION   |T2N|T2R|N2A|R2P| RSTACK| DSTACK|
 *	*---------------------------------------------------------------*
 *	| F | E | D | C | B | A | 9 | 8 | 7 | 6 | 5 | 4 | 3 | 2 | 1 | 0 |
 *	*---------------------------------------------------------------*
 *
 *      T   : Top off variable stack
 *	N   : Next on variable stack
 *	PC  : Program Counter
 *
 *      LITERAL VALUES : push a value onto the variable stack
 *      CONDITIONAL    : JUMPS pop and test the T
 *      CALLS          : PC+1 onto the return stack
 *
 *	T2N : Move T to N
 *	T2R : Move T to top of return stack
 *	N2A : STORE T to memory location addressed by N
 *	R2P : Move top of return stack to PC
 *
 *	RSTACK and DSTACK are signed values (twos compliment) that are
 *	the stack delta (the amount to increment or decrement the stack
 *	by for their respective stacks: return and variable)
 *
 * ALU OPERATIONS
 *
 * All ALU operations replace T:
 * 
 *	*------------------------*-----------------------*
 *	| Value |   Operation    |     Description       |
 *	*------------------------*-----------------------*
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
 *	*------------------------*-----------------------*
 * 
 * More information about the original J1 CPU can be found at: 
 * 
 * 	http://excamera.com/sphinx/fpga-j1.html
 */

#define DSTACK_LENGTH   (2u)
#define DSTART_START    (0u)
#define RSTACK_LENGTH   (2u)
#define RSTACK_START    (2u)

#define R_TO_PC_BIT     (4u)
#define N_TO_ADDR_T_BIT (5u)
#define T_TO_R_BIT      (6u)
#define T_TO_N_BIT      (7u)

typedef struct {
	uint16_t core[MAX_CORE]; /**< main memory */
	uint16_t rstk[STK_SIZE]; /**< return stack */
	uint16_t vstk[STK_SIZE]; /**< variable stack */
	uint16_t pc;  /**< program counter */
	uint16_t tos; /**< top of stack */
	uint16_t rp;  /**< return stack pointer */
	uint16_t sp;  /**< variable stack pointer */
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

static log_level_e log_level = LOG_NOTE;

typedef struct {
	int error;
	int jmp_buf_valid;
	jmp_buf j;
} error_t;

/* ========================== Preamble: Types, Macros, Globals ============= */

/* ========================== Utilities ==================================== */

/**@todo add in log levels and use an enum instead of isfatal and prefix*/
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

h2_t *h2_new(void)
{
	h2_t *h = allocate_or_die(sizeof(h2_t));
	h->pc = START_ADDR;
	return h;
}

void h2_free(h2_t *h)
{
	assert(h);
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
			warning("file contains too many lines: %zu", i);
			return -1;
		}
		r = string_to_cell(16, &h->core[i], line);
		if(!r) {
			warning("invalid line, expected hex string: %s", line);
			return -1;
		}
		debug("%zu %u", i, (unsigned)h->core[i]);
	}

	if(i < MAX_CORE-1) {
		warning("file contains too few lines: %zu", i);
		return -1;
	}

	return 0;
}

/* ========================== Utilities ==================================== */

/* ========================== Simulation =================================== */
/** @warning Can only push or pop once per cycle */
/** @todo over and underflow checks: it would be nice if the CPU could trap 
 * on under or overflow */
static void vpush(h2_t *h, uint16_t v)
{
	assert(h);
	h->vstk[h->sp + 1 % STK_SIZE] = h->tos;
	h->tos = v;
	h->sp++;
	if(h->sp >= STK_SIZE)
		warning("stack overflow");
	h->sp %= STK_SIZE;
}

static uint16_t vpop(h2_t *h)
{
	uint16_t r;
	assert(h);
	r = h->tos;
	h->tos = h->vstk[h->sp % STK_SIZE];
	h->sp--;
	return r;
}

static int _trace(FILE *output, const char *fmt, ...)
{
	int r = 0;
	va_list ap;
	if(!output)
		return r;
	assert(fmt);
	va_start(ap, fmt);
	r = vfprintf(output, fmt, ap);
	va_end(ap);
	fputc('\n', output);
	fflush(output);
	return r;
}

#define trace(FMT, ...) _trace(output, FMT, ##__VA_ARGS__)

/** @todo interrupt handling, implement instruction set and I/O 
 *  @todo split into a step and a run function */
int h2_run(h2_t *h, FILE *output, unsigned steps)
{
	assert(h);
	for(unsigned i = 0; i < steps || steps == 0; i++) {
		uint16_t inst, lit, val;

		inst = h->core[h->pc];
		lit = 0;
		val = 0;
		trace("%u: pc(%04x) inst(%04x)", 
				i, (unsigned)h->pc, (unsigned)inst);

		/* decode / execute */
		if((inst & 0x8000) == 0x8000) { /* literal */
			lit = inst & 0x7FFF;
			vpush(h, lit);
			h->pc = (h->pc + 1) % MAX_CORE;
		} else if ((inst & 0x6000) == 0x6000){ /* ALU instruction */
			h->pc = (h->pc + 1) % MAX_CORE;
		} else if ((inst & 0x4000) == 0x4000) { /* CALL */
			val = inst & 0x1FFF;
		} else if ((inst & 0x2000) == 0x2000) { /* Conditional Jump */
			val = inst & 0x1FFF;
			if(!vpop(h))
				h->pc = val % MAX_CORE;
			else
				h->pc = (h->pc + 1) % MAX_CORE;
		} else if (!(inst & 0xC000)) { /* Jump */
			val = inst & 0x1FFF;
			h->pc = val;
		} else {
			/* ??? */
		}

	}
	return 0;
}

/* ========================== Simulation =================================== */

/* ========================== Disassembler ================================= */
/* Plan: Use "filter.c"
 *
 * Have two modes: full/partial.
 *
 * Partial decompilation occurs line by line, a hexadecimal number is
 * recieved and an instruction is printed out. This can be used by GTKWave.
 *
 * Full decompilation involves reading in the entire binary and regenerating
 * labels, if possible.
 */

static const char *disassembler_alu(uint16_t instruction)
{
	/**@todo return a more informative string; like "drop", "exit", ...*/
	switch((instruction & 0x1F00) >> 8) {
	case 0: return "T";
	case 1: return "N";
	case 2: return "T+N";
	case 3: return "T&N";
	case 4: return "T|N";
	case 5: return "T^N";
	case 6: return "~T";
	case 7: return "N=T";
	case 8: return "T>N";
	case 9: return "N>>T";
	case 10: return "T-1";
	case 11: return "R";
	case 12: return "[T]";
	case 13: return "N<<T";
	case 14: return "depth";
	case 15: return "Tu>N";
	case 16: return "set_interrupts";
	default: return "unknown";
	}
}

static int disassembler_instruction(uint16_t instruction, FILE *output)
{
	int r = 0;
	assert(output);
	if ((instruction & 0x8000) == 0x8000) {	/**Literal */
		r = fprintf(output, "lit(%hx)", 0x7FFF & instruction);
	} else if ((instruction & 0x6000) == 0x6000) { /**ALU instruction */
		r = fprintf(output, "%s:%hx", disassembler_alu(instruction & 0x1FFF), 0x1FFF & instruction);
	} else if ((instruction & 0x4000) == 0x4000) { /**Call */
		r = fprintf(output, "call(%hx)", 0x1FFF & instruction);
	} else if ((instruction & 0x2000) == 0x2000) { /**Conditional Jump */
		r = fprintf(output, "cjmp(%hx)", 0x1FFF & instruction);
	} else if (!(instruction & 0xC000)) { /**Jump */
		r = fprintf(output, "jmp(%hx)", 0x1FFF & instruction);
	} else {
		r = fprintf(output, "?(%hx)", instruction);
	}
	return r;
}

int h2_disassemble(FILE *input, FILE *output)
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
			if(disassembler_instruction(instruction, output) < 0) {
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

/* ========================== Assembler ==================================== */
/* 
 * Plan: Lexer/Parser/Code-Generation
 *
 * Grammar:
 *
 * Program     := Statement*  EOF 
 * Statement   := Label | Jump | Literal | Instruction | constant
 * Label       := Identifier ':'
 * Jump        := [ "call" | "branch" | "0branch" ] Identifier
 * Constant    := "constant" Identifier Literal
 * Literal     := Hex | Octal | Signed-Decimal
 * Instruction := "@" | "!" | "exit" | ... 
 * Identifier  := Alpha AlphaNumeric*
 *
 * Initially, at least, only forward jumps will be allowed. Structured
 * programming such as 'if...then' and 'begin...until' could be added
 * later, as well as a pseudo Forth like syntax. There really is no
 * need however.
 */

#define MAX_ID_LENGTH (256u)

typedef enum {
	LITERAL,
	IDENTIFIER,
	LABEL,

	CONSTANT, /* start of named tokens */
	CALL,
	JMP,
	CJMP,
	DUP,
	OVER,
	INVERT,
	ADD,
	SWAP,
	NIP,
	DROP,
	EXIT,
	TOR,
	FROMR,
	RAT,
	LOAD,
	STORE, /* end of named tokens */
	ERROR, /* error token: this needs to be after the named tokens */

	EOI = EOF
} token_e;

static const char *keywords[] = 
{
	[CONSTANT]  =  "constant",
	[CALL]      =  "call",
	[JMP]       =  "branch",
	[CJMP]      =  "0branch",
	[DUP]       =  "dup",
	[OVER]      =  "over",
	[INVERT]    =  "invert",
	[ADD]       =  "+",
	[SWAP]      =  "swap",
	[NIP]       =  "nip",
	[DROP]      =  "drop",
	[EXIT]      =  "exit",
	[TOR]       =  ">r",
	[FROMR]     =  "r>",
	[RAT]       =  "r@",
	[LOAD]      =  "@",
	[STORE]     =  "!",
	[ERROR]     =  NULL,
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
} lexer_t;

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
	if(t->type == IDENTIFIER)
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

static int _syntax_error(lexer_t *l, 
		const char *func, unsigned line, const char *fmt, ...)
{
	int r = 0;
	va_list ap;
	assert(l);
	assert(func);
	assert(fmt);
	if(fprintf(stderr, "%s:%u\n", func, line) < 0)
		return -1;
	if(fprintf(stderr, "  syntax error on line %u of input\n", l->line) < 0)
		return -1;

	va_start(ap, fmt);
	r = vfprintf(stderr, fmt, ap);
	va_end(ap);

	if(r >= 0)
		r = fputc('\n', stderr);

	/*print_token(stderr, l->token, 2);*/
	ethrow(&l->error);
	return 0;
}

#define syntax_error(LEXER, ...) _syntax_error(LEXER, __func__, __LINE__, ## __VA_ARGS__)

static uint16_t number(lexer_t *l, int *c)
{
	uint16_t i;
	int ch;
	assert(l);
	assert(c);
	i = 0;
	ch = *c;
	while(isdigit(ch)) {
		i = i * 10 + (ch - '0');
		ch = next_char(l);
		if(ch == EOF)
			syntax_error(l, "number terminated by EOF");
	}
	if(!isspace(ch))
		syntax_error(l, "merged number and identifier");
	*c = ch;
	return i;
}

static void lexer(lexer_t *l)
{
	assert(l);
	int ch;
	ch = next_char(l);
	l->token = token_new(ERROR, l->line);

again:
	switch(ch) {
	case '\n': 
		l->token->line++;
	case ' ':
	case '\t': 
		ch = next_char(l);
		goto again;
	case EOF:  
		l->token->type = EOI; 
		return;
	case '#':
		for(; '\n' != (ch = next_char(l));)
			if(ch == EOF)
				syntax_error(l, "commented terminated by EOF");
		l->token->line++;
		goto again;
	default:
		if(isdigit(ch)) {
			l->token->type = LITERAL;
			l->token->p.number = number(l, &ch);
			break;
		}

		if(isalpha(ch)) {
			unsigned i = 0, sym = 0;
			while(isalnum(ch)) {
				l->id[i++] = ch;
				ch = next_char(l);
			}
			l->id[i] = '\0';
			for(sym = CONSTANT; sym != ERROR && keywords[sym] && strcmp(keywords[sym], l->id); sym++)
				/*do nothing*/;
			if(!keywords[sym]) {
				if(ch == ':') { /* IDENTIFIER ':' */
					ch = next_char(l);
					l->token->type = LABEL;
				} else { /* IDENTIFIER */
					l->token->type = IDENTIFIER;
				}
				l->token->p.id = duplicate(l->id);
			} else {
				l->token->type = sym;
			}
			break;
		}
		syntax_error(l, "invalid token: %s (%d)\n", l->id, ch);
		break;
	}
	unget_char(l, ch);
}

int h2_assemble(FILE *input, FILE *output)
{
	assert(input);
	assert(output);
	lexer_t *l = lexer_new(input);

	l->error.jmp_buf_valid = 1;
	if(setjmp(l->error.j)) {
		lexer_free(l);
		/** @warning leaks parsed nodes */
		return -1;
	}

	/**@todo implement the rest of this, and print tokens out */
	for(;;) {
		lexer(l);
		if(l->token->type == EOI)
			break;
		printf("TOKEN\n");
	}

	lexer_free(l);
	return 0;
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
} command_args_t;

static const char *help = "\
usage ./h2 [-hvdDarR] [-s number] files*\n\n\
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
\t-a\tassemble file\n\
\t-r\trun hex file\n\
\t-R\tassemble file then run it\n\
\t-s #\tnumber of steps to run simulation (0 = forever)\n\
\tfile*\tfile to process\n\n\
Options must precede any files given, if no files have been\n\
given as arguments input is taken from stdin. Output is to\n\
stdout. Program returns zero on success, non zero on failure.\n\n\
";

static int run_command(command_args_t *cmd, FILE *input, FILE *output)
{
	h2_t *h = NULL;
	int r = 0;
	h = h2_new();
	if(h2_load(h, input) < 0)
		return -1;
	note("running for %u cycles (0 = forever)", (unsigned)cmd->steps);
	r = h2_run(h, output, cmd->steps);
	h2_free(h);
	return r;
}

static int assemble_run_command(command_args_t *cmd, FILE *input, FILE *output)
{
	int r = 0;
	FILE *assembled = NULL;

	errno = 0;
	assembled = tmpfile();
	if(!assembled) {
		error("unable to open temporary file: %s", reason());
		return -1;
	}

	if((r = h2_assemble(input, assembled)) < 0)
		goto fail;

	errno = 0;
	if((r = fseek(assembled, 0, SEEK_SET)) < 0) {
		error("unable to seek to beginning of temporary file: %s", reason());
		goto fail;
	}

	r = run_command(cmd, assembled, output);
	
fail:
	if(assembled)
		fclose(assembled);

	return r < 0 ? -1 : 0;
}

int command(command_args_t *cmd, FILE *input, FILE *output)
{
	assert(input);
	assert(output);
	assert(cmd);
	switch(cmd->cmd) {
	case DEFAULT_COMMAND:      /* fall through */
	case DISASSEMBLE_COMMAND:  return h2_disassemble(input, output);
	case ASSEMBLE_COMMAND:     return h2_assemble(input, output);
	case RUN_COMMAND:          return run_command(cmd, input, output);
	case ASSEMBLE_RUN_COMMAND: return assemble_run_command(cmd, input, output);
	default:                   fatal("invalid command: %d", cmd->cmd);
	}
	return -1;
}

/**@todo command line options to make this program more friendly to use 
 * need to be added */
int main(int argc, char **argv)
{
	int i;
	const char *optarg = NULL;
	long steps = 0;
	command_args_t cmd;
	memset(&cmd, 0, sizeof(cmd));
	cmd.steps = DEFAULT_STEPS;

#ifdef _WIN32
	/* Windows Only: Put the used standard streams into binary mode,
	 * this is a binary, not a text utility */
	_setmode(_fileno(stdin),  _O_BINARY);
	_setmode(_fileno(stdout), _O_BINARY);
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
		case 'R':  
			if(cmd.cmd)
				goto fail;
			cmd.cmd = ASSEMBLE_RUN_COMMAND;
			break;
		case 's':  
			if(i >= (argc - 1))
				goto fail;
			optarg = argv[++i];
			if(string_to_long(0, &steps, optarg))
				goto fail;
			break;
		default:
		fail:
			fatal("invalid argument '%s'\n%s\n", argv[i], help);
		}
	}
done:
	if(i == argc)
		if(command(&cmd, stdin, stdout) < 0) {
			fatal("failed to process standard input");
			return 0;
		}

	for(; i < argc; i++) { /* process all files on command line */
		FILE *input = fopen_or_die(argv[i], "rb");
		if(command(&cmd, input, stdout) < 0)
			fatal("failed to process file: %s", argv[i]);
		fclose(input);
	}
	return 0;
}

/* ========================== Main ========================================= */
