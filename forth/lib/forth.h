/** Howe Forth.
 *
 * @file forth.h
 * @brief Configuration file and interface API, it requires
 * stdio.h and stdint.h to be included before it is..
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

#include <stdint.h>

#ifndef forth_h_header_guard    /* begin header guard for forth.h */
#define forth_h_header_guard

#ifndef WORD_TYPE     /** Used to select what type of integer VM uses as default */
#define WORD_TYPE (2) /** Default: Signed 32 bit */
#endif

#define MAX_PRN_STR    (64)
#define MAX_ERR_STR    (64)
#define MAX_STRLEN     (32)
#define MAX_INSTRM     (32)

/*minimum memory requirements*/
#define MIN_REG        (32)
#define MIN_DIC        (512)
#define MIN_RET        (32)
#define MIN_VAR        (32)
#define MIN_STR        (512)
#define MIN_INBUF      (16)
#define MIN_DIC_OFF    (4)

/* Enums */
enum bool { false, true };

typedef enum {
  io_stdin,                     /*read from stdin */
  io_stdout,                    /*write to stdout */
  io_stderr,                    /*output to stderr */
  io_wr_file,                   /*write to file */
  io_rd_file,                   /*read from file */
  io_wr_str,                    /*write to a string */
  io_rd_str                     /*read from a string (null terminated!) */
} forth_io_e;

/********************************************************************************/
#define FORTH_PRIMITIVE_XMACRO_M \
  X(PUSH_INT,       "_push_int"),\
  X(COMPILE,        "_compile"),\
  X(RUN,            "_run"),\
  X(DEFINE,         ":"),\
  X(IMMEDIATE,      "immediate"),\
  X(READ,           "read"),\
  X(COMMENT,        "\\"),\
  X(EXIT,           "exit"),\
  X(BRANCH,         "br"),\
  X(NBRANCH,        "?br"),\
  X(PLUS,           "+"),\
  X(MINUS,          "-"),\
  X(MUL,            "*"),\
  X(MOD,            "mod"),\
  X(DIV,            "/"),\
  X(LS,             "lshift"),\
  X(RS,             "rshift"),\
  X(AND,            "and"),\
  X(OR,             "or"),\
  X(INV,            "invert"),\
  X(XOR,            "xor"),\
  X(INC,            "1+"),\
  X(DEC,            "1-"),\
  X(EQ,             "="),\
  X(LESS,           "<"),\
  X(MORE,           ">"),\
  X(FETCH_REG,      "@reg"),\
  X(FETCH_DIC,      "@"),\
  X(PICK,           "pick"),\
  X(FETCH_STR,      "@str"),\
  X(STORE_REG,      "!reg"),\
  X(STORE_DIC,      "!"),\
  X(STORE_VAR,      "!var"),\
  X(STORE_STR,      "!str"),\
  X(KEY,            "key"),\
  X(EMIT,           "emit"),\
  X(DUP,            "dup"),\
  X(DROP,           "drop"),\
  X(SWAP,           "swap"),\
  X(OVER,           "over"),\
  X(TOR,            ">r"),\
  X(FROMR,          "r>"),\
  X(TAIL,           "tail"),\
  X(QUOTE,          "'"),\
  X(COMMA,          ","),\
  X(PRINTNUM,       "printnum"),\
  X(GET_WORD,       "getword"),\
  X(STRLEN,         "strlen"),\
  X(ISNUMBER,       "isnumber"),\
  X(STRNEQU,        "strnequ"),\
  X(FIND,           "find"),\
  X(EXECUTE,        "execute"),\
  X(KERNEL,         "kernel"),\
  X(ERROR,          "error"),\
  X(LAST_PRIMITIVE, "THIS IS NOT A PRIMITIVE")

#define X(a, b) a
typedef enum {
  FORTH_PRIMITIVE_XMACRO_M
} forth_primitives_e;
#undef X

/********************************************************************************/

#define FORTH_SYSTEM_CALLS_XMACRO_M \
  X(SYS_RESET,        "reset"),\
  X(SYS_FOPEN,        "fopen"),\
  X(SYS_FCLOSE,       "fclose"),\
  X(SYS_FLUSH,        "fflush"),\
  X(SYS_REMOVE,       "remove"),\
  X(SYS_RENAME,       "rename"),\
  X(SYS_REWIND,       "rewind"),\
  X(SYS_SYSTEM,       "system"),\
  X(LAST_ERROR_CALL,  "NOT A SYSTEM CALL")

/**forth_interpreter() return calls*/
#define X(a, b) a
typedef enum {
  FORTH_SYSTEM_CALLS_XMACRO_M
} forth_syscall_e;
#undef X

/*Options passed to system call*/
typedef enum {
  SYS_OPT_IN, SYS_OPT_OUT, SYS_OPT_ERR
} forth_syscall_options_e;

typedef enum {
  onerr_break_e,
  onerr_goto_restart_e,
  onerr_special_e,
  onerr_return_e
} forth_error_action_e;

/********************************************************************************/
/** X macro X(The error code, the error string, which action to take)*/
#define FORTH_ERROR_XMACRO \
  X(ERR_OK,               "OK!\n",                                            onerr_goto_restart_e),\
  X(ERR_FAILURE,          "General Failure? Unknown cause.\n",                onerr_goto_restart_e),\
  X(ERR_REG,              "Err: Register addr.\n",                            onerr_goto_restart_e),\
  X(ERR_DIC,              "Err: Dictionary addr.\n",                          onerr_goto_restart_e),\
  X(ERR_VAR,              "Err: Variable addr.\n",                            onerr_goto_restart_e),\
  X(ERR_RET,              "Err: Return addr.\n",                              onerr_goto_restart_e),\
  X(ERR_STR,              "Err: String addr.\n",                              onerr_goto_restart_e),\
  X(ERR_PWD,              "Err: PWD!\n",                                      onerr_goto_restart_e),\
  X(ERR_NEXT,             "Err: NEXT!\n",                                     onerr_goto_restart_e),\
  X(ERR_PC,               "Err: PC!\n",                                       onerr_goto_restart_e),\
  X(ERR_TOS_REG,          "Err: TOS addr. Reg.\n",                            onerr_goto_restart_e),\
  X(ERR_TOS_DIC,          "Err: TOS addr. Dic.\n",                            onerr_goto_restart_e),\
  X(ERR_TOS_VAR,          "Err: TOS addr. Var.\n",                            onerr_goto_restart_e),\
  X(ERR_TOS_RET,          "Err: TOS addr. Ret.\n",                            onerr_goto_restart_e),\
  X(ERR_TOS_STR,          "Err: TOS addr. Str.\n",                            onerr_goto_restart_e),\
  X(ERR_OP0,              "Err: OP0!\n",                                      onerr_goto_restart_e),\
  X(ERR_OP1,              "Err: OP1!\n",                                      onerr_goto_restart_e),\
  X(ERR_DIV0,             "Err: Division by zero.\n",                         onerr_goto_restart_e),\
  X(ERR_MOD0,             "Err: Modulo by zero.\n",                           onerr_goto_restart_e),\
  X(ERR_IO,               "Err: IO Error.\n",                                 onerr_break_e),\
  X(ERR_EOF,              "EOF\n",                                            onerr_special_e),\
  X(ERR_BASE,             "Err: Base.\n",                                     onerr_goto_restart_e),\
  X(ERR_INSTRUCTION,      "Err: Illegal Instruction.\n",                      onerr_goto_restart_e),\
  X(ERR_WORD,             "Err: Word not found?\n",                           onerr_special_e),\
  X(ERR_ABNORMAL_END,     "Fatal Err: !!!Abnormal End!!!\n",                  onerr_break_e),\
  X(ERR_GENERAL,          "General Error.\n",                                 onerr_break_e),\
  X(ERR_SYSCALL,          "System call failed.\n",                            onerr_break_e),\
  X(ERR_SYSCALL_OPTIONS,  "Not a system call option.\n",                      onerr_break_e),\
  X(ERR_NOTSYSCALL,       "Err: Not a system call\n",                         onerr_break_e),\
  X(ERR_MINIMUM_MEM,      "Fatal Err: Minimum memory requirements not met\n", onerr_break_e),\
  X(ERR_CYCLES,           "Cycles complete\n",                                onerr_break_e),\
  X(HALT,                 "HALTING FORTH\n",                                  onerr_break_e),\
  X(ERR_NEXT_STRM,        "EOF -> Next Stream.\n",                            onerr_special_e),\
  X(ERR_NULL,             "(Internal), Null not expected\n",                  onerr_break_e),\
  X(ERR_SPECIAL_ERROR,    "(Internal), Special error handler not defined!\n", onerr_break_e),\
  X(LAST_ERROR,           "Fatal Err: Incorrect error code or call!\n",       onerr_return_e)

#define X(a, b, c) a
typedef enum {
  FORTH_ERROR_XMACRO
} forth_errors_e;
#undef X

/********************************************************************************/
/** X Macro, This one is optional apart from the ENUM (ie. For debugging)*/
#define FORTH_REGISTER_ENUM_XMACRO_M \
    X(ENUM_NEXT,              "NEXT"),\
    X(ENUM_PC,                "PC"),\
    X(ENUM_TOS,               "TOS"),\
    X(ENUM_RET,               "RET"),\
    X(ENUM_VAR,               "VAR"),\
    X(ENUM_DIC,               "DIC"),\
    X(ENUM_STR,               "STR"),\
    X(ENUM_PWD,               "PWD"),\
    X(ENUM_OP0,               "OP0"),\
    X(ENUM_OP1,               "OP1"),\
    X(ENUM_A,                 "A"),\
    X(ENUM_B,                 "B"),\
    X(ENUM_C,                 "C"),\
    X(ENUM_CPF,               "CPF"),\
    X(ENUM_EXF,               "EXF"),\
    X(ENUM_INI,               "INI"),\
    X(ENUM_maxReg,            "maxReg"),\
    X(ENUM_maxDic,            "maxDic"),\
    X(ENUM_maxRet,            "maxRet"),\
    X(ENUM_maxVar,            "maxVar"),\
    X(ENUM_maxStr,            "maxStr"),\
    X(ENUM_inputBufLen,       "inputBufLen"),\
    X(ENUM_dictionaryOffset,  "dictionaryOffset"),\
    X(ENUM_sizeOfMW,          "sizeOfMW"),\
    X(ENUM_cycles,            "cycles"),\
    X(ENUM_ccount,            "ccount"),\
    X(ENUM_inStrm,            "inStrm"),\
    X(ENUM_wordCount,         "wordCount"),\
    X(ENUM_wordIndex,         "wordIndex"),\
    X(ENUM_LAST_REGISTER,     "THIS IS NOT A REGISTER")

#define X(a, b) a
typedef enum {
  FORTH_REGISTER_ENUM_XMACRO_M
} forth_registers_e;
#undef X

/*vm macros*/
#define NEXT    reg[ENUM_NEXT]
#define PC      reg[ENUM_PC]
#define TOS     reg[ENUM_TOS]
#define RET     reg[ENUM_RET]

#define VAR     reg[ENUM_VAR]
#define DIC     reg[ENUM_DIC]
#define STR     reg[ENUM_STR]
#define PWD     reg[ENUM_PWD]

#define OP0     reg[ENUM_OP0]
#define OP1     reg[ENUM_OP1]
#define A       reg[ENUM_A]
#define B       reg[ENUM_B]

#define C       reg[ENUM_C]
#define CPF     reg[ENUM_CPF]
#define EXF     reg[ENUM_EXF]
#define INI     reg[ENUM_INI]

#define SM_maxReg  reg[ENUM_maxReg]
#define SM_maxDic  reg[ENUM_maxDic]
#define SM_maxRet  reg[ENUM_maxRet]
#define SM_maxVar  reg[ENUM_maxVar]

#define SM_maxStr  reg[ENUM_maxStr]
#define SM_inputBufLen  reg[ENUM_inputBufLen]
#define SM_dictionaryOffset  reg[ENUM_dictionaryOffset]
#define SM_sizeOfMW  reg[ENUM_sizeOfMW]

#define CYCLES  reg[ENUM_cycles]
#define CCOUNT  reg[ENUM_ccount]

#define IN_STRM reg[ENUM_inStrm]
#define WORDCNT reg[ENUM_wordCount]
#define WORDINX reg[ENUM_wordIndex]
/********************************************************************************/

#if   (0 == WORD_TYPE)          /* signed 16 bit */

typedef int16_t mw;
#define SIGNED_WORD (1)
#define BIT_SIZE    (16)
#define WORD_MIN    (INT16_MIN)
#define WORD_MAX    (INT16_MAX)
#define WORD_STRING "int16_t"

#elif (1 == WORD_TYPE)          /* unsigned 16 bit */

typedef uint16_t mw;
#define SIGNED_WORD (0)
#define BIT_SIZE    (16)
#define WORD_MIN    (0)
#define WORD_MAX    (UINT16_MAX)
#define WORD_STRING "uint16_t"

#elif (2 == WORD_TYPE)          /* signed 32 bit, default */

typedef int32_t mw;
#define SIGNED_WORD (1)
#define BIT_SIZE    (32)
#define WORD_MIN    (INT32_MIN)
#define WORD_MAX    (INT32_MAX)
#define WORD_STRING "int32_t"

#elif (3 == WORD_TYPE)          /* unsigned 32 bit */

typedef uint32_t mw;
#define SIGNED_WORD (0)
#define BIT_SIZE    (32)
#define WORD_MIN    (0)
#define WORD_MAX    (UINT32_MAX)
#define WORD_STRING "uint32_t"

#elif (4 == WORD_TYPE)          /* signed 64 bit */

typedef int64_t mw;
#define SIGNED_WORD (1)
#define BIT_SIZE    (64)
#define WORD_MIN    (INT64_MIN)
#define WORD_MAX    (INT64_MAX)
#define WORD_STRING "int64_t"

#elif (5 == WORD_TYPE)          /* unsigned 64 bit */

typedef uint64_t mw;
#define SIGNED_WORD (0)
#define BIT_SIZE    (64)
#define WORD_MIN    (0)
#define WORD_MAX    (UINT64_MAX)
#define WORD_STRING "uint64_t"

#endif

/*if input or output is a file or string, store point to it*/
union io_u {
  FILE *f;
  char *s;
};

/*IO redirections.*/
struct fio_s {
  forth_io_e fio;
  mw str_index;                 /*index into string */
  mw str_max_len;               /*max string length */
  union io_u iou;
};

typedef struct fio_s fio_t;

struct forth_obj {
  fio_t *in_file[MAX_INSTRM];   /*File input redirection */
  fio_t *out_file;              /*File output redirection */
  fio_t *err_file;              /*File error output redirection */
  mw *reg;                      /*pointer to registers */
  mw *dic;                      /*pointer to dictionary */
  mw *var;                      /*pointer to variable stack */
  mw *ret;                      /*pointer to return stack */
  char *str;                    /*pointer to character storage */
};

typedef struct forth_obj fobj_t;

/* Function prototypes for external API */
mw forth_interpreter(fobj_t * fo);
mw forth_monitor(fobj_t * fo);

#endif                          /*end header guard for forth.h */
