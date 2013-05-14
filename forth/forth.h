/* 
 * Richard James Howe
 * Howe Forth.
 *
 * Configuration file and interface API
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

/*#define RUN4X*/

#ifndef forth_h_header_guard
#define forth_h_header_guard

#define true    1
#define false   0

#define    MAX_PRN_STR    64
#define    MAX_ERR_STR    64
#define    MAX_STRLEN     32
#define    MAX_INSTRM     32

/*minimum memory requirements*/
#define MIN_REG        32
#define MIN_DIC        512
#define MIN_RET        32
#define MIN_VAR        32
#define MIN_STR        512
#define MIN_INBUF      16
#define MIN_DIC_OFF    4

/* Enums */
enum forth_io {
        io_stdin,               /*read from stdin */
        io_stdout,              /*write to stdout */
        io_stderr,              /*output to stderr */
        io_wr_file,             /*write to file */
        io_rd_file,             /*read from file */
        io_wr_str,              /*write to a string */
        io_rd_str               /*read from a string (null terminated!) */
};

enum forth_primitives {
        PUSH_INT, COMPILE, RUN, DEFINE, IMMEDIATE, READ, COMMENT, EXIT,
        BRANCH, NBRANCH, PLUS, MINUS, MUL, MOD, DIV,
        LS, RS, AND, OR, INV, XOR, INC, DEC, EQ, LESS, MORE,
        FETCH_REG, FETCH_DIC, PICK, FETCH_STR,
        STORE_REG, STORE_DIC, STORE_VAR, STORE_STR,
        KEY, EMIT, DUP, DROP, SWAP, OVER, TOR, FROMR,
        TAIL, QUOTE, COMMA, PRINTNUM, GET_WORD, STRLEN, ISNUMBER, STRNEQU, FIND,
        EXECUTE,
        KERNEL,
        LAST_PRIMITIVE
};

/*forth_interpreter() return calls*/
enum forth_syscall {
        SYS_RESET,
        SYS_FOPEN, SYS_FCLOSE, SYS_FLUSH,
        SYS_REMOVE, SYS_RENAME, SYS_REWIND
            /* remove() rename() tmpfile() tmpnam() ... other stdio.h functions */
};

/*Options passed to system call*/
enum forth_syscall_options {
        SYS_OPT_IN, SYS_OPT_OUT, SYS_OPT_ERR
};

enum forth_errors {
        ERR_OK,
        ERR_FAILURE,
        ERR_REG, ERR_DIC, ERR_VAR, ERR_RET, ERR_STR,
        ERR_PWD, ERR_NEXT, ERR_PC,
        ERR_TOS_REG, ERR_TOS_DIC, ERR_TOS_VAR, ERR_TOS_RET, ERR_TOS_STR,
        ERR_OP0, ERR_OP1,
        ERR_DIV0, ERR_MOD0, ERR_IO, ERR_EOF, ERR_BASE,
        ERR_INSTRUCTION, ERR_WORD, ERR_ABNORMAL_END, ERR_GENERAL,
        ERR_SYSCALL,
        ERR_SYSCALL_OPTIONS,
        ERR_NOTSYSCALL,
        ERR_MINIMUM_MEM,
        ERR_CYCLES,
        HALT,
        LAST_SYS
};

enum forth_registers {
        ENUM_NEXT,
        ENUM_PC,
        ENUM_TOS,
        ENUM_RET,
        ENUM_VAR,
        ENUM_DIC,
        ENUM_STR,
        ENUM_PWD,
        ENUM_OP0,
        ENUM_OP1,
        ENUM_A,
        ENUM_B,
        ENUM_C,
        ENUM_CPF,
        ENUM_EXF,
        ENUM_INI,
        ENUM_maxReg,
        ENUM_maxDic,
        ENUM_maxRet,
        ENUM_maxVar,
        ENUM_maxStr,
        ENUM_inputBufLen,
        ENUM_dictionaryOffset,
        ENUM_sizeOfMW,
        ENUM_cycles,
        ENUM_ccount,
        ENUM_inStrm
};

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

typedef signed int mw;

/*if input or output is a file or string, store point to it*/
union io_u {
        FILE *f;
        char *s;
};

/*IO redirections.*/
struct fio_s {
        enum forth_io fio;
        mw str_index;           /*index into string */
        mw str_max_len;         /*max string length */
        union io_u iou;
};

typedef struct fio_s fio_t;

struct forth_obj {
        fio_t *in_file[MAX_INSTRM];     /*File input redirection */
        fio_t *out_file;        /*File output redirection */
        fio_t *err_file;        /*File error output redirection */
        mw *reg;                /*pointer to registers */
        mw *dic;                /*pointer to dictionary */
        mw *var;                /*pointer to variable stack */
        mw *ret;                /*pointer to return stack */
        char *str;              /*pointer to character storage */
};

typedef struct forth_obj fobj_t;

/* Function prototypes for external API */
mw forth_interpreter(fobj_t * fo);
mw forth_monitor(fobj_t * fo);
#endif                          /*end header guard */
