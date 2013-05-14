/* 
 * Richard James Howe
 * Howe Forth.
 *
 * Forth interpreter.
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

#include <stdio.h>
#include "forth.h"

#define ISSPACE(X)    ((X=='\n')||(X=='\r')||(X=='\t')||(X=='\v')||(X==' ')||\
            (X=='\f'))

#define ISDIGIT(X)    ((X=='0')||(X=='1')||(X=='2')||(X=='3')||(X=='4')||(X=='5')||\
            (X=='6')||(X=='7')||(X=='8')||(X=='9'))

/* IO wrappers*/

/*Either get input from stdin, a string or a file*/
int wrap_get(fio_t * in_file)
{
        char tmp;
        switch (in_file->fio) {
        case io_stdin:
                return getc(stdin);
        case io_rd_file:
                if (in_file->iou.f != NULL)
                        return fgetc(in_file->iou.f);
                else
                        return EOF;
        case io_rd_str:
                if (in_file->iou.s != NULL) {
                        if (in_file->str_index > in_file->str_max_len)
                                return EOF;
                        tmp = in_file->iou.s[++in_file->str_index];
                        if (tmp != '\0')
                                return tmp;
                        else
                                return EOF;
                } else
                        return EOF;
        default:
                return EOF;
        }
}

/*Either put output to a stdout, stderr, a string or a file*/
int wrap_put(fio_t * out_file, char c)
{
        switch (out_file->fio) {
        case io_stdout:
                return putc(c, stdout);
        case io_stderr:
                return putc(c, stderr);
        case io_wr_file:
                if (out_file->iou.f != NULL)
                        return fputc(c, out_file->iou.f);
                else
                        return EOF;
        case io_wr_str:
                if (out_file->iou.s != NULL) {
                        if (out_file->str_index > out_file->str_max_len)
                                return EOF;
                        out_file->iou.s[++(out_file->str_index)] = c;
                } else
                        return EOF;
        default:
                return EOF;
        }
}

/*Basic IO functions*/

/*This gets a space delimited FORTH word*/
mw get_word(char *str, const mw str_len, fio_t * io_file)
{
        mw i;
        int c;

        /*memset */
        for (i = 0; i < str_len; i++)
                str[i] = 0;

        /*discard spaces */
        for (c = wrap_get(io_file); ISSPACE(c); c = wrap_get(io_file))
                if (c == EOF)
                        return ERR_FAILURE;

        /*copy word */
        for (i = 0; (i < str_len) && (!ISSPACE(c)); i++, c = wrap_get(io_file)) {
                str[i] = c;
                if (c == EOF)
                        return ERR_FAILURE;
        }

        /*fail if word is too long */
        if (i >= str_len)
                return ERR_FAILURE;

        /*null terminate */
        str[++i] = '\0';
        return ERR_OK;          /*success! */
}

/*Basic Utils*/
mw isnumber(const char s[])
{
        mw x = 0;
        if (((s[x] == '-') || (s[x] == '+')) && ISDIGIT(s[x + 1]))
                x++;
        for (; s[x] != '\0'; x++) {
                if (!ISDIGIT(s[x])) {
                        return false;
                }
        }
        return true;
}

/* 1 if not equal, 0 if equal, 2 means string limit exceeded*/
mw strnequ(const char str1[], const char str2[], const mw lim1, const mw lim2)
{

        char *s1 = (char *)str1, *s2 = (char *)str2;

        for (; (*s1 == *s2) && (s1 < str1 + lim1) && (s2 < str2 + lim2);
             s1++, s2++)
                if (*s1 == '\0')
                        return 0;

        if ((s1 >= str1 + lim1) || (s2 >= str2 + lim2)) {
                return 2;
        }
        return 1;
}

mw my_strlen(const char s[], int maxlen)
{
        int i = 0;
        char *p = (char *)s;
        while (*p != '\0' && i++ < maxlen)
                p++;
        return p - s;
}

/*K&R atoi, slighty modified*/
mw kr_atoi(const char s[])
{
        mw i, n, sign;
        for (i = 0; ISSPACE(s[i]); i++) ;

        sign = (s[i] == '-') ? -1 : 1;
        if ((s[i] == '+') || (s[i] == '-'))
                i++;
        for (n = 0; ISDIGIT(s[i]); i++) {
                n = 10 * n + (s[i] - '0');
        }
        return sign * n;
}

/*TODO buf needs replacing with an external pointer
 * Refactor code*/
#define MAX_BASE 16
char *my_itoa(mw value, int base)
{
        mw v = value;
        static char buf[MAX_ERR_STR] = { 0 };
        mw i = MAX_ERR_STR - 2;
        if (value == 0) {
                buf[i] = '0';
                return &buf[i];
        }
        if ((signed)value < 0)
                v *= -1;
        if ((base > 1) && (base <= 16))
                for (; v && i; --i, v /= base)
                        buf[i] = "0123456789abcdef"[v % base];
        if ((signed)value < 0) {
                buf[i] = '-';
                return &buf[i];
        }
        return &buf[i + 1];
}

/*Error handling needed!*/
void print_string(const char *s, const mw max, fio_t * out_file)
{
        mw i;
        for (i = 0; (i < max) && (s[i] != '\0'); i++) {
                wrap_put(out_file, s[i]);
        }
}

void print_line_file(int line, const char *file, fio_t * err_file)
{
        print_string(my_itoa(line, 10), MAX_ERR_STR, err_file);
        wrap_put(err_file, '\t');
        print_string(file, MAX_ERR_STR, err_file);
        wrap_put(err_file, '\n');
}

#ifndef UNCHECK

/*Error Check Bounds*/
#define ECB(MIN,MAX,TEST,ERR,ERR_STRM)    if((TEST>(MAX-2))||(TEST<MIN)){ \
    print_line_file(__LINE__,__FILE__,ERR_STRM);                          \
    return ERR;                                                           \
}
/*Error Check Upper (Bound) (and) Zero (Lower bound)*/
#define ECUZ(MAX,VT,ERR,ERR_STRM) if((VT>(MAX-2))||(VT<0)){  \
    print_line_file(__LINE__,__FILE__,ERR_STRM);             \
    return ERR;                                              \
}
#define ERR_LN_PRN(ERR_STRM) print_line_file(__LINE__,__FILE__,ERR_STRM);

#else

#define ECB(MIN,MAX,TEST,ERR,ERR_STRM)
#define ECUZ(MAX,VT,ERR,ERR_STRM)
#define ERR_LN_PRN(ERR_STRM)

#endif

/*****************************************************************************/
/* Begin the interpreter!                                                    */
/*****************************************************************************/

/*Set OP0 to found word, if found, return error otherwise.*/
mw find_word(fobj_t * fo)
{
        mw *reg = fo->reg, *dic = fo->dic;
        char *str = fo->str;
        fio_t *in_file = fo->in_file[IN_STRM], *err_file = fo->err_file;

        if (get_word(str, SM_inputBufLen, in_file) == ERR_FAILURE) {
                ERR_LN_PRN(err_file);
                return ERR_EOF;
        }
        OP0 = PWD;
        ECUZ(SM_maxDic, OP0 + 1, ERR_OP0, err_file);
        ECUZ(SM_maxStr, dic[OP0 + 1], ERR_DIC, err_file);

        while ((OP1 =
                strnequ(str, &str[dic[OP0 + 1]], SM_inputBufLen, SM_maxStr))) {
                if (OP1 == 2) { /*2 signifies string limited exceeded. */
                        ERR_LN_PRN(err_file);
                        return ERR_IO;
                }
                ECUZ(SM_maxDic, OP0, ERR_OP0, err_file);

                /* Sanity check, prevents loops */
                if ((OP0 == dic[OP0]) || (OP0 < dic[OP0])) {
                        ERR_LN_PRN(err_file);
                        return ERR_PWD;
                }
                OP0 = dic[OP0];
        }

        return ERR_OK;
}

mw compile_word(enum forth_primitives fp, fobj_t * fo)
{
        mw *reg = fo->reg, *dic = fo->dic;
        char *str = fo->str;
        fio_t *in_file = fo->in_file[IN_STRM], *err_file = fo->err_file;

        ECUZ(SM_maxDic - 3, DIC, ERR_DIC, err_file);
        dic[DIC++] = PWD;
        PWD = DIC - 1;
        dic[DIC++] = STR;
        dic[DIC++] = fp;
        if (get_word(str + STR, SM_inputBufLen, in_file) != ERR_OK)
                return ERR_FAILURE;
        STR += my_strlen(str + STR, MAX_STRLEN) + 1;
        return ERR_OK;
}

mw forth_initialize(fobj_t * fo)
{
        /*copy pointers, removes level of indirection */
        mw *reg = fo->reg, *dic = fo->dic, *ret = fo->ret;
        fio_t *err_file = fo->err_file;

        if (SM_maxReg < MIN_REG)
                return ERR_MINIMUM_MEM;
        if (SM_maxDic < MIN_DIC)
                return ERR_MINIMUM_MEM;
        if (SM_maxRet < MIN_RET)
                return ERR_MINIMUM_MEM;
        if (SM_maxVar < MIN_VAR)
                return ERR_MINIMUM_MEM;
        if (SM_maxStr < MIN_STR)
                return ERR_MINIMUM_MEM;
        if (SM_inputBufLen < MIN_INBUF)
                return ERR_MINIMUM_MEM;
        if (SM_dictionaryOffset < MIN_DIC_OFF)
                return ERR_MINIMUM_MEM;

        /*Set up the dictionary */
        DIC = SM_dictionaryOffset;
        STR = SM_inputBufLen;
        PC = 0;
        PWD = 1;

        /*immediate words */
        if (compile_word(DEFINE, fo) != ERR_OK) {
                return ERR_FAILURE;
        }
        if (compile_word(IMMEDIATE, fo) != ERR_OK) {
                return ERR_FAILURE;
        }

        /*define a special word (read) */
        if (compile_word(COMPILE, fo) != ERR_OK) {
                return ERR_FAILURE;
        }
        OP0 = DIC;
        ECUZ(SM_maxDic - 2, DIC, ERR_DIC, err_file);
        dic[DIC++] = READ;
        dic[DIC++] = RUN;
        PC = DIC;
        ECUZ(SM_maxDic - 2, DIC, ERR_DIC, err_file);
        dic[DIC++] = OP0;
        dic[DIC++] = PC - 1;
        /*...end special word definition */

        /*more immediate words */
        if (compile_word(COMMENT, fo) != ERR_OK) {
                return ERR_FAILURE;
        }

        /*define 'compile only' words */
        ECUZ(SM_maxDic - (LAST_PRIMITIVE - EXIT), DIC, ERR_DIC, err_file);
        for (OP0 = EXIT; OP0 < LAST_PRIMITIVE;) {
                if (compile_word(COMPILE, fo) != ERR_OK) {
                        return ERR_FAILURE;
                }
                dic[DIC++] = OP0++;
        }
        ECUZ(SM_maxRet, RET, ERR_RET, err_file);
        ret[++RET] = DIC;
        CPF = true;
        INI = false;

        return ERR_OK;
}

/* on_err();
 * Call the word pointered to in register EXF when
 * an error occurs and reset the VM into a workable
 * state.
 */
void on_err(fobj_t * fo)
{
        fflush(NULL);
        fo->CPF = false;
        fo->VAR = 0;
        fo->TOS = 0;
        fo->RET = 1;
        fo->PC = fo->EXF;
        fo->IN_STRM = 0;
        (fo->in_file[0])->fio = io_stdin;
        (fo->out_file)->fio = io_stdout;
        (fo->err_file)->fio = io_stderr;
}

/*System calls, all arguments are on the stack.*/
mw forth_system_calls(fobj_t * fo, mw enum_syscall)
{
        mw *reg = fo->reg, *var = fo->var;
        char *str = fo->str;
        fio_t *out_file = fo->out_file, *err_file = fo->err_file;

        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
        TOS = var[VAR--];       /* Get rid of top of stack, it's just been used. */

        switch (enum_syscall) {
        case SYS_RESET:
                on_err(fo);
                return ERR_OK;
        case SYS_FOPEN:
                switch (TOS) {
                case SYS_OPT_IN:
                        if (IN_STRM < 0 || IN_STRM > MAX_INSTRM) {
                                ERR_LN_PRN(err_file);
                                return ERR_SYSCALL;
                        }
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--];

                        IN_STRM++;
                        if ((fo->in_file[IN_STRM]->iou.f =
                             fopen(str + TOS, "r")) == NULL) {
                                IN_STRM--;
                                ERR_LN_PRN(err_file);
                                return ERR_SYSCALL;
                        } else {
                                fo->in_file[IN_STRM]->fio = io_rd_file;
                        }
                        return ERR_OK;
                case SYS_OPT_OUT:
                        if (out_file->fio == io_stdin ||
                            out_file->fio == io_rd_str ||
                            out_file->fio == io_rd_file) {
                                ERR_LN_PRN(err_file);
                                return ERR_SYSCALL;
                        }
                        if (out_file->fio != io_stdout) {
                                fflush(out_file->iou.f);
                                fclose(out_file->iou.f);
                        }
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--];
                        if ((out_file->iou.f = fopen(str + TOS, "w")) == NULL) {
                                ERR_LN_PRN(err_file);
                                out_file->fio = io_stdout;
                                return ERR_SYSCALL;
                        } else {
                                out_file->fio = io_wr_file;
                        }
                        return ERR_OK;
                case SYS_OPT_ERR:
                default:
                        ERR_LN_PRN(err_file);
                        return ERR_SYSCALL_OPTIONS;
                }
                return ERR_OK;
        case SYS_FCLOSE:
                switch (TOS) {
                case SYS_OPT_IN:
                        if (IN_STRM < 1 || IN_STRM > MAX_INSTRM) {
                                ERR_LN_PRN(err_file);
                                return ERR_SYSCALL;
                        }
                        if (fo->in_file[IN_STRM]->fio != io_rd_file) {
                                ERR_LN_PRN(err_file);
                                return ERR_SYSCALL;
                        }
                        fo->in_file[IN_STRM]->fio = io_stdin;
                        fclose(fo->in_file[IN_STRM]->iou.f);
                        IN_STRM--;
                        return ERR_OK;
                case SYS_OPT_OUT:
                        if (out_file->fio == io_stdin ||
                            out_file->fio == io_rd_str ||
                            out_file->fio == io_rd_file) {
                                ERR_LN_PRN(err_file);
                                return ERR_SYSCALL;
                        }
                        if (out_file->iou.f != NULL)
                                if (out_file->fio != io_stdout) {
                                        fclose(out_file->iou.f);
                                        out_file->iou.f = NULL;
                                }
                        out_file->fio = io_stdout;
                        return ERR_OK;
                case SYS_OPT_ERR:
                default:
                        ERR_LN_PRN(err_file);
                        return ERR_SYSCALL_OPTIONS;
                }
                return ERR_OK;
        case SYS_FLUSH:
                fflush(NULL);   /* Flush all streams for ease of use. */
                return ERR_OK;
        case SYS_REMOVE:
                if (remove(str + TOS) != 0) {
                        ERR_LN_PRN(err_file);
                        print_string("Could not remove file.\n", MAX_ERR_STR,
                                     fo->err_file);
                }
                ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                TOS = var[VAR--];
                return ERR_OK;
        case SYS_RENAME:
                if (rename(str + TOS, str + var[VAR--]) != 0) {
                        ERR_LN_PRN(err_file);
                        print_string("Could not rename file.\n", MAX_ERR_STR,
                                     fo->err_file);
                }
                TOS = var[--VAR];
                VAR--;
                return ERR_OK;
        case SYS_REWIND:
                switch (TOS) {
                case SYS_OPT_IN:
                        if (fo->in_file[IN_STRM]->iou.f != NULL) {
                                if (fo->in_file[IN_STRM]->fio == io_rd_file) {
                                        rewind(fo->in_file[IN_STRM]->iou.f);
                                }
                        } else if (fo->in_file[IN_STRM]->iou.s != NULL
                                   && fo->in_file[IN_STRM]->fio == io_rd_str) {
                                fo->in_file[IN_STRM]->str_index = 0;
                        }
                        return ERR_OK;
                case SYS_OPT_OUT:
                        if (out_file->iou.f != NULL) {
                                if (out_file->fio == io_wr_file) {
                                        rewind(out_file->iou.f);
                                }
                        } else if (out_file->iou.s != NULL
                                   && out_file->fio == io_wr_str) {
                                out_file->str_index = 0;
                        }
                        return ERR_OK;
                case SYS_OPT_ERR:
                default:
                        ERR_LN_PRN(err_file);
                        return ERR_SYSCALL_OPTIONS;
                }
                return ERR_OK;
        default:
                return ERR_NOTSYSCALL;
        }
        return ERR_SYSCALL;
}

/*Forth interpreter*/
mw forth_interpreter(fobj_t * fo)
{

        /*copy pointers, removes level of indirection */
        mw *reg = fo->reg, *dic = fo->dic, *var = fo->var, *ret = fo->ret;
        char *str = fo->str;
        fio_t *out_file = fo->out_file, *err_file = fo->err_file;

        /*Initialization */
        if (INI == true) {
                if ((OP0 = forth_initialize(fo)) != ERR_OK) {
                        return OP0;
                }
        }

        /*VM*/ while (true) {
                ECB(0, SM_maxDic, PC, ERR_PC, err_file);
                NEXT = dic[PC++];

 TAIL_RECURSE:
#ifdef RUN4X
                if (CYCLES) {
                        if (CCOUNT > 0) {
                                CCOUNT--;
                        } else {
                                return ERR_CYCLES;
                        }
                }
#endif

                /*simple trace macros */
                ECUZ(SM_maxDic, NEXT, ERR_NEXT, err_file);
                switch (dic[NEXT]) {
                case PUSH_INT:
                        ECB(0, SM_maxVar, VAR, ERR_VAR, err_file);
                        var[++VAR] = TOS;
                        ECB(0, SM_maxDic, PC, ERR_PC, err_file);
                        TOS = dic[PC++];
                        break;
                case COMPILE:
                        ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
                        dic[DIC++] = NEXT + 1;
                        break;
                case RUN:
                        ECUZ(SM_maxRet, RET, ERR_RET, err_file);
                        ret[++RET] = PC;
                        PC = NEXT + 1;
                        break;
                case DEFINE:
                        CPF = true;
                        if (compile_word(COMPILE, fo) != ERR_OK) {
                                return ERR_FAILURE;
                        }
                        ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
                        dic[DIC++] = RUN;
                        break;
                case IMMEDIATE:
                        DIC -= 2;
                        ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
                        dic[DIC++] = RUN;
                        break;
                case READ:
                        /* I should split this read case into several
                         * simpler functions, these simpler functions
                         * could be used by the VM as extra primitives */
                        RET--;
                        if ((OP1 = find_word(fo)) != ERR_OK)    /*fo contains OP0 */
                                return OP1;
                        if (OP0 - 1) {
                                NEXT = OP0 + 2; /*Advance over pointers */
                                if (!CPF) {
                                        ECUZ(SM_maxDic, NEXT, ERR_NEXT,
                                             err_file);
                                        if (dic[NEXT] == COMPILE) {
                                                NEXT++;
                                        }
                                }
                                goto TAIL_RECURSE;
                        }
                        if (isnumber(str)) {
                                if (CPF) {
                                        ECUZ(SM_maxDic - 1, DIC, ERR_DIC,
                                             err_file);
                                        dic[DIC++] = PUSH_INT;
                                        dic[DIC++] = kr_atoi(str);
                                } else {
                                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                                        var[++VAR] = TOS;
                                        TOS = kr_atoi(str);
                                }
                        } else {
                                ERR_LN_PRN(err_file);
                                return ERR_WORD;
                        }
                        break;
                case COMMENT:
                        OP0 = 0;
                        while (true) {
                                OP0 = wrap_get(fo->in_file[IN_STRM]);
                                if (OP0 == '\n') {
                                        break;
                                } else if (OP0 == (mw) EOF) {
                                        ERR_LN_PRN(err_file);
                                        return ERR_EOF;
                                }
                        }
                        break;
                case EXIT:
                        ECUZ(SM_maxRet, RET, ERR_RET, err_file);
                        PC = ret[RET--];
                        break;
                case BRANCH:
                        ECUZ(SM_maxDic, PC, ERR_PC, err_file);
                        PC += dic[PC];
                        break;
                case NBRANCH:
                        if (!TOS) {
                                ECB(0, SM_maxDic, PC, ERR_PC, err_file);
                                PC += dic[PC];
                        } else {
                                PC++;
                        }
                        /*ECUZ(SM_maxDic, PC, ERR_PC); */
                        ECUZ(SM_maxVar, VAR, ERR_PC, err_file);
                        TOS = var[VAR--];
                        break;
                case PLUS:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] + TOS;
                        break;
                case MINUS:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] - TOS;
                        break;

                case MUL:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] * TOS;
                        break;
                case MOD:
                        if (TOS) {
                                ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                                TOS = var[VAR--] % TOS;
                                break;
                        }
                        ERR_LN_PRN(err_file);
                        return ERR_MOD0;
                case DIV:
                        if (TOS) {
                                ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                                TOS = var[VAR--] / TOS;
                                break;
                        }
                        ERR_LN_PRN(err_file);
                        return ERR_DIV0;
                case LS:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = (unsigned)var[VAR--] << (unsigned)TOS;
                        break;
                case RS:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = (unsigned)var[VAR--] >> (unsigned)TOS;
                        break;
                case AND:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] & TOS;
                        break;
                case OR:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] | TOS;
                        break;
                case INV:
                        TOS = ~TOS;
                        break;
                case XOR:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] ^ TOS;
                        break;
                case INC:
                        TOS++;
                        break;
                case DEC:
                        TOS--;
                        break;
                case EQ:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] == TOS;
                        break;
                case LESS:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] < TOS;
                        break;
                case MORE:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--] > TOS;
                        break;
                case FETCH_REG:
                        ECUZ(SM_maxReg, TOS, ERR_TOS_REG, err_file);
                        TOS = reg[TOS];
                        break;
                case FETCH_DIC:
                        ECUZ(SM_maxDic, TOS, ERR_TOS_DIC, err_file);
                        TOS = dic[TOS];
                        break;
                case FETCH_STR:
                        ECUZ(SM_maxStr, TOS, ERR_TOS_STR, err_file);
                        TOS = (mw) str[TOS];
                        break;
                case PICK:
                        ECUZ(SM_maxVar, VAR - TOS, ERR_TOS_VAR, err_file);
                        TOS = var[VAR - TOS];
                        break;
                case STORE_REG:
                        ECUZ(SM_maxReg, TOS, ERR_TOS_REG, err_file);
                        ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
                        reg[TOS] = var[VAR--];
                        TOS = var[VAR--];
                        break;
                case STORE_DIC:
                        ECUZ(SM_maxDic, TOS, ERR_TOS_DIC, err_file);
                        ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
                        dic[TOS] = var[VAR--];
                        TOS = var[VAR--];
                        break;
                case STORE_STR:
                        ECUZ(SM_maxStr, TOS, ERR_TOS_STR, err_file);
                        ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
                        str[TOS] = (char)var[VAR--];
                        TOS = var[VAR--];
                        break;
                case STORE_VAR:
                        ECUZ(SM_maxVar, TOS, ERR_TOS_VAR, err_file);
                        ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
                        var[TOS] = var[VAR--];
                        TOS = var[VAR--];
                        break;
                case KEY:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        var[++VAR] = TOS;
                        TOS = wrap_get(fo->in_file[IN_STRM]);
                        if (TOS == (mw) EOF) {
                                ERR_LN_PRN(err_file);
                                return ERR_EOF;
                        }
                        break;
                case EMIT:
                        /*need to check if putchar is winning or not */
                        wrap_put(out_file, (char)TOS);
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--];
                        break;
                case DROP:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--];
                        break;
                case DUP:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        var[++VAR] = TOS;
                        break;
                case SWAP:
                        OP0 = TOS;
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--];
                        var[++VAR] = OP0;
                        break;
                case OVER:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        var[++VAR] = TOS;
                        TOS = var[VAR - 1];
                        break;
                case TOR:
                        ECUZ(SM_maxRet - 1, RET, ERR_RET, err_file);
                        ret[RET + 1] = ret[RET];
                        ret[++RET] = TOS;
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--];
                        break;
                case FROMR:
                        ECUZ(SM_maxVar - 1, VAR, ERR_VAR, err_file);
                        var[++VAR] = TOS;
                        ECUZ(SM_maxRet, RET, ERR_RET, err_file);
                        TOS = ret[RET--];
                        break;
                case TAIL:
                        ECUZ(SM_maxRet, VAR, ERR_RET, err_file);
                        RET--;
                        break;
                case QUOTE:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        var[++VAR] = TOS;
                        ECB(0, SM_maxDic, PC, ERR_PC, err_file);
                        TOS = dic[PC];
                        PC++;
                        break;
                case COMMA:
                        ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
                        dic[DIC++] = TOS;
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        TOS = var[VAR--];
                        break;
                case PRINTNUM:
                        ECB(2, SM_maxVar, VAR, ERR_VAR, err_file);
                        if (var[VAR] > MAX_BASE) {
                                ERR_LN_PRN(err_file);
                                return ERR_BASE;
                        }
                        print_string(my_itoa(TOS, var[VAR--]), MAX_ERR_STR,
                                     out_file);
                        TOS = var[VAR--];
                        break;
                case GET_WORD:
                        ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
                        ECUZ(SM_maxStr, VAR, ERR_TOS_STR, err_file);
                        if (get_word
                            (str + TOS, SM_inputBufLen,
                             fo->in_file[IN_STRM]) == ERR_FAILURE) {
                                ERR_LN_PRN(err_file);
                                return ERR_EOF;
                        }
                        TOS = var[VAR--];
                        break;
                case STRLEN:
                        ECUZ(SM_maxStr, TOS, ERR_TOS_STR, err_file);
                        TOS = my_strlen(TOS + str, MAX_STRLEN);
                        break;
                case ISNUMBER:
                        ECUZ(SM_maxStr, TOS, ERR_TOS_STR, err_file);
                        TOS = isnumber(TOS + str);
                        break;
                case STRNEQU:
                        strnequ(str + TOS, str + var[VAR--], MAX_STRLEN,
                                MAX_STRLEN);
                        TOS = var[--VAR];
                        break;
                case FIND:
                        /*fo contains OP0 */
                        if ((OP1 = find_word(fo)) != ERR_OK)
                                return OP1;
                        /*Check if word found */
                        if (!(OP0 - 1)) {
                                ERR_LN_PRN(err_file);
                                return ERR_WORD;
                        }
                        ECB(0, SM_maxVar, VAR, ERR_VAR, err_file);
                        ECB(0, SM_maxDic - 3, VAR, ERR_OP0, err_file);
                        OP0 += 2;       /*advance over pointers */
                        if (dic[OP0] == COMPILE) {
                                ++OP0;
                        }
                        var[++VAR] = TOS;
                        TOS = OP0;
                        break;
                case EXECUTE:
                        ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
                        NEXT = TOS;
                        TOS = var[VAR--];
                        goto TAIL_RECURSE;
                case KERNEL:
                        if ((OP0 = forth_system_calls(fo, TOS)) != ERR_OK) {
                                ERR_LN_PRN(err_file);
                                return OP0;
                        }
                        break;
                default:
                        ERR_LN_PRN(err_file);
                        return ERR_INSTRUCTION;
                }
        }

        return ERR_ABNORMAL_END;
}

/*Error and IO handler for the Forth interpreter*/
mw forth_monitor(fobj_t * fo)
{
        char *null_err = "Fatal Err: forth_monitor() passed NULL pointer\n";
        mw tmp;
        mw *reg = fo->reg;

        if (fo == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        }

        if (fo->err_file == NULL) {     /*This doesn't work yet obviously. */
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        }

        if (fo->reg == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        }

        if (fo->dic == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        }

        if (fo->var == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        }

        if (fo->ret == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        }

        if (fo->str == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        }

        if (fo->in_file == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        } else {
                if (fo->in_file[IN_STRM]->fio != io_stdin) {
                        if (fo->in_file[IN_STRM]->iou.f == NULL) {
                                print_string(null_err, MAX_ERR_STR,
                                             fo->err_file);
                                return ERR_FAILURE;
                        }
                }
        }

        if (fo->out_file == NULL) {
                print_string(null_err, MAX_ERR_STR, fo->err_file);
                return ERR_FAILURE;
        } else {
                if (fo->out_file->fio != io_stdout
                    && fo->out_file->fio != io_stderr) {
                        if (fo->out_file->iou.f == NULL) {
                                print_string(null_err, MAX_ERR_STR,
                                             fo->err_file);
                                return ERR_FAILURE;
                        }
                }
        }

        /* I should check that EXF is potentially valid, ie. Not zero */
 RESTART:
        switch (tmp = forth_interpreter(fo)) {
        case ERR_OK:
                print_string("OK!\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_FAILURE:
                print_string("General Failure? Unknown cause.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_REG:
                print_string("Err: Register addr.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_DIC:
                print_string("Err: Dictionary addr.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_VAR:
                print_string("Err: Variable addr.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_RET:
                print_string("Err: Return addr.\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_STR:
                print_string("Err: String add.\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_PWD:
                print_string("Err: PWD!\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_NEXT:
                print_string("Err: NEXT!\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_PC:
                print_string("Err: PC!\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_TOS_REG:
                print_string("Err: TOS addr. Reg.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_TOS_DIC:
                print_string("Err: TOS addr. Dic.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_TOS_VAR:
                print_string("Err: TOS addr. Var.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_TOS_RET:
                print_string("Err: TOS addr. Ret.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_TOS_STR:
                print_string("Err: TOS addr. Str.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_OP0:
                print_string("Err: OP0!\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_OP1:
                print_string("Err: OP1!\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_DIV0:
                print_string("Err: Division by zero.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_MOD0:
                print_string("Err: Modulo by zero.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_IO:
                print_string("Err: IO Error.\n", MAX_ERR_STR, fo->err_file);
                /*Reset input to stdin in, if already stdin, quit, same with
                 * stdout?*/
                break;
        case ERR_EOF:          /*There are some problems with EOF error handling. */
                if (IN_STRM > 0 && IN_STRM < MAX_INSTRM) {
                        if (fo->in_file[IN_STRM]->fio == io_rd_file) {
                                if (fo->in_file[IN_STRM]->iou.f != NULL) {
                                        fclose(fo->in_file[IN_STRM]->iou.f);
                                        fo->in_file[IN_STRM]->iou.f = NULL;
                                }
                        }
                        IN_STRM--;
                        print_string("EOF -> Next stream.\n", MAX_ERR_STR,
                                     fo->err_file);

                        goto RESTART;
                }
                print_string("EOF\n", MAX_ERR_STR, fo->err_file);
                break;
        case ERR_BASE:
                print_string("Err: Base.\n", MAX_ERR_STR, fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_INSTRUCTION:
                print_string("Err: Illegal Instruction.\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_WORD:
                print_string(fo->str, MAX_ERR_STR, fo->err_file);
                wrap_put(fo->err_file, '\n');
                print_string("Err: Word not found?\n", MAX_ERR_STR,
                             fo->err_file);
                on_err(fo);
                goto RESTART;
        case ERR_ABNORMAL_END:
                print_string("Fatal Err: !!!Abnormal End!!!\n", MAX_ERR_STR,
                             fo->err_file);
                break;
        case ERR_GENERAL:
                print_string("General Error.\n", MAX_ERR_STR, fo->err_file);
                break;
        case ERR_SYSCALL:
                print_string("System call failed.\n", MAX_ERR_STR,
                             fo->err_file);
                break;
        case ERR_SYSCALL_OPTIONS:
                print_string("Not a system call option.\n", MAX_ERR_STR,
                             fo->err_file);
                break;
        case ERR_NOTSYSCALL:
                print_string("Err: Not a system call\n", MAX_ERR_STR,
                             fo->err_file);
                break;
        case ERR_MINIMUM_MEM:
                print_string("Fatal Err: Minimum memory requirements not met\n",
                             MAX_ERR_STR, fo->err_file);
                break;
        case ERR_CYCLES:       /*If cycles runs out, run this code. */
                print_string("Cycles complete\n", MAX_ERR_STR, fo->err_file);
                break;
        case HALT:
                print_string("HALTING FORTH\n", MAX_ERR_STR, fo->err_file);
                break;
        default:
                print_string("Fatal Err: Incorrect error code or call!\n",
                             MAX_ERR_STR, fo->err_file);
                return ERR_SYSCALL;
        }

        return ERR_ABNORMAL_END;
}
