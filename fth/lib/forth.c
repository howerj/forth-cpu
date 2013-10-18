/** Howe Forth.
 * 
 * @file forth.c 
 * @brief A portable forth interpreter written in C. It can be used as a library
 * and is *very* easy to port to different platforms being written in
 * ANSI C.
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

#include <stdio.h>              /* FILE, putc(), getc(), fopen, fclose, rename... */
#include <stdlib.h>             /* system() */
#include <stdint.h>
#include "forth.h"

/*internal function prototypes*/
static enum bool my_isspace(char x);
static enum bool my_isdigit(char x);
static int wrap_get(fio_t * in_file);
static int wrap_put(fio_t * out_file, char c);
static mw get_word(char *str, const mw str_len, fio_t * io_file);
static mw isnumber(const char s[]);
static mw strnequ(const char str1[], const char str2[], const mw lim1, const mw lim2);
static mw my_strlen(const char s[], const int maxlen);
static mw kr_atoi(const char s[]);
static char *my_itoa(mw value, int base);
static void print_string(const char *s, const mw max, fio_t * out_file);
static void print_line_file(int line, const char *file, fio_t * err_file);
static mw find_word(fobj_t * fo);
static void my_strcpy(char *destination, const char *source);
static mw compile_word(forth_primitives_e fp, fobj_t * fo, enum bool flag, const char *prim);
static mw compile_word_prim(fobj_t * fo, const char *prim);
static mw forth_initialize(fobj_t * fo);
static void on_err(fobj_t * fo);
static mw forth_system_calls(fobj_t * fo, mw enum_syscall);
static void report_error(forth_errors_e e);

/*****************************************************************************/
/*X-Macro definition of error strings*/
#define X(a, b, c) b
static const char *forth_error_str[] = {
  FORTH_ERROR_XMACRO
};

#undef X

/**X-Macro definition of actions to take on error*/
#define X(a, b, c) c
static const forth_error_action_e f_error_action[] = {
  FORTH_ERROR_XMACRO
};

#undef X

/**X-Macro definition of the names of primitives*/
#define X(a, b) b
static const char *forth_primitives_str[] = {
  FORTH_PRIMITIVE_XMACRO_M
};

#undef X

/*****************************************************************************/

/* IO wrappers*/

/** my_isspace, here so we can customize what is treated as a space or not*/
static enum bool my_isspace(char x)
{
  switch (x) {
  case '\n':
  case '\r':
  case '\t':
  case '\v':
  case ' ':
  case '\f':
    return true;
  default:
    return false;
  }
}

/**Like my_isspace, my_isspace is here so we can customize what is
 * treated as a digit.*/
static enum bool my_isdigit(char x)
{
  switch (x) {
  case '0':
  case '1':
  case '2':
  case '3':
  case '4':
  case '5':
  case '6':
  case '7':
  case '8':
  case '9':
    return true;
  default:
    return false;
  }
}

/**Either get input from stdin, a string or a file*/
static int wrap_get(fio_t * in_file)
{
  char tmp;
  switch (in_file->fio) {
  case io_stdin:
    return getc(stdin);
  case io_rd_file:
    if (NULL != in_file->iou.f)
      return fgetc(in_file->iou.f);
    else
      return EOF;
  case io_rd_str:
    if (NULL != in_file->iou.s) {
      if (in_file->str_index > in_file->str_max_len)
        return EOF;
      tmp = in_file->iou.s[++in_file->str_index];
      if (tmp != '\0')
        return (int)tmp;
      else
        return EOF;
    } else
      return EOF;
  default:
    return EOF;
  }
}

/**Either put output to a stdout, stderr, a string or a file*/
static int wrap_put(fio_t * out_file, char c)
{
  switch (out_file->fio) {
  case io_stdout:
    return putc(c, stdout);
  case io_stderr:
    return putc(c, stderr);
  case io_wr_file:
    if (NULL != out_file->iou.f)
      return fputc(c, out_file->iou.f);
    else
      return EOF;
  case io_wr_str:
    if (NULL != out_file->iou.s) {
      if (out_file->str_index > out_file->str_max_len)
        return EOF;
      out_file->iou.s[++(out_file->str_index)] = c;
    } else {
      return EOF;
    }
    return ERR_OK;
  default:
    return EOF;
  }
}

/*Basic IO functions*/

/**This gets a space delimited FORTH word*/
static mw get_word(char *str, const mw str_len, fio_t * io_file)
{
  mw i;
  int c;

  /**memset */
  for (i = 0; i < str_len; i++)
    str[i] = '\0';

  /*discard spaces */
  for (c = wrap_get(io_file); my_isspace((char)c); c = wrap_get(io_file))
    if (EOF == c)
      return ERR_FAILURE;

  /*copy word */
  for (i = 0; (i < str_len) && (!my_isspace((char)c)); i++, c = wrap_get(io_file)) {
    str[i] = (char)c;
    if (EOF == c)
      return ERR_FAILURE;
  }

  /*fail if word is too long */
  if (i >= str_len)
    return ERR_FAILURE;

  /*null terminate */
  str[++i] = '\0';
  return ERR_OK;                /*success! */
}

/**Basic Utils*/
static mw isnumber(const char s[])
{
  mw x = 0;
  if ((('-' == s[x]) || ('+' == s[x])) && my_isdigit(s[x + 1]))
    x++;
  for (; '\0' != s[x]; x++) {
    if (!my_isdigit(s[x])) {
      return false;
    }
  }
  return (mw) true;
}

/** 1 if not equal, 0 if equal, 2 means string limit exceeded*/
static mw strnequ(const char str1[], const char str2[], const mw lim1, const mw lim2)
{

  char *s1 = (char *)str1, *s2 = (char *)str2;

  for (; (*s1 == *s2) && (s1 < str1 + lim1) && (s2 < str2 + lim2); s1++, s2++)
    if ('\0' == (*s1))
      return 0;

  if ((s1 >= str1 + lim1) || (s2 >= str2 + lim2)) {
    return 2;
  }
  return 1;
}

static mw my_strlen(const char s[], const int maxlen)
{
  int i = 0;
  char *p = (char *)s;
  while (('\0' != (*p)) && (i++ < maxlen))
    p++;
  return p - s;
}

/**K&R atoi, slighty modified*/
static mw kr_atoi(const char s[])
{
  mw i, n, sign;
  for (i = 0; my_isspace(s[i]); i++) ;

  sign = (s[i] == '-') ? -1 : 1;
  if ((s[i] == '+') || (s[i] == '-'))
    i++;
  for (n = 0; my_isdigit(s[i]); i++) {
    n = 10 * n + (mw) (s[i] - '0');
  }
  return sign * n;
}

/**TODO buf needs replacing with an external pointer
 * Refactor code*/
#define MAX_BASE 16
static char *my_itoa(mw value, int base)
{
  mw v = value, i = 0;
  static char buf[MAX_ERR_STR];

  for (i = 0; i < MAX_ERR_STR; i++)
    buf[i] = '\0';

  i = MAX_ERR_STR - 2;

  if (0 == value) {
    buf[i] = '0';
    return buf + i;
  }
  if ((signed)value < 0)
    v *= -1;
  if ((base > 1) && (base <= MAX_BASE))
    for (; (0 != v) && (0 != i); --i, v /= base)
      buf[i] = "0123456789abcdef"[v % base];
  if ((signed)value < 0) {
    buf[i] = '-';
    return buf + i;
  }
  return buf + i + 1;
}

/**print a string, nothing special, Error handling needed!*/
static void print_string(const char *s, const mw max, fio_t * out_file)
{
  mw i;
  for (i = 0; (i < max) && ('\0' != s[i]); i++) {
    (void)wrap_put(out_file, s[i]);
  }
}

/**print a line and a file*/
static void print_line_file(int line, const char *file, fio_t * err_file)
{
  print_string(my_itoa(line, 10), MAX_ERR_STR, err_file);
  (void)wrap_put(err_file, '\t');
  print_string(file, MAX_ERR_STR, err_file);
  (void)wrap_put(err_file, '\n');
}

#ifndef UNCHECK /** define UNCHECK to stop bounds being checked */

/*Error Check Bounds*/
#define ECB(MIN,MAX,TEST,ERR,ERR_STRM)    if((TEST>(MAX-2))||(TEST<MIN)){ \
    print_line_file(__LINE__,__FILE__,ERR_STRM);                          \
    return ERR;                                                           \
}

#if (1 == SIGNED_WORD) /** Signed type, check against upper bound and < 0 */
/*Error Check Upper (Bound) (and) Zero (Lower bound)*/
#define ECUZ(MAX,VT,ERR,ERR_STRM) if((VT>(MAX-2))||(VT<0)){  \
    print_line_file(__LINE__,__FILE__,ERR_STRM);             \
    return ERR;                                              \
}

#else /** Do not check against less than zero as we are using and unsigned type */

#define ECUZ(MAX,VT,ERR,ERR_STRM) if(VT>(MAX-2)){            \
    print_line_file(__LINE__,__FILE__,ERR_STRM);             \
    return ERR;                                              \
}

#endif

#define ERR_LN_PRN(ERR_STRM) print_line_file(__LINE__,__FILE__,ERR_STRM);

#else /** No bounds checking */

#define ECB(MIN,MAX,TEST,ERR,ERR_STRM)
#define ECUZ(MAX,VT,ERR,ERR_STRM)
#define ERR_LN_PRN(ERR_STRM)

#endif /** End bounds ifdef */

/** INT_MAX / -1 is an error, we need to check for this condition
 * if and only if the data type we are using is signed */
#if   (1 == SIGNED_WORD)
#define SIGNED_DIVIDE_CHECK(DIVIDEND,DIVISOR) ((WORD_MIN == (DIVISOR)) && (-1 == (DIVIDEND)))
#else
#define SIGNED_DIVIDE_CHECK(DIVIDEND,DIVISOR) (0)
#endif

/*****************************************************************************/
/* Begin the interpreter!                                                    */
/*****************************************************************************/

/*Set OP0 to found word, if found, return error otherwise.*/
static mw find_word(fobj_t * fo)
{
  mw *reg = fo->reg, *dic = fo->dic;
  char *str = fo->str;
  fio_t *in_file = fo->in_file[IN_STRM], *err_file = fo->err_file;

  if (ERR_FAILURE == get_word(str, SM_inputBufLen, in_file)) {
    ERR_LN_PRN(err_file);
    return ERR_EOF;
  }
  OP0 = PWD;
  ECUZ(SM_maxDic, OP0 + 1, ERR_OP0, err_file);
  ECUZ(SM_maxStr, dic[OP0 + 1], ERR_DIC, err_file);

  WORDINX = 0;
  while ((OP1 = strnequ(str, &str[dic[OP0 + 1]], SM_inputBufLen, SM_maxStr)) != 0) {
    if (OP1 == 2) {             /*2 signifies string limited exceeded. */
      ERR_LN_PRN(err_file);
      return ERR_IO;
    }
    ECUZ(SM_maxDic, OP0, ERR_OP0, err_file);

    /* Sanity check, prevents loops */
    if (WORDINX > WORDCNT) {
      ERR_LN_PRN(err_file);
      return ERR_PWD;
    }
    OP0 = dic[OP0];
    WORDINX++;
  }

  return ERR_OK;
}

/**my_strcpy, so I would not have to import all of string.h
 * in an embedded system
 */
static void my_strcpy(char *destination, const char *source)
{
  while ('\0' != *source)
    *destination++ = *source++;
}

static mw compile_word(forth_primitives_e fp, fobj_t * fo, enum bool flag, const char *prim)
{
  mw *reg = fo->reg, *dic = fo->dic;
  char *str = fo->str;
  fio_t *in_file = fo->in_file[IN_STRM], *err_file = fo->err_file;

  ECUZ(SM_maxDic - 3, DIC, ERR_DIC, err_file);
  dic[DIC] = PWD;
  DIC++;
  PWD = DIC - 1;
  dic[DIC] = STR;
  DIC++;
  dic[DIC] = fp;
  DIC++;
  if (flag)
    my_strcpy(str + STR, prim);
  else if (ERR_OK != get_word(str + STR, SM_inputBufLen, in_file))
    return ERR_FAILURE;
  STR += my_strlen(str + STR, MAX_STRLEN) + 1;
  WORDCNT++;
  return ERR_OK;
}

/*should add error checking.*/
static mw compile_word_prim(fobj_t * fo, const char *prim)
{
  mw *reg = fo->reg, *dic = fo->dic, ret;
  ret = compile_word(COMPILE, fo, true, prim);
  dic[DIC] = OP0;
  DIC++;
  OP0++;
  /*Here in case I want to return error codes instead. */
  return ret;
}

#define COMPILE_PRIM(X)  if(compile_word_prim(fo,(X))!=ERR_OK){ return ERR_FAILURE; }
static mw forth_initialize(fobj_t * fo)
{
  mw *reg = fo->reg, *dic = fo->dic, *ret = fo->ret;
  fio_t *err_file = fo->err_file;
  unsigned int i;

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
  if (compile_word(DEFINE, fo, true, ":") != ERR_OK) {
    return ERR_FAILURE;
  }
  if (compile_word(IMMEDIATE, fo, true, "immediate") != ERR_OK) {
    return ERR_FAILURE;
  }

  /*define a special word (read) */
  if (compile_word(COMPILE, fo, true, "read") != ERR_OK) {
    return ERR_FAILURE;
  }
  OP0 = DIC;
  ECUZ(SM_maxDic - 2, DIC, ERR_DIC, err_file);
  dic[DIC] = READ;
  DIC++;
  dic[DIC] = RUN;
  DIC++;
  PC = DIC;
  ECUZ(SM_maxDic - 2, DIC, ERR_DIC, err_file);
  dic[DIC] = OP0;
  DIC++;
  dic[DIC] = PC - 1;
  DIC++;
  /*...end special word definition */

  /*more immediate words */
  if (compile_word(COMMENT, fo, true, "\\") != ERR_OK) {
    return ERR_FAILURE;
  }
  OP0 = EXIT;

  for (i = EXIT; i < LAST_PRIMITIVE; i++) {
    COMPILE_PRIM(forth_primitives_str[i]);
  }

  ECUZ(SM_maxRet, RET, ERR_RET, err_file);
  RET++;
  ret[RET] = DIC;
  CPF = (mw) true;
  INI = (mw) false;

  return ERR_OK;
}

/* on_err();
 * Call the word pointered to in register EXF when
 * an error occurs and reset the VM into a workable
 * state.
 */
static void on_err(fobj_t * fo)
{
  (void)fflush(NULL);
  fo->CPF = (mw) false;
  fo->VAR = 0;
  fo->TOS = 0;
  fo->RET = 1;
  fo->PC = fo->EXF;
  fo->IN_STRM = 0;
  (fo->in_file[0])->fio = io_stdin;
  (fo->out_file)->fio = io_stdout;
  (fo->err_file)->fio = io_stderr;
}

/*
 * mw (*forth_system_calls_handler)(fobj_t* fo, mw enum_syscall) = NULL;
 *
 * Using the above, it should be possible to define placeholders for
 * all the system dependant stuff, for example the file I/O, even
 * for getc and putc as well.
 *
 */

/*System calls, all arguments are on the stack. You will need to
 *edit this for your particular system, for example a uC*/
static mw forth_system_calls(fobj_t * fo, mw enum_syscall)
{
  mw *reg = fo->reg, *var = fo->var;
  char *str = fo->str;
  fio_t *out_file = fo->out_file, *err_file = fo->err_file;

  ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
  TOS = var[VAR];               /* Get rid of top of stack, it's just been used. */
  VAR--;

  switch (enum_syscall) {
  case SYS_RESET:
    on_err(fo);
    return ERR_OK;
  case SYS_FOPEN:
    switch (TOS) {
    case SYS_OPT_IN:

      ECUZ(MAX_INSTRM, IN_STRM, ERR_SYSCALL, err_file);
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR];
      VAR--;

      IN_STRM++;
      if (NULL == (fo->in_file[IN_STRM]->iou.f = fopen(str + TOS, "r"))) {
        IN_STRM--;
        ERR_LN_PRN(err_file);
        return ERR_SYSCALL;
      } else {
        fo->in_file[IN_STRM]->fio = io_rd_file;
      }
      return ERR_OK;
    case SYS_OPT_OUT:
      if (out_file->fio == io_stdin || out_file->fio == io_rd_str || out_file->fio == io_rd_file) {
        ERR_LN_PRN(err_file);
        return ERR_SYSCALL;
      }
      if (out_file->fio != io_stdout) {
        (void)fflush(out_file->iou.f);
        (void)fclose(out_file->iou.f);
      }
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR];
      VAR--;
      if (NULL == (out_file->iou.f = fopen(str + TOS, "w"))) {
        ERR_LN_PRN(err_file);
        out_file->fio = io_stdout;
        return ERR_SYSCALL;
      } else {
        out_file->fio = io_wr_file;
      }
      return ERR_OK;
    case SYS_OPT_ERR:
      ERR_LN_PRN(err_file);
      return ERR_SYSCALL_OPTIONS;
    default:
      ERR_LN_PRN(err_file);
      return ERR_SYSCALL_OPTIONS;
    }
  case SYS_FCLOSE:
    switch (TOS) {
    case SYS_OPT_IN:

      ECB(1, MAX_INSTRM, IN_STRM, ERR_SYSCALL, err_file);

      if (fo->in_file[IN_STRM]->fio != io_rd_file) {
        ERR_LN_PRN(err_file);
        return ERR_SYSCALL;
      }
      fo->in_file[IN_STRM]->fio = io_stdin;
      (void)fclose(fo->in_file[IN_STRM]->iou.f);
      IN_STRM--;
      return ERR_OK;
    case SYS_OPT_OUT:
      if (out_file->fio == io_stdin || out_file->fio == io_rd_str || out_file->fio == io_rd_file) {
        ERR_LN_PRN(err_file);
        return ERR_SYSCALL;
      }
      if (NULL != out_file->iou.f)
        if (io_stdout != out_file->fio) {
          (void)fclose(out_file->iou.f);
          out_file->iou.f = NULL;
        }
      out_file->fio = io_stdout;
      return ERR_OK;
    case SYS_OPT_ERR:
      ERR_LN_PRN(err_file);
      return ERR_SYSCALL_OPTIONS;
    default:
      ERR_LN_PRN(err_file);
      return ERR_SYSCALL_OPTIONS;
    }
  case SYS_FLUSH:
    (void)fflush(NULL);         /* Flush all streams for ease of use. */
    return ERR_OK;
  case SYS_REMOVE:
    if (0 != remove(str + TOS)) {
      ERR_LN_PRN(err_file);
      print_string("Could not remove file.\n", MAX_ERR_STR, fo->err_file);
    }
    ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
    TOS = var[VAR];
    VAR--;
    return ERR_OK;
  case SYS_RENAME:
    if (0 != rename(str + TOS, str + var[VAR])) {
      VAR--;
      ERR_LN_PRN(err_file);
      print_string("Could not rename file.\n", MAX_ERR_STR, fo->err_file);
    }
    VAR--;
    TOS = var[VAR];
    VAR--;
    return ERR_OK;
  case SYS_REWIND:
    switch (TOS) {
    case SYS_OPT_IN:
      if (NULL != fo->in_file[IN_STRM]->iou.f) {
        if (io_rd_file == fo->in_file[IN_STRM]->fio) {
          rewind(fo->in_file[IN_STRM]->iou.f);
        }
      } else if (NULL != fo->in_file[IN_STRM]->iou.s) {
        if (io_rd_str == fo->in_file[IN_STRM]->fio) {
          fo->in_file[IN_STRM]->str_index = 0;
        }
      }
      return ERR_OK;
    case SYS_OPT_OUT:
      if (NULL != out_file->iou.f) {
        if (io_wr_file == out_file->fio) {
          rewind(out_file->iou.f);
        }
      } else if (NULL != out_file->iou.s) {
        if (io_wr_str == out_file->fio) {
          out_file->str_index = 0;
        }
      }
      return ERR_OK;
    case SYS_OPT_ERR:
    default:
      ERR_LN_PRN(err_file);
      return ERR_SYSCALL_OPTIONS;
    }
  case SYS_SYSTEM:
    system(str + TOS);
    ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
    TOS = var[VAR];
    VAR--;
    return ERR_OK;
  default:
    return ERR_NOTSYSCALL;
  }
}

/*Forth interpreter*/
mw forth_interpreter(fobj_t * fo)
{

  /*copy pointers, removes level of indirection */
  mw *reg = fo->reg, *dic = fo->dic, *var = fo->var, *ret = fo->ret;
  char *str = fo->str;
  fio_t *out_file = fo->out_file, *err_file = fo->err_file;

  /*Initialization */
  if (INI == (mw) true) {
    if ((OP0 = forth_initialize(fo)) != ERR_OK) {
      return OP0;
    }
  }

  /*VM*/ while (true) {
    ECUZ(SM_maxDic, PC, ERR_PC, err_file);
    NEXT = dic[PC];
    PC++;

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
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      VAR++;
      var[VAR] = TOS;
      ECUZ(SM_maxDic, PC, ERR_PC, err_file);
      TOS = dic[PC];
      PC++;
      break;
    case COMPILE:
      ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
      dic[DIC] = NEXT + 1;
      DIC++;
      break;
    case RUN:
      ECUZ(SM_maxRet, RET, ERR_RET, err_file);
      RET++;
      ret[RET] = PC;
      PC = NEXT + 1;
      break;
    case DEFINE:
      CPF = (mw) true;
      if (compile_word(COMPILE, fo, false, NULL) != ERR_OK) {
        return ERR_FAILURE;
      }
      ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
      dic[DIC] = RUN;
      DIC++;
      break;
    case IMMEDIATE:
      DIC -= 2;
      ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
      dic[DIC] = RUN;
      DIC++;
      break;
    case READ:
      /* I should split this read case into several
       * simpler functions, these simpler functions
       * could be used by the VM as extra primitives */
      RET--;
      if (ERR_OK != (OP1 = find_word(fo)))      /*fo contains OP0 */
        return OP1;
      if (0 != (OP0 - 1)) {
        NEXT = OP0 + 2;         /*Advance over pointers */
        if (0 == CPF) {
          ECUZ(SM_maxDic, NEXT, ERR_NEXT, err_file);
          if (COMPILE == dic[NEXT]) {
            NEXT++;
          }
        }
        goto TAIL_RECURSE;
      }
      if (0 != isnumber(str)) {
        if (0 != CPF) {
          ECUZ(SM_maxDic - 1, DIC, ERR_DIC, err_file);
          dic[DIC] = PUSH_INT;
          DIC++;
          dic[DIC] = kr_atoi(str);
          DIC++;
        } else {
          ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
          VAR++;
          var[VAR] = TOS;
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
        if (OP0 == (mw) '\n') {
          break;
        } else if (OP0 == (mw) EOF) {
          ERR_LN_PRN(err_file);
          return ERR_EOF;
        }
      }
      break;
    case EXIT:
      ECUZ(SM_maxRet, RET, ERR_RET, err_file);
      PC = ret[RET];
      RET--;
      break;
    case BRANCH:
      ECUZ(SM_maxDic, PC, ERR_PC, err_file);
      PC += dic[PC];
      break;
    case NBRANCH:
      if (0 == TOS) {
        ECUZ(SM_maxDic, PC, ERR_PC, err_file);
        PC += dic[PC];
      } else {
        PC++;
      }
      /*ECUZ(SM_maxDic, PC, ERR_PC); */
      ECUZ(SM_maxVar, VAR, ERR_PC, err_file);
      TOS = var[VAR];
      VAR--;
      break;
    case PLUS:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR] + TOS;
      VAR--;
      break;
    case MINUS:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR] - TOS;
      VAR--;
      break;
    case MUL:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR] * TOS;
      VAR--;
      break;
    case MOD:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      if ((0 == TOS) || SIGNED_DIVIDE_CHECK(TOS, var[VAR])) {
        ERR_LN_PRN(err_file);
        return ERR_MOD0;
      } else {
        TOS = var[VAR] % TOS;
        VAR--;
        break;
      }
    case DIV:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      if ((0 == TOS) || SIGNED_DIVIDE_CHECK(TOS, var[VAR])) {
        ERR_LN_PRN(err_file);
        return ERR_DIV0;
      } else {
        TOS = var[VAR] / TOS;
        VAR--;
        break;
      }
    case LS:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      /** Unsigned shifts only and only by BIT_SIZE - 1 to avoid
       * undefined behavior */
      TOS = (mw) ((unsigned)var[VAR] << ((unsigned)TOS & (BIT_SIZE - 1)));
      VAR--;
      break;
    case RS:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      /** Unsigned shifts only and only by BIT_SIZE - 1 to avoid
       * undefined behavior */
      TOS = (mw) ((unsigned)var[VAR] >> (unsigned)TOS & (BIT_SIZE - 1));
      VAR--;
      break;
    case AND:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR] & TOS;
      VAR--;
      break;
    case OR:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR] | TOS;
      VAR--;
      break;
    case INV:
      TOS = ~TOS;
      break;
    case XOR:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR] ^ TOS;
      VAR--;
      break;
    case INC:
      TOS++;
      break;
    case DEC:
      TOS--;
      break;
    case EQ:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = (mw) (var[VAR] == TOS);
      VAR--;
      break;
    case LESS:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = (mw) (var[VAR] < TOS);
      VAR--;
      break;
    case MORE:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = (mw) (var[VAR] > TOS);
      VAR--;
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
      reg[TOS] = var[VAR];
      VAR--;
      TOS = var[VAR];
      VAR--;
      break;
    case STORE_DIC:
      ECUZ(SM_maxDic, TOS, ERR_TOS_DIC, err_file);
      ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
      dic[TOS] = var[VAR];
      VAR--;
      TOS = var[VAR];
      VAR--;
      break;
    case STORE_STR:
      ECUZ(SM_maxStr, TOS, ERR_TOS_STR, err_file);
      ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
      str[TOS] = (char)var[VAR];
      VAR--;
      TOS = var[VAR];
      VAR--;
      break;
    case STORE_VAR:
      ECUZ(SM_maxVar, TOS, ERR_TOS_VAR, err_file);
      ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
      var[TOS] = var[VAR];
      VAR--;
      TOS = var[VAR];
      VAR--;
      break;
    case KEY:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      VAR++;
      var[VAR] = TOS;
      TOS = wrap_get(fo->in_file[IN_STRM]);
      if ((mw) EOF == TOS) {
        ERR_LN_PRN(err_file);
        return ERR_EOF;
      }
      break;
    case EMIT:
      /*need to check if putchar is winning or not */
      if (EOF == wrap_put(out_file, (char)TOS)) {
        ERR_LN_PRN(err_file);
        return ERR_EOF;
      }
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR];
      VAR--;
      break;
    case DROP:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR];
      VAR--;
      break;
    case DUP:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      VAR++;
      var[VAR] = TOS;
      break;
    case SWAP:
      OP0 = TOS;
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR];
      var[VAR] = OP0;
      break;
    case OVER:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      VAR++;
      var[VAR] = TOS;
      TOS = var[VAR - 1];
      break;
    case TOR:
      ECUZ(SM_maxRet - 1, RET, ERR_RET, err_file);
      ret[RET + 1] = ret[RET];
      RET++;
      ret[RET] = TOS;
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR];
      VAR--;
      break;
    case FROMR:
      ECUZ(SM_maxVar - 1, VAR, ERR_VAR, err_file);
      VAR++;
      var[VAR] = TOS;
      ECUZ(SM_maxRet, RET, ERR_RET, err_file);
      TOS = ret[RET];
      RET--;
      break;
    case TAIL:
      ECUZ(SM_maxRet, VAR, ERR_RET, err_file);
      RET--;
      break;
    case QUOTE:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      VAR++;
      var[VAR] = TOS;
      ECUZ(SM_maxDic, PC, ERR_PC, err_file);
      TOS = dic[PC];
      PC++;
      break;
    case COMMA:
      ECUZ(SM_maxDic, DIC, ERR_DIC, err_file);
      dic[DIC] = TOS;
      DIC++;
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      TOS = var[VAR];
      VAR--;
      break;
    case PRINTNUM:
      ECB(2, SM_maxVar, VAR, ERR_VAR, err_file);
      if (MAX_BASE < var[VAR]) {
        ERR_LN_PRN(err_file);
        return ERR_BASE;
      }
      OP0 = TOS;
      OP1 = var[VAR];
      VAR--;
      print_string(my_itoa(OP0, OP1), MAX_ERR_STR, out_file);
      TOS = var[VAR];
      VAR--;
      break;
    case GET_WORD:
      ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
      ECUZ(SM_maxStr, VAR, ERR_TOS_STR, err_file);
      if (get_word(str + TOS, SM_inputBufLen, fo->in_file[IN_STRM]) == ERR_FAILURE) {
        ERR_LN_PRN(err_file);
        return ERR_EOF;
      }
      TOS = var[VAR];
      VAR--;
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
      ECB(1, SM_maxVar, VAR, ERR_VAR, err_file);
      ECUZ(SM_maxStr, TOS, ERR_TOS_STR, err_file);
      ECUZ(SM_maxStr, var[VAR], ERR_TOS_STR, err_file);
      TOS = strnequ(str + TOS, str + var[VAR], MAX_STRLEN, MAX_STRLEN);
      VAR--;
      break;
    case FIND:
      /*fo contains OP0 */
      if (ERR_OK != (OP1 = find_word(fo)))
        return OP1;
      /*Check if word found */
      if (0 == (OP0 - 1)) {
        ERR_LN_PRN(err_file);
        return ERR_WORD;
      }
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      ECUZ(SM_maxDic - 3, VAR, ERR_OP0, err_file);
      OP0 += 2;                 /*advance over pointers */
      if (COMPILE == dic[OP0]) {
        ++OP0;
      }
      VAR++;
      var[VAR] = TOS;
      TOS = OP0;
      break;
    case EXECUTE:
      ECUZ(SM_maxVar, VAR, ERR_VAR, err_file);
      NEXT = TOS;
      TOS = var[VAR];
      VAR--;
      goto TAIL_RECURSE;
    case KERNEL:
      if (ERR_OK != (OP0 = forth_system_calls(fo, TOS))) {
        ERR_LN_PRN(err_file);
        return OP0;
      }
      break;
    case ERROR:
      return TOS;
    default:
      ERR_LN_PRN(err_file);
      return ERR_INSTRUCTION;
    }
  }
}

static void report_error(forth_errors_e e)
{
  fio_t err_file_local;
  err_file_local.fio = io_stderr;
  print_string(forth_error_str[e], MAX_ERR_STR, &err_file_local);
  return;
}

/*Error and IO handler for the Forth interpreter*/
#define NULLCHK_M(X)  if(NULL == (X)){ report_error(ERR_NULL); return ERR_FAILURE; }
mw forth_monitor(fobj_t * fo)
{
  mw fo_returned_val;
  mw *reg;

  /*pointer checks */
  NULLCHK_M(fo);

  reg = fo->reg;

  NULLCHK_M(fo->reg);
  NULLCHK_M(fo->dic);
  NULLCHK_M(fo->var);
  NULLCHK_M(fo->ret);
  NULLCHK_M(fo->str);
  NULLCHK_M(fo->err_file);

  /*point and sanity checks of the in and output files */
  if (NULL == fo->in_file) {
    report_error(ERR_NULL);
    return ERR_FAILURE;
  } else {
    if (io_stdin != fo->in_file[IN_STRM]->fio) {
      if (NULL == fo->in_file[IN_STRM]->iou.f) {
        report_error(ERR_NULL);
        return ERR_FAILURE;
      }
    }
  }

  if (NULL == fo->out_file) {
    report_error(ERR_NULL);
    return ERR_FAILURE;
  } else {
    if ((io_stdout != fo->out_file->fio)
        && (io_stderr != fo->out_file->fio)) {
      if (NULL == fo->out_file->iou.f) {
        report_error(ERR_NULL);
        return ERR_FAILURE;
      }
    }
  }

  /* I should check that EXF is potentially valid, ie. Not zero */
  while(true){
    fo_returned_val = forth_interpreter(fo);
    if (fo_returned_val >= LAST_ERROR) {
      print_string(forth_error_str[LAST_ERROR], MAX_ERR_STR, fo->err_file);
      return fo_returned_val;
    } else if (onerr_goto_restart_e == f_error_action[fo_returned_val]) {
      print_string(forth_error_str[fo_returned_val], MAX_ERR_STR, fo->err_file);
      on_err(fo);
      continue;
    } else if (onerr_break_e == f_error_action[fo_returned_val]) {
      print_string(forth_error_str[fo_returned_val], MAX_ERR_STR, fo->err_file);
      return fo_returned_val;
    } else if (onerr_special_e == f_error_action[fo_returned_val]) {
      if (fo_returned_val == ERR_EOF) {   /*Some EOF situations might not be handled correctly */
        if ((0 < IN_STRM) && (MAX_INSTRM > IN_STRM)) {
          if (io_rd_file == fo->in_file[IN_STRM]->fio) {
            if (NULL != fo->in_file[IN_STRM]->iou.f) {
              (void)fclose(fo->in_file[IN_STRM]->iou.f);
              fo->in_file[IN_STRM]->iou.f = NULL;
            }
          }
          IN_STRM--;
          print_string(forth_error_str[ERR_NEXT_STRM], MAX_ERR_STR, fo->err_file);
          continue;
        }
        print_string(forth_error_str[ERR_EOF], MAX_ERR_STR, fo->err_file);
        return ERR_EOF;
      } else if (ERR_WORD == fo_returned_val) {   /*Special action: Print out word */
        print_string(fo->str, MAX_ERR_STR, fo->err_file);
        (void)wrap_put(fo->err_file, '\n');
        print_string(forth_error_str[ERR_WORD], MAX_ERR_STR, fo->err_file);
        on_err(fo);
        continue;
      } else {                    /*No special action defined */
        print_string(forth_error_str[ERR_SPECIAL_ERROR], MAX_ERR_STR, fo->err_file);
        return ERR_SPECIAL_ERROR;
      }
    }
  }
  return ERR_ABNORMAL_END;
}

#undef NULLCHK_M
