/* 
 * Richard James Howe
 * Howe Forth.
 *
 * Desktop Interface
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include "forth.h"
#include "hosted.h"

#define DICTIONARY_OFFSET_M (4)
#define IOBUFFER_LENGTH_M   (256)

/*Define me via the command line.*/
#ifdef DEBUG_PRN
/*print out a table of integers*/
static void print_table(mw * p, int len, FILE * f);
static void print_char_table(char *p, int len, FILE * f);

static void debug_print(fobj_t * fo);
static fobj_t *forth_obj_create(mw reg_l, mw dic_l, mw var_l, mw ret_l, mw str_l);
static void forth_obj_destroy(fobj_t * fo);

static void print_table(mw * p, int len, FILE * f)
{
  int i;
  for (i = 0; i < len; i++)
    if (i % 4 == 0)
      fprintf(f, "\n%08X:\t\t%08X\t\t", i, p[i]);
    else
      fprintf(f, "%08X\t\t", p[i]);

  fprintf(f, "\n");
}

/*print out a character table*/
static void print_char_table(char *p, int len, FILE * f)
{
  int i;
  for (i = 0; i < len; i++)
    if (i % 4 == 0) {
      if (p[i] != '\0')
        fprintf(f, "\n%08X:\t\t%c\t\t", i, p[i]);
      else
        fprintf(f, "\n%08X:\t\t' '\t\t", i);
    } else {
      if (p[i] != '\0')
        fprintf(f, "%c\t\t", p[i]);
      else
        fprintf(f, "' '\t\t");
    }
  fprintf(f, "\n");
}

/*print out main memory.*/
void debug_print(fobj_t * fo)
{

  FILE *table_out;
  if ((table_out = fopen("memory.txt", "w")) == NULL) {
    printf("Unable to open log file!\n");
    return;
  }

  fprintf(table_out, "Registers:\n");
  print_table(fo->reg, MAX_REG, table_out);
  fprintf(table_out, "Dictionary:\n");
  print_table(fo->dic, MAX_DIC, table_out);
  fprintf(table_out, "Variable stack:\n");
  print_table(fo->var, MAX_VAR, table_out);
  fprintf(table_out, "Return stack:\n");
  print_table(fo->ret, MAX_RET, table_out);
  fprintf(table_out, "String storage:\n");
  print_char_table(fo->str, MAX_STR, table_out);

  fflush(table_out);
  fclose(table_out);

  fprintf(stderr, "Maximum memory usuage = %d\n",
          (sizeof(mw) * (MAX_REG + MAX_DIC + MAX_VAR + MAX_RET)) + (sizeof(char) * MAX_STR));
}
#endif

fobj_t *forth_obj_create(mw reg_l, mw dic_l, mw var_l, mw ret_l, mw str_l, FILE *input)
{
  /*the vm forth object */
  int i = 0;
  fobj_t *fo = calloc(1, sizeof(fobj_t));

  CALLOC_FAIL(fo, NULL);

  /*setting i/o streams */
  for (i = 0; i < MAX_INSTRM; i++) {
    fo->in_file[i] = calloc(1, sizeof(fio_t));
    CALLOC_FAIL(fo->in_file[i], NULL);
    fo->in_file[i]->fio = io_stdin;
  }

  fo->out_file = calloc(1, sizeof(fio_t));
  fo->err_file = calloc(1, sizeof(fio_t));

  CALLOC_FAIL(fo->in_file, NULL);
  CALLOC_FAIL(fo->out_file, NULL);
  CALLOC_FAIL(fo->err_file, NULL);

  fo->in_file[0]->fio = io_stdin;
  fo->out_file->fio = io_stdout;
  fo->err_file->fio = io_stderr;

  /*memories of the interpreter */
  fo->reg = calloc((unsigned)reg_l, sizeof(mw));
  fo->dic = calloc((unsigned)dic_l, sizeof(mw));
  fo->var = calloc((unsigned)var_l, sizeof(mw));
  fo->ret = calloc((unsigned)ret_l, sizeof(mw));
  fo->str = calloc((unsigned)str_l, sizeof(char));

  CALLOC_FAIL(fo->reg, NULL);
  CALLOC_FAIL(fo->dic, NULL);
  CALLOC_FAIL(fo->var, NULL);
  CALLOC_FAIL(fo->ret, NULL);
  CALLOC_FAIL(fo->str, NULL);

  /*initialize input file, fclose is handled elsewhere */
  fo->in_file[1]->fio = io_rd_file;
  if (NULL != input){
    fo->in_file[1]->iou.f = input;
  }
  else if ((fo->in_file[1]->iou.f = fopen("forth.4th", "r")) == NULL) {
    fprintf(stderr, "Unable to open initial input file!\n");
    return NULL;
  }

  /*initializing memory */
  fo->reg[ENUM_maxReg] = reg_l;
  fo->reg[ENUM_maxDic] = dic_l;
  fo->reg[ENUM_maxVar] = var_l;
  fo->reg[ENUM_maxRet] = ret_l;
  fo->reg[ENUM_maxStr] = str_l;
  fo->reg[ENUM_inputBufLen] = IOBUFFER_LENGTH_M;
  fo->reg[ENUM_dictionaryOffset] = DICTIONARY_OFFSET_M;
  fo->reg[ENUM_sizeOfMW] = (mw) sizeof(mw);
  fo->reg[ENUM_INI] = (mw) true;
  fo->reg[ENUM_cycles] = (mw) false;    /*Run for X amount of cycles turned off by default. */
  fo->reg[ENUM_ccount] = 0;     /*Run for X amount of cycles turned off by default. */
  fo->reg[ENUM_inStrm] = 1;
  fo->reg[ENUM_wordCount] = 0;
  fo->reg[ENUM_wordIndex] = 0;

#ifdef DEBUG_PRN
  fprintf(stderr, "\tOBJECT INITIALIZED.\n");
#endif
  return fo;
}

void forth_obj_destroy(fobj_t * fo)
{
  int i = 0;
  if (NULL != fo) {
    free(fo->reg);
    free(fo->dic);
    free(fo->var);
    free(fo->ret);
    free(fo->str);
    for (i = 0; i < MAX_INSTRM; i++)
      free(fo->in_file[i]);
    free(fo->out_file);
    free(fo->err_file);
    free(fo);
  }
#ifdef DEBUG_PRN
  fprintf(stderr, "\tOBJECT DESTROYED.\n");
#endif
}

