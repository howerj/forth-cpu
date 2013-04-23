/* 
 * Richard James Howe
 * Howe Forth.
 *
 * Top level .c file, sets up structures and I/O
 * to pass to the Forth interpreter. It's basically
 * a simple wrapper.
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include "forth.h"

/* This will give us enough memory for most of our applications */
#define MAX_REG 32
#define MAX_DIC (1024*1024*8/sizeof(mw))
#define MAX_VAR (1024)
#define MAX_RET (1024)
#define MAX_STR (1024*1024*8/sizeof(mw))

/* // Debugging Only.
void print_table(mw *p,int len,FILE* f){
    int i;
    for(i=0;i<len;i++)
        if(i%4==0)
            fprintf(f,"\n%d:\t\t%d\t\t",i,p[i]);
        else
            fprintf(f,"%d\t\t",p[i]);

    fprintf(f,"\n");
}

void print_char_table(char *p,int len,FILE* f){
    int i;
    for(i=0;i<len;i++)
        if(i%4==0){
            if(p[i]!='\0')
                fprintf(f,"\n%d:\t\t%c\t\t",i,p[i]);
            else
                fprintf(f,"\n%d:\t\t' '\t\t",i);
        }
        else{
            if(p[i]!='\0')
                fprintf(f,"%c\t\t",p[i]);
            else
                fprintf(f,"' '\t\t");
        }
    fprintf(f,"\n");
}
*/

int main(void)
{
	fio_t in_file = { io_stdin, 0, 0, {NULL} };
	fio_t out_file = { io_stdout, 0, 0, {NULL} };
	fio_t err_file = { io_stderr, 0, 0, {NULL} };

	mw *reg = NULL;
	mw *dic = NULL;
	mw *var = NULL;
	mw *ret = NULL;
	char *str = NULL;

	/* // Debugging only.
	 * FILE *table_out;
	 */
	fobj_t fo;

	reg = calloc(MAX_REG, sizeof(mw));
	dic = calloc(MAX_DIC, sizeof(mw));
	var = calloc(MAX_VAR, sizeof(mw));
	ret = calloc(MAX_RET, sizeof(mw));
	str = calloc(MAX_STR, sizeof(char));

    if(reg==NULL||dic==NULL||var==NULL||ret==NULL||str==NULL){
        fprintf(stderr, "Unable to calloc(), out of memory?\n");
        return 1;
    }

	in_file.fio = io_rd_file;
	if ((in_file.iou.f = fopen("start.fs", "r")) == NULL) {
		fprintf(stderr, "Unable to open initial input file!\n");
		return 1;
	}

	SM_maxReg = MAX_REG;
	SM_maxDic = MAX_DIC;
	SM_maxVar = MAX_VAR;
	SM_maxRet = MAX_RET;
	SM_maxStr = MAX_STR;
	SM_inputBufLen = 32;
	SM_dictionaryOffset = 4;
	SM_sizeOfMW = sizeof(mw);
	INI = true;

	fo.in_file = &in_file;
	fo.out_file = &out_file;
	fo.err_file = &err_file;
	fo.reg = reg;
	fo.dic = dic;
	fo.var = var;
	fo.ret = ret;
	fo.str = str;

	forth_monitor(&fo);

	/* // Debugging only.
	   if((table_out=fopen("memory.txt","w"))==NULL){
	   printf("Unable to open log file!\n");
	   return 1;
	   }

	   fprintf(table_out,"Registers:\n");
	   print_table(fo.reg,MAX_REG,table_out);
	   fprintf(table_out,"Dictionary:\n");
	   print_table(fo.dic,MAX_DIC,table_out);
	   fprintf(table_out,"Variable stack:\n");
	   print_table(fo.var,MAX_VAR,table_out);
	   fprintf(table_out,"Return stack:\n");
	   print_table(fo.ret,MAX_RET,table_out);
	   fprintf(table_out,"String storage:\n");
	   print_char_table(fo.str,MAX_STR,table_out);

	   fflush(table_out);
	   fclose(table_out);
	 */
	fprintf(stderr, "Maximum memory usuage = %d\n",
		(sizeof(mw) * (MAX_REG + MAX_DIC + MAX_VAR + MAX_RET)) +
		(sizeof(char) * MAX_STR));

	return 0;
}
