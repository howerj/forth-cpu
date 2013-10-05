/**
 * @file filter.c
 * @brief This is a filter to be used with GTKWave, it is called
 * by a script (gtkwave.tcl) when GTKWave is run via makefile in
 * the main project file. It takes the input from a signal, which
 * represents an instruction for the H2 Core, and disassembles it.
 * The result is fed back into GTKWave to be used by it to display
 * that signal with more clarity. 
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#define BUF_LEN_M (1025u)
#define LOG_FILE  "log.txt"

/**The disassembler, it only *partially* disassembles the
 * signal both because the instruction set is constantly
 * changing and because there is only a limited amount of
 * space to display any information. */
void disInsr_h2(char *in, char *out, FILE *log){
  uint16_t instruction;
  sscanf(in,"%hx", &instruction);

  if(instruction & 0x8000){
    sprintf(out,"lit(%hx)",0x7FFF&instruction);
  }else if(instruction & 0x6000){
    sprintf(out,"alu(%hx)",0x1FFF&instruction);
  }else if(instruction & 0x4000){
    sprintf(out,"call(%hx)",0x1FFF&instruction);
  }else if(instruction & 0x2000){
    sprintf(out,"cjmp(%hx)",0x1FFF&instruction);
  }else if(!(instruction & 0xC000)){
    sprintf(out,"jmp(%hx)",0x1FFF&instruction);
  } else{
    memcpy(out,in,BUF_LEN_M);
  }
}

/**For every line of input the filter *must* produce a
 * line of output otherwise GTKwave will hang.*/
int main(void){
  char buf[BUF_LEN_M], output[BUF_LEN_M];
  FILE *log;

  if((log = fopen(LOG_FILE,"w")) == NULL){
    fprintf(stderr,"Opening of %s for writing failed.\n",LOG_FILE);
    exit(EXIT_FAILURE);
  }

  while(!feof(stdin)){
    memset(buf, '\0', BUF_LEN_M);
    scanf("%s", buf);
    if(buf[0]){
      disInsr_h2(buf,output,log);
      printf("%s\n",output);
      fflush(stdout);
    }
  }
  fclose(log);

  return EXIT_SUCCESS;
}
