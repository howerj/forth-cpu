#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#define BUF_LEN_M (1025u)
#define LOG_FILE  "log.txt"

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
  } else{
    memcpy(out,in,BUF_LEN_M);
  }
}

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
