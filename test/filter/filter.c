#include <stdio.h>
#include <string.h>
#include <stdint.h>

#define BUF_LEN_M (1025u)

void disInsr_h2(char *in, char *out){
  

  memcpy(out,in,BUF_LEN_M);
}

int main(int argc, char **argv){
  char buf[BUF_LEN_M], output[BUF_LEN_M];
  while(!feof(stdin)){
    memset(buf, '\0', BUF_LEN_M);
    scanf("%s", buf);
    if(buf[0]){
      disInsr_h2(buf,output);
      printf("%s_TEST\n",output);
      fflush(stdout);
    }
  }
  return 0;
}
