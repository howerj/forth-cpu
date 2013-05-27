#include <stdio.h>
#include "h2.h"

int main(void){
  int ret;
  h2_state_t *st = calloc(1,sizeof(h2_state_t));

  if(st==NULL){
    fprintf(stderr,"Could not calloc()\n");
    return 1;
  }

  ret=h2_cpu(st);

  fprintf(stderr,"h2_cpu(st) [%d]\n", ret);

  free(st);
  return 0;
}
