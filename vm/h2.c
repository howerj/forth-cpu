#include <stdio.h>
#include "h2.h"

int h2_cpu(h2_state_t * st){
  if(st == NULL) return 1;

  while(true){
    if(st->cycles>0){
      st->cycles--;
    } else {
      fprintf(stderr,"Ran out of cycles\n");
      return 0;
    }


  }
}
