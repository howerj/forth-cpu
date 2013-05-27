#include <stdint.h>
#define RAM_SZ 8192
#define VAR_SZ 32
#define RET_SZ 32

#define false 0
#define true  1

typedef uint16_t mw;

struct h2_state{
  int error;        /*Any error code returned*/
  int cycles;       /*Number of cycles to run for*/
  mw ram[RAM_SZ];   /*Main RAM*/
  mw data[VAR_SZ];  /*Data stack*/
  mw retn[RET_SZ];  /*Return stack*/
  mw pc;            /*Program counter*/
  mw datap;         /*Data stack pointer*/
  mw retnp;         /*Return stack pointer*/
};

typedef struct h2_state h2_state_t;

int h2_cpu(h2_state_t * st);
