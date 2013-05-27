#include <stdio.h>
#include "h2.h"

int h2_cpu(h2_state_t * st)
{
        if (st == NULL)
                return 1;

        while (true) {

                fprintf(stdout,
                        "(h2 (cycles %d) (tos %d) (pc %d) (dp %d) (rp %d))\n",
                        st->cycles, st->tos, st->pc, st->datap, st->retnp);

                if (st->cycles > 0) {
                        st->cycles--;
                } else {
                        fprintf(stderr, "Ran out of cycles\n");
                        return 0;
                }

        }
}
