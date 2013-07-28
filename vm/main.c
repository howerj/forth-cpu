#include <stdio.h>
#include <stdlib.h>
#include "h2.h"

#define CYCLES 128
#define LIN_SZ 80
int main(void)
{
        unsigned int ret, i, j;
        char s[LIN_SZ];
        FILE *ram_init, *ram_final;
        h2_state_t *st = calloc(1, sizeof(h2_state_t));

        if (st == NULL) {
                fprintf(stderr, "(error \"Could not calloc().\")\n");
                return 1;
        }

        st->cycles = CYCLES;

        fprintf(stderr, "H2 CPU Simulator. Running for %d clock cycles.\n",
                st->cycles);

        if ((ram_init = fopen("../vhdl/mem_h2.binary", "r")) == NULL) {
                fprintf(stderr,
                        "(error \"Could not open inital H2 CPU RAM file.\")\n");
                goto FAIL;
        }

        /*Read in intial input file for h2 RAM */
        for (i = 0; i < RAM_SZ; i++) {

                if (fgets(s, LIN_SZ, ram_init) == NULL) {
                        fprintf(stderr, "fgets()==NULL\n");
                        goto FAIL;
                }

                for (j = 0; j < LIN_SZ; j++) {
                        if (s[j] == '\n' || s[j] == '\0') {
                                if (j >= (sizeof(mw) * 8)) {
                                        break;
                                } else {
                                        fprintf(stderr,
                                                "(error \"Too few characters on line %d for ram_init.\")\n",
                                                i);
                                        goto FAIL;
                                }
                        }
                        if (s[j] != '0' && s[j] != '1') {
                                fprintf(stderr,
                                        "(error \"On line %d, character number %d, an invalid character (%c) was detected.\")\n",
                                        i, j, s[j]);
                                goto FAIL;
                        }

                        st->ram[i] = strtol(s, NULL, 2);
                }

        }

        /*Execute h2 vm */
        ret = h2_cpu(st);
        fprintf(stderr, "(error-returned (\"h2_cpu(st)\" %d))\n", ret);

        if ((ram_final = fopen("ram.log", "w")) == NULL) {
                fprintf(stderr,
                        "(error \"Could not open output file for final RAM contents.\")\n");
                goto FAIL;
        }

        for (i = 0; i < RAM_SZ; i++) {
                fprintf(ram_final, "%04X\n", st->ram[i]);
        }

        free(st);
        fclose(ram_init);
        fclose(ram_final);
        return 0;
 FAIL:
        free(st);
        fprintf(stderr, "(error \"Fission Mailed\")\n");
        return 1;
}
