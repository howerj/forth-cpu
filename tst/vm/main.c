/** 
 *
 * @file main.c
 * @brief Wrapper/Top Level for H2 Virtual Machine
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

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

  fprintf(stdout, "(h2 \"H2 CPU Simulation running for %d cycles\"", CYCLES);

  if (st == NULL) {
    fprintf(stdout, "(error \"Could not calloc().\")\n");
    return 1;
  }

  st->cycles = CYCLES;

  if ((ram_init = fopen("mem_h2.binary", "r")) == NULL) {
    fprintf(stdout, "(error \"Could not open inital H2 CPU RAM file.\")\n");
    goto FAIL;
  }

  /*Read in intial input file for h2 RAM */
  for (i = 0; i < RAM_SZ; i++) {

    if (fgets(s, LIN_SZ, ram_init) == NULL) {
      fprintf(stdout, "(error \"fgets()==NULL\")\n");
      goto FAIL;
    }

    for (j = 0; j < LIN_SZ; j++) {
      if (s[j] == '\n' || s[j] == '\0') {
        if (j >= (sizeof(uint16_t) * 8)) {
          break;
        } else {
          fprintf(stdout, "(error \"Too few characters on line %d for ram_init.\")\n", i);
          goto FAIL;
        }
      }
      if (s[j] != '0' && s[j] != '1') {
        fprintf(stdout,
                "(error \"On line %d, character number %d, an invalid character (%c) was detected.\")\n", i, j, s[j]);
        goto FAIL;
      }

      st->ram[i] = strtol(s, NULL, 2);
    }

  }

  /*Execute h2 vm */
  ret = h2_cpu(st);
  fprintf(stdout, "(error-returned (\"h2_cpu(st)\" %d))\n)\n", ret);

  if ((ram_final = fopen("ram.log", "w")) == NULL) {
    fprintf(stdout, "(error \"Could not open output file for final RAM contents.\")\n");
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
  fprintf(stdout, "(error \"main.c Failure\")\n)\n");
  return 1;
}
