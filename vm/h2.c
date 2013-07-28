#include <stdio.h>
#include <stdlib.h>
#include "h2.h"

#define ST_ERROR(X) do{ st->error = (X); return (X); }while(1)
#define ST_CATCH(X) do{ if((X)!=err_ok) return st->err; }while(1)
/*
h2_state_t *h2_cpu_init(void){
  h2_state_t *st = calloc(1,sizeof(h2_state_t));
  if(st==NULL) return NULL;
  return st;
}*/

int h2_cpu(h2_state_t * state)
{
        h2_state_t *st = state;
        mw insn;                /*the instruction or ram[pc] */
        mw is_x;                /*is_alu,is_jmp,is_call,is_cjmp */

        /*
           if (st == NULL){
           fprintf(stderr,"(initializing)\n");
           } */

        while (true) {

                fprintf(stdout,
                        "(h2 (cycles %d) (tos %d) (pc %d) (dp %d) (rp %d))\n",
                        st->cycles, st->tos, st->pc, st->datap, st->retnp);

                if (st->cycles > 0) {
                        st->cycles--;
                } else {
                        fprintf(stderr, "(error \"Ran out of cycles\")\n");
                        ST_ERROR(err_cycles);
                }

                insn = (st->ram[st->pc & 0x1FFF]);      /*makes sure this is in bounds */

                if (insn & 0x8000) {    /*literal */
                        fprintf(stdout, "(h2_literal (tos %d) (dp %d))\n",
                                st->tos, st->datap);
                        st->data[++(st->datap) % VAR_SZ] = st->tos;
                        st->tos = insn & 0x7FFF;
                        (st->pc)++;
                } else {
                        is_x = insn & 0x6000;   /*mask, 0b0110 0000 0000 0000 */

                        switch (is_x) {
                        case 0x0000:   /*jmp */
                                fprintf(stdout, "(h2_jump (pc %d) (pc_n %d))\n",
                                        st->pc, insn & 0x1FFF);
                                st->pc = insn & 0x1FFF;
                                break;
                        case 0x2000:   /*cjmp */
                                if (st->tos == 0) {
                                        st->pc = insn & 0x1FFF;
                                        st->tos =
                                            st->data[(st->datap)-- % VAR_SZ];
                                } else {
                                        (st->pc)++;
                                }
                                break;
                        case 0x4000:   /*call */
                                break;
                        case 0x6000:   /*alu */
                                /*This is the most complex section */
                                /* insn & 0x0003 , stack delta data
                                 * insn & 0x000C , stack delta return
                                 * insn & 0x1F00,  alu
                                 */

                                /* dd stack delta, implements signed addition */
                                if ((insn & 0x0003) == 0x0) {
                                } else if ((insn & 0x0003) == 0x1) {
                                } else if ((insn & 0x0003) == 0x2) {
                                } else if ((insn & 0x0003) == 0x3) {
                                }

                                /* rd stack delta, implements signed addition */
                                if ((insn & 0x000C) == (0x0 << 2)) {
                                } else if ((insn & 0x000C) == (0x1 << 2)) {
                                } else if ((insn & 0x000C) == (0x2 << 2)) {
                                } else if ((insn & 0x000C) == (0x3 << 2)) {
                                }

                                (st->pc)++;
                                break;
                        default:       /*something went wrong */
                                fprintf(stderr,
                                        "(error \"Incorrect instruction\")\n");
                                ST_ERROR(err_instruction);
                        }

                }

        }
}
