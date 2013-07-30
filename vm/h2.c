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

int h2_cpu(h2_state_t * st)
{
        mw insn;                /*the instruction or ram[pc] */
        mw is_x;                /*is_alu,is_jmp,is_call,is_cjmp */
        mw alu_op;

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
                                fprintf(stdout, "(h2_alu ");

                                alu_op = (insn & 0x1F00) >> 8;
                                fprintf(stdout, "(alu %x) ",
                                        alu_op);

                                switch (alu_op) {
                                case alu_tos: /* tos = tos */
                                        break;
                                case alu_nos: 
                                        st->tos = st->data[st->datap-1];
                                        break;
                                case alu_rtos:
                                        break;
                                case alu_din:
                                        st->tos = st->data[st->tos & 0x1FFF];
                                        break;
                                case alu_depth: 
                                        st->tos = (mw)((st->datap << 11) | (st->retnp ));
                                        break;
                                case alu_or: 
                                        st->tos =  st->data[st->datap-1] | st->tos;
                                        break;
                                case alu_and: 
                                        st->tos =  st->data[st->datap-1] & st->tos;
                                        break;
                                case alu_xor:
                                        st->tos =  st->data[st->datap-1] ^ st->tos;
                                        break;
                                case alu_xnor:
                                        st->tos =  ~(st->data[st->datap-1] ^ st->tos);
                                        break;
                                case alu_not:
                                        st->tos =  ~st->tos;
                                        break;
                                case alu_add: 
                                        st->tos =  st->data[st->datap-1] + st->tos;
                                        break;
                                case alu_sub: 
                                        st->tos =  st->data[st->datap-1] - st->tos;
                                        break;
                                case alu_sll:
                                        st->tos =  st->data[st->datap-1] << (st->tos & 0x000F);
                                        break;
                                case alu_srl:
                                        st->tos =  st->data[st->datap-1] >> (st->tos & 0x000F);
                                        break;
                                case alu_rol:
                                        break;
                                case alu_ror:
                                        break;
                                case alu_mul:
                                        st->tos =  (mw)((st->data[st->datap-1] & 0x00FF) * (st->tos & 0x00FF));
                                        break;
                                case alu_sLT:
                                        st->tos =  (signed)st->data[st->datap-1] < (signed)st->tos;
                                        break;
                                case alu_equ:
                                        st->tos =  st->data[st->datap-1] == st->tos;
                                        break;
                                case alu_uLT:
                                        st->tos =  st->data[st->datap-1] < st->tos;
                                        break;
                                case alu_A:
                                        break;
                                case alu_B:
                                        break;
                                case alu_C:
                                        break;
                                case alu_D:
                                        break;
                                case alu_E:
                                        break;
                                case alu_F:
                                        break;
                                case alu_G:
                                        break;
                                case alu_H:
                                        break;
                                case alu_I:
                                        break;
                                case alu_dec:
                                        st->tos--;
                                        break;
                                case alu_ioDin:
                                        /*fget*/
                                        break;
                                case alu_ioW:
                                        /*fput*/
                                        break;
                                default:
                                        fprintf(stderr,
                                                "(error \"Incorrect ALU instruction\")\n");
                                        ST_ERROR(err_instruction);
                                }

                                if ((insn & 0x0010)) {
                                } else {
                                }

                                if ((insn & 0x0020)) {
                                } else {
                                }

                                if ((insn & 0x0040)) {
                                } else {
                                }

                                if ((insn & 0x0080)) {
                                } else {
                                }

                                /* dd stack delta, implements signed addition */
                                if ((insn & 0x0003) == 0x0) {
                                        fprintf(stdout, "(dd +/-0) ");
                                } else if ((insn & 0x0003) == 0x1) {
                                        fprintf(stdout, "(dd +1) ");
                                        st->datap++;
                                } else if ((insn & 0x0003) == 0x2) {
                                        fprintf(stdout, "(dd -2) ");
                                        st->datap -= 2;
                                } else if ((insn & 0x0003) == 0x3) {
                                        fprintf(stdout, "(dd -1) ");
                                        st->datap -= 1;
                                }

                                /* rd stack delta, implements signed addition */
                                if ((insn & 0x000C) == (0x0 << 2)) {
                                        fprintf(stdout, "(rd +/-0) ");
                                } else if ((insn & 0x000C) == (0x1 << 2)) {
                                        fprintf(stdout, "(rd +1) ");
                                        st->retnp++;
                                } else if ((insn & 0x000C) == (0x2 << 2)) {
                                        fprintf(stdout, "(rd -2) ");
                                        st->retnp -= 2;
                                } else if ((insn & 0x000C) == (0x3 << 2)) {
                                        fprintf(stdout, "(rd -1) ");
                                        st->retnp -= 1;
                                }

                                fprintf(stdout, ")\n");
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
