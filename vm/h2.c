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

char aluop_str[][20]={
    "tos",
    "nos",
    "rtos",
    "din",
    "depth",
    "or",
    "and",
    "xor",
    "xnor",
    "not",
    "add",
    "sub",
    "sll",
    "srl",
    "rol",
    "ror",
    "mul",
    "sLT",
    "equ",
    "uLT",
    "A",
    "B",
    "C",
    "D",
    "E",
    "F",
    "G",
    "H",
    "I",
    "dec",
    "ioDin",
    "ioW"
};



int h2_cpu(h2_state_t * st)
{
        mw insn;                /*the instruction or ram[pc] */
        mw is_x;                /*is_alu,is_jmp,is_call,is_cjmp */
        mw alu_op;

        /*
           if (st == NULL){
           fprintf(stdout,"(initializing)\n");
           } */

        while (true) {

                fprintf(stdout,
                        "  (state (cycles %d) (tos %d) (pc %d) (dp %d) (rp %d)\n",
                        st->cycles, st->tos, st->pc, st->datap, st->retnp);

                if (st->cycles > 0) {
                        st->cycles--;
                } else {
                        fprintf(stdout, "    (error \"Ran out of cycles\")\n  )\n");
                        ST_ERROR(err_cycles);
                }

                insn = (st->ram[st->pc % RAM_SZ]);      /*makes sure this is in bounds */

                if (insn & 0x8000) {    /*literal */
                        fprintf(stdout, "(literal (tos %d) (dp %d))\n",
                                st->tos, st->datap);
                        st->data[++(st->datap) % VAR_SZ] = st->tos;
                        st->tos = insn & 0x7FFF;
                        (st->pc)++;
                } else {
                        is_x = insn & 0x6000;   /*mask, 0b0110 0000 0000 0000 */

                        switch (is_x) {
                        case 0x0000:   /*jmp */
                                fprintf(stdout, "    (jump (pc %d) (pc_n %d))\n",
                                        st->pc, insn & 0x1FFF);
                                st->pc = insn & 0x1FFF;
                                break;
                        case 0x2000:   /*cjmp */
                                fprintf(stdout, "    (cjmp (pc %d) (cond %s) (pc_n %d))\n", st->pc, st->tos?"true":"false",st->tos?insn&0x1FFF:st->pc+1);
                                if (st->tos == 0) {
                                        st->pc = insn & 0x1FFF;
                                        st->tos =
                                            st->data[(st->datap)-- % VAR_SZ];
                                } else {
                                        (st->pc)++;
                                }
                                break;
                        case 0x4000:   /*call */
                                fprintf(stdout, "    (call (pc %d) (pc_n %d))\n",
                                        st->pc, insn & 0x1FFF);
                                st->retn[++(st->retnp) % RET_SZ] = st->pc;
                                st->pc = insn & 0x1FFF;
                                break;
                        case 0x6000:   /*alu */
                                /*This is the most complex section */
                                /* insn & 0x0003 , stack delta data
                                 * insn & 0x000C , stack delta return
                                 * insn & 0x1F00,  alu
                                 */
                                fprintf(stdout, "    (alu\n");

                                alu_op = (insn & 0x1F00) >> 8;
                                fprintf(stdout, "        (aluop %d %s)\n",
                                        alu_op, aluop_str[alu_op]);

                                switch (alu_op) {
                                case alu_tos: /* tos = tos */
                                        break;
                                case alu_nos: 
                                        st->tos = st->data[(st->datap-1)%VAR_SZ];
                                        break;
                                case alu_rtos:
                                        break;
                                case alu_din:
                                        st->tos = st->ram[st->tos % RAM_SZ];
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
                                        st->tos =  st->data[(st->datap-1)%VAR_SZ] << (st->tos & 0x000F);
                                        break;
                                case alu_srl:
                                        st->tos =  st->data[(st->datap-1)%VAR_SZ] >> (st->tos & 0x000F);
                                        break;
                                case alu_rol:
                                        break;
                                case alu_ror:
                                        break;
                                case alu_mul:
                                        st->tos =  (mw)((st->data[(st->datap-1)%VAR_SZ] & 0x00FF) * (st->tos & 0x00FF));
                                        break;
                                case alu_sLT:
                                        st->tos =  (signed)st->data[(st->datap-1)%VAR_SZ] < (signed)st->tos;
                                        break;
                                case alu_equ:
                                        st->tos =  st->data[(st->datap-1)%VAR_SZ] == st->tos;
                                        break;
                                case alu_uLT:
                                        st->tos =  st->data[(st->datap-1)%VAR_SZ] < st->tos;
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
                                        fprintf(stdout,
                                                "    (error \"Incorrect ALU instruction\")\n  )\n");
                                        ST_ERROR(err_instruction);
                                }

                                /*Does all this go before of after the stack delta?*/
                                if ((insn & 0x0010)) { /* R->PC */
                                  fprintf(stdout,"        (R->PC true)\n");
                                  st->pc = st->retn[st->retnp % RET_SZ];
                                } else {
                                  fprintf(stdout,"        (R->PC false)\n");
                                }

                                if ((insn & 0x0020)) { /* N->[T] */
                                  fprintf(stdout,"        (N->[T] true)\n");
                                  st->ram[st->tos] = st->data[(st->datap-1)%VAR_SZ];
                                } else {
                                  fprintf(stdout,"        (N->[T] false)\n");
                                }

                                if ((insn & 0x0040)) { /* T->R */
                                  fprintf(stdout,"        (T->R true)\n");
                                  st->retn[st->retnp%RET_SZ] = st->tos;
                                } else {
                                  fprintf(stdout,"        (T->R false)\n");
                                }

                                if ((insn & 0x0080)) { /* T->N */
                                  fprintf(stdout,"        (T->N true)\n");
                                  st->data[(st->datap-1)%VAR_SZ] = st->tos;
                                } else {
                                  fprintf(stdout,"        (T->N false)\n");
                                }

                                /* dd stack delta, implements signed addition */
                                if ((insn & 0x0003) == 0x0) {
                                        fprintf(stdout, "        (dd +/-0)\n");
                                } else if ((insn & 0x0003) == 0x1) {
                                        fprintf(stdout, "        (dd +1)\n");
                                        st->datap++;
                                } else if ((insn & 0x0003) == 0x2) {
                                        fprintf(stdout, "        (dd -2)\n");
                                        st->datap -= 2;
                                } else if ((insn & 0x0003) == 0x3) {
                                        fprintf(stdout, "        (dd -1)\n");
                                        st->datap -= 1;
                                }

                                /* rd stack delta, implements signed addition */
                                if ((insn & 0x000C) == (0x0 << 2)) {
                                        fprintf(stdout, "        (rd +/-0)\n");
                                } else if ((insn & 0x000C) == (0x1 << 2)) {
                                        fprintf(stdout, "        (rd +1)\n");
                                        st->retnp++;
                                } else if ((insn & 0x000C) == (0x2 << 2)) {
                                        fprintf(stdout, "        (rd -2)\n");
                                        st->retnp -= 2;
                                } else if ((insn & 0x000C) == (0x3 << 2)) {
                                        fprintf(stdout, "        (rd -1)\n");
                                        st->retnp -= 1;
                                }

                                fprintf(stdout, "    )\n");
                                (st->pc)++;
                                break;
                        default:       /*something went wrong */
                                fprintf(stdout,
                                        "    (error \"Incorrect instruction\")\n  )\n");
                                ST_ERROR(err_instruction);
                        }

                }
          fprintf(stdout, "  )\n");
        }
}
