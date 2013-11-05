/** 
 *
 * @file h2.c
 * @brief H2 Virtual Machine
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

#define ST_ERROR(X) do{ st->error = (X); return (X); }while(1)
#define ST_CATCH(X) do{ if((X)!=err_ok) return st->err; }while(1)
#define LOGFILE "testbench.binary"


#define X(a, b) b
static char *aluop_str[] = {
  XMACRO_ALU_INSTRUCTIONS
};

#undef X

void printbinary(uint16_t a, FILE *f){
  int i;
  for(i = 0; i<16; i++){
    if(a&(1<<(15-i)))
        putc('1',f);
    else
        putc('0',f);
  }
  putc(' ',f);
}

/**This will print out the test data for the VHDL implementation
 * to be compared against, it will generate a pletheora of files
 * with a common prefix and suffix. */
void print_test_data(h2_state_t *st, FILE *logfile){
    uint16_t insn;
    printbinary(st->pc,logfile);
    printbinary(st->tos,logfile);
    printbinary(st->datap,logfile);
    printbinary(st->retnp,logfile);
    insn = (st->ram[st->pc % RAM_SZ]);
    printbinary(insn,logfile);
    putc('\n',logfile);
}

int h2_cpu(h2_state_t * st)
{
  uint16_t insn;    /**the instruction or ram[pc] */
  uint16_t is_x;    /**is_alu,is_jmp,is_call,is_cjmp */
  uint16_t alu_op;  /**which ALU operation are we executing (if it is one)*/           
  uint16_t temp;
  FILE* logfile; 

  /**note on carry. x=y+z, a=lower(x), c=(higher(x)>>tolower) & 1 */

  if(NULL == st){
    fprintf(stdout, "    (error \"Passed NULL pointer.\")\n  )\n");
    ST_ERROR(err_file);
  }

  if(NULL == (logfile=fopen(LOGFILE, "w"))){
    fprintf(stdout, "    (error \"Could not open <%s> for writing.\")\n  )\n",
        LOGFILE);
    ST_ERROR(err_file);
  }

  while (true) {
    fprintf(stdout,
            "  (state (cycles %d) (tos %d) (pc %d) (dp %d) (rp %d)\n",
            st->cycles, st->tos, st->pc, st->datap, st->retnp);

    if (st->cycles > 0) {
      st->cycles--;
      print_test_data(st, logfile);
    } else {
      fprintf(stdout, "    (error \"Ran out of cycles\")\n  )\n");
      fclose(logfile);
      ST_ERROR(err_cycles);
    }

    insn = (st->ram[st->pc % RAM_SZ]);  /*makes sure this is in bounds */

    if (insn & 0x8000) {        /*literal */
      fprintf(stdout, "        (literal (tos %d) (dp %d))\n", st->tos, st->datap);
      st->data[++(st->datap) % VAR_SZ] = st->tos;
      st->tos = insn & 0x7FFF;
      (st->pc)++;
    } else {
      is_x = insn & 0x6000;     /*mask, 0b0110 0000 0000 0000 */

      switch (is_x) {
      case 0x0000:             /*jmp */
        fprintf(stdout, "    (jump (pc %d) (pc_n %d))\n", st->pc, insn & 0x1FFF);
        st->pc = insn & 0x1FFF;
        break;
      case 0x2000:             /*cjmp */
        fprintf(stdout, "    (cjmp (pc %d) (cond %s) (pc_n %d))\n", st->pc, st->tos ? "true" : "false",
                st->tos ? insn & 0x1FFF : st->pc + 1);
        if (st->tos == 0) {
          st->pc = insn & 0x1FFF;
          st->tos = st->data[(st->datap)-- % VAR_SZ];
        } else {
          (st->pc)++;
        }
        break;
      case 0x4000:             /*call */
        fprintf(stdout, "    (call (pc %d) (pc_n %d))\n", st->pc, insn & 0x1FFF);
        st->retn[++(st->retnp) % RET_SZ] = st->pc;
        st->pc = insn & 0x1FFF;
        break;
      case 0x6000:             /*alu */
        /*This is the most complex section */
        /* insn & 0x0003 , stack delta data
         * insn & 0x000C , stack delta return
         * insn & 0x1F00,  alu
         */
        fprintf(stdout, "    (alu\n");

        /** output */
        if(insn & (1<<5)){
            if((st->tos & 0x6000) == 0x6000){ /** fputc() or io access */
              fprintf(stdout, "        (output io)\n");
            } else { /** normal memory access output */
              fprintf(stdout, "        (output mem)\n");
            }
        }



        alu_op = (insn & 0x1F00) >> 8;
        fprintf(stdout, "        (aluop %d %s)\n", alu_op, aluop_str[alu_op]);

        switch (alu_op) {
        case alu_tos:
          break;
        case alu_nos:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ];
          break;
        case alu_rtos:
          break;
        case alu_din:
          if((st->tos & 0x6000) == 0x6000){ /** fgetc() or io access */
            fprintf(stdout, "        (aluop %d %s io)\n", alu_op, aluop_str[alu_op]);
          } else { /** normal memory access */
            fprintf(stdout, "        (aluop %d %s mem)\n", alu_op, aluop_str[alu_op]);
          }
          break;
        case alu_depth:
          st->tos = (uint16_t) ((st->datap << 11u) | (st->retnp));
          break;
        case alu_or:
          st->tos = st->data[st->datap - 1u] | st->tos;
          break;
        case alu_and:
          st->tos = st->data[st->datap - 1u] & st->tos;
          break;
        case alu_xor:
          st->tos = st->data[st->datap - 1u] ^ st->tos;
          break;
        case alu_xnor:
          st->tos = ~(st->data[st->datap - 1u] ^ st->tos);
          break;
        case alu_not:
          st->tos = ~st->tos;
          break;
        case alu_add:
          st->tos = st->data[st->datap - 1u] + st->tos;
          break;
        case alu_sub:
          st->tos = st->data[st->datap - 1u] - st->tos;
          break;
        case alu_sll:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] << (st->tos & 0x000F);
          break;
        case alu_srl:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] >> (st->tos & 0x000F);
          break;
        case alu_rol:
          break;
        case alu_ror:
          break;
        case alu_mul:
          st->tos = (uint16_t) ((st->data[(st->datap - 1u) % VAR_SZ] & 0x00FF) * (st->tos & 0x00FF));
          break;
        case alu_sLT:
          st->tos = (int16_t)st->data[(st->datap - 1u) % VAR_SZ] < (int16_t)st->tos;
          break;
        case alu_uLT:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] < st->tos;
          break;
        case alu_equ:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] == st->tos;
          break;
        case alu_neg:
          st->tos = (st->tos & 0x8000) >> 15u;
          break;
        case alu_eqz:
          st->tos = st->tos == 0;
          break;
        case alu_swapbyte: /** swap high and low bytes in a 16-bit word*/
          temp = (st->tos & 0xFF00) >> 8u;
          st->tos = (st->tos << 8u) & 0xFF00;
          st->tos |= temp;
          break;
        case alu_togglei:
          st->interrupt_enable = (st->interrupt_enable ^ 0x0001) & 0x0001;
          break;
        case alu_decrement:
          st->tos--;
          break;
        case alu_clear:
          st->tos = 0;
          st->carry = 0;
          break;
        case alu_setcarry:
          st->carry = st->tos & 0x0001;
          break;
        case alu_flags:
          break;
        case alu_I:/** no instruction implemented*/
          break;
        case alu_J:/** no instruction implemented*/
          break;
        case alu_K:/** no instruction implemented*/
          break;
        case alu_L:/** no instruction implemented*/
          break;
        case LAST_ALU:
          break;

#if 0   /*not implemented yet*/
        case alu_rtos:
          break;
        case alu_din:
          st->tos = st->ram[st->tos % RAM_SZ];
          break;
        case alu_rol:
          break;
        case alu_ror:
          break;
#endif
        default:
          fprintf(stdout, "    (error \"Incorrect ALU instruction\")\n  )\n");
          ST_ERROR(err_instruction);
        }

        /*Does all this go before of after the stack delta? */
        if ((insn & 0x0010)) {  /* R->PC */
          fprintf(stdout, "        (R->PC true)\n");
          st->pc = st->retn[st->retnp % RET_SZ];
        } else {
          fprintf(stdout, "        (R->PC false)\n");
        }

        if ((insn & 0x0020)) {  /* N->[T] */
          fprintf(stdout, "        (N->[T] true)\n");
          st->ram[st->tos] = st->data[(st->datap - 1) % VAR_SZ];
        } else {
          fprintf(stdout, "        (N->[T] false)\n");
        }

        if ((insn & 0x0040)) {  /* T->R */
          fprintf(stdout, "        (T->R true)\n");
          st->retn[st->retnp % RET_SZ] = st->tos;
        } else {
          fprintf(stdout, "        (T->R false)\n");
        }

        if ((insn & 0x0080)) {  /* T->N */
          fprintf(stdout, "        (T->N true)\n");
          st->data[(st->datap - 1) % VAR_SZ] = st->tos;
        } else {
          fprintf(stdout, "        (T->N false)\n");
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
      default:                 /*something went wrong */
        fprintf(stdout, "    (error \"Incorrect instruction\")\n  )\n");
        ST_ERROR(err_instruction);
      }

    }
    fprintf(stdout, "  )\n\n");
  }
}
