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
#include <string.h>
#include "h2.h"

#define ST_ERROR(X) do{ st->error = (X); return (X); }while(1)
#define ST_CATCH(X) do{ if((X)!=err_ok) return st->err; }while(1)

#define BUFLEN        (256u)

#define LOGFILE       "testbench.binary"

#define PC_BITLEN     (13u)
#define TOS_BITLEN    (16u)
#define DATAP_BITLEN  (5u)
#define RETNP_BITLEN  (5u)
#define INSN_BITLEN   (16u)
#define INPUT_BITLEN  (16u)
#define OUTPUT_BITLEN (16u)

#define X(a, b) b
static char *aluop_str[] = {
  XMACRO_ALU_INSTRUCTIONS
};
#undef X

/** ...starring these functions in order of appearance */
static void printbinary(uint16_t a, unsigned int bits, FILE * f);
static void print_column(FILE * logfile, char *s, unsigned int bitlen);
static uint16_t rotr(uint16_t value, uint16_t shift);
static uint16_t rotl(uint16_t value, uint16_t shift);
int h2_cpu(h2_state_t * st);

/**print out a number in binary*/
void printbinary(uint16_t a, unsigned int bits, FILE * f)
{
  unsigned int i;
  for (i = 0; i < bits; i++) {
    /*without if: putc("01"[(a&(1<< (bits - 1 -i)))>>(bits - 1 - i)], f);*/
    if (a & (1 << (bits - 1 - i)))
      (void)putc('1', f);
    else
      (void)putc('0', f);
  }
  (void)putc(' ', f);
}

/** print out the column headings neatly.*/
void print_column(FILE * logfile, char *s, unsigned int bitlen)
{
  char buf[BUFLEN];
  memset(buf, '\0', BUFLEN);
  memset(buf, ' ', bitlen + 1);
  strncpy(buf, s, strlen(s));
  fprintf(logfile, buf);
}

/** rotate left */
uint16_t rotl(uint16_t value, uint16_t shift) {
    return (value << (shift & 0x000Fu)) | (value >> ((sizeof(value) * 8u - shift) & 0x000Fu));
}
/** rotate right */
uint16_t rotr(uint16_t value, uint16_t shift) {
    return (value >> (shift & 0x000Fu)) | (value << ((sizeof(value) * 8u - shift)) & 0x000Fu);
}

int h2_cpu(h2_state_t * st)
{
  uint16_t insn;    /**the instruction or ram[pc] */
  uint16_t is_x;    /**is_alu,is_jmp,is_call,is_cjmp */
  uint16_t alu_op;  /**which ALU operation are we executing (if it is one)*/
  uint16_t temp;
  FILE *logfile;

  /**note on carry. x=y+z, a=lower(x), c=(higher(x)>>tolower) & 1 */

  if (NULL == st) {
    fprintf(stdout, "    (error \"Passed NULL pointer.\")\n  )\n");
    ST_ERROR(err_file);
  }

  if (NULL == (logfile = fopen(LOGFILE, "w"))) {
    fprintf(stdout, "    (error \"Could not open <%s> for writing.\")\n  )\n", LOGFILE);
    ST_ERROR(err_file);
  }

  print_column(logfile, "pc_c", PC_BITLEN);
  print_column(logfile, "tos_c", TOS_BITLEN);
  print_column(logfile, "dp_c", DATAP_BITLEN);
  print_column(logfile, "rp_c", RETNP_BITLEN);
  print_column(logfile, "insn_c", INSN_BITLEN);
  print_column(logfile, "i", 1u);
  print_column(logfile, "d", 1u);
  print_column(logfile, "output", OUTPUT_BITLEN);
  print_column(logfile, "input", INPUT_BITLEN);
  fputc('\n', logfile);

  while (true) {
    fprintf(stdout,
            "  (state (cycles %d) (tos %d) (pc %d) (dp %d) (rp %d)\n",
            st->cycles, st->tos, st->pc, st->datap, st->retnp);

    if (st->cycles > 0) {
      st->cycles--;
    } else {
      fprintf(stdout, "    (error \"Ran out of cycles\")\n  )\n");
      fclose(logfile);
      ST_ERROR(err_cycles);
    }

    printbinary(st->pc, PC_BITLEN, logfile);
    printbinary(st->tos, TOS_BITLEN, logfile);
    printbinary(st->datap, DATAP_BITLEN, logfile);
    printbinary(st->retnp, RETNP_BITLEN, logfile);
    insn = (st->ram[st->pc % RAM_SZ]);  /*makes sure this is in bounds */
    printbinary(insn, INSN_BITLEN, logfile);

    if (insn & 0x8000u) {        /*literal */
      fprintf(stdout, "        (literal (tos %d) (dp %d))\n", st->tos, st->datap);
      st->data[++(st->datap) % VAR_SZ] = st->tos;
      st->tos = insn & 0x7FFFu;
      (st->pc)++;
      printbinary(0, 1, logfile);
      printbinary(0, 1, logfile);
      printbinary(0, OUTPUT_BITLEN, logfile);
      printbinary(0, INPUT_BITLEN, logfile);
    } else {
      is_x = insn & 0x6000u;     /*mask, 0b0110 0000 0000 0000 */

      switch (is_x) {
      case 0x0000u:             /*jmp */
        fprintf(stdout, "    (jump (pc %d) (pc_n %d))\n", st->pc, insn & 0x1FFFu);
        st->pc = insn & 0x1FFFu;

        printbinary(0, 1, logfile);
        printbinary(0, 1, logfile);
        printbinary(0, OUTPUT_BITLEN, logfile);
        printbinary(0, INPUT_BITLEN, logfile);

        break;
      case 0x2000u:             /*cjmp */
        fprintf(stdout, "    (cjmp (pc %d) (cond %s) (pc_n %d))\n", st->pc, st->tos ? "true" : "false",
                st->tos ? insn & 0x1FFFu : st->pc + 1u);
        if (0u == st->tos) {
          st->pc = insn & 0x1FFFu;
          st->tos = st->data[(st->datap)-- % VAR_SZ];
        } else {
          (st->pc)++;
        }
        printbinary(0, 1, logfile);
        printbinary(0, 1, logfile);
        printbinary(0, OUTPUT_BITLEN, logfile);
        printbinary(0, INPUT_BITLEN, logfile);
        break;
      case 0x4000u:             /*call */
        fprintf(stdout, "    (call (pc %d) (pc_n %d))\n", st->pc, insn & 0x1FFF);
        st->retn[++(st->retnp) % RET_SZ] = st->pc;
        st->pc = insn & 0x1FFFu;

        printbinary(0, 1, logfile);
        printbinary(0, 1, logfile);
        printbinary(0, OUTPUT_BITLEN, logfile);
        printbinary(0, INPUT_BITLEN, logfile);
        break;
      case 0x6000u:             /*alu */
        /*This is the most complex section */
        /* insn & 0x0003 , stack delta data
         * insn & 0x000C , stack delta return
         * insn & 0x1F00,  alu
         */
        fprintf(stdout, "    (alu\n");

        /** output */
        if (insn & (1 << 5)) {
          if ((st->tos & 0x6000u) == 0x6000u) { /** fputc() or io access */
            fprintf(stdout, "        (output io %d)\n",
                st->data[(st->datap - 1u) % VAR_SZ]);
            printbinary(1, 1, logfile);
            printbinary(0, 1, logfile);
          } else {   /** normal memory access output */
            fprintf(stdout, "        (output mem)\n");
            printbinary(0, 1, logfile);
            printbinary(1, 1, logfile);
          }
        } else {
          printbinary(0, 1, logfile);
          printbinary(1, 1, logfile);
        }
        printbinary(st->tos, OUTPUT_BITLEN, logfile);

        alu_op = (insn & 0x1F00u) >> 8u;
        fprintf(stdout, "        (aluop %d %s)\n", alu_op, aluop_str[alu_op]);

        switch (alu_op) {
        case alu_tos:
          break;
        case alu_nos:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ];
          break;
        case alu_rtos:
          st->tos = st->retn[(st->retnp) % RET_SZ];
          break;
        case alu_din:
          if ((st->tos & 0x6000u) == 0x6000u) {
                                            /** fgetc() or io access */
            fprintf(stdout, "        (aluop %d %s io)\n", alu_op, aluop_str[alu_op]);
            printbinary(0, INPUT_BITLEN, logfile);
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
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] << (st->tos & 0x000Fu);
          break;
        case alu_srl:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] >> (st->tos & 0x000Fu);
          break;
        case alu_rol:
          st->tos = rotl(st->data[(st->datap - 1u) % VAR_SZ], st->tos);
          break;
        case alu_ror:
          st->tos = rotr(st->data[(st->datap - 1u) % VAR_SZ], st->tos);
          break;
        case alu_mul:
          st->tos = (uint16_t) ((st->data[(st->datap - 1u) % VAR_SZ] & 0x00FFu) * (st->tos & 0x00FFu));
          break;
        case alu_sLT:
          st->tos = (int16_t) st->data[(st->datap - 1u) % VAR_SZ] < (int16_t) st->tos;
          break;
        case alu_uLT:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] < st->tos;
          break;
        case alu_equ:
          st->tos = st->data[(st->datap - 1u) % VAR_SZ] == st->tos;
          break;
        case alu_neg:
          st->tos = (st->tos & 0x8000u) >> 15u;
          break;
        case alu_eqz:
          st->tos = st->tos == 0u;
          break;
        case alu_swapbyte:/** swap high and low bytes in a 16-bit word*/
          temp = (st->tos & 0xFF00u) >> 8u;
          st->tos = (st->tos << 8u) & 0xFF00u;
          st->tos |= temp;
          break;
        case alu_togglei:
          st->interrupt_enable = (st->interrupt_enable ^ 0x0001u) & 0x0001u;
          break;
        case alu_decrement:
          st->tos--;
          break;
        case alu_clear:
          st->tos = 0u;
          st->carry = 0u;
          break;
        case alu_setcarry:
          st->carry = st->tos & 0x0001u;
          break;
        case alu_flags:
          break;
        case alu_I:
                   /** no instruction implemented*/
          break;
        case alu_J:
                   /** no instruction implemented*/
          break;
        case alu_K:
                   /** no instruction implemented*/
          break;
        case alu_L:
                   /** no instruction implemented*/
          break;
        case LAST_ALU: /** fall through */
        default:
          fprintf(stdout, "    (error \"Incorrect ALU instruction\")\n  )\n");
          ST_ERROR(err_instruction);
        }

        /*Does all this go before of after the stack delta? */
        if ((insn & 0x0010u)) {  /* R->PC */
          fprintf(stdout, "        (R->PC true)\n");
          st->pc = st->retn[st->retnp % RET_SZ];
        } else {
          fprintf(stdout, "        (R->PC false)\n");
        }

        if ((insn & 0x0020u)) {  /* N->[T] */
          fprintf(stdout, "        (N->[T] true)\n");
          st->ram[st->tos] = st->data[(st->datap - 1) % VAR_SZ];
        } else {
          fprintf(stdout, "        (N->[T] false)\n");
        }

        if ((insn & 0x0040u)) {  /* T->R */
          fprintf(stdout, "        (T->R true)\n");
          st->retn[st->retnp % RET_SZ] = st->tos;
        } else {
          fprintf(stdout, "        (T->R false)\n");
        }

        if ((insn & 0x0080u)) {  /* T->N */
          fprintf(stdout, "        (T->N true)\n");
          st->data[(st->datap - 1u) % VAR_SZ] = st->tos;
        } else {
          fprintf(stdout, "        (T->N false)\n");
        }

        /* dd stack delta, implements signed addition */
        if ((insn & 0x0003u) == 0x0u) {
          fprintf(stdout, "        (dd +/-0)\n");
        } else if ((insn & 0x0003u) == 0x1u) {
          fprintf(stdout, "        (dd +1)\n");
          st->datap++;
        } else if ((insn & 0x0003u) == 0x2u) {
          fprintf(stdout, "        (dd -2)\n");
          st->datap -= 2u;
        } else if ((insn & 0x0003u) == 0x3u) {
          fprintf(stdout, "        (dd -1)\n");
          st->datap -= 1;
        }

        /* rd stack delta, implements signed addition */
        if ((insn & 0x000Cu) == (0x0u << 2u)) {
          fprintf(stdout, "        (rd +/-0)\n");
        } else if ((insn & 0x000Cu) == (0x1u << 2u)) {
          fprintf(stdout, "        (rd +1)\n");
          st->retnp++;
        } else if ((insn & 0x000Cu) == (0x2u << 2u)) {
          fprintf(stdout, "        (rd -2)\n");
          st->retnp -= 2;
        } else if ((insn & 0x000Cu) == (0x3u << 2u)) {
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
    putc('\n', logfile);
    fprintf(stdout, "  )\n\n");
  }
}
