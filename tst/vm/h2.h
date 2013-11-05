/**
 *
 * @file h2.h
 * @brief H2 Virtual Machine header
 *
 * @author         Richard James Howe.
 * @copyright      Copyright 2013 Richard James Howe.
 * @license        LGPL      
 * @email          howe.r.j.89@gmail.com
 *
 */

#include <stdint.h>
#define RAM_SZ 8192
#define VAR_SZ 32
#define RET_SZ 32

typedef enum {
  false,
  true
} bool;

typedef enum {
  err_ok,
  err_malloc,
  err_cycles,
  err_instruction
} vm_err;

#define XMACRO_ALU_INSTRUCTIONS\
  X(alu_tos,              "T"),\
  X(alu_nos,              "N"),\
  X(alu_rtos,             "R"),\
  X(alu_din,              "[T]"),\
  X(alu_depth,            "depth"),\
  X(alu_or,               "T|N"),\
  X(alu_and,              "T&N"),\
  X(alu_xor,              "T^N"),\
  X(alu_xnor,             "~(T^N)"),\
  X(alu_not,              "~T"),\
  X(alu_add,              "T+N"),\
  X(alu_sub,              "N-T"),\
  X(alu_sll,              "N<<T"),\
  X(alu_srl,              "N>>T"),\
  X(alu_rol,              "NrolT"),\
  X(alu_ror,              "NrorT"),\
  X(alu_mul,              "L(N)*L(T)"),\
  X(alu_sLT,              "Nu<T"),\
  X(alu_uLT,              "N<T"),\
  X(alu_equ,              "N=T"),\
  X(alu_neg,              "T<0"),\
  X(alu_eqz,              "T=0"),\
  X(alu_swapbyte,         "swapbytes"),\
  X(alu_togglei,          "togglei"),\
  X(alu_decrement,        "T-1"),\
  X(alu_clear,            "clr"),\
  X(alu_setcarry,         "setcarry"),\
  X(alu_flags,            "flags"),\
  X(alu_I,                ""),\
  X(alu_J,                ""),\
  X(alu_K,                ""),\
  X(alu_L,                ""),\
  X(LAST_ALU,             "NOT AN ALU INSTRUCTION, THIS IS AN ERROR!")\

#define X(a, b) a
typedef enum {
  XMACRO_ALU_INSTRUCTIONS
} h2_alu;
#undef X

typedef struct {
  int error;                    /*Any error code returned */
  int cycles;                   /*Number of cycles to run for */
  /* Need to sort out IO, should it be a 2D array or an array of FILE *ptr?
     FILE *input;            //Input to processor
     FILE *output;           //Output of processor
   */
  uint16_t ram[RAM_SZ];               /*Main RAM */
  uint16_t tos;                       /*Top of data stack */
  uint16_t data[VAR_SZ];              /*Data stack */
  uint16_t retn[RET_SZ];              /*Return stack */
  uint16_t pc;                        /*Program counter */
  uint16_t datap;                     /*Data stack pointer */
  uint16_t retnp;                     /*Return stack pointer */

  uint16_t interrupt_enable;          /*Interrupt enable flag!*/

  uint32_t carry;
} h2_state_t;

int h2_cpu(h2_state_t * st);
