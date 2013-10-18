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

enum boolean{
  false,
  true
};

typedef uint16_t mw;

enum vm_err {
        err_ok,
        err_malloc,
        err_cycles,
        err_instruction
};


enum h2_alu {
        alu_tos,
        alu_nos,
        alu_rtos,
        alu_din,
        alu_depth,
        alu_or,
        alu_and,
        alu_xor,
        alu_xnor,
        alu_not,
        alu_add,
        alu_sub,
        alu_sll,
        alu_srl,
        alu_rol,
        alu_ror,
        alu_mul,
        alu_sLT,
        alu_equ,
        alu_uLT,
        alu_A,
        alu_B,
        alu_C,
        alu_D,
        alu_E,
        alu_F,
        alu_G,
        alu_H,
        alu_I,
        alu_dec,
        alu_ioDin,
        alu_ioW
};

struct h2_state {
        int error;              /*Any error code returned */
        int cycles;             /*Number of cycles to run for */
        /* Need to sort out IO, should it be a 2D array or an array of FILE *ptr?
        FILE *input;            //Input to processor
        FILE *output;           //Output of processor
        */
        mw ram[RAM_SZ];         /*Main RAM */
        mw tos;                 /*Top of data stack */
        mw data[VAR_SZ];        /*Data stack */
        mw retn[RET_SZ];        /*Return stack */
        mw pc;                  /*Program counter */
        mw datap;               /*Data stack pointer */
        mw retnp;               /*Return stack pointer */
};

typedef struct h2_state h2_state_t;

int h2_cpu(h2_state_t * st);
