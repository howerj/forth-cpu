#include <stdint.h>
#include <stdio.h>

typedef enum {
    ident, number, lparen, rparen, times, slash, plus,
    minus, eql, neq, lss, leq, gtr, geq, callsym, beginsym, 
    semicolon, endsym, ifsym, whilesym, becomes, thensym, 
    dosym, constsym, comma, varsym, procsym, period, oddsym
} symbol_e;

typedef struct{
  symbol_e sym;
  FILE *in;
  FILE *out;
  FILE *err;
} parser_st;

typedef enum{
  false,
  true
} bool;

#define SYM (ps->sym)
#define IN  (ps->in)
#define OUT (ps->out)
#define ERR (ps->err)

static void getsym(parser_st *ps);
static void error(const char msg[], parser_st *ps);
static void expression(parser_st *ps);
static int accept(symbol_e s, parser_st *ps);
static int expect(symbol_e s, parser_st *ps);
static void factor(parser_st *ps);
static void term(parser_st *ps);
static void expression(parser_st *ps);
static void condition(parser_st *ps);
static void statement(parser_st *ps);
static void block(parser_st *ps);

void parse_program(parser_st *ps);

#undef SYM
#undef IN
#undef OUT
#undef ERR
