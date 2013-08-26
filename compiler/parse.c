#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include "parse.h"
/*=============================================================================

  The Grammar (See https://en.wikipedia.org/wiki/Recursive_descent_parser)
  
      program = block "." .
        
      block =
        ["const" ident "=" number {"," ident "=" number} ";"]
        ["var" ident {"," ident} ";"]
        {"procedure" ident ";" block ";"} statement .
       
      statement =
        ident ":=" expression
        | "!" ident
        | "?" ident
        | "call" ident
        | "begin" statement ";" {statement ";"} "end"
        | "if" condition "then" statement
        | "while" condition "do" statement .

      condition =
       "odd" expression
        | expression ("="|"#"|"<"|"<="|">"|">=") expression .

      expression = ["+"|"-"] term {("+"|"-") term} .
        
      term = factor {("*"|"/") factor} .

      factor =
        ident
        | number
        | "(" expression ")" .

=============================================================================*/

#define AST_CUR (ps->current)
#define BUF     (ps->buf)
#define SYM     (ps->sym)
#define IN      (ps->in)
#define OUT     (ps->out)
#define ERR     (ps->err)
#define SNUM    (ps->snum)

static bool isnumber(const char *num);
static symbol_e findsym(const char *sym_str);

static void getsym(parser_st *ps);
static void error(const char msg[], parser_st *ps, unsigned int line);

/*Abstract syntax tree allocations*/
static void calloc_ast(ast_t **tree, parser_st *ps);
static void calloc_ast_car(ast_t *tree, parser_st *ps);
static void calloc_ast_cdr(ast_t *tree, parser_st *ps);
/*Identifier database*/
static void calloc_id(id_t **id, parser_st *ps);
static id_t* find_id(id_t *id, parser_st *ps);
static void add_id(const char *name, symbol_e sym, parser_st *ps);
/*parsing*/
static bool accept(symbol_e s, parser_st *ps);
static bool expect(symbol_e s, parser_st *ps);
static void factor(parser_st *ps);
static void term(parser_st *ps);
static void expression(parser_st *ps);
static void condition(parser_st *ps);
static void statement(parser_st *ps);
static void block(parser_st *ps);

/*============================================================================*/

#define XMAC_SYM(a, b) b,
char *symbol_name[] = {
    SYMBOL_TABLE
};
#undef XMAC_SYM

static bool isnumber(const char *num){
  int i = 0;
  if((num[0] == '+')||(num[0] == '-')){
    if(num[1] == '\0'){
      return false;
    } else{
      i = 1;
    }
  }

  for(;num[i]!='\0'; i++)
    if(!isdigit(num[i]))
      return false;

  return true;
}

static symbol_e findsym(const char *sym_str){
  int i;

  for(i = 0; i < FINAL_SYMBOL; i++){
    if(!strcmp(symbol_name[i],sym_str)){
      return i; /*found a symbol*/
    }
  }

  if(isnumber(sym_str)){
    return number; /*found a number*/
  }

  return ident; /*must be an identifier*/
}

static void getsym(parser_st *ps){
  int c;
START:
  SNUM++;
  /*get space delimited symbol*/
  if(fscanf(IN,"%s",BUF)==EOF){
    if(!strcmp(symbol_name[period],"BUF")){
      error("getsym: EOF",ps,__LINE__);
    }
  }

  /*comments*/
  if(!strcmp("{",BUF)){
    fprintf(OUT,"comment\n");
    while(true){
      c = fgetc(IN);
      if(c == EOF)
        error("getsym: EOF",ps,__LINE__);
      else if(c == '}')
        goto START;
    }
  }

  SYM = findsym(BUF);
  fprintf(OUT,"%s ", symbol_name[SYM]);
  if((SYM == semicolon) || (SYM == period))
    fputc('\n',OUT);
  return;
}

static void error(const char msg[], parser_st *ps, unsigned int line){
  fprintf(ERR,
      "(error (msg \"%s\") (symbol \"%s\") (snum %d) (source %d))\n"
      ,msg, BUF, SNUM , line
      );
  exit(EXIT_FAILURE);
}

/*============================================================================*/
static void calloc_ast(ast_t **tree, parser_st *ps){
  if((*tree=calloc(1,sizeof(ast_t)))==NULL){
    error("Could not calloc()",ps,__LINE__);
  }
}
static void calloc_ast_car(ast_t *tree, parser_st *ps){
  if(tree == NULL){
    error("Passed a NULL, expected a valid pointer",ps,__LINE__);
  }
  calloc_ast(&(tree->car),ps);
}
static void calloc_ast_cdr(ast_t *tree, parser_st *ps){
  if(tree == NULL){
    error("Passed a NULL, expected a valid pointer",ps,__LINE__);
  }
  calloc_ast(&(tree->cdr),ps);
}

static void calloc_id(id_t **id, parser_st *ps){
  if((*id = calloc(1,sizeof(id_t))) == NULL){
    error("Could not calloc()",ps,__LINE__);
  }
}
static id_t* find_id(id_t *id, parser_st *ps){
  id_t *idt = ps->id_next;
  for(;idt!=NULL;idt = idt->idn){
    

  }
}

static void add_id(const char *name, symbol_e sym, parser_st *ps){
}

/*============================================================================*/


static bool accept(symbol_e s, parser_st *ps){
  if(SYM == s){
    getsym(ps);
    return true;
  } else {
    return false;
  }
}

static bool expect(symbol_e s, parser_st *ps){
  if(accept(s,ps)){
    return true;
  } else{
    error("expect: unexpected symbol",ps,__LINE__);
    return false;
  }
}

static void factor(parser_st *ps){
  if(accept(ident, ps)){
    ; /* do nothing */
  } else if (accept(number, ps)){
    ; /* do nothing */
  } else if (accept(lparen,ps)){
    expression(ps);
    expect(rparen,ps);
  } else{
    error("factor: syntax error",ps,__LINE__);
    getsym(ps);
  }
}

static void term(parser_st *ps){
  factor(ps);
  while ((SYM == times) || (SYM == slash)) {
    getsym(ps);
    factor(ps);
  }
}

static void expression(parser_st *ps){
  if ((SYM == plus) || (SYM == minus)){
    getsym(ps);
  }
  term(ps);
  while ((SYM == plus) || (SYM == minus)) {
    getsym(ps);
    term(ps);
  }
}

static void condition(parser_st *ps){
  if (accept(oddsym,ps)) {
    expression(ps);
  } else {
    expression(ps);
    if ((SYM == eql) || (SYM == neq) || (SYM == lss) || 
        (SYM == leq) || (SYM == gtr) || (SYM == geq)) {
      getsym(ps);
      expression(ps);
    } else {
      error("condition: invalid operator",ps,__LINE__);
      getsym(ps);
    }
  } 
}

void statement(parser_st *ps) {
  if (accept(ident,ps)) {
    expect(becomes,ps);
    expression(ps);
  } else if (accept(output,ps)){
    expect(ident,ps);
  } else if (accept(input,ps)){
    expect(ident,ps);
  } else if (accept(callsym,ps)) {
    expect(ident,ps);
  } else if (accept(beginsym,ps)) {
    do {
      statement(ps);
    } while (accept(semicolon,ps));
    expect(endsym,ps);
  } else if (accept(ifsym,ps)) {
    condition(ps);
    expect(thensym,ps);
    statement(ps);
  } else if (accept(whilesym,ps)) {
    condition(ps);
    expect(dosym,ps);
    statement(ps);
  } else {
    error("statement: syntax error",ps,__LINE__);
    getsym(ps);
  }
}

void block(parser_st *ps) {
  if (accept(constsym,ps)) {
    do {
      expect(ident,ps);
      expect(eql,ps);
      expect(number,ps);
    } while (accept(comma,ps));
    expect(semicolon,ps);
  }
  if (accept(varsym,ps)) {
    do {
    expect(ident,ps);
    } while (accept(comma,ps));
    expect(semicolon,ps);
  }
  while (accept(procsym,ps)) {
    expect(ident,ps);
    expect(semicolon,ps);
    block(ps);
    expect(semicolon,ps);
  }
    statement(ps);
}

void parse_program(parser_st *ps) {
  getsym(ps);
  block(ps);
  expect(period,ps);
}

#undef SNUM
#undef BUF
#undef SYM
#undef IN
#undef OUT
#undef ERR

int main(void){
  parser_st ps = {
    {0},
    0,
    NULL,
    NULL,
    NULL,
    0,
    NULL,
    NULL,
    NULL
  };
  ps.in   = stdin;
  ps.out  = stdout;
  ps.err  = stderr;
  calloc_ast(&(ps.head),&ps);
  ps.current = ps.head;

  parse_program(&ps);

  return 0;
}


