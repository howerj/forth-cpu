#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
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


/*===========================================================================*/
#define SYMBOL_TABLE \
  XMAC_SYM(ident, "ident") /*implicit definition checked later*/\
  XMAC_SYM(number, "number")/*implicit definition checked later*/\
  XMAC_SYM(output, "!")\
  XMAC_SYM(input, "?")\
  XMAC_SYM(lparen, "(")\
  XMAC_SYM(rparen, ")")\
  XMAC_SYM(times, "*")\
  XMAC_SYM(slash, "/")\
  XMAC_SYM(plus, "+")\
  XMAC_SYM(minus, "-")\
  XMAC_SYM(eql, "=")\
  XMAC_SYM(neq, "#")\
  XMAC_SYM(lss, "<")\
  XMAC_SYM(leq, "<=")\
  XMAC_SYM(gtr, ">")\
  XMAC_SYM(geq, ">=")\
  XMAC_SYM(callsym, "call")\
  XMAC_SYM(beginsym, "begin")\
  XMAC_SYM(semicolon, ";")\
  XMAC_SYM(endsym, "end")\
  XMAC_SYM(ifsym, "if")\
  XMAC_SYM(whilesym, "while")\
  XMAC_SYM(becomes, ":=")\
  XMAC_SYM(thensym, "then")\
  XMAC_SYM(dosym, "do")\
  XMAC_SYM(constsym, "const")\
  XMAC_SYM(comma, ",")\
  XMAC_SYM(varsym, "var")\
  XMAC_SYM(procsym, "procedure")\
  XMAC_SYM(period, ".")\
  XMAC_SYM(oddsym, "odd")\
  XMAC_SYM(FINAL_SYMBOL,"")\

#define XMAC_SYM(a, b) a,
typedef enum{
    SYMBOL_TABLE
} symbol_e;
#undef XMAC_SYM

/* The other X Macro is defined in the file it is used in*/

/*===========================================================================*/

#define BUF_SZ (512)

typedef struct{
  char buf[BUF_SZ];
  symbol_e sym;
  FILE *in;
  FILE *out;
  FILE *err;
  unsigned int snum;
} parser_st;

typedef enum{
  false,
  true
} bool;

#define BUF   (ps->buf)
#define SYM   (ps->sym)
#define IN    (ps->in)
#define OUT   (ps->out)
#define ERR   (ps->err)
#define SNUM  (ps->snum)

static bool isnumber(const char *num);
static symbol_e findsym(const char *sym_str);

static void getsym(parser_st *ps);
static void error(const char msg[], parser_st *ps, unsigned int line);

static bool accept(symbol_e s, parser_st *ps);
static bool expect(symbol_e s, parser_st *ps);
static void factor(parser_st *ps);
static void term(parser_st *ps);
static void expression(parser_st *ps);
static void condition(parser_st *ps);
static void statement(parser_st *ps);
static void block(parser_st *ps);

void parse_program(parser_st *ps);
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
    error("getsym: EOF",ps,__LINE__);
  }

  /*comments*/
  if(!strcmp("{",BUF)){
    printf("FOUND COMMENT\n");
    while(true){
      c = fgetc(IN);
      if(c == EOF)
        error("getsym: EOF",ps,__LINE__);
      else if(c == '}')
        goto START;
    }
  }

  SYM = findsym(BUF);

  printf("FOUND SYMBOL (%s)\n", symbol_name[SYM]);
  return;
}

static void error(const char msg[], parser_st *ps, unsigned int line){
  fprintf(ERR,
      "(error (msg \"%s\") (symbol \"%s\") (snum %d) (source %d))\n"
      ,msg, BUF, SNUM , line
      );
  exit(1);
}

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
    0
  };
  ps.in   = stdin;
  ps.out  = stdout;
  ps.err  = stderr;

  parse_program(&ps);

  return 0;
}


