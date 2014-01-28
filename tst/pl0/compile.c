/**
 * Adapted from <https://en.wikipedia.org/wiki/Recursive_descent_parser> 
 * by Richard James Howe. Not the original author.
 *
 * @file compile.c
 *
 */

#include <stdio.h>
#include "compile.h"

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


typedef enum {ident, number, lparen, rparen, times, slash, plus,
    minus, eql, neq, lss, leq, gtr, geq, callsym, beginsym, semicolon,
    endsym, ifsym, whilesym, becomes, thensym, dosym, constsym, comma,
    varsym, procsym, period, oddsym} Symbol;
 
Symbol sym;
void getsym(void);
void error(const char msg[]);
void expression(void);
 
int accept(Symbol s) {
    if (sym == s) {
        getsym();
        return 1;
    }
    return 0;
}
 
int expect(Symbol s) {
    if (accept(s))
        return 1;
    error("expect: unexpected symbol");
    return 0;
}
 
void factor(void) {
    if (accept(ident)) {
        ;
    } else if (accept(number)) {
        ;
    } else if (accept(lparen)) {
        expression();
        expect(rparen);
    } else {
        error("factor: syntax error");
        getsym();
    }
}
 
void term(void) {
    factor();
    while (sym == times || sym == slash) {
        getsym();
        factor();
    }
}
 
void expression(void) {
    if (sym == plus || sym == minus)
        getsym();
    term();
    while (sym == plus || sym == minus) {
        getsym();
        term();
    }
}
 
void condition(void) {
    if (accept(oddsym)) {
        expression();
    } else {
        expression();
        if (sym == eql || sym == neq || sym == lss || sym == leq || sym == gtr || sym == geq) {
            getsym();
            expression();
        } else {
            error("condition: invalid operator");
            getsym();
        }
    }
}
 
void statement(void) {
    if (accept(ident)) {
        expect(becomes);
        expression();
    } else if (accept(callsym)) {
        expect(ident);
    } else if (accept(beginsym)) {
        do {
            statement();
        } while (accept(semicolon));
        expect(endsym);
    } else if (accept(ifsym)) {
        condition();
        expect(thensym);
        statement();
    } else if (accept(whilesym)) {
        condition();
        expect(dosym);
        statement();
    } else {
        error("statement: syntax error");
        getsym();
    }
}
 
void block(void) {
    if (accept(constsym)) {
        do {
            expect(ident);
            expect(eql);
            expect(number);
        } while (accept(comma));
        expect(semicolon);
    }
    if (accept(varsym)) {
        do {
            expect(ident);
        } while (accept(comma));
        expect(semicolon);
    }
    while (accept(procsym)) {
        expect(ident);
        expect(semicolon);
        block();
        expect(semicolon);
    }
    statement();
}
 
void program(void) {
    getsym();
    block();
    expect(period);
}
