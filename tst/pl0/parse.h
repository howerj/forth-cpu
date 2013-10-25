/* @file parse.h
 * @brief Parser/Compiler that will target a dialect of Forth of my own devising,
 * header only.
 *
 * @author    Richard James Howe
 * @copyright Copyright 2013 Richard James Howe.
 * @license   LGPL
 * @email     howe.r.j.89@gmail.com
 */
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

typedef enum{
  false,
  true
} bool;

typedef struct identifier_dictionary{
  char name[BUF_SZ];
  symbol_e type;
  struct identifier_dictionary *idn;
} id_t;

typedef struct abstract_syntax_tree{
  symbol_e sym;         /*symbol*/
  id_t *id;
  char *s;              /*names if needed*/

  struct abstract_syntax_tree *car;
  struct abstract_syntax_tree *cdr;

  bool terminal_f;
  bool head_f;          /*is the top node in a tree?*/
} ast_t;

typedef struct{
  char buf[BUF_SZ];
  symbol_e sym;
  FILE *in;
  FILE *out;
  FILE *err;
  unsigned int snum;

  struct abstract_syntax_tree *head;
  struct abstract_syntax_tree *current;
  struct identifier_dictionary *id_next;
  struct identifier_dictionary *id_head;
} parser_st;

void parse_program(parser_st *ps);
