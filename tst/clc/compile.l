%{
#include <stdlib.h>
#include "compile.h"
#include "y.tab.h"
#include <stdint.h>

static uint32_t hashes[MAX_MEMORY]; /* hashes of identifiers that exist */
static unsigned int hashindex = 0;

void yyerror(char *);
void comment(void);
uint32_t hash(char *s); /* collision resolution is handled by prayer */
int hashexists(uint32_t h);
%}

%%
"/*"        { comment(); }

"//"[^\n]*  { /* consume //-comment */ }

"<<"            return LS;
">>"            return RS;
">="            return GE;
"<="            return LE;
"=="            return EQ;
"!="            return NE;
"while"         return WHILE;
"if"            return IF;
"else"          return ELSE;
"print"         return PRINT;


[_a-zA-Z][_0-9a-zA-Z]* { 
                uint32_t h;
                yylval.sSymbol = strdup(yytext);
                h = hash(yytext);
                if(!hashexists(h)){
                  printf("alloc %s\n", yytext);
                  hashes[hashindex++] = h;
                } 
                return VARIABLE;
             }

0[0-7]*     { /* octal --and-- zero */
                yylval.iValue =   strtol(yytext,NULL,8u);
                return INTEGER;
            }

[1-9][0-9]* {
                yylval.iValue = atoi(yytext);
                return INTEGER;
            }

0[xX][0-9a-fA-F]* { /* hex */
                yylval.iValue =   strtol(yytext,NULL,16u);
                return INTEGER;
            }

[-()<>=+*/;{}.^&|~] {
                return *yytext;
             }


[ \t\n]+        ;       /* ignore whitespace */

.               yyerror("Unknown character");
%%
int yywrap(void) {
    return 1;
}

void comment(void)
{
  char c, prev = 0;
  while ((c = input()) != 0) /* (EOF maps to 0) */
  {
    if (c == '/' && prev == '*')
      return;
    prev = c;
  }
  yyerror("unterminated comment");
}

uint32_t hash(char *s){ /* djb <http://www.cse.yorku.ca/~oz/hash.html> */
  uint32_t h = 5381, c;

  while ((c = *s++))
    h = ((h << 5) + h) + c; /* hash * 33 + c */

  return h;
}

int hashexists(uint32_t h){
  unsigned int i;
  for(i = 0; i < MAX_MEMORY && i < hashindex; i++){
    if(hashes[i] == h)
      return 1;
  }
  return 0;
}

