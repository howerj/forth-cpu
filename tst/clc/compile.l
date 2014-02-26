%{
#include <stdlib.h>
#include "compile.h"
#include "y.tab.h"
#include <stdint.h>

void yyerror(char *);
void comment(void);
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
                yylval.sSymbol = strdup(yytext);
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

