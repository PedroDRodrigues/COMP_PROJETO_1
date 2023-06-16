%option c++ prefix="mml_scanner_" outfile="mml_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <cstring>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "mml_parser.tab.h"

// output stream for building string literals
static std::ostringstream strlit;

// don't change this
#define yyerror LexerError
%}

%x X_STRING X_BACKSLASH X_COMMENT 
%%
  yydebug=1; set_debug(1);

[ ]*"..."\n            ;

"//".*\n               { yyless(yyleng - 1); }

"/*"                   yy_push_state(X_COMMENT);
<X_COMMENT>"/*"        yy_push_state(X_COMMENT);
<X_COMMENT>"*/"        yy_pop_state();
<X_COMMENT>.|\n        ;  /* ignore inside comments */


"int"                  return tTYPE_INT;
"double"               return tTYPE_DOUBLE;
"string"               return tTYPE_STRING;
"void"                 return tTYPE_VOID;

"begin"                return tBEGIN; 
"end"                  return tEND; 
"null"                 return tNULL;
"sizeof"               return tSIZEOF;

"public"               return tPUBLIC;
"forward"              return tFORWARD;
"foreign"              return tFOREIGN;
"auto"                 return tTYPE_AUTO;

"if"                   return tIF; 
"elif"                 return tELIF;
"else"                 return tELSE; 

"while"                return tWHILE;
"stop"                 return tSTOP;
"next"                 return tNEXT;

[\[\](),?]             return *yytext;
[-+*/%<>@?=]           return *yytext;
"=="                   return tEQ;
"!="                   return tNE;
"<="                   return tLE;
">="                   return tGE;
"~"                    return tNOT;
"&&"                   return tAND;
"||"                   return tOR;
"input"                return tINPUT;

[-()<>=+*/%;{}.]       return *yytext;
"return"               return tRETURN;
"->"                   return tARROW;
"!"                    return tPRINT; 
"!!"                   return tPRINTLN;

0[0-7]+                { 
                          try {
                            yylval.i = std::stoi(yytext, nullptr, 8); 
                            return tINTEGER; 
                          } catch (const std::out_of_range& e) {
                            yyerror("Base 8 out of range.");
                          } 
                       }

0[0-9]+                yyerror("Invalid base 8 number.");

0|[1-9][0-9]*          {
                          try { 
                            yylval.i = std::stoi(yytext, NULL, 10); 
                            return tINTEGER;
                          } catch (const std::out_of_range& e) {
                            yyerror("Integer is out of range");
                          }
                        }

[0-9]*\.[0-9]+([Ee][-+]?[0-9]+)?        { 
                                          try { 
                                            yylval.d = std::stod(yytext); 
                                            return tDOUBLE; 
                                          } catch (const std::out_of_range& e) {
                                            yyerror("Double is out of range");
                                          }  
                                      }

[0-9]+\.[0-9]*([Ee][-+]?[0-9]+)?        { 
                                          try { 
                                            yylval.d = std::stod(yytext); 
                                            return tDOUBLE; 
                                          } catch (const std::out_of_range& e) {
                                            yyerror("Double is out of range");
                                          } 
                                        }

[0-9]+([Ee][-+]?[0-9]+)                 { 
                                          try { 
                                            yylval.d = std::stod(yytext); 
                                            return tDOUBLE; 
                                          } catch (const std::out_of_range& e) {
                                            yyerror("Double is out of range");
                                          }  
                                        }

[A-Za-z][A-Za-z0-9]*  yylval.s = new std::string(yytext); return tIDENTIFIER;

"\""                           yy_push_state(X_STRING);
<X_STRING>\\                   yy_push_state(X_BACKSLASH);
<X_STRING>"\""                 {
                                 yylval.s = new std::string(strlit.str());
                                 strlit.str("");
                                 yy_pop_state();
                                 return tSTRING;
                               }

<X_STRING>"...\n"              ;
<X_STRING>\n                   yyerror("Newline in string");          
<X_STRING>\0                   yyerror("NULL character in string");               
<X_STRING>.                    strlit << *yytext;

<X_BACKSLASH>r                 strlit << '\r'; yy_pop_state();
<X_BACKSLASH>t                 strlit << '\t'; yy_pop_state();
<X_BACKSLASH>n                 strlit << '\n'; yy_pop_state();
<X_BACKSLASH>\"                strlit << '\"'; yy_pop_state();
<X_BACKSLASH>\\                strlit << '\\'; yy_pop_state();

<X_BACKSLASH>[0-7]{1,3}        {
                                 unsigned long octalValue = std::stoul(yytext, NULL, 8);
                                 if (octalValue > 255) {
                                  yyerror("Invalid 8 bit number.");
                                 }
                                 strlit << (char)(unsigned char)octalValue;
                                 yy_pop_state();
                               }
<X_BACKSLASH>.                 strlit << *yytext; yy_pop_state(); 

[ \t\r]|\n                      ; /* ignore whitespace */

.                               yyerror("Unknown character");

%%
