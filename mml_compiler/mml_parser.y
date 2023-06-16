%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
//-- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  //--- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  //-- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;	/* double value */
  std::string          *s;	/* symbol name or string literal */
  
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  mml::block_node      *block;
  mml::block_node      *opt_block;
  
  mml::function_definition_node    *fundef;
  std::vector<std::shared_ptr<cdk::basic_type>> *types;
};

%token tAND tOR tNE tLE tGE tEQ tSIZEOF  
%token tINPUT tPRINT tPRINTLN tARROW
%token tPUBLIC tFORWARD tFOREIGN tPRIVATE
%token tTYPE_STRING tTYPE_INT tTYPE_DOUBLE tTYPE_VOID tTYPE_AUTO 

%token <i> tINTEGER
%token <d> tDOUBLE
%token <s> tIDENTIFIER tSTRING
%token tWHILE tIF tELIF tELSE tBEGIN tEND tTHEN
%token tRETURN tSTOP tNEXT
%token <expression> tNULL

%nonassoc tIF tWHILE
%nonassoc tTHEN
%nonassoc tELSE tELIF

%right '='
%left tOR
%left tAND
%nonassoc tNOT
%left tNE tEQ
%left '<' tLE tGE '>'
%left '+' '-'
%left '*' '/' '%'
%right tUNARY
%nonassoc '(' '['

%type <node> declaration file_declaration program instr argdec iffalse 
%type <sequence> declarations file_declarations file exprs instrs argdecs opt_exprs 
%type <expression> expr integer double opt_initializer  
%type <lvalue> lval
%type<block> block opt_block
%type<fundef> fundef

%type<s> string
%type<type> data_type auto_type function_type 
%type<types> data_types

%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file      : /* empty */                                                           { compiler->ast($$ = new cdk::sequence_node(LINE));         }
          | file_declarations                                                     { compiler->ast($1);                                        }                
          | file_declarations   program                                           { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); } 
          |                     program                                           { compiler->ast($$ = new cdk::sequence_node(LINE, $1));     }                                                        
          ;

file_declarations   :                   file_declaration                          { $$ = new cdk::sequence_node(LINE, $1);     }
                    | file_declarations file_declaration                          { $$ = new cdk::sequence_node(LINE, $2, $1); }
                    ;

file_declaration    : tPUBLIC                 tIDENTIFIER '=' expr ';'            { $$ = new mml::declaration_node(LINE, tPUBLIC, nullptr, *$2, $4); delete $2;  }
                    | tPUBLIC   data_type     tIDENTIFIER opt_initializer ';'     { $$ = new mml::declaration_node(LINE, tPUBLIC, $2, *$3, $4); delete $3;       }
                    | tPUBLIC   auto_type     tIDENTIFIER '=' expr ';'            { $$ = new mml::declaration_node(LINE, tPUBLIC, $2, *$3, $5); delete $3;       }
                    | tFOREIGN function_type  tIDENTIFIER ';'                     { $$ = new mml::declaration_node(LINE, tFOREIGN, $2, *$3, nullptr); delete $3;  }
                    | tFORWARD  data_type     tIDENTIFIER ';'                     { $$ = new mml::declaration_node(LINE, tFORWARD, $2, *$3, nullptr); delete $3;  }
                    | declaration                                                 { $$ = $1;                                                                     }                      
                    ;

program   : tBEGIN opt_block tEND                                                 { $$ = new mml::function_definition_node(LINE, cdk::primitive_type::create(4, cdk::TYPE_INT), $2, true); }
          ;
                  
declarations    :              declaration                                        { $$ = new cdk::sequence_node(LINE, $1);     }
	              | declarations declaration                                        { $$ = new cdk::sequence_node(LINE, $2, $1); }
	              ;

declaration   :     data_type     tIDENTIFIER  opt_initializer ';'                { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, $3); delete $2;       }
              |     auto_type     tIDENTIFIER '=' expr ';'                        { $$ = new mml::declaration_node(LINE, tPRIVATE, nullptr, *$2, $4); delete $2;  } 
              ;

data_type     : tTYPE_STRING                                                      { $$ = cdk::primitive_type::create(8, cdk::TYPE_STRING); }
              | tTYPE_INT                                                         { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT);    }
              | tTYPE_DOUBLE                                                      { $$ = cdk::primitive_type::create(4, cdk::TYPE_DOUBLE); } 
              | '[' tTYPE_VOID ']'                                                { $$ = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID)); }  
              | '[' data_type ']'                                                 { if ($2->name() == cdk::TYPE_POINTER && 
                                                                                      cdk::reference_type::cast($2)->referenced()->name() == cdk::TYPE_VOID) {
                                                                                        $$ = cdk::reference_type::create(4, cdk::primitive_type::create(4, cdk::TYPE_VOID)); 
                                                                                    }                        
                                                                                    else {
                                                                                      $$ = cdk::reference_type::create(4, $2);     
                                                                                    }   
                                                                                  }
              | function_type                                                     { $$ = $1;                                               }
              ;

data_types    :                data_type                                          { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); 
                                                                                    $$->push_back($1); 
                                                                                  }
              | data_types ',' data_type                                          { $$ = $1; 
                                                                                    $$->push_back($3); 
                                                                                  }
              ;

auto_type   : tTYPE_AUTO                                                          { $$ = nullptr; }
            ;

function_type    : tTYPE_VOID '<'            '>'                                  { $$ = cdk::functional_type::create(cdk::primitive_type::create(4, cdk::TYPE_VOID));                 }
                 | tTYPE_VOID '<' data_types '>'                                  { $$ = cdk::functional_type::create(*$3, cdk::primitive_type::create(4, cdk::TYPE_VOID)); delete $3; }
                 | data_type  '<'            '>'                                  { $$ = cdk::functional_type::create($1);                                                             }
                 | data_type  '<' data_types '>'                                  { $$ = cdk::functional_type::create(*$3, $1); delete $3;                                             }
                 ;
       
opt_initializer  : /* empty */                                                    { $$ = NULL; }                
                 | '=' expr                                                       { $$ = $2;   }
                 ;

fundef   : '(' argdecs ')' tARROW tTYPE_VOID block                                { $$ = new mml::function_definition_node(LINE, cdk::primitive_type::create(4, cdk::TYPE_VOID), $2, $6, false); }
         | '('         ')' tARROW tTYPE_VOID block                                { $$ = new mml::function_definition_node(LINE, cdk::primitive_type::create(4, cdk::TYPE_VOID), $5, false);     }
         | '(' argdecs ')' tARROW data_type  block                                { $$ = new mml::function_definition_node(LINE, $5, $2, $6, false);                                             }
         | '('         ')' tARROW data_type  block                                { $$ = new mml::function_definition_node(LINE, $4, $5, false);                                                 }
         ;

argdecs  :             argdec                                                     { $$ = new cdk::sequence_node(LINE, $1);     }
         | argdecs ',' argdec                                                     { $$ = new cdk::sequence_node(LINE, $3, $1); }        
         ;

argdec   : data_type tIDENTIFIER                                                  { $$ = new mml::declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); delete $2; }
         ;

block    : '{'                     '}'                                            { $$ = new mml::block_node(LINE, nullptr, nullptr); }
         | '{' declarations        '}'                                            { $$ = new mml::block_node(LINE, $2, nullptr);      }
         | '{'              instrs '}'                                            { $$ = new mml::block_node(LINE, nullptr, $2);      } 
         | '{' declarations instrs '}'                                            { $$ = new mml::block_node(LINE, $2, $3);           }
         ;

opt_block   :                                                                     { $$ = new mml::block_node(LINE, nullptr, nullptr); }
            |  declarations                                                       { $$ = new mml::block_node(LINE, $1, nullptr);      }           
            |                instrs                                               { $$ = new mml::block_node(LINE, nullptr, $1);      }
            |  declarations  instrs                                               { $$ = new mml::block_node(LINE, $1, $2);           }
            ;

expr            : integer                                             { $$ = $1;                                             }
                | double                                              { $$ = $1;                                             }         
                | string                                              { $$ = new cdk::string_node(LINE, $1);                 }
                | tNULL                                               { $$ = new mml::null_node(LINE);                       }
                | lval '?'                                            { $$ = new mml::address_of_node(LINE, $1);             }
                | lval                                                { $$ = new cdk::rvalue_node(LINE, $1);                 }    
                | '(' expr ')'                                        { $$ = $2;                                             }
                |'[' expr ']'                                         { $$ = new mml::stack_alloc_node(LINE, $2);            }
                | '@' '(' opt_exprs ')'                               { $$ = new mml::function_call_node(LINE, nullptr, $3); }
                | expr '(' opt_exprs ')'                              { $$ = new mml::function_call_node(LINE, $1, $3);      }
                | tSIZEOF '(' expr ')'                                { $$ = new mml::sizeof_node(LINE, $3);                 }
                | tINPUT                                              { $$ = new mml::input_node(LINE);                      }
                | '+' expr %prec tUNARY                               { $$ = new mml::identity_node(LINE, $2);               }
                | '-' expr %prec tUNARY                               { $$ = new cdk::neg_node(LINE, $2);                    } 
                | expr '+' expr                                       { $$ = new cdk::add_node(LINE, $1, $3);                }
                | expr '-' expr	                                      { $$ = new cdk::sub_node(LINE, $1, $3);                }  
                | expr '*' expr	                                      { $$ = new cdk::mul_node(LINE, $1, $3);                }
                | expr '/' expr	                                      { $$ = new cdk::div_node(LINE, $1, $3);                }
                | expr '%' expr	                                      { $$ = new cdk::mod_node(LINE, $1, $3);                }
                | expr '<' expr	                                      { $$ = new cdk::lt_node(LINE, $1, $3);                 }
                | expr '>' expr	                                      { $$ = new cdk::gt_node(LINE, $1, $3);                 }
                | expr tGE expr	                                      { $$ = new cdk::ge_node(LINE, $1, $3);                 } 
                | expr tLE expr                                       { $$ = new cdk::le_node(LINE, $1, $3);                 }
                | expr tNE expr	                                      { $$ = new cdk::ne_node(LINE, $1, $3);                 }
                | expr tEQ expr	                                      { $$ = new cdk::eq_node(LINE, $1, $3);                 }
                | tNOT expr                                           { $$ = new cdk::not_node(LINE,$2);                     }
                | expr tAND expr	                                    { $$ = new cdk::and_node(LINE, $1, $3);                }
                | expr tOR expr	                                      { $$ = new cdk::or_node(LINE, $1, $3);                 }
                | lval '=' expr                                       { $$ = new cdk::assignment_node(LINE, $1, $3);         }
                | fundef                                              { $$ = $1;                                             }
                ;

exprs     : expr                                                      { $$ = new cdk::sequence_node(LINE, $1);     }
          | exprs ',' expr                                            { $$ = new cdk::sequence_node(LINE, $3, $1); }
          ;

opt_exprs : /* empty */                                               { $$ = new cdk::sequence_node(LINE);          }
          | exprs                                                     { $$ = $1;                                    }
          ;

instr     : expr ';'                                                  { $$ = new mml::evaluation_node(LINE, $1);      }
          | exprs tPRINT                                              { $$ = new mml::print_node(LINE, $1);           }
          | exprs tPRINTLN                                            { $$ = new mml::print_node(LINE, $1, true);     }
          | tNEXT tINTEGER    ';'                                     { $$ = new mml::next_node(LINE, $2);            } 
          | tSTOP tINTEGER    ';'                                     { $$ = new mml::stop_node(LINE, $2);            }
          | tNEXT             ';'                                     { $$ = new mml::next_node(LINE, 1);             }
          | tSTOP             ';'                                     { $$ = new mml::stop_node(LINE, 1);             }
          | tRETURN expr ';'                                          { $$ = new mml::return_node(LINE, $2);          }
          | tRETURN           ';'                                     { $$ = new mml::return_node(LINE, nullptr);     }   
          | tIF '(' expr ')' instr %prec tIF                          { $$ = new mml::if_node(LINE, $3, $5);          }
          | tIF '(' expr ')' instr iffalse                            { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
          | tWHILE '(' expr ')' instr                                 { $$ = new mml::while_node(LINE, $3, $5);       }
          | block                                                     { $$ = $1;                                      }
          ;

instrs    :             instr                                         { $$ = new cdk::sequence_node(LINE, $1);                                                                                                                 }                                                         
          | instr       instrs                                        { std::reverse($2->nodes().begin(), $2->nodes().end()); $$ = new cdk::sequence_node(LINE, $1, $2); std::reverse($$->nodes().begin(), $$->nodes().end()); } 
          ;

iffalse         : tELSE instr                                         { $$ = $2;                                      }
                | tELIF '(' expr ')' instr %prec tTHEN                { $$ = new mml::if_node(LINE, $3, $5);          }
                | tELIF '(' expr ')' instr iffalse                    { $$ = new mml::if_else_node(LINE, $3, $5, $6); }
                ;

lval      : tIDENTIFIER                                               { $$ = new cdk::variable_node(LINE, $1); delete $1; }
          | expr   '[' expr ']'                                       { $$ = new mml::index_node(LINE, $1, $3);           }
          ;

integer   : tINTEGER                                                  { $$ = new cdk::integer_node(LINE,$1); }
          ;

double    : tDOUBLE                                                   { $$ = new cdk::double_node(LINE,$1); }
          ;

string    : tSTRING                                                   { $$ = $1;                             }               
          | string tSTRING                                            { $$ = $1; $$->append(*$2); delete $2; }
          ;
%%