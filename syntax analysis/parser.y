%{
    #include <stdlib.h>
    #include <stdio.h>
    #include "symbol_table.h"

    entry_ht** symbol_table;
    entry_ht** constant_table;

    int curr_dtype;
    int yyerror(char *error_msg);

%}

%union
{
    int dval;
    int ival;
    entry_ht* entry;
}

%token <entry> IDENTIFIER

 /* Constants */
%token <dval> DEC_CONSTANT

 /* Arithmetic Operators */
%token AND SUB DIV MUL MOD

 /* Logical and Relational operators */
%token LAND LOR LEQ GEQ EQ NEQ LESSER GREATER

 /* Short hand assignment operators */
%token MUL_ASSIGN DIV_ASSIGN MOD_ASSIGN ADD_ASSIGN SUB_ASSIGN
%token INCR DECR

 /* Data types */
%token INT DEC CHAR BOOL

 /* Keywords */
%token FOR WHILE IF ELSE ELIF VOID TRUE FALSE PRINT CONTINUE BREAK RETURN

%type <dval> expr
%type <dval> sub_expr
%type <dval> const
%type <dval> unary_expr
%type <dval> arithmetic_expr
%type <dval> assign_expr
%type <entry> left_value
%type <ival> assign_op

%start starter

%left ','
%right '='
%left LOR
%left LAND
%left EQ NEQ
%left LESSER GREATER LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%right NOT


%nonassoc UMINUS

%%

starter: starter main
             |main;

main: function|
       declaration;

function: d_type IDENTIFIER '(' parameter_list ')' multi_stmt;

d_type :
    INT                                {curr_dtype = INT;}
    |DEC                               {curr_dtype = DEC;}
    |CHAR                              {curr_dtype = CHAR;}
    |BOOL                              {curr_dtype = BOOL;}
    ;

parameter_list :parameters
    |
    ;

parameters :parameters ',' param
    | param
    ;

param :d_type IDENTIFIER
   ;

stmt: multi_stmt
    |single_stmt
    ;

multi_stmt :'{' stmts '}'
    ;

stmts: stmts stmt
    |
    ;

single_stmt :if_stmt
    |RETURN ';'
    |CONTINUE ';'
    |BREAK ';'
    ;

if_stmt:
      IF '(' expr ')' multi_stmt
    | IF '(' expr ')' multi_stmt elif_stmt  
    | IF '(' expr ')' multi_stmt else_stmt 
    ;

elif_stmt:
      ELIF '(' expr ')' multi_stmt 
    | ELIF '(' expr ')' multi_stmt elif_stmt
    | ELIF '(' expr ')' multi_stmt else_stmt
    ;

else_stmt: ELSE multi_stmt;

%%

#include "lex.yy.c"
#include <ctype.h>

int main(int argc, char *argv[])
{
    symbol_table =  create_new_hash_table() ;

    yyin = fopen(argv[1], "r");

    if(!yyparse())
    {
        printf("\nParsing completed Successfully\n");
    }
    else
    {
            printf("\nParsing failed\n");
    }


    printf("\n\tSymbol table");
    display_table(symbol_table);


    fclose(yyin);
    return 0;
}

int yyerror(char *error_msg)
{
    printf("Line no: %d \n Error message: %s \n Token: %s\n", yylineno, error_msg, yytext);
}