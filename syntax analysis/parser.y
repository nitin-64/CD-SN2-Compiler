%{
    #include <stdlib.h>
    #include <stdio.h>
    #include "symbol_table.h"

    entry_ht** symbol_table;
    entry_ht** constant_table;

    #include "lex.yy.c"

    int curr_dtype;
    int ArithmeticAssign (int left_value, int assign_type, int right_value);
    int yyerror(char *error_msg);

%}

%union
{
    int dval;
    int ival;
    float fval;
    char cval;
    entry_ht* entry;
}

%token <entry> IDENTIFIER

 /* Constants */
%token <dval> DEC_CONSTANT
%token <cval> CHAR_CONSTANT  
%token <dval> BOOL_CONSTANT
%token <fval> FLOAT_CONSTANT  


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
    |for_stmt
    |while_stmt
    |declaration
    |function_call ';'
    |RETURN ';'
    |CONTINUE ';'
    |BREAK ';'
    |RETURN sub_expr ';'
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

for_stmt:FOR '(' expr_stmt  expr_stmt ')' multi_stmt
    |FOR '(' expr_stmt expr_stmt expr ')' multi_stmt
    ;

while_stmt: WHILE '(' expr')' multi_stmt
        ;

declaration :d_type declaration_list ';'
             | declaration_list ';'
             | unary_expr ';'

declaration_list: declaration_list ',' sub_decl
        |sub_decl;

sub_decl: assign_expr
    |IDENTIFIER                     {$1 -> data_type = curr_dtype;}
    ;

expr_stmt :expr ';'
    |';'
    ;

expr:
    expr ',' sub_expr                                   {$$ = $1,$3;}
    |sub_expr                                           {$$ = $1;}
    ;

sub_expr:
    sub_expr GREATER sub_expr                   {$$ = ($1 > $3);}
    |sub_expr LESSER sub_expr                   {$$ = ($1 < $3);}
    |sub_expr EQ sub_expr                       {$$ = ($1 == $3);}
    |sub_expr NEQ sub_expr                      {$$ = ($1 != $3);}
    |sub_expr LEQ sub_expr                      {$$ = ($1 <= $3);}
    |sub_expr GEQ sub_expr                      {$$ = ($1 >= $3);}
    |sub_expr LAND sub_expr                     {$$ = ($1 && $3);}
    |sub_expr LOR sub_expr                      {$$ = ($1 || $3);}
    |NOT sub_expr                               {$$ = (!$2);}
    |arithmetic_expr                            {$$ = $1;}
    |assign_expr                                {$$ = $1;}
    |unary_expr                                 {$$ = $1;}
    ;

assign_expr :left_value assign_op arithmetic_expr  {$$ = $1->token_name = ArithmeticAssign($1->token_name,$2,$3);}
    |left_value assign_op function_call            {$$ = 0;}
    |left_value assign_op unary_expr               {$$ = $1->token_name = ArithmeticAssign($1->token_name,$2,$3);}
    |unary_expr assign_op unary_expr               {$$ = 0;}
    ;

unary_expr: left_value INCR                          {$$ = $1->token_name = ($1->token_name)++;}
    |left_value DECR                                 {$$ = $1->token_name = ($1->token_name)--;}
    |DECR left_value                                 {$$ = $2->token_name = --($2->token_name);}
    |INCR left_value                                 {$$ = $2->token_name = ++($2->token_name);}
    ;

left_value :IDENTIFIER                               {$$ = $1; if(! $1->data_type) $1->data_type = curr_dtype;}
    ;

assign_op: '='                                       {$$ = '=';}
    |ADD_ASSIGN                                    {$$ = ADD_ASSIGN;}
    |SUB_ASSIGN                                    {$$ = SUB_ASSIGN;}
    |MUL_ASSIGN                                    {$$ = MUL_ASSIGN;}
    |DIV_ASSIGN                                    {$$ = DIV_ASSIGN;}
    |MOD_ASSIGN                                    {$$ = MOD_ASSIGN;}
    ;

arithmetic_expr: arithmetic_expr ADD arithmetic_expr    {$$ = $1 + $3;}
    |arithmetic_expr SUB arithmetic_expr                {$$ = $1 - $3;}
    |arithmetic_expr MUL arithmetic_expr                {$$ = $1 * $3;}
    |arithmetic_expr DIV arithmetic_expr                {$$ = ($3 == 0) ? yyerror("Divide by 0!") : ($1 / $3);}
    |arithmetic_expr MOD arithmetic_expr                {$$ = (int)$1 % (int)$3;}
    |'(' arithmetic_expr ')'                            {$$ = $2;}
    |'-' arithmetic_expr %prec UMINUS                   {$$ = -$2;}
    |IDENTIFIER                                         {$$ = $1 -> token_name;}
    |const                                              {$$ = $1;}
    ;

const: DEC_CONSTANT                                  {$$ = $1;}
    | CHAR_CONSTANT                                  {$$ = (int)$1;}
    | BOOL_CONSTANT                                  {$$ = $1;}
    ;

function_call: IDENTIFIER '(' values_list ')'
             |IDENTIFIER '(' ')'
             ;

values_list:
              values_list ','  value
              |value
              ;

value: sub_expr
        ;

%%


#include <ctype.h>


int ArithmeticAssign (int left_value, int assign_type, int right_value)
{
    switch(assign_type)
    {
        case '=': return right_value;
        case ADD_ASSIGN: return (left_value + right_value);
        case SUB_ASSIGN: return (left_value - right_value);
        case MUL_ASSIGN: return (left_value * right_value);
        case DIV_ASSIGN: return (left_value / right_value);
        case MOD_ASSIGN: return ((int)left_value % (int)right_value);
    }
}

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





