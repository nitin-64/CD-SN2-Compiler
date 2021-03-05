%{
    #include <stdlib.h>
    #include <stdio.h>
    #include "symbol_table.h"
    #include "lex.yy.c"

    entry_ht** symbol_table;
    entry_ht** constant_table;

    int curr_dtype;
    int ArithmeticAssign (int left_value, int assign_type, int right_value);
    int yyerror(char *error_msg);
    entry_ht* checkMultiDeclarations(char *varName);

    int success_flag = 1;
    int is_declared = 0;

    struct exp_node{
		int value;
		int dtype;
	};

%}

%union
{
    int dval;
    int ival;
    float fval;
    char cval;
    entry_ht* entry;
    struct exp_node* expnode;
}


%token <entry> IDENTIFIER
%type <entry> id

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
%type <expnode> const
%type <dval> unary_expr
%type <expnode> arithmetic_expr
%type <expnode> assign_expr
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

function: d_type id '(' parameter_list ')' {is_declared = 0;}  multi_stmt  
		;

d_type :
    INT                                {curr_dtype = 1; is_declared =1;}
    |DEC                               {curr_dtype = 2; is_declared =1;}
    |CHAR                              {curr_dtype = 3; is_declared =1;}
    |BOOL                              {curr_dtype = 4; is_declared =1;}
    ;

parameter_list :parameters 
    |
    ;

parameters :parameters ',' param
    | param
    ;

param :d_type id
   ;

declaration :d_type declaration_list ';' { is_declared = 0;}
             | declaration_list ';'
             | unary_expr ';'

declaration_list: declaration_list ',' sub_decl
        |sub_decl;

sub_decl: assign_expr
    | id                     { $1 ->data_type = curr_dtype; }
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
    |PRINT '(' id ')' ';'
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

expr_stmt :expr ';'
    |';'
    ;

expr:
    expr ',' sub_expr                                   { $$ = $1,$3; }
    |sub_expr                                           { $$ = $1; }
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
    |arithmetic_expr                            { $$ = $1->value; }
    |assign_expr                                { $$ = $1->value; }
    |unary_expr                                 {$$ = $1;}
    ;

assign_expr :
	left_value assign_op arithmetic_expr           { $$ = (struct exp_node *) malloc(sizeof(struct exp_node));
													$$->dtype = typecheck($1->data_type, $3->dtype, 1); 
													 $$->value = $1->token_name = ArithmeticAssign($1->token_name,$2,$3->value);
													  }
    |left_value assign_op function_call            {$$ = 0;}
    |left_value assign_op unary_expr               {$$ = $1->token_name = ArithmeticAssign($1->token_name,$2,$3);}
    |unary_expr assign_op unary_expr               {$$ = 0;}
    ;

unary_expr: left_value INCR                          {$$ = $1->token_name = ($1->token_name)++;}
    |left_value DECR                                 {$$ = $1->token_name = ($1->token_name)--;}
    |DECR left_value                                 {$$ = $2->token_name = --($2->token_name);}
    |INCR left_value                                 {$$ = $2->token_name = ++($2->token_name);}
    ;

left_value :id                               {$$ = $1; if(! $1->data_type) $1->data_type = curr_dtype;}
    ;



assign_op: '='                                       {$$ = '=';}
    |ADD_ASSIGN                                    {$$ = ADD_ASSIGN;}
    |SUB_ASSIGN                                    {$$ = SUB_ASSIGN;}
    |MUL_ASSIGN                                    {$$ = MUL_ASSIGN;}
    |DIV_ASSIGN                                    {$$ = DIV_ASSIGN;}
    |MOD_ASSIGN                                    {$$ = MOD_ASSIGN;}
    ;

arithmetic_expr: arithmetic_expr ADD arithmetic_expr    { $$ = (struct exp_node *) malloc(sizeof(struct exp_node));
														  $$->dtype = typecheck($1->dtype, $3->dtype, 0); 
														  $$->value = $1->value + $3->value; }
    |arithmetic_expr SUB arithmetic_expr                {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    														$$->dtype = typecheck($1->dtype, $3->dtype, 0); 
														  	$$->value = $1->value - $3->value;}
    |arithmetic_expr MUL arithmetic_expr                {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    														$$->dtype = typecheck($1->dtype, $3->dtype, 0); 
														  	$$->value = $1->value * $3->value;}
    |arithmetic_expr DIV arithmetic_expr                {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    														$$->dtype = typecheck($1->dtype, $3->dtype, 0); 
														  	$$->value = ($3->value == 0) ? yyerror("Divide by 0!") : ($1->value / $3->value);
														  }
    |arithmetic_expr MOD arithmetic_expr                {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    														$$->dtype = typecheck($1->dtype, $3->dtype, 0); 
														  	$$->value = (int)$1->value % (int)$3->value;}
    |'(' arithmetic_expr ')'                            {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    														$$ = $2;}
    |'-' arithmetic_expr %prec UMINUS                   {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    													$$->value = -$2->value; $$->dtype = $2->dtype;}
    |id                                                 {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    														$$->value = $1->token_name; $$->dtype = $1->data_type;}
    |const                                              {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
    														$$ = $1;}
    ;

const: DEC_CONSTANT                                  {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
														$$->value = $1; 
														$$->dtype = 1;}
    | CHAR_CONSTANT                                  {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
														$$->dtype = 3;
														$$->value = (int)$1;}
    | BOOL_CONSTANT                                  {$$ = (struct exp_node *) malloc(sizeof(struct exp_node));
														$$->value = $1; 
														$$->dtype = 4;}
    ;

function_call: id '(' values_list ')'
             |id '(' ')'
             ;

id : IDENTIFIER 		{ $1 = checkMultiDeclarations(yytext); $$ = $1; }
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
    yyparse();

    if(success_flag)
    {
        printf("\nParsing completed Successfully\n");
         printf("\n\tSymbol table");
    	display_table(symbol_table);
    }
    else
    {
            printf("\n\tParsing failed\n");
    }

    fclose(yyin);
    return 0;
}

int yyerror(char *error_msg)
{
    printf("\t Line no: %d \n\t Error message: %s \n ", yylineno, error_msg);
    success_flag = 0;
}

// Checks for Re-declaration of variables

entry_ht* checkMultiDeclarations(char varName[40]){
	if(is_declared)
	{
		if(search(symbol_table, varName) != NULL){		
			printf("Line no: %d \n Error message: %s %s \n ", yylineno, "Variable Already Declared", varName);
			success_flag = 0;
		}

	
	}
	else {
		if(search(symbol_table, varName) == NULL){		
			printf("Line no: %d \n Error message: %s %s \n ", yylineno, "Variable Not Declared", varName);
			success_flag = 0;
		}
	}
	entry_ht* entry= insert(symbol_table, varName, INT_MAX);
	return entry;

}

// Typechecking for assignment and arithmetic expressions

int typecheck (int l, int r, int is_assignmentop){

	if(l!=r){

		yyerror("Type Mismatch");
		success_flag = 0;
		return -1;
	}
	else{
		if(l == 4 && !is_assignmentop){
			yyerror("bool8 doesn't support arithmetic operations");
			success_flag = 0;
		}
	}
	
	return l;
	
}




