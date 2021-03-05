%{
    #include <string.h>
    #include <stdio.h>
    #include <limits.h>
    /* initialzing symbol table */
    #include "y.tab.h"
    int cmnt_strt = 0;

    
%}

letter  [a-zA-Z_$]
digit [0-9]
ws [ \t\r\f\v]+
identifier {letter}({letter}|{digit})*|({letter}+\.)+{letter}+
int_token  {digit}+
FLOAT_TOKEN  {digit}+(\.{digit}+)
CHAR_TOKEN   \'.\'
STRING_TOKEN  \".*\"

%%


"int32"  { return INT;}
"dec32"  { return DEC;}   
"char8"  { return CHAR;}
"bool8"  { return BOOL;}
"while"  { return WHILE;}
"for"    { return FOR;}
"if"     { return IF;}
"else"   { return ELSE;}
"elif"   { return ELIF;}
"break"  { return BREAK;}
"continue"  {return CONTINUE;}
"void"   { return VOID;}
"printToConsole"  { return PRINT;}
"return"  { return RETURN;} 
"true"			{yylval.dval = 1; return BOOL_CONSTANT;}
"false"			{yylval.dval = 0; return BOOL_CONSTANT;}
\n 			{yylineno++;}


{identifier} {yylval.entry = insert(symbol_table, yytext, INT_MAX); return  IDENTIFIER;}
{ws}          ;
{int_token}     {yylval.dval = atoi(yytext); return DEC_CONSTANT;}
{FLOAT_TOKEN}   {yylval.fval = atof(yytext); return FLOAT_CONSTANT; }
{CHAR_TOKEN}	{yylval.cval = (yytext[1]); return CHAR_CONSTANT;}

    
"<="     {return LEQ;}
">="     {return GEQ;}
"=="     {return EQ;}
"!="     {return NEQ;}
"!"      {return NOT;}
"&&"     {return LAND;}
"||"     {return LOR;}

"+"      {return ADD;}
"-"      {return SUB;}
"/"      {return DIV;}
"*"      {return MUL;}
"%"      {return MOD;}

"+="     {return ADD_ASSIGN;}
"-="     {return SUB_ASSIGN;}
"/="     {return DIV_ASSIGN;}
"*="     {return MUL_ASSIGN;}
"%="     {return MOD_ASSIGN;}

"<"      {return LESSER;}
">"      {return GREATER;}
"++"     {return INCR;}
"--"     {return DECR;}

.        {return yytext[0];}


%%

