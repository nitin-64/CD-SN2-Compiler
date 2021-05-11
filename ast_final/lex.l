%{
    #include <string>
    #include "node.h"
    #include "parser.hpp"
    #define SAVE_TOKEN yylval.string = new std::string(yytext, yyleng)
    #define TOKEN(t) (yylval.token = t)
    extern int lineNo;    
%}

letter  [a-zA-Z_$]
digit [0-9]
ws [ \t\r\f\v]+
identifier {letter}({letter}|{digit})*|({letter}+\.)+{letter}+
INT_TOKEN {digit}+
FLOAT_TOKEN  {digit}+(\.{digit}+)
CHAR_TOKEN   \'.\'
STRING_TOKEN  \".*\"

%%


"int32"  { SAVE_TOKEN; return INT32;}
"float32"  { SAVE_TOKEN; return FLOAT;}   
"char8"  { SAVE_TOKEN; return CHAR;}
"bool8"  { SAVE_TOKEN; return BOOL;}
"while"  { return TOKEN(WHILE);}
"for"    { return TOKEN(FOR);}
"if"     { return TOKEN(IF);}
"else"   { return TOKEN(ELSE);}
"elif"   { return TOKEN(ELIF);}
"break"  { return TOKEN(BREAK);}
"continue"  {return TOKEN(CONTINUE);}
"void"   { SAVE_TOKEN; return VOID;}
"return"  { return TOKEN(RETURN);} 
"true"			{return TOKEN(TRUE);}
"false"			{return TOKEN(FALSE);}
"extern"        {return TOKEN(EXTERN);}
"\n" 			{lineNo++;}

{identifier} { SAVE_TOKEN ; return  IDENTIFIER;}
{ws}          ;
{INT_TOKEN}     {SAVE_TOKEN; return INT_CONSTANT;}
{FLOAT_TOKEN}   {SAVE_TOKEN;return FLOAT_CONSTANT; }
{CHAR_TOKEN}	{SAVE_TOKEN; return CHAR_CONSTANT;}
["].*["]        {
                yylval.string = new std::string(yytext, yyleng);
                yylval.string->erase(yylval.string->begin());
                yylval.string->erase(yylval.string->end() - 1);
                return STR_CONSTANT;
                }

$$(.*)       ;
    
"<="     {return TOKEN(LEQ);}
">="     {return TOKEN(GEQ);}
"=="     {return TOKEN(EQ);}
"!="     {return TOKEN(NEQ);}
"!"      {return TOKEN(NOT);}
"&&"     {return TOKEN(LAND);}
"||"     {return TOKEN(LOR);}

"+"      {return TOKEN(ADD);}
"-"      {return TOKEN(SUB);}
"/"      {return TOKEN(DIV);}
"*"      {return TOKEN(MUL);}
"%"      {return TOKEN(MOD);}

"<"      {return TOKEN(LESSER);}
">"      {return TOKEN(GREATER);}

.        {return yytext[0];}


%%

int yywrap()
{
return 1;
}

