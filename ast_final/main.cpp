#include <iostream>
#include "node.h"
#include "codegen.h"
extern BlockNode* programBlock;
extern int yyparse();
extern bool success_flag;
extern FILE *yyin;
extern void linkExternalFunctions(CodeGenContext &context);

int main(int argc, char **argv)
{
	yyin = fopen(argv[1], "r");
    yyparse();
    if(success_flag){
        CodeGenContext context;
        linkExternalFunctions(context);
        context.generateCode(*programBlock);
        context.runCode();
    }
    
    return 0;
}
