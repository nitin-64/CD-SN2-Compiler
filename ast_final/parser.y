%{
    #include "node.h"
    #include <map>
    using namespace std;

    int lineNo  = 1;
    map<string,E_TYPE> varTable;
    void addNewVar(string name,E_TYPE type);
    string typeStr(E_TYPE type);
    void emptyMap();
    void setVarType(IdentifierNode *);
    void printVarTable();
 
    BlockNode *programBlock; /* the top level root node of our final AST */

    extern int yylex();
    bool success_flag = true;
    void yyerror(const char *s) { printf("Line no: %d ERROR: %s\n", lineNo, s); success_flag = false; return ;}
      
%}


%union
{
    Node *node;
    BlockNode *block;
    ExpressionNode *expr;
    std::vector<ExpressionNode*> *exprs; 
    StatementNode *stmt;
    IdentifierNode *ident;
    VariableDeclarationNode *var_dec;
    std::vector<VariableDeclarationNode*> *varvec;
    std::vector<ExpressionNode*> *exprvec;
    std::string *string;
    int token;
}

%token <string> IDENTIFIER INT_CONSTANT FLOAT_CONSTANT CHAR_CONSTANT STR_CONSTANT INT32 FLOAT CHAR BOOL VOID
%token<token> ADD SUB DIV MUL MOD LAND LOR LEQ GEQ EQ NEQ LESSER GREATER 
%token<token> FOR WHILE IF ELSE ELIF TRUE FALSE PRINT CONTINUE BREAK RETURN EXTERN

%type <ident> ident var_type
%type <expr> numeric expr logical_expr
%type <exprs> invoke_args
%type <varvec> func_dec_args
%type <exprvec> call_args
%type <block> program stmts block else_stmt
%type <stmt> stmt var_dec fun_dec if_stmt while_stmt for_stmt elif_stmt extern_decl arr_dec


%left ','
%right '='
%left LOR
%left LAND
%left EQ NEQ
%left LESSER GREATER LEQ GEQ
%left ADD SUB
%left MUL DIV MOD
%right NOT

%start program

%%

program : stmts { programBlock = $1;} 
        ;

stmts : stmt { $$ = new BlockNode(); $$->statements.push_back($<stmt>1); } | 
        stmts stmt { $1->statements.push_back($<stmt>2); } 
        ;

stmt : var_dec ';'
      | arr_dec ';' {$$ = $1 ;}
      | fun_dec     
      | extern_decl ';'
      | expr  ';'     { $$ = new ExpressionStatementNode(*$1); }
      | if_stmt     {$$ = $1;}
      | while_stmt  {$$ = $1;}
      | for_stmt    {$$ = $1;}
      | RETURN ';'  {$$ = new ReturnStatementNode(NULL);}
      | RETURN expr ';' { $$ = new ReturnStatementNode($2); }
      ;

block : '{''}' {  $$ = new BlockNode(); } | 
        '{'stmts'}' { $$ = $2; emptyMap();  }
        ;

var_dec : var_type ident  { $2->_type = $1->_type;  $$ = new VariableDeclarationNode(*$1, *$2); addNewVar($2->name, $2->_type);}| 
          var_type ident '=' expr  { $2->_type = $1->_type; $$ = new VariableDeclarationNode(*$1, *$2, $4);  addNewVar($2->name, $2->_type);  }
        ;


var_type : INT32             { $$ = new IdentifierNode(*$1,E_INT) ; delete $1;}
        | FLOAT                {  $$ = new IdentifierNode(*$1, E_FLOAT) ; delete $1;}
        | CHAR               {  $$ = new IdentifierNode(*$1, E_CHAR) ; delete $1;}
        | VOID               {  $$ = new IdentifierNode(*$1,E_VOID) ; delete $1;}   
        ;

arr_dec : var_type ident '[' INT_CONSTANT ']' { $$ = new ArrayDecNode($1, $2, atol($4->c_str()));}
        | var_type ident '['']' '=' STR_CONSTANT {  $$ = new ArrayDecNode($1, $2, *$6); }
        | var_type ident '['']' '=' '{' invoke_args '}' {  $$ = new ArrayDecNode($1, $2, $7);}
        ;

invoke_args : /*NULL*/ { $$ = new std::vector<ExpressionNode*>(); }
            | expr { $$ = new std::vector<ExpressionNode*>(); $$->push_back($1); }
            | invoke_args ',' expr  { $1->push_back($3); $$ = $1; }
            ;

fun_dec : var_type ident '('func_dec_args')' block { $$ = new FunctionDeclarationNode(*$1, *$2, *$4, *$6); delete $4; }
        ;

extern_decl : EXTERN var_type ident '(' func_dec_args ')'
                {  $$ = new ExternDeclarationNode(*$2, *$3, *$5); delete $5;  }
                ;

func_dec_args :  { $$ = new VariableList(); }
          | var_dec { $$ = new VariableList(); $$->push_back($<var_dec>1); }
          | func_dec_args ',' var_dec { $1->push_back($<var_dec>3); }
          ;

ident : IDENTIFIER { $$ = new IdentifierNode(*$1); setVarType($$); delete $1;  }
      ;

numeric : INT_CONSTANT { $$ = new IntegerNode(atoi($1->c_str()));$$->_type = E_INT; delete $1; }
        | FLOAT_CONSTANT {  $$ = new FloatNode(atof($1->c_str()));$$->_type = E_FLOAT; delete $1;}
        | CHAR_CONSTANT {  $$ = new CharNode($1->c_str()[1]); $$->_type = E_CHAR ;delete $1; }
        ;


expr : ident '=' expr { $$ = new AssignmentNode(*$<ident>1, *$3); setVarType($1); $$->_type = $1->_type;}
     | ident '(' call_args ')' { $$ = new MethodCallNode(*$1, *$3); addNewVar($1->name, E_FUNC); setVarType($1); $$->_type = $1->_type;delete $3; }
     | ident { setVarType($1) ; $$ = $1; }
     | numeric { $$ = $1; }
     | expr ADD expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr SUB expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr MUL expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr DIV expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr MOD expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | logical_expr  { $$ = $1; }
     | '('expr')' { $$ = $2; }
     | ident '[' expr ']' { $$ = new IndexExprNode($1, $3); $$->_type = $1->_type; }
     | ident '[' expr ']' '=' expr { $$ = new IndexExprNode($1, $3, $6); $$->_type = $1->_type; }
     ;


        

logical_expr :  expr LAND expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr LOR expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr LEQ expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr GEQ expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr EQ expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr NEQ expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr LESSER expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     | expr GREATER expr { $$ = new BinaryOperatorNode(*$1, $2, *$3); }
     ;

call_args :  { $$ = new ExpressionList(); }
          | expr { $$ = new ExpressionList(); $$->push_back($1); }
          | call_args ',' expr  { $1->push_back($3); }
          ;

if_stmt:
      IF '(' logical_expr ')' block             {$$ = new IfNode($3, $5);}
    | IF '(' logical_expr ')' block elif_stmt   { }
    | IF '(' logical_expr ')' block else_stmt   {$$ = new IfElseNode($3, $5, $6);}
    ;

elif_stmt:
      ELIF '(' logical_expr ')' block           { }
    | ELIF '(' logical_expr ')' block elif_stmt { }
    | ELIF '(' logical_expr ')' block else_stmt { }
    ;

else_stmt: ELSE block  {$$ = $2;}
          ;

for_stmt: FOR '(' expr ';' logical_expr ';' expr ')' block   {$$ = new ForNode($3,$5,$7,$9);}
        ; 

while_stmt: WHILE '(' logical_expr')' block     {$$ = new WhileNode($3, $5);}
        ;


%%

void emptyMap(){
  varTable.clear();
}

void addNewVar(string name, E_TYPE type) {
  map<string, E_TYPE>::iterator it;
  it = varTable.find(name);
  if (it == varTable.end()) {
    varTable[name] = type;
  } else if (type == E_FUNC) {
    varTable[name] = type;
  } else {
    success_flag = false;
    cout << "line " << lineNo << ": redefinition of variable " << name << " from (" << typeStr((*it).second) << ") to (" << typeStr(type) << ")." << endl;
    varTable[name] = type;
  }
}
string typeStr(E_TYPE type) {
    switch (type) {
    case E_VOID:
        return "void";
    case E_INT:
        return "int";
    case E_CHAR:
        return "char";
    case E_FLOAT:
        return "float";
    case E_FUNC:
        return "function part";
    default:
        return "unknown";
    }
}

void setVarType(IdentifierNode *var){
  map<string, E_TYPE>::iterator it;
  it = varTable.find(var->name);
  if (it == varTable.end()) {
    var->_type = E_UNKNOWN;
  } else {
    var->_type = (*it).second;
  }
}

void printVarTable() {
  std::map<std::string, E_TYPE>::iterator it;
  for (it = varTable.begin(); it != varTable.end(); it++) {
    cout << (*it).first << " : " << typeStr((*it).second) << std::endl;
  }
}

