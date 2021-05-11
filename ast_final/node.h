#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
#include <llvm/IR/Value.h>

enum E_TYPE {
    E_UNKNOWN = -1,
    E_VOID = 0,
    E_CHAR,
    E_INT,
    E_FLOAT,
    E_FUNC,
};

class CodeGenContext;
class StatementNode;
class ExpressionNode;
class VariableDeclarationNode;
class IfElseNode;

typedef std::vector<StatementNode*> StatementList;
typedef std::vector<ExpressionNode*> ExpressionList;
typedef std::vector<IfElseNode*> ElifList;
typedef std::vector<VariableDeclarationNode*> VariableList;

class Node {
public:
    virtual ~Node() {}
    virtual llvm::Value* codeGen(CodeGenContext& context) {return NULL;}
};

class ExpressionNode : public Node {
    public:
        E_TYPE _type;
};

class StatementNode : public Node {
};

class IntegerNode : public ExpressionNode {
public:
    int value;
    IntegerNode(int value) : value(value) {
        _type = E_INT;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};


class FloatNode : public ExpressionNode {
public:
    double value;
    FloatNode(double value) : value(value) { 
         _type = E_FLOAT;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};


class CharNode : public ExpressionNode {
public:
    char value;
    CharNode(char value) : value(value) { 
        _type = E_CHAR; 
    }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class IdentifierNode : public ExpressionNode {
public:
    std::string name;
    IdentifierNode(const std::string& name,E_TYPE type = E_UNKNOWN ) : name(name) { 
        _type = type;    
    }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

// a[0] = 3 
class IndexExprNode : public ExpressionNode {
public:
    IdentifierNode *name;
    ExpressionNode *expr;
    ExpressionNode *assign;
public:
    IndexExprNode(IdentifierNode *name, ExpressionNode *expr): name(name), expr(expr), assign(NULL) {}
    IndexExprNode(IdentifierNode *name, ExpressionNode *expr, ExpressionNode *assign): name(name), expr(expr), assign(assign) {}
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class MethodCallNode : public ExpressionNode {
public:
    const IdentifierNode& id;
    ExpressionList arguments;
    MethodCallNode(const IdentifierNode& id, ExpressionList& arguments) :
        id(id), arguments(arguments) { }
    MethodCallNode(const IdentifierNode& id) : id(id) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class BinaryOperatorNode : public ExpressionNode {
public:
    int op;
    ExpressionNode& lhs;
    ExpressionNode& rhs;
    BinaryOperatorNode(ExpressionNode& lhs, int op, ExpressionNode& rhs) :
        lhs(lhs), rhs(rhs), op(op) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class AssignmentNode : public ExpressionNode {
public:
    IdentifierNode& lhs;
    ExpressionNode& rhs;
    AssignmentNode(IdentifierNode& lhs, ExpressionNode& rhs) : 
        lhs(lhs), rhs(rhs) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class BlockNode : public ExpressionNode {
public:
    StatementList statements;
    BlockNode() { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class ExpressionStatementNode : public StatementNode {
public:
    ExpressionNode& expression;
    ExpressionStatementNode(ExpressionNode& expression) : 
        expression(expression) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};


class VariableDeclarationNode : public StatementNode {
public:
    const IdentifierNode& type;
    IdentifierNode& id;
    ExpressionNode *assignmentExpr = NULL;
    VariableDeclarationNode(const IdentifierNode& type, IdentifierNode& id) :
        type(type), id(id) { }
    VariableDeclarationNode(const IdentifierNode& type, IdentifierNode& id, ExpressionNode *assignmentExpr) :
        type(type), id(id), assignmentExpr(assignmentExpr) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class FunctionDeclarationNode : public StatementNode {
public:
    const IdentifierNode& type;
    const IdentifierNode& id;
    VariableList arguments;
    BlockNode& block;
    FunctionDeclarationNode(const IdentifierNode& type, const IdentifierNode& id, 
            const VariableList& arguments, BlockNode& block) :
        type(type), id(id), arguments(arguments), block(block) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class ReturnStatementNode : public StatementNode {
public:
    ExpressionNode *expr;
public:
    ReturnStatementNode(ExpressionNode *expr): expr(expr) {}
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class IfElseNode : public StatementNode {
public:
    ExpressionNode *assignmentExpr;
    BlockNode *ifBlock;
    BlockNode *elseBlock;
    
    IfElseNode(ExpressionNode *assignmentExpr,BlockNode *ifBlock,BlockNode *elseBlock) : 
        assignmentExpr(assignmentExpr), ifBlock(ifBlock), elseBlock(elseBlock) { }

    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class IfNode : public StatementNode {
    public:
        ExpressionNode *assignmentExpr;
        BlockNode *ifBlock;
    IfNode(ExpressionNode *assignmentExpr,BlockNode *ifBlock) : 
        assignmentExpr(assignmentExpr), ifBlock(ifBlock) { }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class ForNode : public StatementNode {
public:
    ExpressionNode *initExpr;
    ExpressionNode *condExpr;
    ExpressionNode *loopExpr;
    BlockNode *block;
public:
    ForNode(ExpressionNode *initExpr, ExpressionNode *condExpr, ExpressionNode *loopExpr, BlockNode *block): initExpr(initExpr), condExpr(condExpr), loopExpr(loopExpr), block(block) {}
    virtual llvm ::Value* codeGen(CodeGenContext& context) ;
};

class WhileNode : public StatementNode {
public:
    ExpressionNode *whileExpr;
    BlockNode *block;
public:
    WhileNode(ExpressionNode *whileExpr, BlockNode *block): whileExpr(whileExpr), block(block) {}
    virtual llvm ::Value* codeGen(CodeGenContext& context) ;
};


class ExternDeclarationNode : public StatementNode {
public:
    const IdentifierNode& type;
    const IdentifierNode& id;
    VariableList arguments;
    ExternDeclarationNode(const IdentifierNode& type, const IdentifierNode& id,
            const VariableList& arguments) :
        type(type), id(id), arguments(arguments) {}
    virtual llvm::Value* codeGen(CodeGenContext& context);
};

class ArrayDecNode : public StatementNode {
public:
    IdentifierNode *type;
    IdentifierNode *name;
    ExpressionList *init;
    long long size;
    bool isString;
public:
    ArrayDecNode(IdentifierNode *type, IdentifierNode *name, long long size): type(type), name(name), init(new ExpressionList()), size(size), isString(false) {}
    ArrayDecNode(IdentifierNode *type, IdentifierNode *name, ExpressionList *init): type(type), name(name), init(init), size(init->size()), isString(false) {}
    ArrayDecNode(IdentifierNode *type, IdentifierNode *name, const std::string &str): type(type), name(name), init(new ExpressionList()), isString(true) {
        for(auto it = str.begin(); it != str.end(); it++)
            init->push_back((ExpressionNode*)(new CharNode(*it)));
        init->push_back((ExpressionNode*)(new CharNode(0)));
        size = init->size() + 1;
    }
    virtual llvm::Value* codeGen(CodeGenContext& context);
};