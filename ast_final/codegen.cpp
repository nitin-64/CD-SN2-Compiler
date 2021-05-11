#include "node.h"
#include "codegen.h"
#include "parser.hpp"
#include <fstream>

using namespace std;
static Value* cast(Value* value, Type* type, CodeGenContext& context) {
    if (type == value->getType())
        return value;
    if (type == Type::getDoubleTy(MyContext)) {
        if (value->getType() == Type::getInt64Ty(MyContext) || value->getType() == Type::getInt8Ty(MyContext))
            value = new SIToFPInst(value, type, "", context.currentBlock());
        else
            cout << "Cannot cast this value.\n";
    }
    else if (type == Type::getInt64Ty(MyContext)) {
        if (value->getType() == Type::getDoubleTy(MyContext))
            value = new FPToSIInst(value, type, "", context.currentBlock());
        else if (value->getType() == Type::getInt8Ty(MyContext))
            value = new SExtInst(value, type, "", context.currentBlock());
        else if (value->getType() == Type::getInt32Ty(MyContext))
            value = new ZExtInst(value, type, "", context.currentBlock());
        else if (value->getType() == Type::getInt8PtrTy(MyContext))
            value = new PtrToIntInst(value, type, "", context.currentBlock());
        else if (value->getType() == Type::getInt64PtrTy(MyContext))
            value = new PtrToIntInst(value, type, "", context.currentBlock());
        else
            cout << "Cannot cast this value.\n";
    } else if (type == Type::getInt8Ty(MyContext)) {
        if (value->getType() == Type::getDoubleTy(MyContext))
            value = new FPToSIInst(value, type, "", context.currentBlock());
        else if (value->getType() == Type::getInt64Ty(MyContext))
            value = new TruncInst(value, type, "", context.currentBlock());
        else
            cout << "Cannot cast this value.\n";
    } else
        cout << "Cannot cast this value.\n";
    return value;
}

Function* printfFloat(CodeGenContext& context) {
    vector<Type*> printfArgs;
    printfArgs.push_back(Type::getInt8PtrTy(MyContext));
	FunctionType* printfType = FunctionType::get(Type::getDoubleTy(MyContext), printfArgs, true);
    Function *printfFunc = Function::Create(printfType, Function::ExternalLinkage, Twine("printfFloat"), context.module);
    printfFunc->setCallingConv(CallingConv::C);
    return printfFunc;
}

Function* printfInt(CodeGenContext& context) {
    vector<Type*> printfArgs;
    printfArgs.push_back(Type::getInt8PtrTy(MyContext));
    FunctionType* printfType = FunctionType::get(Type::getInt32Ty(MyContext), printfArgs, true);
    Function *printfFunc = Function::Create(printfType, Function::ExternalLinkage, Twine("printfInt"), context.module);
    printfFunc->setCallingConv(CallingConv::C);
    return printfFunc;
}

void linkExternalFunctions(CodeGenContext& context) {
    printfFloat(context);
	printfInt(context);
}


/* Compile the AST into a module */
void CodeGenContext::generateCode(BlockNode& root)
{
	
	/* Create the top level interpreter function to call as entry */
	vector<Type*> argTypes;
	FunctionType *ftype = FunctionType::get(Type::getVoidTy(MyContext), makeArrayRef(argTypes), false);
	mainFunction = Function::Create(ftype, GlobalValue::InternalLinkage, "main", module);
	BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", mainFunction, 0);
	
	/* Push a new variable/block context */
	pushBlock(bblock);
	root.codeGen(*this); /* emit bytecode for the toplevel block */
	ReturnInst::Create(MyContext,bblock);
	popBlock();
	
	legacy::PassManager pm;
    raw_string_ostream *out = new raw_string_ostream(code);
    pm.add(createPrintModulePass(*out));
	pm.run(*module);

	std::ofstream outfile("output.ll");
    outfile << code;
    outfile.close();
}

/* Executes the AST by running the main function */
GenericValue CodeGenContext::runCode() {
	ExecutionEngine *ee = EngineBuilder( unique_ptr<Module>(module) ).create();
	ee->finalizeObject();
	vector<GenericValue> noargs;
	GenericValue v = ee->runFunction(mainFunction, noargs);
	return v;
}

/* Returns an LLVM type based on the identifier */
static Type *typeOf(const IdentifierNode& type) 
{
	if (type.name.compare("int32") == 0) {
		return Type::getInt64Ty(MyContext);
	}
	else if (type.name.compare("float32") == 0) {
		return Type::getDoubleTy(MyContext);
	}
	else if (type.name.compare("char8") == 0) {
		return Type::getInt8Ty(MyContext);
	}
	return Type::getVoidTy(MyContext);
}

/* -- Code Generation -- */

Value* IntegerNode::codeGen(CodeGenContext& context)
{
	return ConstantInt::get(Type::getInt64Ty(MyContext), value, true);
}


Value* CharNode::codeGen(CodeGenContext& context) {
    return ConstantInt::get(Type::getInt8Ty(MyContext), value, true);
}

Value* FloatNode::codeGen(CodeGenContext& context) {
    return ConstantFP::get(Type::getDoubleTy(MyContext), value);
}

Value* IndexExprNode::codeGen(CodeGenContext& context) {
    Value* array = name->codeGen(context);

    Value* num = cast(expr->codeGen(context), Type::getInt64Ty(MyContext), context);
    
    num = new TruncInst(num, Type::getInt32Ty(MyContext), "", context.currentBlock());
    Type* arrayType = cast<PointerType>(array->getType()->getScalarType())->getElementType();
    Instruction* instr;
    Value* retInst;
    instr = GetElementPtrInst::Create(arrayType, array, num, "", context.currentBlock());
    
    // whether read or write
    if (assign == NULL)
        retInst = new LoadInst(instr, "", false, context.currentBlock());
    else
        retInst = new StoreInst(assign->codeGen(context), instr, false, context.currentBlock());
    return retInst;

}


Value* IdentifierNode::codeGen(CodeGenContext& context)
{
	std::stack<CodeGenBlock*> blocksCpy = context.blocks;
	while(!blocksCpy.empty()){
		CodeGenBlock *block = blocksCpy.top();
		if(block->locals.find(name) != block->locals.end()){
			Value* val = block->locals[name];
				if (((AllocaInst*)val)->getAllocatedType()->isArrayTy()) {
			         ConstantInt* constInt = ConstantInt::get(MyContext, APInt(32, StringRef("0"), 10));
			         vector<Value*> args;
			         args.push_back(constInt);
			         args.push_back(constInt);
			         Type* type;
			         type = ((AllocaInst*)val)->getAllocatedType();
			         val = GetElementPtrInst::Create(type, val, args, "", context.currentBlock());
			         return val;
			     }
			    else
					return new LoadInst(val, "", false, context.currentBlock());
		}
		blocksCpy.pop();
	}
	std::cout << "undeclared variable " << name << endl;
	return NULL;
}

Value* MethodCallNode::codeGen(CodeGenContext& context)
{
	Function *function = context.module->getFunction(id.name.c_str());
	if (function == NULL) {
		std::cout << "no such function " << id.name << endl;
	}
	std::vector<Value*> args;
	ExpressionList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		args.push_back((**it).codeGen(context));
	}
	CallInst *call = CallInst::Create(function, makeArrayRef(args), "", context.currentBlock());
	return call;
}

Value* BinaryOperatorNode::codeGen(CodeGenContext& context)
{
	Instruction::BinaryOps instr;
	ICmpInst::Predicate pred;
	
	Value* leftVal = lhs.codeGen(context);
    Value* rightVal = rhs.codeGen(context);
	bool floatOp = false;

    if (leftVal->getType()->isDoubleTy() || rightVal->getType()->isDoubleTy()) {
        leftVal = cast(leftVal, Type::getDoubleTy(MyContext), context);
        rightVal = cast(rightVal, Type::getDoubleTy(MyContext), context);
        floatOp = true;
    } else if (leftVal->getType() != rightVal->getType()) {
        leftVal = cast(leftVal, Type::getInt64Ty(MyContext), context);
        rightVal = cast(rightVal, Type::getInt64Ty(MyContext), context);
    }

	if(floatOp){
	switch (op) {
		case ADD: 		instr = Instruction::FAdd; goto math;
		case SUB: 		instr = Instruction::FSub; goto math;
		case MUL: 		instr = Instruction::FMul; goto math;
		case DIV: 		instr = Instruction::FDiv; goto math;
		
	}
	}
	else{
	switch (op) {
		case ADD: 		instr = Instruction::Add; goto math;
		case SUB: 		instr = Instruction::Sub; goto math;
		case MUL: 		instr = Instruction::Mul; goto math;
		case DIV: 		instr = Instruction::SDiv; goto math;
		case LAND:		instr = Instruction::And; goto math;
		case LOR:		instr = Instruction::Or; goto math;

		case EQ:		pred = ICmpInst::ICMP_EQ; goto logical;
		case LEQ:		pred = ICmpInst::ICMP_SLE; goto logical;
		case GEQ:		pred = ICmpInst::ICMP_SGE; goto logical;
		case NEQ:		pred = ICmpInst::ICMP_NE; goto logical;
		case GREATER:	pred = ICmpInst::ICMP_SGT; goto logical;
		case LESSER:    pred = ICmpInst::ICMP_SLT; goto logical;
		
	}
	}

	return NULL;
math:
	return BinaryOperator::Create(instr, leftVal, 
		rightVal, "", context.currentBlock());
logical:
	return new ICmpInst(*context.currentBlock(), pred, leftVal, rightVal, "");
}

Value* AssignmentNode::codeGen(CodeGenContext& context)
{
	std::stack<CodeGenBlock*> blocksCpy = context.blocks;
	while(!blocksCpy.empty()){
		CodeGenBlock *block = blocksCpy.top();
		if(block->locals.find(lhs.name) != block->locals.end()){
			Value* leftVal = lhs.codeGen(context);
    		Value* rightVal = rhs.codeGen(context);

    	if (leftVal->getType()->isDoubleTy() || rightVal->getType()->isDoubleTy()) {
        	leftVal = cast(leftVal, Type::getDoubleTy(MyContext), context);
        	rightVal = cast(rightVal, Type::getDoubleTy(MyContext), context);
    	}
			return new StoreInst(rhs.codeGen(context), block->locals[lhs.name], false, context.currentBlock());
		}
		blocksCpy.pop();
	}
	std::cerr << "undeclared variable " << lhs.name << endl;
	return NULL;
	
}

Value* BlockNode::codeGen(CodeGenContext& context)
{
	StatementList::const_iterator it;
	Value *last = NULL;
	for (it = statements.begin(); it != statements.end(); it++) {
		last = (**it).codeGen(context);
	}
	return last;
}

Value* ExpressionStatementNode::codeGen(CodeGenContext& context)
{
	return expression.codeGen(context);
}

Value* VariableDeclarationNode::codeGen(CodeGenContext& context)
{
	
	AllocaInst *alloc = new AllocaInst(typeOf(type), 4, id.name.c_str(), context.currentBlock());
	context.locals()[id.name] = alloc;
	if (assignmentExpr != NULL) {
		AssignmentNode assn(id, *assignmentExpr);
		assn.codeGen(context);
	}
	return alloc;
}

Value* FunctionDeclarationNode::codeGen(CodeGenContext& context)
{
	vector<Type*> argTypes;
	VariableList::const_iterator it;
	for (it = arguments.begin(); it != arguments.end(); it++) {
		argTypes.push_back(typeOf((**it).type));
	}
	FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
	Function *function = Function::Create(ftype, GlobalValue::InternalLinkage, id.name.c_str(), context.module);
	BasicBlock *bblock = BasicBlock::Create(MyContext, "entry", function, 0);

	context.pushBlock(bblock);
	context.currentFunction(function);

	Function::arg_iterator argsValues = function->arg_begin();
    Value* argumentValue;

	for (it = arguments.begin(); it != arguments.end(); it++) {
		(**it).codeGen(context);
		
		argumentValue = &*argsValues++;
		argumentValue->setName((*it)->id.name.c_str());
		StoreInst *inst = new StoreInst(argumentValue, context.locals()[(*it)->id.name], false, bblock);
	}
	
	block.codeGen(context);
	ReturnInst::Create(MyContext, context.getCurrentReturnValue(), bblock);

	context.popBlock();
	return function;
}

Value* ReturnStatementNode::codeGen(CodeGenContext& context) {
	if(expr == NULL){
		ReturnInst::Create(MyContext, context.currentBlock());
		return NULL;
	}
	Value* retVal = expr->codeGen(context);
	ReturnInst::Create(MyContext, retVal, context.currentBlock());
    return retVal;
}

Value* IfElseNode::codeGen(CodeGenContext &context){

	BasicBlock* ifTrue = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BasicBlock* ifFalse = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BasicBlock* ifEnd = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BranchInst::Create(ifTrue, ifFalse, assignmentExpr->codeGen(context), context.currentBlock());
    // Entering IF
    context.pushBlock(ifTrue);
    ifBlock->codeGen(context);
    // JMP to END
    BranchInst::Create(ifEnd, context.currentBlock());
    context.popBlock();
    // Entering ELSE
    context.pushBlock(ifFalse);
    elseBlock->codeGen(context);
    // JMP to END
    BranchInst::Create(ifEnd, context.currentBlock());
    context.popBlock();
    // Return END
    context.ret(ifEnd);
    return ifEnd;

}

Value* IfNode::codeGen(CodeGenContext &context){
	BasicBlock* ifTrue = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BasicBlock* ifEnd = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BranchInst::Create(ifTrue, ifEnd, assignmentExpr->codeGen(context), context.currentBlock());

	// Entering IF
    context.pushBlock(ifTrue);
    ifBlock->codeGen(context);
    // JMP to END
    BranchInst::Create(ifEnd, context.currentBlock());
    context.popBlock();

	context.ret(ifEnd);
    return ifEnd;
}

Value* ForNode::codeGen(CodeGenContext& context) {
    // Initialize
    initExpr->codeGen(context);
    BasicBlock* forIter = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BasicBlock* forEnd = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BasicBlock* forCheck = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    
	// Check condition satisfaction
    BranchInst::Create(forCheck, context.currentBlock());
    context.pushBlock(forCheck);
    
	// Whether break the loop
    BranchInst::Create(forIter, forEnd, condExpr->codeGen(context), forCheck);
    context.popBlock();
	
    // Entering loop block
    context.pushBlock(forIter);
    block->codeGen(context);
	
    // Iteration
    loopExpr->codeGen(context);
    // Jump back to condition checking
    BranchInst::Create(forCheck, context.currentBlock());
    context.popBlock();
    // Return END
    context.ret(forEnd);
    return forEnd;
}

Value* WhileNode::codeGen(CodeGenContext& context) {
    BasicBlock* whileIter = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BasicBlock* whileEnd = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    BasicBlock* whileCheck = BasicBlock::Create(MyContext, "", context.currentFunction(), 0);
    // Check condition satisfaction
    BranchInst::Create(whileCheck, context.currentBlock());
    context.pushBlock(whileCheck);
    // Whether break the loop
    BranchInst::Create(whileIter, whileEnd, whileExpr->codeGen(context), context.currentBlock());
    context.popBlock();
    // Entering loop block
    context.pushBlock(whileIter);
    block->codeGen(context);
    // Jump back to condition checking
    BranchInst::Create(whileCheck, context.currentBlock());
    context.popBlock();
    // Return END
    context.ret(whileEnd);
    return whileEnd;
}

Value* ExternDeclarationNode::codeGen(CodeGenContext& context)
{
    vector<Type*> argTypes;
    VariableList::const_iterator it;
    for (it = arguments.begin(); it != arguments.end(); it++) {
        argTypes.push_back(typeOf((**it).type));
    }
    FunctionType *ftype = FunctionType::get(typeOf(type), makeArrayRef(argTypes), false);
    Function *function = Function::Create(ftype, GlobalValue::ExternalLinkage, id.name.c_str(), context.module);
    return function;
}

Value* ArrayDecNode::codeGen(CodeGenContext& context) {
    ArrayType* arrayType = ArrayType::get(typeOf(*type), size);
    AllocaInst *alloc = new AllocaInst(arrayType,size*4, name->name.c_str(), context.currentBlock());
    context.locals()[name->name] = alloc;
    if (init->size() != 0) {
        for (auto it = init->begin(); it != init->end(); ++it) {
            ExpressionNode* num = new IntegerNode(it - init->begin());
            IndexExprNode* a = new IndexExprNode(name, num, (*it));

            a->codeGen(context);
            delete num;
        }
    }
    return alloc;
}

