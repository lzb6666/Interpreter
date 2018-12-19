#include"AST.h"
using namespace std;
Value *NumberExprAST::codegen() {
	return ConstantFP::get(TheContext, APFloat(numVal));
}
Value *VariableExprAST::codegen() {
	// Look this variable up in the function.
	Value *V = NamedValues[Name];
	if (!V)
		LogErrorV("Unknown variable name");
	return V;
}
Value *BinaryExprAST::codegen() {
	Value *L = LHS->codegen();
	Value *R = RHS->codegen();
	if (!L || !R)
		return nullptr;

	switch (Op) {
	case '+':
		return Builder.CreateFAdd(L, R, "addtmp");
	case '-':
		return Builder.CreateFSub(L, R, "subtmp");
	case '*':
		return Builder.CreateFMul(L, R, "multmp");
	case '<':
		L = Builder.CreateFCmpULT(L, R, "cmptmp");
		// Convert bool 0/1 to double 0.0 or 1.0
		return Builder.CreateUIToFP(L, Type::getDoubleTy(TheContext),
			"booltmp");
	default:
		return LogErrorV("invalid binary operator");
	}
}
Value *CallExprAST::codegen() {
	// Look up the name in the global module table.
	Function *CalleeF = TheModule->getFunction(callee);
	if (!CalleeF)
		return LogErrorV("Unknown function referenced");

	// If argument mismatch error.
	if (CalleeF->arg_size() != Args.size())
		return LogErrorV("Incorrect # arguments passed");

	std::vector<Value *> ArgsV;
	for (unsigned i = 0, e = Args.size(); i != e; ++i) {
		ArgsV.push_back(Args[i]->codegen());
		if (!ArgsV.back())
			return nullptr;
	}

	return Builder.CreateCall(CalleeF, ArgsV, "calltmp");
}
Value *IfExprAST::codegen() {
	Value *CondV = Cond->codegen();
	if (!CondV)
		return nullptr;
	//��cond��0�Ƚ�
	CondV = Builder.CreateFCmpONE(
		CondV, ConstantFP::get(TheContext, APFloat(0.0)), "ifcond");
	//��ȡ��ǰ����
	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	//��������������
	//���캯������TheFunction���룬���ֱ�ӽ�then����뵱ǰ���캯��
	BasicBlock *ThenBB =
		BasicBlock::Create(TheContext, "then", TheFunction);
	//�������
	BasicBlock *ElseBB = BasicBlock::Create(TheContext, "else");
	BasicBlock *MergeBB = BasicBlock::Create(TheContext, "ifcont");

	Builder.CreateCondBr(CondV, ThenBB, ElseBB);
	// Emit then value.
	Builder.SetInsertPoint(ThenBB);

	Value *ThenV = Then->codegen();
	if (!ThenV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// Codegen of 'Then' can change the current block, update ThenBB for the PHI.
	ThenBB = Builder.GetInsertBlock();
	// Emit else block.
	TheFunction->getBasicBlockList().push_back(ElseBB);
	Builder.SetInsertPoint(ElseBB);

	Value *ElseV = Else->codegen();
	if (!ElseV)
		return nullptr;

	Builder.CreateBr(MergeBB);
	// codegen of 'Else' can change the current block, update ElseBB for the PHI.
	ElseBB = Builder.GetInsertBlock();
	// Emit merge block.
	TheFunction->getBasicBlockList().push_back(MergeBB);
	Builder.SetInsertPoint(MergeBB);
	PHINode *PN =
		Builder.CreatePHI(Type::getDoubleTy(TheContext), 2, "iftmp");
	//��else��then�Ŀ�ֵ�����������Ȼ�������תѡ�񷵻��ĸ�ֵ
	PN->addIncoming(ThenV, ThenBB);
	PN->addIncoming(ElseV, ElseBB);
	return PN;
}
Value *WhileExprAST::codegen() {
	Value * Cond = While->codegen();
	if (!Cond)
		return nullptr;

	Function *TheFunction = Builder.GetInsertBlock()->getParent();
	BasicBlock *PreheaderBB = Builder.GetInsertBlock();
	BasicBlock *WhileBB =
		BasicBlock::Create(TheContext, "while", TheFunction);
	Builder.CreateBr(WhileBB);
	Builder.SetInsertPoint(WhileBB);
	PHINode * Variable = Builder.CreatePHI(Type::getDoubleTy(TheContext),
		2, "while");
	Variable->addIncoming(Cond, PreheaderBB);

	BasicBlock *AfterBB =
		BasicBlock::Create(TheContext, "afterloop", TheFunction);
	Cond = Builder.CreateFCmpONE(
		Cond, ConstantFP::get(TheContext, APFloat(0.0)), "loopcond");
	Builder.CreateCondBr(Cond, WhileBB, AfterBB);
	Builder.SetInsertPoint(AfterBB);
	return Constant::getNullValue(Type::getDoubleTy(TheContext));
}
Function *PrototypeAST::codegen() {
	// Make the function type:  double(double,double) etc.
	std::vector<Type*> Doubles(Args.size(),
		Type::getDoubleTy(TheContext));
	//get��ȡһ����������
	FunctionType *FT =
		FunctionType::get(Type::getDoubleTy(TheContext), Doubles, false);

	Function *F =
		Function::Create(FT, Function::ExternalLinkage, Name, TheModule.get());
	// Set names for all arguments.
	unsigned Idx = 0;
	for (auto &Arg : F->args())
		Arg.setName(Args[Idx++]);

	return F;
}
Function *FunctionAST::codegen() {
	// First, check for an existing function from a previous 'extern' declaration.
	Function *TheFunction = TheModule->getFunction(Proto->getName());

	if (!TheFunction)
		TheFunction = Proto->codegen();

	if (!TheFunction)
		return nullptr;

	if (!TheFunction->empty())
		return (Function*)LogErrorV("Function cannot be redefined.");
	// Create a new basic block to start insertion into.
	BasicBlock *BB = BasicBlock::Create(TheContext, "entry", TheFunction);
	Builder.SetInsertPoint(BB);


	// Record the function arguments in the NamedValues map.
	NamedValues.clear();
	for (auto &Arg : TheFunction->args())
		NamedValues[Arg.getName()] = &Arg;

	if (Value *RetVal = Return->codegen()) {
		// Finish off the function.
		Builder.CreateRet(RetVal);

		// Validate the generated code, checking for consistency.
		verifyFunction(*TheFunction);

		return TheFunction;
	}
	// Error reading body, remove function.
	TheFunction->eraseFromParent();
	return nullptr;
}
Value *VarExprAST::codegen() {
	NamedValues[Name] = value->codegen();
	return NamedValues[Name];
}