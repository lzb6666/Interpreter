#pragma once
#include"parser.h"
#include"log.h"
//解析函数定义
std::unique_ptr<PrototypeAST> ParsePrototype() {
	Log("ParsePrototype");
	std::string FnName;

	unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary.
	unsigned BinaryPrecedence = 30;

	switch (CurTok) {
	default:
		return LogErrorP("Expected function name in prototype");
	case tok_identifier://函数定义
		FnName = identifierStr;
		Kind = 0;
		getNextToken();
		break;
		//case tok_binary://二元运算符
		//	getNextToken();
		//	if (!isascii(CurTok))
		//		return LogErrorP("Expected binary operator");
		//	FnName = "binary";
		//	FnName += (char)CurTok;
		//	Kind = 2;
		//	getNextToken();

		//	// Read the precedence if present.
		//	if (CurTok == tok_number) {
		//		if (numVal < 1 || numVal > 100)
		//			return LogErrorP("Invalid precedecnce: must be 1..100");
		//		BinaryPrecedence = (unsigned)numVal;
		//		getNextToken();
		//	}
		//	break;
	}

	if (CurTok != '(')
		return LogErrorP("Expected '(' in prototype");

	std::vector<std::string> ArgNames;
	while (getNextToken() == tok_identifier)
		ArgNames.push_back(identifierStr);
	if (CurTok != ')')
		return LogErrorP("Expected ')' in prototype");

	// 参数读取完毕
	getNextToken(); // eat ')'.

					// Verify right number of names for operator.
	if (Kind && ArgNames.size() != Kind)
		return LogErrorP("Invalid number of operands for operator");

	return llvm::make_unique<PrototypeAST>(FnName, ArgNames, Kind != 0,
		BinaryPrecedence);
}
std::unique_ptr<ExprAST> ParseNumberExpr() {
	Log("ParseNumberExpr");
	auto Result = llvm::make_unique<NumberExprAST>(numVal);//numVal在获取tok_num时已经被赋值
	getNextToken();
	return std::move(Result);
}
std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
	Log("ParseBinOpRHS");
	// 二元运算符获取优先级
	while (true) {
		int TokPrec = GetTokPrecedence();

		//与传入的当前优先级比较，优先级更高就继续执行该优先级，否则返回
		if (TokPrec < ExprPrec)
			return LHS;


		int BinOp = CurTok;
		getNextToken(); // eat二元运算符


		auto RHS = ParsePrimary();//获取右操作数
		if (!RHS)
			return nullptr;

		//如果当前操作符运算级低于右边的操作符，那就把这个右操作数当作左操作数迭代此函数
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}

		// 使用左右操作数构成二元抽象树
		LHS =
			llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
	}
}
//对表达式的解析
std::unique_ptr<ExprAST> ParseExpression() {
	Log("ParseExpression");
	auto LHS = ParsePrimary();//获得左运算符
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));
}

std::unique_ptr<ExprAST> ParseParenExpr() {
	Log("ParseParenExpr");
	getNextToken(); // eat (.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return LogError("expected ')'");
	getNextToken(); // eat ).
	return V;
}

//处理变量引用和函数调用
std::unique_ptr<ExprAST> ParseIdentifierExpr() {
	Log("ParseIdentifierExpr");
	std::string IdName = identifierStr;

	getNextToken(); // 

	if (CurTok != '(') // Simple variable ref.
		return llvm::make_unique<VariableExprAST>(IdName);
	Log("Func Call");
	//函数调用
	getNextToken(); // eat (
	std::vector<std::unique_ptr<ExprAST>> Args;
	if (CurTok != ')') {
		while (true) {
			if (auto Arg = ParseExpression())
				Args.push_back(std::move(Arg));
			else
				return nullptr;

			if (CurTok == ')')
				break;

			if (CurTok != ',')
				return LogError("Expected ')' or ',' in argument list");
			getNextToken();
		}
	}

	// Eat the ')'.
	getNextToken();

	return llvm::make_unique<CallExprAST>(IdName, std::move(Args));
}

//static std::unique_ptr<ExprAST> ParseUnary() {
//	// If the current token is not an operator, it must be a primary expr.
//	if (!isascii(CurTok) || CurTok == '(' || CurTok == ',')
//		return ParsePrimary();
//
//	// If this is a unary operator, read it.
//	int Opc = CurTok;
//	getNextToken();
//	if (auto Operand = ParseUnary())
//		return llvm::make_unique<UnaryExprAST>(Opc, std::move(Operand));
//	return nullptr;
//}
std::unique_ptr<ExprAST> ParseIfExpr() {
	Log("ParseIfExpr");
	getNextToken();   //eat 'if'
	auto Cond = ParseExpression();//解析if条件
	if (!Cond)
		return nullptr;
	if (CurTok != tok_then)
		return LogError("expected then");
	getNextToken(); // eat then

	auto Then = ParseExpression();//解析then下的代码
	if (!Then)
		return nullptr;

	if (CurTok != tok_else)
		return LogError("expected else");
	getNextToken();

	auto Else = ParseExpression();//解析else下的代码
	if (!Else)
		return nullptr;
	if (CurTok != tok_fi)
	{
		return LogError("expected fi");
	}
	getNextToken();
	return llvm::make_unique<IfExprAST>(std::move(Cond), std::move(Then),
		std::move(Else));
}

//static std::unique_ptr<ExprAST> ParseVarExpr() {
//	getNextToken();
//	return llvm::make_unique<VariableExprAST>(CurTok);
//}

std::unique_ptr<ExprAST> ParseWhileExpr() {
	Log("ParseWhileExpr");
	getNextToken();  //eat 'while'
	auto While = ParseExpression();
	if (!While)
	{
		return nullptr;
	}

	if (CurTok != tok_do)
	{
		return LogError("while缺少do");
	}
	getNextToken();
	if (CurTok != '{')
	{
		return LogError("while缺少{");//目前强制要求{}
	}
	getNextToken();
	auto Do = ParseExpression();
	if (!Do)
	{
		return nullptr;
	}

	if (CurTok != '}')
	{
		return LogError("while缺少}");
	}
	getNextToken();
	if (CurTok != tok_done)
	{
		return LogError("while缺少done");
	}
	getNextToken();
	return llvm::make_unique<WhileExprAST>(std::move(While), std::move(Do));
}

//static std::unique_ptr<ExprAST> ParseReturnExpr() {
//	getNextToken();
//	auto returnVal = ParsePrimary();
//	return llvm::make_unique<ExprAST>(returnVal);
//}
std::unique_ptr<ExprAST> ParseVar() {
	getNextToken();
	if (CurTok != tok_identifier)
	{

	}
	getNextToken();
	if (CurTok != tok_assign)
	{
		numVal = 0;
	}
	else {
		getNextToken();
	}
	auto value = ParseNumberExpr();
	std::string name = identifierStr;
	return llvm::make_unique<VarExprAST>(name, std::move(value));
}

std::unique_ptr<ExprAST> ParsePrintExpr() {
	Log("ParsePrintExpr");
	return ParsePrimary();
}
//解析定义
std::unique_ptr<FunctionAST> ParseDefinition() {
	Log("ParseDefinition");
	getNextToken(); // eat func.
	auto Proto = ParsePrototype();
	if (!Proto)
		return nullptr;
	//
	if (auto E = ParseExpression())
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	return nullptr;
}
std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	Log("ParseTopLevelExpr");
	if (auto E = ParseExpression()) {
		// Make an anonymous proto.
		auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
			std::vector<std::string>());
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}
//解析｛｝内的内容主体
std::unique_ptr<ExprAST> ParsePrimary() {
	Log("ParsePrimary");
	switch (CurTok) {
	default:
		return LogError("unknown token when expecting an expression");
	case tok_identifier:
		return ParseIdentifierExpr();
	case tok_number:
		return ParseNumberExpr();
	case '(':
		return ParseParenExpr();
	case tok_if:
		return ParseIfExpr();
		//case tok_var:
		//	return ParseVarExpr();//获取变量名
	case tok_while:
		return ParseWhileExpr();
	case tok_print:
		return ParsePrintExpr();
	case tok_var:
		return ParseVar();
	case '{':
		getNextToken();
		auto f = ParseExpression();
		if (!f)
		{
			return LogError("函数定义错误");
		}
		if (CurTok != '}')
		{
			return LogError("func缺少}");
		}
		return f;

	}

}