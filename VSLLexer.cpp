#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/STLExtras.h"
#include "llvm/IR/BasicBlock.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DerivedTypes.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "KaleidoscopeJIT.h"
#include <algorithm>
#include <cassert>
#include <cctype>
#include <cstdint>
#include <cstdio>
#include <cstdlib>
#include <map>
#include <memory>
#include <string>
#include <utility>
#include <vector>

using namespace llvm;
using namespace llvm::orc;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//
static std::map<char, int> BinopPrecedence;
static std::string identifierStr;//Filled in if tok_identifier
static double numVal;//Filled in if tok_number
static std::string text;//Filled in if tok_text
  /// CurTok - Provide a simple token buffer.  CurTok is the current token 
  /// the parser is looking at.  
  /// getNextToken - reads another token from the lexer and updates CurTok with its results.
static int CurTok;
enum token {
	//结束符
	tok_eof = -1,

	//函数
	tok_func = -2,
	tok_return = -3,

	//变量名
	tok_identifier = -4,

	//数值
	tok_number = -5,

	//if语句
	tok_if = -6,
	tok_then = -7,
	tok_else = -8,
	tok_fi = -9,


	//do while
	tok_do = -12,
	tok_while = -13,
	tok_done = -14,

	tok_continue = -15,

	//输出
	tok_print = -16,

	tok_var = -17,

	//assign symbol :=
	tok_assign = -18,

	//text
	tok_text = -19,
};
static int getToken() {

	static int lastChar = ' ';
	//剔除空白符
	while (isspace(lastChar)) {
		lastChar = getchar();
	}

	//字母开头
	if (isalpha(lastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
		identifierStr = lastChar;
		while (isalnum((lastChar = getchar())))
			identifierStr += lastChar;

		if (identifierStr == "FUNC")
		{
			return tok_func;
		}
		if (identifierStr == "RETURN")
		{
			return tok_return;
		}
		if (identifierStr == "IF")
		{
			return tok_if;
		}
		if (identifierStr == "ELSE")
		{
			return tok_else;
		}
		if (identifierStr == "THEN")
		{
			return tok_then;
		}
		if (identifierStr == "FI")
		{
			return tok_fi;
		}
		if (identifierStr == "DO")
		{
			return tok_do;
		}
		if (identifierStr == "WHILE")
		{
			return tok_while;
		}
		if (identifierStr == "DONE")
		{
			return tok_done;
		}
		if (identifierStr == "CONTINUE")
		{
			return tok_continue;
		}
		if (identifierStr == "PRINT")
		{
			return tok_print;
		}
		if (identifierStr == "VAR")
		{
			return tok_var;
		}

		return tok_identifier;
	}

	//数字开头
	//digit [0-9]
	//integer {digit}+
	if (isdigit(lastChar))
	{
		std::string numStr;
		do
		{
			numStr += lastChar;
			lastChar = getchar();
		} while (isdigit(lastChar));

		numVal = strtod(numStr.c_str(), 0);
		return tok_number;
	}

	//注释
	//comment：  "//".*
	if (lastChar == '"')
	{
		std::string tmp;
		do
		{
			tmp += lastChar;
			lastChar = getchar();
		} while (lastChar != '"');

		if (tmp == "\"//") {
			do
			{
				lastChar = getchar();
			} while (lastChar != EOF && lastChar != '\n' && lastChar != '\r');

			if (lastChar != EOF)
				return getToken();

		}
	}

	//text： \"({ascii_char}|{escaped_char})*\"
	if (lastChar == '"')
	{
		std::string tmp;
		do
		{
			tmp += lastChar;
			lastChar = getchar();
		} while (lastChar != '"');

		if (tmp != "\"//") {
			text = tmp.substr(1, tmp.length() - 1);
			return tok_text;
		}
	}

	//assign :=
	if (lastChar == ':')
	{
		lastChar = getchar(); // eat :
		if (lastChar != '=')
		{
			fprintf(stderr, "Error: lack '=' when assign.\n");
			return lastChar;
		}
		else
		{
			lastChar = getchar(); //eat =
			return tok_assign;

		}

	}



	//结尾符号
	if (lastChar == EOF) {
		return tok_eof;
	}

	//其他情况，直接返回该字符
	int thisChar = lastChar;
	lastChar = getchar();
	return thisChar;
}

static int getNextToken() { return CurTok = getToken(); }

/// GetTokPrecedence - Get the precedence of the pending binary operator token.
static int GetTokPrecedence() {
	if (!isascii(CurTok))
		return -1;

	int TokPrec;
	if (CurTok == -18)
	{
		TokPrec = BinopPrecedence['='];
	}
	else
	{
		// 获取main函数中自定义的优先级
		TokPrec = BinopPrecedence[CurTok];
	}
	if (TokPrec <= 0)
		return -1;
	return TokPrec;
}
//===----------------------------------------------------------------------===//
// Abstract Syntax Tree (aka Parse Tree)
//===----------------------------------------------------------------------===//
namespace {

	/// ExprAST - Base class for all expression nodes.
	//表达式抽象类
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
		virtual Value *codegen() = 0;
	};

	/// NumberExprAST - Expression class for numeric literals like "1.0".
	//数字表达式
	class NumberExprAST : public ExprAST {
		double val;

	public:
		NumberExprAST(double val) : val(val) {}
		virtual Value *codegen();
	};

	//变量表达式
	class VariableExprAST : public ExprAST {
		std::string Name;

	public:
		VariableExprAST(const std::string &Name) : Name(Name) {}
		virtual Value *codegen();
	};

	//二元表达式
	class BinaryExprAST : public ExprAST {
		char Op;//操作符
		std::unique_ptr<ExprAST> LHS, RHS;//左右操作数

	public:
		BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS,
			std::unique_ptr<ExprAST> RHS)
			: Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		virtual Value *codegen();
	};

	//函数调用表达式
	class CallExprAST : public ExprAST {
		std::string callee;//被调用函数名
		std::vector<std::unique_ptr<ExprAST>> Args;//函数参数

	public:
		CallExprAST(const std::string &callee,
			std::vector<std::unique_ptr<ExprAST>> Args)
			: callee(callee), Args(std::move(Args)) {}
		virtual Value *codegen();
	};

	/// IfExprAST - Expression class for if/then/else.
	class IfExprAST : public ExprAST {
		std::unique_ptr<ExprAST> Cond, Then, Else;

	public:
		IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
			std::unique_ptr<ExprAST> Else)
			: Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
		virtual Value *codegen();
	};

	/// WhileExprAST - Expression class for for/in.
	class WhileExprAST : public ExprAST {
		std::string VarName;
		std::unique_ptr<ExprAST> Start, End, Step, Body;

	public:
		WhileExprAST(const std::string &VarName, std::unique_ptr<ExprAST> Start,
			std::unique_ptr<ExprAST> End, std::unique_ptr<ExprAST> Step,
			std::unique_ptr<ExprAST> Body)
			: VarName(VarName), Start(std::move(Start)), End(std::move(End)),
			Step(std::move(Step)), Body(std::move(Body)) {}
		virtual Value *codegen();
	};


	class PrototypeAST {
		std::string Name;
		std::vector<std::string> Args;
		bool IsOperator;
		unsigned Precedence; // 如果是操作符，则该位表示优先级

	public:
		PrototypeAST(const std::string &Name, std::vector<std::string> Args,
			bool IsOperator = false, unsigned Prec = 0)
			: Name(Name), Args(std::move(Args)), IsOperator(IsOperator),
			Precedence(Prec) {}

		Function *codegen();
		const std::string &getName() const { return Name; }

		bool isUnaryOp() const { return IsOperator && Args.size() == 1; }
		bool isBinaryOp() const { return IsOperator && Args.size() == 2; }

		char getOperatorName() const {
			assert(isUnaryOp() || isBinaryOp());
			return Name[Name.size() - 1];
		}

		unsigned getBinaryPrecedence() const { return Precedence; }

	};

	/// FunctionAST - This class represents a function definition itself.
	class FunctionAST {
		std::unique_ptr<PrototypeAST> Proto;
		std::unique_ptr<ExprAST> Body;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			std::unique_ptr<ExprAST> Body)
			: Proto(std::move(Proto)), Body(std::move(Body)) {}

		Function *codegen();
	};

} // end anonymous namespace
//IR部分所需全局变量
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;
/// LogError* - These are little helper functions for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}
//IR部分log函数
Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}


//codegen实现
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
Function *PrototypeAST::codegen() {
	// Make the function type:  double(double,double) etc.
	std::vector<Type*> Doubles(Args.size(),
		Type::getDoubleTy(TheContext));
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
	if (Value *RetVal = Body->codegen()) {
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
//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//
///各种解析函数
//函数声明
static std::unique_ptr<ExprAST> ParsePrimary();

//解析函数定义
static std::unique_ptr<PrototypeAST> ParsePrototype() {
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
		//case tok_unary://一元运算符
		//	getNextToken();
		//	if (!isascii(CurTok))
		//		return LogErrorP("Expected unary operator");
		//	FnName = "unary";
		//	FnName += (char)CurTok;
		//	Kind = 1;
		//	getNextToken();
		//	break;
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
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	auto Result = llvm::make_unique<NumberExprAST>(numVal);//numVal在获取tok_num时已经被赋值
	getNextToken();
	return std::move(Result);
}
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
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
static std::unique_ptr<ExprAST> ParseExpression() {
	auto LHS = ParsePrimary();//获得左运算符
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<ExprAST> ParseParenExpr() {
	getNextToken(); // eat (.
	auto V = ParseExpression();
	if (!V)
		return nullptr;

	if (CurTok != ')')
		return LogError("expected ')'");
	getNextToken(); // eat ).
	return V;
}


static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
	std::string IdName = identifierStr;

	getNextToken(); // 

	if (CurTok != '(') // Simple variable ref.
		return llvm::make_unique<VariableExprAST>(IdName);

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
static std::unique_ptr<ExprAST> ParseIfExpr() {
	getNextToken();   //eat 'if'

	if (CurTok)   //条件为真
	{
		getNextToken();   //eat 'n'
		if (CurTok != tok_then)
			fprintf(stderr, "Lack of key word: then");
		getNextToken();   //eat 'Then'
		ParsePrimary();

		if (CurTok == tok_else)
		{
			while (CurTok != tok_fi)
				getNextToken();
		}
	}
	else   //条件为假
	{
		while (CurTok != tok_else)
			getNextToken();
		getNextToken();
		ParsePrimary();

		while (CurTok != tok_fi)
			getNextToken();
	}

	getNextToken();
	return llvm::make_unique<IfExprAST>();
}

//static std::unique_ptr<ExprAST> ParseVarExpr() {
//	getNextToken();
//	return llvm::make_unique<VariableExprAST>(CurTok);
//}

//TODO:该处需完善
static std::unique_ptr<ExprAST> ParseWhileExpr() {
	getNextToken();  //eat 'while'
	auto condition = ParseExpression();
	if (condition)
	{
		if (CurTok != tok_do)
			fprintf(stderr, "Lack of key word: do");

		getNextToken();   //eat 'do'
		if (CurTok != '{')
			fprintf(stderr, "Lack of key symbol: {");

		getNextToken();   //eat '{'
		ParsePrimary();

		if (CurTok == '}')
			getNextToken();   //eat '}'

		if (CurTok != tok_done)
			fprintf(stderr, "Lack of key word: done");
	}
	else
	{
		while (1)
		{
			getNextToken();
			if (CurTok == tok_done)
				break;
		}
	}
	getNextToken();   //eat 'done'
	return llvm::make_unique<WhileExprAST>();
}

static std::unique_ptr<ExprAST> ParseReturnExpr() {
	getNextToken();
	auto returnVal = ParsePrimary();
	return llvm::make_unique<ExprAST>(returnVal);
}

//TODO:print解析需完善
static std::unique_ptr<ExprAST> ParsePrintExpr() {

	return ParsePrimary();
}

static std::unique_ptr<FunctionAST> ParseDefinition() {
	getNextToken(); // eat func.
	auto Proto = ParsePrototype();
	if (!Proto)
		return nullptr;

	if (auto E = ParseExpression())
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	return nullptr;
}
static void HandleDefinition() {
	/*if (auto FnAST = ParseDefinition()) {
	if (auto *FnIR = FnAST->codegen()) {
	fprintf(stderr, "Read function definition:");
	FnIR->print(errs());
	fprintf(stderr, "\n");
	TheJIT->addModule(std::move(TheModule));
	InitializeModule();
	}
	}*/
	if (ParseDefinition())//返回包含函数定义的Prototype对象
	{
		fprintf(stderr, "Parsed a function definition.\n");
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	if (auto E = ParseExpression()) {
		// Make an anonymous proto.
		auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
			std::vector<std::string>());
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}
static void HandleTopLevelExpression() {
	if (ParseTopLevelExpr())
	{
		fprintf(stderr, "Parsed a top-level xepr\n");
	}
	else {
		getNextToken();
	}
}
//解析｛｝内的内容主体
static std::unique_ptr<ExprAST> ParsePrimary() {
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
	case tok_return:
		return ParseReturnExpr();
	case tok_print:
		return ParsePrintExpr();
	}
}

static void MainLoop() {
	while (true) {
		fprintf(stderr, "ready> ");
		switch (CurTok) {
		case tok_eof:
			return;
		case ';': // ignore top-level semicolons.
			getNextToken();
			break;
		case tok_func:
			HandleDefinition();//此函数没有eat函数的两个大括号
			break;
			/*case tok_extern:
			HandleExtern();
			break;*/
		default:
			HandleTopLevelExpression();
			break;
		}
	}
}
int main() {
	// InitializeNativeTarget();
	// InitializeNativeTargetAsmPrinter();
	// InitializeNativeTargetAsmParser();

	// Install standard binary operators.
	// 1 is lowest precedence.

	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40; // highest.
	BinopPrecedence['='] = 60;

	// Prime the first token.
	fprintf(stderr, "ready> ");
	getNextToken();

	// TheJIT = llvm::make_unique<KaleidoscopeJIT>();
	// InitializeModule();

	// Run the main "interpreter loop" now.
	MainLoop();

	return 0;
}


