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
static std::string identifierStr;//��ʶ��
static double numVal;//����ֵ
static std::string text;
//��ǰtoken
static int CurTok;
enum token {
	//������
	tok_eof = -1,

	//����
	tok_func = -2,
	tok_return = -3,

	//������
	tok_identifier = -4,

	//��ֵ
	tok_number = -5,

	//if���
	tok_if = -6,
	tok_then = -7,
	tok_else = -8,
	tok_fi = -9,

	//do while
	tok_do = -12,
	tok_while = -13,
	tok_done = -14,

	tok_continue = -15,

	//���
	tok_print = -16,

	tok_var = -17,

	//assign symbol :=
	tok_assign = -18,

	//text
	tok_text = -19,
};
//��ȡ��һ��token������
static int getToken() {

	static int lastChar = ' ';
	//�޳��հ׷�
	while (isspace(lastChar)) {
		lastChar = getchar();
	}

	//��ĸ��ͷ
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

	//���ֿ�ͷ
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

	//ע��
	//comment��  "//".*
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

	//text�� \"({ascii_char}|{escaped_char})*\"
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



	//��β����
	if (lastChar == EOF) {
		return tok_eof;
	}

	//���������ֱ�ӷ��ظ��ַ�.{}�ᱻֱ�ӷ���
	int thisChar = lastChar;
	lastChar = getchar();
	return thisChar;
}
//��ȡtoken��ֵ��Curtok
static int getNextToken() { return CurTok = getToken(); }

//��ȡtok����������ȼ�
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
		// ��ȡmain�������Զ�������ȼ�
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

	//���ʽ�����࣬���б��ʽ�ڵ�Ļ���
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
		virtual Value *codegen() = 0;
	};

	//���ֱ��ʽAST
	class NumberExprAST : public ExprAST {
		double val;

	public:
		NumberExprAST(double val) : val(val) {}
		Value *codegen() override;
	};

	//�������ʽAST
	class VariableExprAST : public ExprAST {
		std::string Name;

	public:
		VariableExprAST(const std::string &Name) : Name(Name) {}
		Value *codegen() override;
	};

	class VarExprAST :public ExprAST {
		std::string Name;
		std::unique_ptr<ExprAST> value;
	public:
		VarExprAST(const std::string &Name, std::unique_ptr<ExprAST> value):Name(Name),value(std::move(value)){}
		Value *codegen()override;
	};

	//��Ԫ���ʽAST
	class BinaryExprAST : public ExprAST {
		char Op;//������
		std::unique_ptr<ExprAST> LHS, RHS;//���Ҳ�����

	public:
		BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS,
			std::unique_ptr<ExprAST> RHS)
			: Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		Value *codegen() override;
	};

	//�������ñ��ʽAST
	class CallExprAST : public ExprAST {
		std::string callee;//�����ú�����
		std::vector<std::unique_ptr<ExprAST>> Args;//��������

	public:
		CallExprAST(const std::string &callee,
			std::vector<std::unique_ptr<ExprAST>> Args)
			: callee(callee), Args(std::move(Args)) {}
		Value *codegen() override;
	};

	//if�ṹAST
	class IfExprAST : public ExprAST {
		std::unique_ptr<ExprAST> Cond, Then, Else;

	public:
		IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
			std::unique_ptr<ExprAST> Else)
			: Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
		Value *codegen() override;
	};

	//while�ṹAST
	class WhileExprAST : public ExprAST {
		std::unique_ptr<ExprAST> While,Do;

	public:
		WhileExprAST(std::unique_ptr<ExprAST> While,
			std::unique_ptr<ExprAST> Do)
			: While(std::move(While)), Do(std::move(Do)) {}
		Value *codegen() override;
	};

	//��������AST
	class PrototypeAST {
		std::string Name;
		std::vector<std::string> Args;
		bool IsOperator;
		unsigned Precedence; // ����ǲ����������λ��ʾ���ȼ�

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

	//��������AST
	class FunctionAST {
		std::unique_ptr<PrototypeAST> Proto;
		//std::unique_ptr<ExprAST> FuncBody;
		std::unique_ptr<ExprAST> Return;

	public:
		FunctionAST(std::unique_ptr<PrototypeAST> Proto,
			//std::unique_ptr<ExprAST> FuncBody,
			std::unique_ptr<ExprAST> Return)
			: Proto(std::move(Proto)), 
			//FuncBody(std::move(FuncBody)),
			Return(std::move(Return)) {}

		Function *codegen();
	};

} // end anonymous namespace
//IR��������ȫ�ֱ���
static LLVMContext TheContext;
static IRBuilder<> Builder(TheContext);
static std::unique_ptr<Module> TheModule;
static std::map<std::string, Value *> NamedValues;
/// LogError* - These are little helper functions for error handling.
static int logLevel = 2;
static void Log(const char* Str) {
	if (logLevel>1)
	{
		fprintf(stderr, "function stack:%s\n", Str);
	}
	
}
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}
//IR����log����
Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}


//codegenʵ��
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
//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

static std::unique_ptr<ExprAST> ParsePrimary();

//������������
static std::unique_ptr<PrototypeAST> ParsePrototype() {
	Log("ParsePrototype");
	std::string FnName;

	unsigned Kind = 0; // 0 = identifier, 1 = unary, 2 = binary.
	unsigned BinaryPrecedence = 30;

	switch (CurTok) {
	default:
		return LogErrorP("Expected function name in prototype");
	case tok_identifier://��������
		FnName = identifierStr;
		Kind = 0;
		getNextToken();
		break;
		//case tok_binary://��Ԫ�����
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

	// ������ȡ���
	getNextToken(); // eat ')'.

					// Verify right number of names for operator.
	if (Kind && ArgNames.size() != Kind)
		return LogErrorP("Invalid number of operands for operator");

	return llvm::make_unique<PrototypeAST>(FnName, ArgNames, Kind != 0,
		BinaryPrecedence);
}
static std::unique_ptr<ExprAST> ParseNumberExpr() {
	Log("ParseNumberExpr");
	auto Result = llvm::make_unique<NumberExprAST>(numVal);//numVal�ڻ�ȡtok_numʱ�Ѿ�����ֵ
	getNextToken();
	return std::move(Result);
}
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec,
	std::unique_ptr<ExprAST> LHS) {
	Log("ParseBinOpRHS");
	// ��Ԫ�������ȡ���ȼ�
	while (true) {
		int TokPrec = GetTokPrecedence();

		//�봫��ĵ�ǰ���ȼ��Ƚϣ����ȼ����߾ͼ���ִ�и����ȼ������򷵻�
		if (TokPrec < ExprPrec)
			return LHS;


		int BinOp = CurTok;
		getNextToken(); // eat��Ԫ�����


		auto RHS = ParsePrimary();//��ȡ�Ҳ�����
		if (!RHS)
			return nullptr;

		//�����ǰ���������㼶�����ұߵĲ��������ǾͰ�����Ҳ���������������������˺���
		int NextPrec = GetTokPrecedence();
		if (TokPrec < NextPrec) {
			RHS = ParseBinOpRHS(TokPrec + 1, std::move(RHS));
			if (!RHS)
				return nullptr;
		}

		// ʹ�����Ҳ��������ɶ�Ԫ������
		LHS =
			llvm::make_unique<BinaryExprAST>(BinOp, std::move(LHS), std::move(RHS));
	}
}
//�Ա��ʽ�Ľ���
static std::unique_ptr<ExprAST> ParseExpression() {
	Log("ParseExpression");
	auto LHS = ParsePrimary();//����������
	if (!LHS)
		return nullptr;

	return ParseBinOpRHS(0, std::move(LHS));
}

static std::unique_ptr<ExprAST> ParseParenExpr() {
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

//����������úͺ�������
static std::unique_ptr<ExprAST> ParseIdentifierExpr() {
	Log("ParseIdentifierExpr");
	std::string IdName = identifierStr;

	getNextToken(); // 

	if (CurTok != '(') // Simple variable ref.
		return llvm::make_unique<VariableExprAST>(IdName);
	Log("Func Call");
	//��������
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
	Log("ParseIfExpr");
	getNextToken();   //eat 'if'
	auto Cond = ParseExpression();//����if����
	if (!Cond)
		return nullptr;
	if (CurTok != tok_then)
		return LogError("expected then");
	getNextToken(); // eat then

	auto Then = ParseExpression();//����then�µĴ���
	if (!Then)
		return nullptr;

	if (CurTok != tok_else)
		return LogError("expected else");
	getNextToken();

	auto Else = ParseExpression();//����else�µĴ���
	if (!Else)
		return nullptr;
	if (CurTok!=tok_fi)
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

static std::unique_ptr<ExprAST> ParseWhileExpr() {
	Log("ParseWhileExpr");
	getNextToken();  //eat 'while'
	auto While = ParseExpression();
	if (!While)
	{
		return nullptr;
	}

	if (CurTok!=tok_do)
	{
		return LogError("whileȱ��do");
	}
	getNextToken();
	if (CurTok!='{')
	{
		return LogError("whileȱ��{");//Ŀǰǿ��Ҫ��{}
	}
	getNextToken();
	auto Do = ParseExpression();
	if (!Do)
	{
		return nullptr;
	}
	
	if (CurTok!='}')
	{
		return LogError("whileȱ��}");
	}
	getNextToken();
	if (CurTok!=tok_done)
	{
		return LogError("whileȱ��done");
	}
	getNextToken();
	return llvm::make_unique<WhileExprAST>(std::move(While),std::move(Do));
}

//static std::unique_ptr<ExprAST> ParseReturnExpr() {
//	getNextToken();
//	auto returnVal = ParsePrimary();
//	return llvm::make_unique<ExprAST>(returnVal);
//}
static std::unique_ptr<ExprAST> ParseVar() {
	getNextToken();
	if (CurTok!= tok_identifier)
	{
		
	}
	getNextToken();
	if (CurTok!=tok_assign)
	{
		numVal = 0;
	}
	else {
		getNextToken();
	}
	auto value = ParseNumberExpr();
	std::string name = identifierStr;
	return llvm::make_unique<VarExprAST>(name,std::move(value));
}

static std::unique_ptr<ExprAST> ParsePrintExpr() {
	Log("ParsePrintExpr");
	getNextToken();  //eat "print"
	std::string str = "";
	while (1)
	{
		if (CurTok == tok_text)
		{
			if (text != "\n")
			{
				str += text;
				getNextToken();
			}
			else
			{
				std::cout << str << std::endl;
				getNextToken();     //eat " "\n" "
				return llvm::make_unique<ExprAST>(str);
			}
		}
		if (CurTok == 44)  //���� ASCII��
		{
			getNextToken();
		}
		if (CurTok == tok_identifier)
		{
			str += identifierStr;
			getNextToken();
		}
	}
	return ParsePrimary();
}
//��������
static std::unique_ptr<FunctionAST> ParseDefinition() {
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
static void HandleDefinition() {
	Log("HandleDefinition");
	if (auto FnAST = ParseDefinition()) {
		if (auto *FnIR = FnAST->codegen()) {
			fprintf(stderr, "Read function definition:");
			FnIR->print(errs());
			fprintf(stderr, "\n");
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}
static std::unique_ptr<FunctionAST> ParseTopLevelExpr() {
	Log("ParseTopLevelExpr");
	if (auto E = ParseExpression()) {
		// Make an anonymous proto.
		auto Proto = llvm::make_unique<PrototypeAST>("__anon_expr",
			std::vector<std::string>());
		return llvm::make_unique<FunctionAST>(std::move(Proto), std::move(E));
	}
	return nullptr;
}
static void HandleTopLevelExpression() {
	Log("HandleTopLevelExpression");
	if (auto FnAST = ParseTopLevelExpr()) {
		if (auto *FnIR = FnAST->codegen()) {
			fprintf(stderr, "Read top-level expression:");
			FnIR->print(errs());
			fprintf(stderr, "\n");
		}
	}
	else {
		// Skip token for error recovery.
		getNextToken();
	}
}
//���������ڵ���������
static std::unique_ptr<ExprAST> ParsePrimary() {
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
		//	return ParseVarExpr();//��ȡ������
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
			return LogError("�����������");
		}
		if (CurTok!='}')
		{
			return LogError("funcȱ��}");
		}
		return f;

	}

}

static void MainLoop() {
	while (true) {
		fprintf(stderr, "ready> ");
		switch (CurTok) {
		case tok_eof:
			return;
		case ';': 
			getNextToken();
			break;
		case tok_func:
			HandleDefinition();
			break;
		default:
			HandleTopLevelExpression();
			break;
		}
	}
}
int main() {
	
	BinopPrecedence['<'] = 10;
	BinopPrecedence['+'] = 20;
	BinopPrecedence['-'] = 20;
	BinopPrecedence['*'] = 40; 
	BinopPrecedence['='] = 60;

	fprintf(stderr, "ready> ");
	getNextToken();
	TheModule = llvm::make_unique<Module>("my cool jit", TheContext);
	
	MainLoop();
	TheModule->print(errs(), nullptr);
	return 0;
}


