#pragma once
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
#include"llvmContext.h"
#include"lexer.h"
#include"log.h"
using namespace std;

	//表达式抽象类，所有表达式节点的基类
	class ExprAST {
	public:
		virtual ~ExprAST() = default;
		virtual Value *codegen() = 0;
	};

	//数字表达式AST
	class NumberExprAST : public ExprAST {
		double val;

	public:
		NumberExprAST(double val) : val(val) {}
		Value *codegen() override;
	};

	//变量表达式AST
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
		VarExprAST(const std::string &Name, std::unique_ptr<ExprAST> value) :Name(Name), value(std::move(value)) {}
		Value *codegen()override;
	};

	//二元表达式AST
	class BinaryExprAST : public ExprAST {
		char Op;//操作符
		std::unique_ptr<ExprAST> LHS, RHS;//左右操作数

	public:
		BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS,
			std::unique_ptr<ExprAST> RHS)
			: Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
		Value *codegen() override;
	};

	//函数调用表达式AST
	class CallExprAST : public ExprAST {
		std::string callee;//被调用函数名
		std::vector<std::unique_ptr<ExprAST>> Args;//函数参数

	public:
		CallExprAST(const std::string &callee,
			std::vector<std::unique_ptr<ExprAST>> Args)
			: callee(callee), Args(std::move(Args)) {}
		Value *codegen() override;
	};

	//if结构AST
	class IfExprAST : public ExprAST {
		std::unique_ptr<ExprAST> Cond, Then, Else;

	public:
		IfExprAST(std::unique_ptr<ExprAST> Cond, std::unique_ptr<ExprAST> Then,
			std::unique_ptr<ExprAST> Else)
			: Cond(std::move(Cond)), Then(std::move(Then)), Else(std::move(Else)) {}
		Value *codegen() override;
	};

	//while结构AST
	class WhileExprAST : public ExprAST {
		std::unique_ptr<ExprAST> While, Do;

	public:
		WhileExprAST(std::unique_ptr<ExprAST> While,
			std::unique_ptr<ExprAST> Do)
			: While(std::move(While)), Do(std::move(Do)) {}
		Value *codegen() override;
	};

	//函数声明AST
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

	//函数定义AST
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
