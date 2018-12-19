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
		VarExprAST(const std::string &Name, std::unique_ptr<ExprAST> value) :Name(Name), value(std::move(value)) {}
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
		std::unique_ptr<ExprAST> While, Do;

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
