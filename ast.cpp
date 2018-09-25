#include<string>
#include<vector>
//表达式抽象类
class ExprAST {
public:
	virtual ~ExprAST() {}
};

//数字表达式
class NumberExprAST : public ExprAST {
	double val;

public:
	NumberExprAST(double val) : val(val) {}
};

//变量表达式
class VariableExprAST : public ExprAST {
	std::string Name;

public:
	VariableExprAST(const std::string &Name) : Name(Name) {}
};

//二元表达式
class BinaryExprAST : public ExprAST {
	char Op;//操作符
	std::unique_ptr<ExprAST> LHS, RHS;//左右操作数

public:
	BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS,
		std::unique_ptr<ExprAST> RHS)
		: Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

//函数调用表达式
class CallExprAST : public ExprAST {
	std::string callee;//被调用函数名
	std::vector<std::unique_ptr<ExprAST>> args;//函数参数

public:
	CallExprAST(const std::string &callee,
		std::vector<std::unique_ptr<ExprAST>> args)
		: callee(callee), args(std::move(args)) {}
};