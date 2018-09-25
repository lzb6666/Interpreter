#include<string>
#include<vector>
//���ʽ������
class ExprAST {
public:
	virtual ~ExprAST() {}
};

//���ֱ��ʽ
class NumberExprAST : public ExprAST {
	double val;

public:
	NumberExprAST(double val) : val(val) {}
};

//�������ʽ
class VariableExprAST : public ExprAST {
	std::string Name;

public:
	VariableExprAST(const std::string &Name) : Name(Name) {}
};

//��Ԫ���ʽ
class BinaryExprAST : public ExprAST {
	char Op;//������
	std::unique_ptr<ExprAST> LHS, RHS;//���Ҳ�����

public:
	BinaryExprAST(char op, std::unique_ptr<ExprAST> LHS,
		std::unique_ptr<ExprAST> RHS)
		: Op(op), LHS(std::move(LHS)), RHS(std::move(RHS)) {}
};

//�������ñ��ʽ
class CallExprAST : public ExprAST {
	std::string callee;//�����ú�����
	std::vector<std::unique_ptr<ExprAST>> args;//��������

public:
	CallExprAST(const std::string &callee,
		std::vector<std::unique_ptr<ExprAST>> args)
		: callee(callee), args(std::move(args)) {}
};