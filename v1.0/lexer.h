#pragma once
#include <string>
static std::string identifierStr;//标识符
static double numVal;//数字值
static std::string text;
//当前token
static int CurTok;
static std::map<char, int> BinopPrecedence;
//获取tok运算符的优先级
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
//获取下一个token并返回
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

	//其他情况，直接返回该字符.{}会被直接返回
	int thisChar = lastChar;
	lastChar = getchar();
	return thisChar;
}
//获取token赋值给Curtok
static int getNextToken() { return CurTok = getToken(); }

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