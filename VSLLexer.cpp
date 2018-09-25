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

enum token {
	//结束符
	tok_eof = -1,

	//函数
	tok_func=-2,
	tok_return=-3,

	//变量名
	tok_identifier = -4,

	//数值
	tok_number = -5,

	//if语句
	tok_if = -6,
	tok_then = -7,
	tok_else = -8,
	tok_if=-9,

	//for语句
	tok_for = -10,
	tok_in = -11,

	//do while
	tok_do=-12,
	tok_while=-13,
	tok_done=-14,

	tok_continue=-15,

	//输出
	tok_print=-16,

};


static std::string identifierStr;
static double numVal;

static int getToken() {
	
	static int lastChar=' ';
	while (isspace(lastChar)) {
		lastChar = getchar();
	}

	//字母开头
	if (isalpha(lastChar)) { // identifier: [a-zA-Z][a-zA-Z0-9]*
		identifierStr = lastChar;
		while (isalnum((lastChar = getchar())))
			identifierStr += lastChar;

		if (identifierStr == "FUNC")
			return tok_func;
		if (identifierStr == "RETURN")
			return tok_return;
		if (identifierStr == "IF")
			return tok_if;
		if (identifierStr == "ELSE")
		{
			return tok_else;
		}
		if (identifierStr=="THEN")
		{
			return tok_then;
		}
		if (identifierStr == "IF")
		{
			return tok_if;
		}
		if (identifierStr == "DO")
		{
			return tok_do;
		}
		if (identifierStr=="WHILE")
		{
			return tok_while;
		}
		if (identifierStr=="DONE")
		{
			return tok_done;
		}
		if (identifierStr=="CONTINUE")
		{
			return tok_continue;
		}
		if (identifierStr=="PRINT")
		{
			return tok_print;
		}
		return tok_identifier;
	}

	//数字开头
	if (isdigit(lastChar)||lastChar=='.')
	{
		std::string numStr;
		do
		{
			numStr += lastChar;
			lastChar = getchar();
		} while (isdigit(lastChar) || lastChar == '.');

		//TODO:这里需要在上下文进行更多的错误检查

		numVal= strtod(numStr.c_str(), 0);
		return tok_number;
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

