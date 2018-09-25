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
	//������
	tok_eof = -1,

	//����
	tok_func=-2,
	tok_return=-3,

	//������
	tok_identifier = -4,

	//��ֵ
	tok_number = -5,

	//if���
	tok_if = -6,
	tok_then = -7,
	tok_else = -8,
	tok_if=-9,

	//for���
	tok_for = -10,
	tok_in = -11,

	//do while
	tok_do=-12,
	tok_while=-13,
	tok_done=-14,

	tok_continue=-15,

	//���
	tok_print=-16,

};


static std::string identifierStr;
static double numVal;

static int getToken() {
	
	static int lastChar=' ';
	while (isspace(lastChar)) {
		lastChar = getchar();
	}

	//��ĸ��ͷ
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

	//���ֿ�ͷ
	if (isdigit(lastChar)||lastChar=='.')
	{
		std::string numStr;
		do
		{
			numStr += lastChar;
			lastChar = getchar();
		} while (isdigit(lastChar) || lastChar == '.');

		//TODO:������Ҫ�������Ľ��и���Ĵ�����

		numVal= strtod(numStr.c_str(), 0);
		return tok_number;
	}

	//��β����
	if (lastChar == EOF) {
		return tok_eof;
	}

	//���������ֱ�ӷ��ظ��ַ�
	int thisChar = lastChar;
	lastChar = getchar();
	return thisChar;
}

