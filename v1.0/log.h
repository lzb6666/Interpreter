#pragma once
#include"AST.cpp"
using namespace std;
static int logLevel = 2;
std::unique_ptr<ExprAST> LogError(const char *Str);
Value *LogErrorV(const char *Str);
void Log(const char* Str);
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str);
