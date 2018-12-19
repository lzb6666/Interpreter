#include"log.h"
std::unique_ptr<ExprAST> LogError(const char *Str) {
	fprintf(stderr, "Error: %s\n", Str);
	return nullptr;
}
Value *LogErrorV(const char *Str) {
	LogError(Str);
	return nullptr;
}
void Log(const char* Str) {
	if (logLevel > 1)
	{
		fprintf(stderr, "function stack:%s\n", Str);
	}
}
std::unique_ptr<PrototypeAST> LogErrorP(const char *Str) {
	LogError(Str);
	return nullptr;
}