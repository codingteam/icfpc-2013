
#include "generator.h"

#include <cstring>

const char* ops[] =
{
"not",
"shl1",
"shr1",
"shr4",
"shr16",
"and",
"or",
"xor",
"plus",
"if0",
"tfold",
"fold",
nullptr
};

void Ops::Set(const char* str)
{
	//std::find(std::begin(ops), std::end(ops)
	//	, [&str](const char *op){ return strcmp(op, str) == 0; });
}

void Ops::Set(const std::string& str)
{
	//std::find(std::begin(ops), std::end(ops)
	//	, [&str](const char *op){ return str == op; });	
}

std::list<Expr> Generate(size_t prog_size, Ops ops_set)
{
	return std::list<Expr>();
}

