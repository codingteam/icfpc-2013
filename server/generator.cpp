
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
	for(size_t i = 0; i < Ops::max_index; ++ i)
	{
		if(strcmp(ops[i], str) == 0)
		{
			m_Data |= (1 << i);
		}
	}
}

void Ops::Set(const std::string& str)
{
	for(size_t i = 0; i < Ops::max_index; ++ i)
	{
		if(str == ops[i])
		{
			m_Data |= (1 << i);
		}
	}
}

std::list<Expr> GenerateRecursion(size_t prog_size, Ops ops_set, bool fold_used, size_t max_id);

std::list<Expr> Generate(size_t prog_size, Ops ops_set)
{
	std::list<Expr> res;
	if(ops_set.Check<Ops::TFOLD>())
	{
		// (lambda (x_0) (fold x_0 0 (lambda (x_1 x_2) e)))
		// |(lambda (x_0) (fold x_0 0 (lambda (x_1 x_2) e)))| =
		// = 1 + |(fold x_0 0 (lambda (x_1 x_2) e))| =
		// = 1 + 2 + |x_0| + |0| + |e| = 
		// = 3 + 1 + 1 + |e| =
		// = 5 + |e|
		if(prog_size <= 5)
		{
			return res;
		}
		auto es = GenerateRecursion(prog_size - 5, ops_set, true, 2);
		for(auto& e : es)
		{
			res.push_back(Fold(Id(0), 0, e));
		}
	}

	// (lambda (x_0) e)
	// |(lambda (x_0) e)| =
	// = 1 + |e|
	if(prog_size <= 1)
	{
		return res;
	}
	return GenerateRecursion(prog_size - 1, ops_set, false, 0);
}

std::list<Expr> GenerateRecursion(size_t prog_size, Ops ops_set, bool fold_used, size_t max_id)
{
	return std::list<Expr>();
}

