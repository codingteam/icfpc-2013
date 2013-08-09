
#include "generator.h"

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
	std::list<Expr> res;
	if(prog_size < 1)
	{
		throw std::runtime_error("Expression size cannot be less than 1.");
	}

	if(prog_size >= 1)
	{
		// can use only constants and ids.
		// |0| = 1
		// |1| = 1
		res.insert(res.begin(), {0, 1});

		// |x| = 1
		for(size_t id = 0; id <= max_id; ++ id)
		{
			res.push_back(Id(id));
		}
	}
	if(prog_size >= 2)
	{
		// |(lambda (x) e)| = 1 + |e| (only with fold)
		
		// |(op1 e0)| = 1 + |e0|
		auto op1_res = GenerateRecursion(prog_size - 1, ops_set, fold_used, max_id);

		if(ops_set.Check<Ops::NOT>())
		{
			for(const auto& op1 : op1_res)
			{
				res.push_back(Op1<Ops::NOT>(op1));
			}
		}
	}
	return std::list<Expr>();
}

