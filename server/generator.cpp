#include "generator.h"

#include <boost/make_shared.hpp>

#define GENDEBUG 0

std::list<Expr> GenerateRecursion(size_t prog_size, Ops ops_set, bool fold_used, size_t max_id);

void ShowCheck(const std::pair<size_t, Ops>& check, const Expr& prog)
{
	std::cout << "= Size: " << boost::apply_visitor(ProgramSize(), prog) + 1 << " Folds: " << check.first << " Ops: (";
	for(size_t i = 0 ; i < Ops::max_index; ++ i)
	{
		if(check.second.Check(static_cast<Ops::OpsIndex>(i)))
		{
			std::cout << Ops::names[i] << " ";
		}
	}
	std::cout << ") : ";
	PrintProgram(std::cout, prog);
	std::cout << std::endl;
}

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
		std::cout << "Generation comleted with " << es.size() << std::endl;
		for(auto& e : es)
		{
			Expr prog = boost::make_shared<Fold>(Id(0), 0, std::move(e));
#if GENDEBUG
			if(prog_size - 1 != boost::apply_visitor(ProgramSize(), prog))
			{
				std::cerr << "Size: " << prog_size << " ";
				Printer prn(std::cerr);
				boost::apply_visitor(prn, prog);
				std::cerr << std::endl;
				throw std::runtime_error("Size mismatch.");
			}
#endif
			// check for ops
			auto check = boost::apply_visitor(ProgramInfo(), prog);
			//ShowCheck(check, prog);
			if((check.first == 1) && (check.second.Cmp(ops_set)))
			{
				res.push_back(std::move(prog));
			}
		}

		return res;
	}

	// (lambda (x_0) e)
	// |(lambda (x_0) e)| =
	// = 1 + |e|
	if(prog_size <= 1)
	{
		return res;
	}
	auto es = GenerateRecursion(prog_size - 1, ops_set, false, 0);
	std::cout << "Generation comleted with " << es.size() << std::endl;
	for(auto& e : es)
	{
#if GENDEBUG
		if(prog_size - 1 != boost::apply_visitor(ProgramSize(), e))
		{
			std::cerr << "Size: " << prog_size << " ";
			Printer prn(std::cerr);
			boost::apply_visitor(prn, e);
			std::cerr << std::endl;
			throw std::runtime_error("Size mismatch.");
		}
#endif
		//check for fold and ops
		auto check = boost::apply_visitor(ProgramInfo(), e);
		//ShowCheck(check, e);
		if((check.first <= 1) && (check.second.Cmp(ops_set)))
		{
			res.push_back(std::move(e));
		}
	}
	
	return res;
}

template<Ops::OpsIndex O> void inline GenOp1(std::list<Expr>& res, Ops ops_set, const std::list<Expr>& op1_res)
{	
	if(ops_set.Check<O>())
	{
		for(const auto& op1 : op1_res)
		{
			res.push_back(boost::make_shared<Op1<O>>(op1));
		}
	}
}

template<Ops::OpsIndex O> void inline GenOp2(std::list<Expr>& res, Ops ops_set, const std::list<Expr>& e0_res
	, const std::list<Expr>& e1_res)
{	
	if(ops_set.Check<O>())
	{
		for(const auto& e0 : e0_res)
		{
			for(const auto& e1 : e1_res)
			{
				res.push_back(boost::make_shared<Op2<O>>(e0, e1));
			}
		}
	}
}

void inline GenIf0(std::list<Expr>& res, const std::list<Expr>& e0_res
	, const std::list<Expr>& e1_res
	, const std::list<Expr>& e2_res)
{	
	for(const auto& e0 : e0_res)
	{
		for(const auto& e1 : e1_res)
		{
			for(const auto& e2 : e2_res)
			{
				res.push_back(boost::make_shared<If0>(e0, e1, e2));
			}
		}
	}
}

void inline GenFold(std::list<Expr>& res, const std::list<Expr>& e0_res
	, const std::list<Expr>& e1_res
	, const std::list<Expr>& e2_res)
{	
	for(const auto& e0 : e0_res)
	{
		for(const auto& e1 : e1_res)
		{
			for(const auto& e2 : e2_res)
			{
				res.push_back(boost::make_shared<Fold>(e0, e1, e2));
			}
		}
	}
}

std::list<Expr> GenerateRecursion(size_t prog_size, Ops ops_set, bool fold_used, size_t max_id)
{
	std::list<Expr> res;
	if(prog_size < 1)
	{
		throw std::runtime_error("Expression size cannot be less than 1.");
	}

	if(prog_size == 1)
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
		if(ops_set.Check<Ops::NOT>()
			|| ops_set.Check<Ops::SHL1>()
			|| ops_set.Check<Ops::SHR1>()
			|| ops_set.Check<Ops::SHR4>()
			|| ops_set.Check<Ops::SHR16>())
		{
			//check if actual
			auto op1_res = GenerateRecursion(prog_size - 1, ops_set, fold_used, max_id);

			if(op1_res.size() > 0)
			{
				GenOp1<Ops::NOT>(res, ops_set, op1_res);
				GenOp1<Ops::SHL1>(res, ops_set, op1_res);
				GenOp1<Ops::SHR1>(res, ops_set, op1_res);
				GenOp1<Ops::SHR4>(res, ops_set, op1_res);
				GenOp1<Ops::SHR16>(res, ops_set, op1_res);
			}
		}
	}

	if(prog_size >= 3)
	{
		// |(op2 e0 e1)| = 1 + |e0| + |e1|
		if(ops_set.Check<Ops::AND>()
			|| ops_set.Check<Ops::OR>()
			|| ops_set.Check<Ops::XOR>()
			|| ops_set.Check<Ops::PLUS>())
		{
			for(size_t e0_size = 1; e0_size <= prog_size - 2; ++ e0_size)
			{
				auto e0_res = GenerateRecursion(e0_size, ops_set, fold_used, max_id);
				if(e0_res.size() > 0)
				{
					auto e1_res = GenerateRecursion(prog_size - e0_size - 1, ops_set, fold_used, max_id);
					if(e1_res.size() > 0)
					{
						GenOp2<Ops::AND>(res, ops_set, e0_res, e1_res);
						GenOp2<Ops::OR>(res, ops_set, e0_res, e1_res);
						GenOp2<Ops::XOR>(res, ops_set, e0_res, e1_res);
						GenOp2<Ops::PLUS>(res, ops_set, e0_res, e1_res);
					}
				}
			}
		}
	}

	if((prog_size >= 4) && ops_set.Check<Ops::IF0>())
	{
		// |(if0 e0 e1 e2)| = 1 + |e0| + |e1| + |e2|
		for(size_t e0_size = 1; e0_size <= prog_size - 3; ++ e0_size)
		{
			auto e0_res = GenerateRecursion(e0_size, ops_set, fold_used, max_id);
			if(e0_res.size() > 0)
			{
				for(size_t e1_size = 1; e1_size <= prog_size - e0_size - 2; ++ e1_size)
				{
					auto e1_res = GenerateRecursion(e1_size, ops_set, fold_used, max_id);
					if(e1_res.size() > 0)
					{
						auto e2_res = GenerateRecursion(prog_size - e0_size - e1_size - 1, ops_set, fold_used, max_id);
						if(e2_res.size() > 0)
						{
							GenIf0(res, e0_res, e1_res, e2_res);
						}
					}
				}
			}
		}			
	}

	if((prog_size >= 5) && ops_set.Check<Ops::FOLD>() && (! fold_used))
	{
		// |(fold e0 e1 (lambda (x y) e2))| = 2 + |e0| + |e1| + |e2|
		for(size_t e0_size = 1; e0_size <= prog_size - 4; ++ e0_size)
		{
			auto e0_res = GenerateRecursion(e0_size, ops_set, true, max_id);
			if(e0_res.size() > 0)
			{
				for(size_t e1_size = 1; e1_size <= prog_size - e0_size - 3; ++ e1_size)
				{
					auto e1_res = GenerateRecursion(e1_size, ops_set, true, max_id);
					if(e1_res.size() > 0)
					{
						auto e2_res = GenerateRecursion(prog_size - e0_size - e1_size - 2, ops_set, true, max_id + 2);
						if(e2_res.size() > 0)
						{
							GenFold(res, e0_res, e1_res, e2_res);
						}
					}
				}
			}
		}			
	}

#if GENDEBUG
	for(const auto& r : res)
	{
		if(prog_size != boost::apply_visitor(ProgramSize(), r))
		{
			std::cerr << "Size: " << prog_size << " ";
			Printer prn(std::cerr);
			boost::apply_visitor(prn, r);
			std::cerr << std::endl;
			throw std::runtime_error("Size mismatch.");
		}
	}
#endif
	return res;
}

