
#ifndef _EXPR_H_
#define _EXPR_H_

#include <stdexcept>
#include <cstdint>
#include <vector>
#include <ostream>

#include <boost/variant.hpp>

class Id
{
	friend class Evaluator;
	friend class Printer;
public:
	Id(size_t id)
	: m_Id(id)
	{
	}

private:
	size_t m_Id;
};

class If0;
class Fold;

struct Not
{
	uint64_t operator()(uint64_t x) const
	{
		return ~x;
	}
	const char* get_id() const
	{
		return "not";
	}
};
struct Shl1
{
	uint64_t operator()(uint64_t x) const
	{
		return x << 1;
	}
	const char* get_id() const
	{
		return "shl1";
	}
};
struct Shr1
{
	uint64_t operator()(uint64_t x) const
	{
		return x >> 1;
	}
	const char* get_id() const
	{
		return "shr1";
	}
};
struct Shr4
{
	uint64_t operator()(uint64_t x) const
	{
		return x >> 4;
	}
	const char* get_id() const
	{
		return "shr4";
	}
};
struct Shr16
{
	uint64_t operator()(uint64_t x) const
	{
		return x >> 16;
	}
	const char* get_id() const
	{
		return "shr16";
	}
};
template <class OpTag> class Op1;

struct And
{
	uint64_t operator()(uint64_t x, uint64_t y) const
	{
		return x & y;
	}
	const char* get_id() const
	{
		return "and";
	}
};
struct Or
{
	uint64_t operator()(uint64_t x, uint64_t y) const
	{
		return x | y;
	}
	const char* get_id() const
	{
		return "or";
	}
};
struct Xor
{
	uint64_t operator()(uint64_t x, uint64_t y) const
	{
		return x ^ y;
	}
	const char* get_id() const
	{
		return "xor";
	}
};
struct Plus
{
	uint64_t operator()(uint64_t x, uint64_t y) const
	{
		return x + y;
	}
	const char* get_id() const
	{
		return "plus";
	}
};
template <class OpTag> class Op2;

typedef boost::variant<uint64_t
	, Id
	, boost::recursive_wrapper<If0>
	, boost::recursive_wrapper<Fold>
	, boost::recursive_wrapper<Op1<Not>>
	, boost::recursive_wrapper<Op1<Shl1>>
	, boost::recursive_wrapper<Op1<Shr1>>
	, boost::recursive_wrapper<Op1<Shr4>>
	, boost::recursive_wrapper<Op1<Shr16>>
	, boost::recursive_wrapper<Op2<And>>
	, boost::recursive_wrapper<Op2<Or>>
	, boost::recursive_wrapper<Op2<Xor>>
	, boost::recursive_wrapper<Op2<Plus>>
	> Expr;

class If0
{
	friend class Evaluator;
	friend class Printer;
public:	
	If0(const Expr& cond, const Expr& _true, const Expr& _false)
		: m_Cond(cond)
		, m_True(_true)
		, m_False(_false)
	{
	}
private:
	Expr m_Cond;
	Expr m_True;
	Expr m_False;
};

class Fold
{
	friend class Evaluator;
	friend class Printer;
public:	
	Fold(const Expr& value, const Expr& accum, const Expr& lambda)
		: m_Value(value)
		, m_Accum(accum)
		, m_Lambda(lambda)
	{
	}
private:
	Expr m_Value;
	Expr m_Accum;
	Expr m_Lambda;
};

template <typename OpTag> class Op1
{
	friend class Evaluator;
	friend class Printer;
public:
	Op1(const Expr& expr)
		: m_Op(expr)
	{
	}
private:
	Expr m_Op;
};

template <typename OpTag> class Op2
{
	friend class Evaluator;
	friend class Printer;
public:
	Op2(const Expr& op1, const Expr& op2)
		: m_Op1(op1)
		, m_Op2(op2)
	{
	}
private:
	Expr m_Op1;
	Expr m_Op2;
};

class Evaluator : public boost::static_visitor<uint64_t>
{
public:
	Evaluator(uint64_t x)
		: m_Ids({x})
	{
	}

	Evaluator(Evaluator&& ev)
		: m_Ids(std::move(ev.m_Ids))
	{
	}

	Evaluator Lambda(uint64_t v1, uint64_t v2) const
	{
		Evaluator ev(m_Ids);
		ev.m_Ids.push_back(v1);
		ev.m_Ids.push_back(v2);
		return ev;
	}

	uint64_t operator()(uint64_t v) const
	{
		return v;
	}

	uint64_t operator()(Id id) const
	{
		if(m_Ids.size() <= id.m_Id)
		{
			throw std::runtime_error("Id too big.");
		}
		return m_Ids[id.m_Id];
	}
	uint64_t operator()(const If0& _if) const
	{
		if(boost::apply_visitor(*this, _if.m_Cond))
		{
			return boost::apply_visitor(*this, _if.m_True);
		}
		return boost::apply_visitor(*this, _if.m_False);
	}
	uint64_t operator()(const Fold& fold) const
	{
		uint64_t val = boost::apply_visitor(*this, fold.m_Value);
		uint64_t accum = boost::apply_visitor(*this, fold.m_Accum);

		int shift = 0;
		while(shift < 64)
		{
			accum = boost::apply_visitor(Lambda(val >> shift, accum), fold.m_Lambda);
			
			shift += 8;
		}
		return accum;
	}
	template <class T> uint64_t operator()(const Op1<T>& op1) const
	{
		return T()(boost::apply_visitor(*this, op1.m_Op));
	}
	template <class T> uint64_t operator()(const Op2<T>& op2) const
	{
		return T()(boost::apply_visitor(*this, op2.m_Op1)
			, boost::apply_visitor(*this, op2.m_Op2));
	}
private:
	Evaluator(const std::vector<uint64_t>& ids)
		: m_Ids(ids)
	{
		
	}

	std::vector<uint64_t> m_Ids;
};

class Printer : public boost::static_visitor<>
{
public:
	Printer(std::ostream& os)
		: m_OS(os)
		, m_MaxId(0)
	{
	}

	Printer Lambda()
	{
		Printer prn(m_OS);
		prn.m_MaxId += 2;
		return prn;
	}

	void operator()(uint64_t v)
	{
		m_OS << v << " ";
	}

	void operator()(Id id)
	{
		if(m_MaxId <= id.m_Id)
		{
			throw std::runtime_error("Id too big.");
		}
		m_OS << "x_" << id.m_Id << " ";
	}
	void operator()(const If0& _if)
	{
		m_OS << "(if0 ";
		boost::apply_visitor(*this, _if.m_Cond);
		boost::apply_visitor(*this, _if.m_True);
		boost::apply_visitor(*this, _if.m_False);
		m_OS << ")";
	}
	void operator()(const Fold& fold)
	{
		m_OS << "(fold ";
		boost::apply_visitor(*this, fold.m_Value);
		boost::apply_visitor(*this, fold.m_Accum);
		m_OS << "(lambda(x_" << m_MaxId + 1 << " x_" << m_MaxId + 2 << ")";
		Printer lambda_printer = Lambda();
		boost::apply_visitor(lambda_printer, fold.m_Lambda);
		m_OS << "))";
	}
	template <class T> void operator()(const Op1<T>& op1)
	{
		m_OS << "(" << T().get_id() << " ";
		boost::apply_visitor(*this, op1.m_Op);
		m_OS << ")";
	}
	template <class T> void operator()(const Op2<T>& op2)
	{
		m_OS << "(" << T().get_id() << " ";
		boost::apply_visitor(*this, op2.m_Op1);
		boost::apply_visitor(*this, op2.m_Op2);
		m_OS << ")";
	}
private:
	std::ostream& m_OS;
	size_t m_MaxId;
};

#endif // _EXPR_H_

