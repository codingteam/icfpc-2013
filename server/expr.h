
#ifndef _EXPR_H_
#define _EXPR_H_

#include <stdexcept>
#include <cstdint>
#include <vector>
#include <ostream>

#include <boost/variant.hpp>

class Ops
{
public:
	enum OpsIndex
	{
		NOT = 0,
		SHL1,
		SHR1,
		SHR4,
		SHR16,

		AND,
		OR,
		XOR,
		PLUS,

		IF0,
		TFOLD,
		FOLD,
		max_index
	};

	static const unsigned int FOLD_MASK = ~((1 << TFOLD) | (1 << FOLD));

	Ops()
		: m_Data(0)
	{
	}

	void Set(const char* str);
	void Set(const std::string& str);

	template<OpsIndex O> void inline Set()
	{
		m_Data |= (1 << O);
	}
	
	template<OpsIndex O> bool inline Check() const
	{
		return m_Data & (1 << O);
	}

	Ops operator|(const Ops& lhs) const
	{
		Ops res;
		res.m_Data = m_Data | lhs.m_Data;
		return res;
	}

	bool Cmp(const Ops& lhs) const
	{
		const bool fold = Check<FOLD>() || Check<TFOLD>();
		const bool fold2 = lhs.Check<FOLD>() || lhs.Check<TFOLD>();
		return (fold == fold2) && ((m_Data & FOLD_MASK) == (lhs.m_Data & FOLD_MASK));
	}
private:
	unsigned int m_Data;
};

class Id
{
	friend class Evaluator;
	friend class Printer;
	friend class ProgramSize;
	friend class ProgramInfo;
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

class Op1Base;
template<Ops::OpsIndex O> class Op1;

class Op2Base;
template <Ops::OpsIndex O> class Op2;

typedef boost::variant<uint64_t
	, Id
	, boost::recursive_wrapper<If0>
	, boost::recursive_wrapper<Fold>
	, boost::recursive_wrapper<Op1<Ops::NOT>>
	, boost::recursive_wrapper<Op1<Ops::SHL1>>
	, boost::recursive_wrapper<Op1<Ops::SHR1>>
	, boost::recursive_wrapper<Op1<Ops::SHR4>>
	, boost::recursive_wrapper<Op1<Ops::SHR16>>
	, boost::recursive_wrapper<Op2<Ops::AND>>
	, boost::recursive_wrapper<Op2<Ops::OR>>
	, boost::recursive_wrapper<Op2<Ops::XOR>>
	, boost::recursive_wrapper<Op2<Ops::PLUS>>
	> Expr;

class If0
{
	friend class Evaluator;
	friend class Printer;
	friend class ProgramSize;
	friend class ProgramInfo;
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
	friend class ProgramSize;
	friend class ProgramInfo;
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

class Op1Base
{
	friend class Evaluator;
	friend class Printer;
	friend class ProgramSize;
	friend class ProgramInfo;
public:
	Op1Base(const Expr& expr)
		: m_Op(expr)
	{
	}
private:
	Expr m_Op;
};

template<> class Op1<Ops::NOT> : public Op1Base
{
public:
	Op1(const Expr& expr)
		: Op1Base(expr)
	{
	}

	static uint64_t eval(uint64_t x)
	{
		return ~x;
	}
	static const char* get_id()
	{
		return "not";
	}
};

template<> class Op1<Ops::SHL1> : public Op1Base
{
public:
	Op1(const Expr& expr)
		: Op1Base(expr)
	{
	}

	static uint64_t eval(uint64_t x)
	{
		return x << 1;
	}
	static const char* get_id()
	{
		return "shl1";
	}
};

template<> class Op1<Ops::SHR1> : public Op1Base
{
public:
	Op1(const Expr& expr)
		: Op1Base(expr)
	{
	}

	static uint64_t eval(uint64_t x)
	{
		return x >> 1;
	}
	static const char* get_id()
	{
		return "shr1";
	}
};

template<> class Op1<Ops::SHR4> : public Op1Base
{
public:
	Op1(const Expr& expr)
		: Op1Base(expr)
	{
	}

	static uint64_t eval(uint64_t x)
	{
		return x >> 4;
	}
	static const char* get_id()
	{
		return "shr4";
	}
};

template<> class Op1<Ops::SHR16> : public Op1Base
{
public:
	Op1(const Expr& expr)
		: Op1Base(expr)
	{
	}

	static uint64_t eval(uint64_t x)
	{
		return x >> 16;
	}
	static const char* get_id()
	{
		return "shr16";
	}
};

class Op2Base
{
	friend class Evaluator;
	friend class Printer;
	friend class ProgramSize;
	friend class ProgramInfo;
public:
	Op2Base(const Expr& op1, const Expr& op2)
		: m_Op1(op1)
		, m_Op2(op2)
	{
	}
private:
	Expr m_Op1;
	Expr m_Op2;
};

template <> class Op2<Ops::AND> : public Op2Base
{
public:
	Op2(const Expr& op1, const Expr& op2)
		: Op2Base(op1, op2)
	{
	}
	static uint64_t eval(uint64_t x, uint64_t y)
	{
		return x & y;
	}
	static const char* get_id()
	{
		return "and";
	}
};

template <> class Op2<Ops::OR> : public Op2Base
{
public:
	Op2(const Expr& op1, const Expr& op2)
		: Op2Base(op1, op2)
	{
	}
	static uint64_t eval(uint64_t x, uint64_t y)
	{
		return x | y;
	}
	static const char* get_id()
	{
		return "or";
	}
};

template <> class Op2<Ops::XOR> : public Op2Base
{
public:
	Op2(const Expr& op1, const Expr& op2)
		: Op2Base(op1, op2)
	{
	}
	static uint64_t eval(uint64_t x, uint64_t y)
	{
		return x ^ y;
	}
	static const char* get_id()
	{
		return "xor";
	}
};

template <> class Op2<Ops::PLUS> : public Op2Base
{
public:
	Op2(const Expr& op1, const Expr& op2)
		: Op2Base(op1, op2)
	{
	}
	static uint64_t eval(uint64_t x, uint64_t y)
	{
		return x + y;
	}
	static const char* get_id()
	{
		return "plus";
	}
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
	template <Ops::OpsIndex O> uint64_t operator()(const Op1<O>& op1) const
	{
		return Op1<O>::eval(boost::apply_visitor(*this, op1.m_Op));
	}
	template <Ops::OpsIndex O> uint64_t operator()(const Op2<O>& op2) const
	{
		return Op2<O>::eval(boost::apply_visitor(*this, op2.m_Op1)
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
		if(m_MaxId < id.m_Id)
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
	template <Ops::OpsIndex O> void operator()(const Op1<O>& op1)
	{
		m_OS << "(" << Op1<O>::get_id() << " ";
		boost::apply_visitor(*this, op1.m_Op);
		m_OS << ")";
	}
	template <Ops::OpsIndex O> void operator()(const Op2<O>& op2)
	{
		m_OS << "(" << Op2<O>::get_id() << " ";
		boost::apply_visitor(*this, op2.m_Op1);
		boost::apply_visitor(*this, op2.m_Op2);
		m_OS << ")";
	}
private:
	std::ostream& m_OS;
	size_t m_MaxId;
};

class ProgramSize : public boost::static_visitor<size_t>
{
public:
	size_t operator()(uint64_t) const
	{
		return 1;
	}

	size_t operator()(Id) const
	{
		return 1;
	}
	size_t operator()(const If0& _if) const
	{
		return 1 
			+ boost::apply_visitor(*this, _if.m_Cond)
			+ boost::apply_visitor(*this, _if.m_True)
			+ boost::apply_visitor(*this, _if.m_False);
	}
	size_t operator()(const Fold& fold) const
	{
		return 2
			+ boost::apply_visitor(*this, fold.m_Value)
			+ boost::apply_visitor(*this, fold.m_Accum)
			+ boost::apply_visitor(*this, fold.m_Lambda);
	}
	template <Ops::OpsIndex O> size_t operator()(const Op1<O>& op1) const
	{
		return 1 + boost::apply_visitor(*this, op1.m_Op);
	}
	template <Ops::OpsIndex O> size_t operator()(const Op2<O>& op2) const
	{
		return 2
			+ boost::apply_visitor(*this, op2.m_Op1)
			+ boost::apply_visitor(*this, op2.m_Op2);
	}
};

class ProgramInfo : public boost::static_visitor<std::pair<size_t, Ops>>
{
public:
	std::pair<size_t, Ops> operator()(uint64_t) const
	{
		return std::make_pair(0, Ops());
	}

	std::pair<size_t, Ops> operator()(Id) const
	{
		return std::make_pair(0, Ops());
	}
	std::pair<size_t, Ops> operator()(const If0& _if) const
	{
		const auto e_cond = boost::apply_visitor(*this, _if.m_Cond);
		const auto e_true = boost::apply_visitor(*this, _if.m_True);
		const auto e_false = boost::apply_visitor(*this, _if.m_False);
		Ops if0;
		if0.Set<Ops::IF0>();

		return std::make_pair(e_cond.first + e_true.first + e_false.first
			, if0 | e_cond.second | e_true.second | e_false.second);

	}
	std::pair<size_t, Ops> operator()(const Fold& fold) const
	{
		const auto e_value = boost::apply_visitor(*this, fold.m_Value);
		const auto e_accum = boost::apply_visitor(*this, fold.m_Accum);
		const auto e_lambda = boost::apply_visitor(*this, fold.m_Lambda);
		Ops tfold;
		tfold.Set<Ops::FOLD>();
		tfold.Set<Ops::TFOLD>();

		return std::make_pair(1 + e_value.first + e_accum.first + e_lambda.first
			, tfold | e_value.second | e_accum.second | e_lambda.second);
	}
	template <Ops::OpsIndex O> std::pair<size_t, Ops> operator()(const Op1<O>& op1) const
	{
		const auto e1 = boost::apply_visitor(*this, op1.m_Op);
		Ops _op1;
		_op1.Set<O>();
		return std::make_pair(e1.first, e1.second | _op1);
	}
	template <Ops::OpsIndex O> std::pair<size_t, Ops> operator()(const Op2<O>& op2) const
	{
		const auto e1 = boost::apply_visitor(*this, op2.m_Op1);
		const auto e2 = boost::apply_visitor(*this, op2.m_Op2);
		Ops _op2;
		_op2.Set<O>();
		return std::make_pair(e1.first + e2.first, _op2 | e1.second | e2.second);
	}	
};

#endif // _EXPR_H_

