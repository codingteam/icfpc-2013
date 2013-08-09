
#ifndef _GENERATOR_H_
#define _GENERATOR_H_

#include <list>

#include "expr.h"

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

	Ops()
		: m_Data(0)
	{
	}

	void Set(const char* str);
	void Set(const std::string& str);
	
	template<OpsIndex O> bool Check() const
	{
		return m_Data & (1 << O);
	}
private:
	unsigned int m_Data;
};

std::list<Expr> Generate(size_t prog_size, Ops ops_set);

#endif // _GENERATOR_H_

