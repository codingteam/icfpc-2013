
#include "expr.h"

#include <cstring>

const char* Ops::names[] =
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
		if(strcmp(Ops::names[i], str) == 0)
		{
			m_Data |= (1 << i);
		}
	}
}

void Ops::Set(const std::string& str)
{
	for(size_t i = 0; i < Ops::max_index; ++ i)
	{
		if(str == Ops::names[i])
		{
			m_Data |= (1 << i);
		}
	}
}

