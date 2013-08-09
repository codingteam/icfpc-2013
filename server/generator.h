
#ifndef _GENERATOR_H_
#define _GENERATOR_H_

#include <list>

#include "expr.h"

std::list<Expr> Generate(size_t prog_size, Ops ops_set);

#endif // _GENERATOR_H_

