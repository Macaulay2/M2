/* Copyright 2006 by Michael E. Stillman */
#pragma once

#include "ExponentList.hpp"

// Legacy specialization
using varpower_monomials = ExponentList<long, false>;
using index_varpower_monomial = ExponentListIterator<long, false>;

typedef varpower_monomials::Exponent varpower_word;
typedef varpower_word *varpower_monomial;
typedef const varpower_word *const_varpower_monomial;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
