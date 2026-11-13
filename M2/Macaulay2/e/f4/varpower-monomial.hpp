/* Copyright 2006 by Michael E. Stillman */
#ifndef M2_F4_VARPOWER_MONOMIAL_HPP
#define M2_F4_VARPOWER_MONOMIAL_HPP

#include "monomials/ExponentList.hpp"

// Legacy specialization
using varpower_monomials = ExponentList<long, false>;
using index_varpower_monomial = ExponentListIterator<long, false>;

typedef varpower_monomials::Exponent varpower_word;
typedef varpower_word *varpower_monomial;
typedef const varpower_word *const_varpower_monomial;

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
