/* Copyright 2006 by Michael E. Stillman */
#ifndef M2_F4_VARPOWER_MONOMIAL_HPP
#define M2_F4_VARPOWER_MONOMIAL_HPP

#include "monomials/ExponentList.hpp"
#include "f4/monomial-word.hpp"  // for monomial_word

// Legacy specialization
using varpower_monomials = ExponentList<monomial_word, false>;
using index_varpower_monomial = ExponentListIterator<monomial_word, false>;

typedef varpower_monomials::Exponent varpower_word;
typedef varpower_word *varpower_monomial;
typedef const varpower_word *const_varpower_monomial;

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
