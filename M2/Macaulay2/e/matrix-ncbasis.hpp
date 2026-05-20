// TODO: should this be in the interface?

#ifndef matrix_ncbasis_hpp_
#define matrix_ncbasis_hpp_

/**
 * @file matrix-ncbasis.hpp
 * @brief `ncBasis` --- non-commutative analogue of `basis(d, M)` over `NCAlgebras/FreeAlgebra`.
 *
 * Declares the entry point `ncBasis(A, gb, lo_degree, hi_degree,
 * limit, result)`. Given a non-commutative free algebra `A`, a
 * Groebner basis `gb` of the defining ideal (only its leading
 * words matter), and a degree range, it enumerates the standard
 * words of the quotient --- words avoiding every leading word in
 * `gb` --- and appends them to the supplied `result` polynomial
 * list. Empty `lo_degree` is `-infinity` and empty `hi_degree`
 * is `+infinity`; `limit` caps the output count (`< 0` disables).
 *
 * The implementation in `matrix-ncbasis.cpp` builds a
 * `NCAlgebras/WordTable` from the leading words and runs a
 * length-bounded breadth-first walk over the non-commutative
 * word monoid, pruning whenever a candidate contains a leading
 * word. The commutative sibling `matrix-kbasis.cpp` lives at the
 * top level of `e/` alongside this file but uses a different
 * monomial-ideal traversal because the commutative monoid is
 * structurally different.
 *
 * @see matrix-kbasis.cpp
 * @see NCAlgebras/FreeAlgebra.hpp
 */

#include <vector>

#include "Polynomial.hpp"

class FreeAlgebra;

/**
 * \ingroup matrices
 */

bool ncBasis(
             const FreeAlgebra& A,
             const ConstPolyList& gb, // actually, only the lead terms are ever considered
             const std::vector<int>& lo_degree, // length 0: means -infinity, i.e. 0.
             const std::vector<int>& hi_degree, // length 0: +infinity
             int limit, // <0 means no limit
             PolyList& result
             ); 

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
