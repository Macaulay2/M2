// TODO: should this be in the interface?

#ifndef matrix_ncbasis_hpp_
#define matrix_ncbasis_hpp_

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
