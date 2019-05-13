#ifndef matrix_ncbasis_hpp_
#define matrix_ncbasis_hpp_

#include <vector>
#include "style.hpp"
#include "NCAlgebras/FreeAlgebra.hpp"

/**
 * \ingroup matrices
 */

PolyList ncBasis(
                 const FreeAlgebra& A,
                 const ConstPolyList& gb, // actually, only the lead terms are ever considered
                 const std::vector<int>& lo_degree, // length 0: means -infinity, i.e. 0.
                 const std::vector<int>& hi_degree // length 0: +infinity
                 ); 

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
