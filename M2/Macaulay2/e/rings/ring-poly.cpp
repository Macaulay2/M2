// Versions of this routine:
//  all versions assume that in(g) | in(f) logically.
//  over finite field: assume that g is monic
//  over Schreyer orders: (f is, g is), (f is, g is in ring)
//  over skew commutative

#include "coeff-ZZp.hpp"
#include "monoms.hpp"
#include "ring-poly.hpp"
template class PolyRing<CoeffZZp,Monomials>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
