/* Copyright 2015, Michael E. Stillman */

#ifndef _res_poly_io_hpp_
#define _res_poly_io_hpp_

#include "res-poly-ring.hpp"
#include <iosfwd>

class ResPolyIO
{
public:
  // Read/write polynomial ring info

  // Read/write a polynomial
  // Format for a polynomial:
  //  #terms
  //  term1 term2 ... termN
  // where
  //    term: coefficient #non-zero exponents v1 e1 ... vr er component
  // Assumptions:
  //  a. each coefficient is non-zero
  //  b. 0 <= v1 < v2 < ... < vr < #vars in R
  //  c. ei > 0
  //  d. the monomials are in descending order in the free module
  //  e. component >= 0, and represents a basis element of this free module.
  static std::istream& readPolynomial(std::istream& i, const ResPolyRing& R, poly& result);
  static std::ostream& writePolynomial(std::ostream& o, const ResPolyRing& R, const poly& result);

  static std::ostream& writeMonomial(std::ostream& o, const ResPolyRing& R, const packed_monomial m);
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
