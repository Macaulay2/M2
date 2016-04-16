/* Copyright 2015, Michael E. Stillman */

#include "stdinc.hpp"
#include "res-poly-io.hpp"

#include <iostream>


std::istream& ResPolyIO::readPolynomial(std::istream& i, const ResPolyRing& R, poly& result)
{
  return i;
}

std::ostream& ResPolyIO::writePolynomial(std::ostream& o, const ResPolyRing& R, const poly& f)
{
  o << f.len << " ";
  for (int i=0; i<f.len; i++)
    o << f.coeffs[i];
  return o;
}

std::ostream& ResPolyIO::writeMonomial(std::ostream& o, const ResPolyRing& R, const packed_monomial m)
{
  int nv = 0;
  for (int i=0; i<R.monoid().numVariables(); i++)
    if (m[i] > 0) nv++;
  o << nv << " ";
  for (int i=0; i<R.monoid().numVariables(); i++)
    if (m[i] > 0)
      {
        o << i << " " << m[i] << " ";
      }
  return o;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions "
// indent-tabs-mode: nil
// End:
