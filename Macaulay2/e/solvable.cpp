// Copyright 2003 Michael E. Stillman

#include "solvable.hpp"
#include "gbring.hpp"

SolvableAlgebra::~SolvableAlgebra()
{
  // Nothing to do
}
bool SolvableAlgebra::initialize_solvable(const Matrix *Q)
{
  // TODO
  return true;
}

SolvableAlgebra *SolvableAlgebra::create(const PolynomialRing *R,
					 const Matrix *Q)
{
  // CHECK: R is a polynomial ring, and is commutative.
  SolvableAlgebra *result = new SolvableAlgebra;

  result->initialize_poly_ring(R->Ncoeffs(), R->Nmonoms());
  if (!result->initialize_solvable(Q)) return 0;
  const PolynomialRing *flatR = result->get_flattened_ring();
  result->_gb_ring = GBRing::create_SolvableAlgebra(flatR->Ncoeffs(), 
						    flatR->Nmonoms(), 
						    result);
  return result;
}

ring_elem SolvableAlgebra::imp_mult_by_term(const ring_elem f, 
					       const ring_elem c, 
					       const int *m) const
  // Computes c*m*f, BUT NOT doing normal form wrt a quotient ideal..
{
  // TODO
#warning "implement SolvableAlgebra::imp_mult_by_term"
  return (Nterm*)NULL;
}

ring_elem SolvableAlgebra::power(const ring_elem f, mpz_t n) const
{
  int n1;
  if (ZZ::get_si(n1,n))
    return power(f,n1);
  else 
    {
      ERROR("exponent too large");
      return (Nterm *)NULL;
    }
}

ring_elem SolvableAlgebra::power(const ring_elem f, int n) const
{
  return PolynomialRing::power2(f,n);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
