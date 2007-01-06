// Copyright 2003 Michael E. Stillman

#include "solvable.hpp"
#include "gbring.hpp"

SolvableAlgebra::~SolvableAlgebra()
{
  // Nothing to do
}
bool SolvableAlgebra::initialize_solvable(const Matrix *Q)
{
  Q_ = Q;
  return true;
}

SolvableAlgebra *SolvableAlgebra::create(const Ring *K,
					 const Monoid *M,
					 const Matrix *Q)
{
  SolvableAlgebra *result = new SolvableAlgebra;

  result->initialize_poly_ring(K,M);
  if (!result->initialize_solvable(Q)) return 0;
  result->gb_ring_ = GBRing::create_SolvableAlgebra(K,M,result);
  return result;
}

SolvableAlgebra *SolvableAlgebra::create(const PolynomialRing *R,
					 const Matrix *Q)
{
  return create(R->getCoefficients(),
		R->getMonoid(),
		Q);
}

#if 0
// const SolvableAlgebra *SolvableAlgebra::createPolyRing(const Monoid *M) const
//   // creates this[M], which is commutative in M variables, but skew commutative in
//   // (some of) the variables of this
// {
//   const Monoid *newM = Monoid::tensor_product(M, getMonoid());
//   if (newM == 0) return 0;
// 
//   // Somehow generate a new matrix Q?
//   const Matrix *Q = Q_;
// 
//   return create(getCoefficients(),
// 		newM,
// 		this,
// 		M,
// 		Q);
// }
#endif

ring_elem SolvableAlgebra::mult_by_term(const ring_elem f, 
					       const ring_elem c, 
					       const int *m) const
  // Computes c*m*f, BUT NOT doing normal form wrt a quotient ideal..
{
  // TODO
#ifdef DEVELOPMENT
#warning "implement SolvableAlgebra::mult_by_term"
#endif
  return ZERO_RINGELEM;
}

ring_elem SolvableAlgebra::power(const ring_elem f, mpz_t n) const
{
  int n1;
  if (RingZZ::get_si(n1,n))
    return power(f,n1);
  else 
    {
      ERROR("exponent too large");
      return ZERO_RINGELEM;
    }
}

ring_elem SolvableAlgebra::power(const ring_elem f, int n) const
{
  return Ring::power(f,n);
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
