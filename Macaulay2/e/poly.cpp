#include "poly.hpp"
#include "ring.hpp"
#include "monoid.hpp"

PolynomialRing::~PolynomialRing()
{
}

void PolynomialRing::appendQuotientElement(Nterm *f, gbvector *g)
{
  quotient_ideal_.push_back(f);
  quotient_gbvectors_.push_back(g);
}


#if 0
const RRing *PPolynomialRing::findCoefficientRing(const RRing *A) const
{
  // There are two cases: (1) A is a basic ring, (2) A is a poly ring
  // possibly with fractions or quotients.
  const PolyRing *R = getAmbientRing();
  const PPolynomialRing *B = A->cast_to_PPolynomialRing();
  if (B == 0)
    {
      // This means that A is a basic ring
      if (R->getFlatCoefficients() == A) return A;
      return 0;
    }
  const PolyRing *C = B->getAmbientRing(); // This is basically the ring A
  while (R != 0)
    {
      if (C == R) return C;
      const RRing *RC = R->getCoefficients();
      R = RC->cast_to_PolyRing();
    }
  return 0;
}


const PolyRing *PolyRing::create(const RRing *K, const Monoid *M)
  // Create the ring K[M].  
  // K must be either a basic ring, or an ambient polynomial ring,
  //  possibly non-commutative of some sort.
{
  const PPolynomialRing *A = K->cast_to_PPolynomialRing();
  if (A == 0)
    {
      // A is a basic ring
      PolyRing *R = new PolyRing;
      R->initialize(K,M,  K,M);
      return R;
    }
  const PolyRing *B = A->getAmbientRing();
  return B->createPolyRing(M);
}

const PolyRing *PolyRingSkew::createPolyRing(const Monoid *M) const
{
  M2_arrayint skew;
  getSkewInfo(skew);
  int n = M->n_vars();
  M2_arrayint newskew = addScalar(skew, n);
  const Monoid *flatM = Monoid::tensor_product(M, getMonoid());
  
  PolyRingSkew *R = new PolyRingSkew;
  R->initialize(this, M,  getCoefficients(), flatM);
  R->setSkewInfo(newskew);
  return R;
}

const PolyRingSkew *PolyRingSkew::create(const PolyRing *P, 
					 M2_arrayint skewvars)
{
  PolyRingSkew *R = new PolyRingSkew;
  R->initialize(P->getCoefficients(),
		P->getMonoid(),
		P->getFlatCoefficients(),
		P->getFlatMonoid());
  R->setSkewInfo(skewvars);
  return R;
}

const PolyRingWeyl *PolyRingWeyl::create(const PolyRing *P,
					 M2_arrayint derivatives,
					 M2_arrayint commutatives,
					 int homog_var)
{
  PolyRingWeyl *R = new PolyRingWeyl;
  R->initialize(P->getCoefficients(),
		P->getMonoid(),
		P->getFlatCoefficients(),
		P->getFlatMonoid());
  R->setWeylInfo(derivatives, commutatives, homog_var);
  return R;
}

const PolyQuotient *PolyQuotient::create(const Matrix *quotient_gb)
{
  // There are two cases here?
  // If the base is a field, then set Rideal, MonomialTable.
  //   and the normal form routine only considers monomials
  //   and the GB is set to be monic (or assumed so?).
  // If the base is ZZ (the only other case), then we make a MonomialTableZZ,
  //   and use a different normal form.
}
PPolynomialRing *PPolynomialRing::createQuotient(const Matrix *quotients) const
{
  // This depends on the kind of ring
  // (a) PolyRing
  //    Create a PolyQuotient
  // (b) PolyQuotient
  //    Create a new PolyQuotient, with the union of two GB's.
  // (c) PolyFrac
  //    Create a PolyFracQuotient
  // (d) PolyFracQuotient
  //    Create a new PolyFracQuotient, with the union of two GB's.

  return makeQuotientRing(quotients); // virtual call
}

PPolynomialRing *PPolynomialRing::createQuotient(const PPolynomialRing *B) const
{
  // First check that B is a valid quotient of this.
  // Then make the quotient elements in this ring
  return makeQuotientRing(quotients); // virtual call: 
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
