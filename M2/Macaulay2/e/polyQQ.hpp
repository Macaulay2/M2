// Copyright 2005 Michael E. Stillman

#ifndef _polyQQ_hpp_
#define _polyQQ_hpp_

#include "polyring.hpp"

class PolyQQ : public PolynomialRing
{
  // Polynomial rings, possibly a quotient ring, which are 
  // f.g. over QQ.

  // Implementation: (Nterm *numer, mpz_ptr denom)
  
  const PolyRing *ambientR; // Numerator ring, no quotient
                      // This is where any other multiplication 
                      //  such as skew, Weyl, is present.

  // Quotient elements for this ring are present here?
protected:
  virtual ~PolyQQ();
  PolyQQ() {}
public:
  static PolyQQ *create(const PolyRing *P);

  static PolyQQ *create_quotient(const PolyQQ *P, const Matrix *I);
  // I should be be a one row matrix, and a GB in P.
  // These facts are NOT checked.
  // Any quotient elements of P are ignored?

  static PolyQQ *create_quotient(const PolyQQ *P, const PolyQQ *B);
  // B should be a logical coeff ring of P.
  // All quotient elements of B are extended up to P.
  // and a new ring is made.

  virtual const PolyQQ * cast_to_PolyQQ()  const      { return this; }
  virtual       PolyQQ * cast_to_PolyQQ()             { return this; }

  // The implementation coeff ring of 'this'.  This is either a basic ring (field, ZZ), or
  // is another PolyRing.
  virtual const Ring *getCoefficients() const {
    return globalZZ;
  }
  
  // The implementation monoid of this ring.
  virtual const Monoid *getMonoid() const {
    return ambientR->getMonoid();
  }

  // Yields the ambient PolyRing corresponding to this polynomial ring
  // This ring has no quotients, no fractions (not even QQ), but may have
  // skew, weyl, or solvable multiplication, or even (later) be an associative
  // free algebra.
  virtual const PolyRing * getAmbientRing() const {
    return ambientR;
  }

  virtual const RingOrNull *getDenominatorRing() const {
    return globalZZ;
  }

  virtual GBRing *get_gb_ring() const {
    return ambientR->get_gb_ring();
  }

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
