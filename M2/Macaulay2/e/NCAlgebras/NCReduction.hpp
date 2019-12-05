#ifndef __ncreduction_hpp__
#define __ncreduction_hpp__

#include "NCGroebner.hpp"

class PolynomialHeap
{
public:
  virtual ~PolynomialHeap() {}

  virtual PolynomialHeap& addPolynomial(const Poly& poly) = 0;
  
  virtual PolynomialHeap& addPolynomial(ring_elem coeff,
                                        Word left,
                                        Word right,
                                        const Poly& poly) = 0;

  virtual bool isZero() = 0;
  virtual std::pair<ring_elem, Monom> viewLeadTerm() = 0;  // TODO: really want ConstMonom here...
  virtual void removeLeadTerm() = 0;

  virtual Poly* value() = 0; // returns the polynomial
};

enum class HeapTypes {
                      Trivial
};

std::unique_ptr<PolynomialHeap>
makePolynomialHeap(HeapTypes type, const FreeAlgebra& F);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
