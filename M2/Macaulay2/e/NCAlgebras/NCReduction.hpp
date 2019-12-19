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

  // viewLeadTerm: should only be called if isZero() returns false.
  virtual std::pair<Monom, ring_elem> viewLeadTerm() = 0;

  // removeLeadTerm: should only be called if isZero() returns false.
  virtual void removeLeadTerm() = 0;

  virtual Poly* value() = 0; // returns the polynomial

  virtual size_t getMemoryUsedInBytes() = 0;

  virtual std::string getName() const = 0; // returns the 'type' of the underlying heap structure
};

enum class HeapTypes {
  Trivial,
  Map,
  NaiveGeobucket,
  NaiveHeap,
  NaiveTourTree,
  NaiveDedupGeobucket,
  HashedGeobucket
};

std::unique_ptr<PolynomialHeap>
makePolynomialHeap(HeapTypes type, const FreeAlgebra& F);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
