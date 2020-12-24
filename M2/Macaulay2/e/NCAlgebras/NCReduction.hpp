#ifndef __ncreduction_hpp__
#define __ncreduction_hpp__

#include "Polynomial.hpp"  // for Poly, Monom (ptr only)
#include "ringelem.hpp"    // for ring_elem

#include <iosfwd>          // for string
#include <memory>          // for unique_ptr
#include <utility>         // for pair

class FreeAlgebra;
class Word;

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

  virtual void clear() = 0;
  
  virtual std::string getName() const = 0; // returns the 'type' of the underlying heap structure
};

enum class HeapType {
  Trivial,
  Map, // based on std::map
  PriorityQueue, // based on std::priority_queue (with underlying std::vector)
  NaiveGeobucket,
  NaiveHeap,
  NaiveTourTree,
  NaiveDedupGeobucket //,
  // HashedGeobucket
};

HeapType getHeapType(int strategy);
std::string getHeapName(HeapType type);

std::unique_ptr<PolynomialHeap>
makePolynomialHeap(HeapType type, const FreeAlgebra& F);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
