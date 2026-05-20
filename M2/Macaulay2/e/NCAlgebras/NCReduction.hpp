#ifndef __ncreduction_hpp__
#define __ncreduction_hpp__

/**
 * @file NCAlgebras/NCReduction.hpp
 * @brief `PolynomialHeap` abstract interface --- batched-subtraction heap for non-commutative reduction.
 *
 * Declares the pure-virtual surface every `NCGroebner` /
 * `NCF4` reduction uses to combine many tail-polynomial
 * subtractions into a single `O(n log k)` pipeline rather than a
 * quadratic term-by-term merge. `addPolynomial(poly)` and
 * `addPolynomial(coeff, left, right, poly)` queue a polynomial
 * (or its left-coeff-right scaling) for subtraction;
 * `viewLeadTerm` / `removeLeadTerm` give the running result's
 * current leading term and pop it; `value()` materialises the
 * accumulated polynomial when reduction is complete. The
 * `HeapType` enum selects between concrete implementations
 * (`Trivial`, `Map`, `PriorityQueue`, the various geobucket /
 * tournament-tree / dedup variants) which `makePolynomialHeap`
 * instantiates --- kept side-by-side for benchmarking against
 * each other.
 *
 * Non-commutative analogue of the commutative engine's
 * `gbvectorHeap`. `getHeapType(strategy)` maps the user-facing
 * `Strategy =>` integer onto a concrete `HeapType`, and
 * `getName()` on a live heap reports which one is active so
 * profile output is unambiguous.
 *
 * @see Polynomial.hpp
 * @see NCGroebner.hpp
 * @see NCF4.hpp
 * @see FreeAlgebra.hpp
 */

#include "Polynomial.hpp"  // for Poly, Monom (ptr only)
#include "ringelem.hpp"    // for ring_elem

#include <iosfwd>          // for string
#include <memory>          // for unique_ptr
#include <utility>         // for pair

class FreeAlgebra;
class Word;

/**
 * @brief Abstract interface for accumulating a polynomial as a sum of
 * `(coeff, left * poly * right)` contributions in the free algebra.
 *
 * @details The standard "geobucket"-shaped API used by `NCGroebner` / `NCF4`
 * reduction loops: `addPolynomial` slots a new contribution in,
 * `isZero` collapses the heap until either a non-zero lead term is
 * available or the heap is genuinely empty, `viewLeadTerm` /
 * `removeLeadTerm` walk the result, and `value()` materialises
 * everything into a `Poly`. Concrete subclasses
 * (`TrivialPolynomialHeap`, `MapPolynomialHeap`,
 * `PriorityQueuePolynomialHeap`, the geobucket-backed
 * `NaivePolynomialHeap<Queue>`) live in `NCReduction.cpp` and
 * make different storage / deduplication trade-offs.
 */
class PolynomialHeap : public our_new_delete
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
