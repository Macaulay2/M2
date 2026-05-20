#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

/**
 * @file NCAlgebras/NCGroebner.hpp
 * @brief `NCGroebner` --- Buchberger-style two-sided Gröbner basis driver over a `FreeAlgebra`.
 *
 * Declares the non-commutative analogue of `gbA`: given a list
 * of input polynomials in a `FreeAlgebra`, the class drives a
 * Buchberger-style loop where the role of S-pairs is taken by
 * "overlaps" --- pairs of basis words whose suffix and prefix
 * match. An `OverlapTable` schedules the queue, a `WordTable`
 * (with an experimental `SuffixTree` alternative) indexes
 * leading words for divisor lookup, and the reduction step uses
 * `NCReduction`'s `PolynomialHeap` (a min-heap that fuses many
 * tail-polynomial subtractions, analogous to the commutative
 * engine's `gbvectorHeap`). `computeHomogeneous` and
 * `computeInhomogeneous` carry the sugar-degree variants the
 * `compute(softDegreeLimit)` entry dispatches between.
 *
 * Non-commutative two-sided GBs need not terminate, so
 * construction takes a `hardDegreeLimit` and the algorithm
 * stops respecting it; the resulting (partial) basis is still
 * usable for normal-form work up to that degree. The
 * `tryOutMathicCode` free function is a benchmark hook for an
 * experimental mathic integration and is not part of the
 * production API.
 *
 * @see FreeAlgebra.hpp
 * @see WordTable.hpp
 * @see SuffixTree.hpp
 * @see OverlapTable.hpp
 * @see NCReduction.hpp
 * @see NCF4.hpp
 */

#include "NCAlgebras/NCReduction.hpp"   // for PolynomialHeap
#include "NCAlgebras/OverlapTable.hpp"  // for OverlapTable
#include "NCAlgebras/WordTable.hpp"     // for Overlap, WordTable
#include "NCAlgebras/SuffixTree.hpp"    // for experimental SuffixTree code
#include "Polynomial.hpp"               // for Poly, ConstPolyList
#include "newdelete.hpp"                // for our_new_delete

#include <iostream>                     // for ostream
#include <memory>                       // for unique_ptr
#include <vector>                       // for vector

class FreeAlgebra;

extern void tryOutMathicCode();

/**
 * @brief One-overlap-at-a-time Groebner basis driver for the free associative
 * algebra (the "Naive" companion to the F4-style `NCF4`).
 *
 * @details Keeps the running GB in `mGroebner` (a `PolyList`) with per-element
 * sugar degrees in `mGroebnerDegrees` (-1 marks a retired entry).
 * Overlap pairs are tracked in `mOverlapTable`; divisibility queries
 * use `mWordTable` (with a commented-out `SuffixTree` alternative
 * available as a swap-in). Reduction goes through the lazily owned
 * `mHeap` (a `PolynomialHeap`, see `NCReduction.hpp`), letting the
 * driver pick among the heap backends.
 */
class NCGroebner : public our_new_delete
{
private:
  const FreeAlgebra& mFreeAlgebra;

  WordTable mWordTable;
  //SuffixTree mWordTable;
  OverlapTable mOverlapTable;

  const ConstPolyList mInput;
  std::vector<int> mGeneratorDegrees; // heft degree or sugar degree of corresponding mGroebner element.

  //ConstPolyList mGroebner;
  PolyList mGroebner;
  std::vector<int> mGroebnerDegrees; // sugar degree.  -1 means removed.

  mutable std::unique_ptr<PolynomialHeap> mHeap;

  bool mIsGraded;
  int mTopComputedDegree;
  int mHardDegreeLimit;

public:
  NCGroebner(const FreeAlgebra& A,
             const ConstPolyList& input,
             int hardDegreeLimit,
             int strategy
             );

  const FreeAlgebra& freeAlgebra() const { return mFreeAlgebra; }
  
  void compute(int softDegreeLimit);

  // This function uses sugar degree, and heuristics to deal with non-minimally
  // constructed Groebner basis elements
  void computeInhomogeneous(int softDegreeLimit);
  void computeHomogeneous(int softDegreeLimit);
  
  const PolyList& currentValue() const;

  // old version of reduction code
  auto twoSidedReductionOld(const FreeAlgebra& A,
                            const Poly* reducee,
                            const ConstPolyList& reducers,
                            const WordTable& W) -> Poly*;

  // this is the old version of the SuffixTree reduction code.
  // static auto twoSidedReduction(const FreeAlgebra& A,
  //                               const Poly* reducee,
  //                               const ConstPolyList& reducers,
  //                               const SuffixTree& W) -> Poly*;

  auto twoSidedReduction(const ConstPolyList& reducees) const -> ConstPolyList;

  // this is logically const, but not actually because the heap may be changed by
  // this function.
  auto twoSidedReduction(const Poly* reducee) const -> Poly*;

  void addToGroebnerBasis(Poly* toAdd);

  void autoreduceByLastElement();

  void updateOverlaps(const Poly* toAdd);

  auto initReductionOnly() -> void;
  
  static auto createOverlapPoly(const FreeAlgebra& A,
                                const PolyList& polyList,
                                int polyIndex1,
                                int polyIndex2,
                                int overlapIndex) -> Poly*;
                          
  auto createOverlapPoly(Overlap o) const -> Poly*;
  auto createOverlapLeadWord(Poly& wordAsPoly, Overlap o) const -> void;
  
  auto overlapWordLength(Overlap o) const -> int;
  auto overlapHeft(Overlap o) const -> int;
  
  auto printOverlapData(std::ostream& o, Overlap overlap) const -> void;
  
  auto insertNewOverlaps(std::vector<Overlap>& newOverlaps) -> void;

  auto isOverlapNecessary(Overlap o) const -> bool;

  auto displayGroebnerBasis(std::ostream& o) const -> void;

private:
  // utility functions
  // move to FreeAlgebra?
  ring_elem getCoeffOfMonom(const Poly& f, const Monom& m);

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

