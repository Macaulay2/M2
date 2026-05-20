/* Copyright 2018, Michael E. Stillman */

#ifndef _res_monomial_sorter_hpp_
#define _res_monomial_sorter_hpp_

/**
 * @file schreyer-resolution/res-monomial-sorter.hpp
 * @brief Schreyer-order column sorters for the F4 resolution Macaulay matrix.
 *
 * Declares the sort kernel called once per `(level, degree)`
 * cell to permute matrix columns into Schreyer order before
 * reduction. `MonomialSorterObject` captures a `Monoid` plus a
 * `vector<int*>` of encoded monomials and exposes
 * `operator()(int a, int b)` as the comparator that walks the
 * encoded order then applies the Schreyer tie-breaker stored
 * in the first slot of each monomial; a static
 * `mNumComparisons` counter tracks comparator calls for
 * profiling.
 *
 * `ResMonomialSorter` is the wrapper the engine calls in
 * practice: it takes the level-1 `ResSchreyerOrder` and a
 * vector of level-side `res_packed_monomial`s, expands each
 * column through the Schreyer "total monomial" composition
 * `var_expvec(column) * var_expvec(orderTotal[comp])` into a
 * `[tiebreaker, basecomp, encoded monomial]` row in an
 * arena-allocated buffer, and runs `std::stable_sort` to
 * return an index permutation. Stable sort preserves relative
 * order among equal monomials, which the tiebreaker relies on.
 * The `#if 0`'d `ResMonomialTransformer` records an earlier
 * API shape kept for reference.
 *
 * @see res-moninfo.hpp
 * @see res-schreyer-order.hpp
 * @see res-monomial-types.hpp
 * @see monoid.hpp
 * @see res-f4-computation.hpp
 */

#include "ExponentVector.hpp"                          // for ntuple
#include "monoid.hpp"                                  // for Monoid
#include "schreyer-resolution/res-moninfo.hpp"         // for ResMonoid
#include "schreyer-resolution/res-schreyer-order.hpp"  // for ResSchreyerOrder
#include "schreyer-resolution/res-monomial-types.hpp"  // for res_packed_mon...
#include "style.hpp"                                   // for GT, LT

#include <memtailor/Arena.h>                           // for Arena
#include <algorithm>                                   // for stable_sort
#include <utility>                                     // for pair
#include <vector>                                      // for vector

/**
 * @brief Strict-weak comparator on integer indices into a `std::vector<int*>`
 * of monomials, used by the resolution code to sort columns.
 *
 * @details Each `mMonoms[i]` is laid out as `[tiebreaker, basecomp,
 * actual_monomial...]`. `operator()(a, b)` compares the monomial
 * bodies via `Monoid::compare`, returning the corresponding
 * `bool`; on `EQ` it falls back to the `tiebreaker` slot. The
 * static `mNumComparisons` counter records how many comparisons
 * the sort performed --- handy when profiling a large frame's
 * column-sort cost.
 */
class MonomialSorterObject
{
private:
  const Monoid& mMonoid;
  const std::vector<int*> mMonoms;
  static long mNumComparisons;
public:
  MonomialSorterObject(const Monoid& M, const std::vector<int*> monoms) : mMonoid(M), mMonoms(monoms) {}

  
  bool operator()(int a, int b)
  {
    // implements < function.  In fact, a and b should not refer to objects that are == under this order.
    // should we flag that?

    mNumComparisons++;
    bool result = false;
    const int* m = mMonoms[a];
    const int* n = mMonoms[b];
    // TODO: make sure this is the order we want!!
    int cmp = mMonoid.compare(m+2, m[1], n+2, n[1]);
    if (cmp == LT) result = false;
    else if (cmp == GT) result = true;
    else
      {
        // compare using tie breaker
        auto cmptie = m[0] - n[0];
        result = (cmptie > 0);
      }
#if 0    
    buffer o;
    o << "comparing: ";
    mMonoid.elem_text_out(o, m+2);
    o << " and ";
    mMonoid.elem_text_out(o, n+2);
    o << " result: " << (result ? "true" : "false");
    emit_line(o.str());
#endif
    return result;
  }

  void resetNumComparisons() { mNumComparisons = 0; }
  long numComparisons() const { return mNumComparisons; }
};

/**
 * @brief Sorter that orders `res_packed_monomial`s by their *total* (Schreyer)
 * monomial, with a stable tiebreaker derived from input order.
 *
 * @details Allocates the `[tiebreaker, basecomp, totalmon]` triples inside
 * its own `memt::Arena` so the sort can run without touching the
 * caller's monomial storage. `mSchreyerOrder` supplies the
 * per-component multiplier that turns each `mColumns[i]` into its
 * total monomial; the result is held in `mMonoms` and the
 * permutation in `mPositions`. Keeps a `mNumComparisons` counter
 * for profiling.
 */
class ResMonomialSorter
{
private:
  const Monoid& mMonoid;
  const ResMonoid& mResMonoid;
  const ResSchreyerOrder& mSchreyerOrder;
  const std::vector<res_packed_monomial>& mColumns;

  long mNumComparisons;
  memt::Arena mArena;
  std::vector<int*> mMonoms; // each monom: [tiebreaker, basecomp, followed by totalmon]
  std::vector<int> mPositions;
public:
  ResMonomialSorter(const Monoid& M,
                   const ResMonoid& resMonoid,
                   const ResSchreyerOrder& S, // order at level-1 in free res
                   const std::vector<res_packed_monomial>& columns // at level.
                   ) :
    mMonoid(M),
    mResMonoid(resMonoid),
    mSchreyerOrder(S),
    mColumns(columns),
    mNumComparisons(0)
  {
  }

  void setMonoms()
  {
    for (int i=0; i<mColumns.size(); i++)
      {
        std::pair<int*, int*> mon = mArena.allocArrayNoCon<int>(mMonoid.monomial_size() + 2);
        toMonomial(mColumns[i], mon);
        mMonoms.push_back(mon.first);
      }
  }

  bool ordered()
  {
    setMonoms();
    MonomialSorterObject C(mMonoid, mMonoms);
    for (int i=1; i<mMonoms.size(); i++)
      {
        if (not C(i-1,i)) return false;
      }
    return true;
  }
  
  std::vector<int> sort()
  {
    setMonoms();
    
    std::vector<int> result;

    for (int i=0; i<mColumns.size(); i++)
      result.push_back(i);

    MonomialSorterObject C(mMonoid, mMonoms);
    C.resetNumComparisons();
    
    std::stable_sort(result.begin(), result.end(), C);

    mNumComparisons = C.numComparisons();
    return result;
  }

  long numComparisons() const { return mNumComparisons; }
private:
  void toMonomial(res_packed_monomial mon, std::pair<int*,int*> resultAlreadyAllocateds)
  {
    int comp, comp2;
    int nvars = mMonoid.n_vars();
    std::pair<int*, int*> exp = mArena.allocArrayNoCon<int>(nvars);
    std::pair<int*, int*> exp2 = mArena.allocArrayNoCon<int>(nvars);
    mResMonoid.to_expvector(mon, exp.first, comp);
    mResMonoid.to_expvector(mSchreyerOrder.mTotalMonom[comp], exp2.first, comp2);
    exponents::mult(nvars, exp.first, exp2.first, exp2.first);
    auto p = resultAlreadyAllocateds.first;
    *p++ = mSchreyerOrder.mTieBreaker[comp];
    *p++ = comp2;
    mMonoid.from_expvector(exp2.first, p);
    mArena.freeTop(exp2.first); // note: can only pop one at a time from an mt::Arena!
    mArena.freeTop(exp.first);
  }
};

#if 0
class ResMonomialTransformer
{
  ResMonomialTransformer(const Monoid& M,
                         const ResMonoid& resM,
                         const ResSchreyerOrder& schreyerOrder,
                         memt::Arena& arena
                         ) :
    mArena(arena),
    mMonoid(M),
    mResMonoid(resM),
    mSchreyerOrder(schreyerOrder)
  {
    // nothing further to do here
  }
                         
public:
  // input: res_packed_monomial range
  // output: (monomial,comp) range: 
  std::pair<int*, int*> transform(std::pair<int*, int*>& input)
  {
    int comp, comp2;
    int nvars = mMonoid.n_vars();
    std::pair<int*, int*> result = mArena.allocArrayNoCon<int>(mMonoid.monomial_size() + 2);
    std::pair<int*, int*> exp = mArena.allocArrayNoCon<int>(nvars);
    std::pair<int*, int*> exp2 = mArena.allocArrayNoCon<int>(nvars);
    mResMonoid.to_expvector(mon.first, exp.first, comp);
    mResMonoid.to_expvector(mSchreyerOrder.mTotalMonom[comp], exp2.first, comp2);
    exponents::mult(nvars, exp.first, exp2.first, exp2.first);
    auto p = resultAlreadyAllocateds.first;
    *p++ = mSchreyerOrder.mTieBreaker[comp];
    *p++ = comp2;
    mMonoid.from_expvector(exp2.first, p);
    mArena.freeTop(exp2.first); // note: can only pop one at a time from an mt::Arena!
    mArena.freeTop(exp.first);
  }
private:
  memt::Arena& mArena;
  const Monoid& mMonoid;
  const ResMonoid& mResMonoid;
  const ResSchreyerOrder& mSchreyerOrder;
};
#endif

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
