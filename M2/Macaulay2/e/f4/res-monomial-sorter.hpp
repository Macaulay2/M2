/* Copyright 2018, Michael E. Stillman */

#ifndef _res_monomial_sorter_hpp_
#define _res_monomial_sorter_hpp_

#include "monoid.hpp"
#include "ntuple.hpp"
#include "res-moninfo.hpp"
#include "res-schreyer-order.hpp"

#include "memtailor.h"
#include <vector>
#include <utility>
#include <algorithm>

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
    mResMonoid.to_exponent_vector(mon, exp.first, comp);
    mResMonoid.to_exponent_vector(mSchreyerOrder.mTotalMonom[comp], exp2.first, comp2);
    ntuple::mult(nvars, exp.first, exp2.first, exp2.first);
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
    mResMonoid.to_exponent_vector(mon.first, exp.first, comp);
    mResMonoid.to_exponent_vector(mSchreyerOrder.mTotalMonom[comp], exp2.first, comp2);
    ntuple::mult(nvars, exp.first, exp2.first, exp2.first);
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
