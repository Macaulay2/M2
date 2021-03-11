// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_TYPICAL_REDUCER_GUARD
#define MATHICGB_TYPICAL_REDUCER_GUARD

#include "Reducer.hpp"
#include "Poly.hpp"
#include "PolyRing.hpp"

MATHICGB_NAMESPACE_BEGIN

class SigPolyBasis;
class PolyBasis;

/** Uses the template method pattern (not C++ templates) to implement
  reduction with some steps left to sub-classes.

  The template method pattern defines an algorithm that calls several
  virtual methods. Sub-classes can then redefine those methods to
  change some parts of the algorithm without recoding the whole
  thing. The word "template" here has nothing to do with C++
  templates. See http://en.wikipedia.org/wiki/Template_method_pattern
*/
class TypicalReducer : public Reducer {
public:
  virtual unsigned int preferredSetSize() const;

  virtual std::unique_ptr<Poly> regularReduce(
    ConstMonoRef sig,
    ConstMonoRef multiple,
    size_t basisElement,
    const SigPolyBasis& basis
  );

  virtual std::unique_ptr<Poly> classicReduce
    (const Poly& poly, const PolyBasis& basis);

  virtual std::unique_ptr<Poly> classicTailReduce
    (const Poly& poly, const PolyBasis& basis);

  virtual std::unique_ptr<Poly> classicReduceSPoly
    (const Poly& a, const Poly& b, const PolyBasis& basis);

  virtual void classicReduceSPolySet(
    std::vector<std::pair<size_t, size_t> >& spairs,
    const PolyBasis& basis,
    std::vector<std::unique_ptr<Poly> >& reducedOut
  );

  virtual void classicReducePolySet(
    const std::vector<std::unique_ptr<Poly> >& polys,
    const PolyBasis& basis,
    std::vector<std::unique_ptr<Poly> >& reducedOut
  );

  virtual void setMemoryQuantum(size_t quantum);

protected:
  // These are the methods that sub-classes define in order to carry
  // out sub-steps in the reduction.
  virtual void insertTail(const_term multiplier, const Poly* f) {
    MATHICGB_ASSERT(f != 0);
    NewConstTerm t = {multiplier.coeff, multiplier.monom};
    insertTail(t, *f);
  }

  virtual void insert(monomial multiplier, const Poly* f) {
    MATHICGB_ASSERT(f != 0);
    ConstMonoRef mono = multiplier;
    insert(mono, *f);
  }

  virtual bool leadTerm(const_term& result) {
    NewConstTerm t;
    auto hasLead = leadTerm(t);
    if (hasLead) {
      result.monom = Monoid::toOld(*t.mono);
      result.coeff = t.coef;
    }
    return hasLead;
  }

  virtual void insertTail(NewConstTerm multiplier, const Poly& f) {}
  virtual void insert(ConstMonoRef multiplier, const Poly& f) {}
  virtual bool leadTerm(NewConstTerm& lead) {return false;}

  virtual void removeLeadTerm() = 0;
  virtual void resetReducer() = 0;

  virtual size_t getMemoryUse() const;

  // Sub-classes can use this
  memt::Arena mArena;

private:
  void reset();
  std::unique_ptr<Poly> classicReduce(const PolyBasis& basis);
  std::unique_ptr<Poly> classicReduce
    (std::unique_ptr<Poly> partialResult, const PolyBasis& basis);
};

MATHICGB_NAMESPACE_END
#endif
