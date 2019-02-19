#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "../PolynomialAlgebra.hpp"
#include "WordTable.hpp"

#if 0
  Notes from 19 Feb 2019.
  For polynomial reduction:

  Use PolyWithPos as an Entry
  Use mathic Geobucket code (and/or Heap, TourTree)
    Use code/ideas from ReducerPack from mathicgb. (will need to crib from this code).
    Probably: a hash table for all monomials/words in out polynomials.
    SO: PolyWithPos: array of [ring_elem coeff, pointer to a monomial in the hash table structure, iterator in]
    
#endif


#if 0
  class AugmentedTriple
  {
    int mDegree;
    Triple mTriple;
    // perhaps other info for determining order in Triples structure.
  };

  // Triples: the set of critical "pairs": 2 polynomials often have multiple critical pairs...
  class Triples
  {
  public:
    void insertTriple(AugmentedTriple t); // this might remove other Triple's or even this one!  "Early retirement"
    AugmentedTriple removeMinimalTriple(); // this might also remove some triples, finally return one. "Late retirement"
  };

  class Reductor
  {
  public:
    Reductor(Poly*); // also needs the ring information.

    void subtract(ConstMonomial& left, Poly* gbElement, ConstMonomial& right, Coefficient c);

    // returns true if leadMonom is set, returns false if entire poly represented is zero.
    bool leadMonomial(ConstMonomial& leadMonom); 
  };

  class PolynomialConstructor
  {
  public:
    PolynomialConstructor(); // input should be a ring.
    appendTerm(Coefficient c, ConstMonomial& m);
    appendMonomial(ConstMonomial& m);
    Poly* value();
  };
  // during reduction: structure to keep track of what reductions we have done.

class Reducer
{
public:
  using Poly = PolynomialAlgebra::Poly;
  using PolyList = std::vector<Poly*>;
  using ConstPolyList = std::vector<const Poly*>;

  Reducer(const PolynomialAlgebra* A,
          const ConstPolyList& reducers,
          const WordTable& leadWords)
    : mRing(A),
      mReducers(reducers),
      mLeadWords(leadWords)
  {
  }
  
private:
  const PolynomialAlgebra* mRing;
  const ConstPolyList& mReducers;
  const WordTable& mLeadWords;

};
#endif

class NCGroebner
{
public:
  using Poly = PolynomialAlgebra::Poly;
  using PolyList = std::vector<Poly*>;
  using ConstPolyList = std::vector<const Poly*>;

  NCGroebner(const PolynomialAlgebra* A, const ConstPolyList& input)
    : mRing(A),
      mInput(input),
      mTopComputedDegree(-1)
  {
  }
  
  void compute(int maxdeg);

  const ConstPolyList* currentValue();

  static ConstPolyList twoSidedReduction(const PolynomialAlgebra* A,
                                         const ConstPolyList& reducees,
                                         const ConstPolyList& reducers);

  static auto twoSidedReduction(const PolynomialAlgebra* A,
                                const Poly* reducee,
                                const ConstPolyList& reducers,
                                const WordTable& W) -> const Poly*;

private:
  const PolynomialAlgebra* mRing;
  WordTable mWordTable;
  const ConstPolyList mInput;
  int mTopComputedDegree;

#if 0
  // chose one of these two, or use VECTOR.
  std::vector<Poly*> mGroebner;
  std::vector<Poly> mGroebner2;
#endif
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

