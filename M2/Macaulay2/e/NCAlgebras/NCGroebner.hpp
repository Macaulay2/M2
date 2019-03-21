#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "../style.hpp"
#include "../PolynomialAlgebra.hpp"
#include "WordTable.hpp"
#include "OverlapTable.hpp"

class NCGroebner
{
public:

  NCGroebner(const PolynomialAlgebra* A, const ConstPolyList& input)
    : mRing(A),
      mInput(input),
      mTopComputedDegree(-1)
  {
  }
  
  void compute(int maxdeg);

  const ConstPolyList* currentValue();

  static auto twoSidedReduction(const FreeAlgebra* A,
                                         const ConstPolyList& reducees,
                                         const ConstPolyList& reducers) -> ConstPolyList;

  static auto twoSidedReduction(const PolynomialAlgebra* A,
                                         const ConstPolyList& reducees,
                                         const ConstPolyList& reducers) -> ConstPolyList;

  static auto twoSidedReduction(const FreeAlgebra* A,
                                const Poly* reducee,
                                const ConstPolyList& reducers,
                                const WordTable& W) -> const Poly*;

  static auto twoSidedReduction(const PolynomialAlgebra* A,
                                const Poly* reducee,
                                const ConstPolyList& reducers,
                                const WordTable& W) -> const Poly*;

  static auto createOverlapPoly(const FreeAlgebra* A,
                                const ConstPolyList& polyList,
                                int polyIndex1,
                                int polyIndex2,
                                int overlapIndex) -> const Poly*;
                          
  
private:
  const PolynomialAlgebra* mRing;
  WordTable mWordTable;
  const ConstPolyList mInput;
  int mTopComputedDegree;
  PolyList mGroebner;
  
#if 0
  // chose one of these two, or use VECTOR.
  std::vector<Poly*> mGroebner;
  std::vector<Poly> mGroebner2;
#endif
};

#endif

// Notes: 5 March 2019:
// working on naive remainder function.
// might need to refactor monomials, ConstMonomial-->Word.
//   also make a PolynomialALgebra class that is slim, current PolynomialALgebra
//   will be a wrapper on that.
  
//   Notes from 19 Feb 2019.
  
//   For polynomial reduction:

//   Use PolyWithPos as an Entry
//   Use mathic Geobucket code (and/or Heap, TourTree)
//     Use code/ideas from ReducerPack from mathicgb. (will need to crib from this code).
//     Probably: a hash table for all monomials/words in out polynomials.
//     SO: PolyWithPos: array of [ring_elem coeff, pointer to a monomial in the hash table structure, iterator in]

//     PolyWithPos structure
//     add(f, g: PolyWithPos): PolyWithPos;
//     multByTerm(m:Term, f : PolyWithPos, n:Term) : PolyWithPos;
//     remainder(f:PolyWithPos, G:GroebnerList) : PolyWithPos;

//     Poly: will contain the integers defining its monomials
//     PolyHashed: will contain pointers to monomials.
//       PolyHashedWithPos
//     MonomialPool: actual monomials are stored here.
//       add in a new monomial.
//       clear all the monomials.
//       need: hash function for words.
//     MonomialPool: hashtable of monomials
//     Poly: {coeffs: CoefficientArray, monoms: array of pointers}
//     PolyIter{Poly}
//     PolyWithPos: {PolyIter}

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

  auto remainder(const Poly* f) -> Poly*
  {
    Poly* g = mRing->copy(f);
    Poly* remainder = mRing->zero();
    while (not g->is_zero())
      {
        // lookup lead monomial of g in the word table
        // TODO: change ConstMonomial to Word? or WordRef?
        ConstMonomial w { g->leadMonomial() };
        if (mLeadWords->
        // if it matches, compute g -= a*v*h*w, for h in the PolyList.
        mRing->subtractMultipleTo(g, a, v, h, w);

        // if it doesn't match: append lead term of g to remainder, g = g->tail();
        mRing->appendTerm(leadCoefficient(g), g->leadMonomial());
        mRing->removeLeadTerm(g); // destructive to g: remove the first coefficient, and first monomial, from g, requiring copy.
      }
    return remainder;
  }
    
private:
  const PolynomialAlgebra* mRing;
  const ConstPolyList& mReducers;
  const WordTable& mLeadWords;
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:

