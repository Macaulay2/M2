#ifndef _NCGroebner_hpp_
#define _NCGroebner_hpp_

#include "PolynomialAlgebra.hpp"

class KnuthMorrisPratt; // defined in NCGroebner.cpp

#if 0
  class Triple
  {
  public:
    size_t mGroebnerElement1Index; // will be suffix of this one
    size_t mGroebnerElement2Index; // and a prefix of this one
    size_t mOverlapLength; // length of the overlap
  };

  class MonomialLookup
  {
    // ConstMonomial m : m.cbegin(), m.cend(),  *i is the current variable.
    // degree(m).
    // ++i, --i.  Maybe not --i...
    // for (auto v : m) {
    //    v is each variables in turn from the first.
    // }
  public:
    MonomialLookup() {}
    ~MonomialLookup() {}
    
    void insert(ConstMonomial& mon);
    // perhaps: remove() later...

    auto subwords(ConstMonomial& word) -> std::vector<std::tuple<int,int,int>>; // which monomial, index into word, length of monomial.
    //    void subwords(ConstMonomial& word, std::vector<std::tuple<int,int,int>>& matches);

    auto leftOverlaps(ConstMonomial& word) -> std::vector<Triple>;
    auto rightOverlaps(ConstMonomial& word) -> std::vector<Triple>;

    bool isSubword(ConstMonomial& word);
    bool isSuperword(ConstMonomial& word);

    // iterator? const_iterator?
  private:
    size_t mMonomialCount;
    MonomialPool mTipMonomials;
    std::vector<int> mTipMonomials2;
  };

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
private:
  static auto twoSidedReduction(const PolynomialAlgebra* A,
                                const Poly* reducee,
                                const ConstPolyList& reducers,
                                KnuthMorrisPratt& table
                                ) -> Poly*;

private:
  const PolynomialAlgebra* mRing;
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

