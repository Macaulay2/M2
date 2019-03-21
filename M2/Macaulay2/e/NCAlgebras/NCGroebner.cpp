#include "NCGroebner.hpp"

void NCGroebner::compute(int maxdeg)
{
  // TODO
}

const ConstPolyList* NCGroebner::currentValue()
{
  // TODO
  return &mInput;
}

// Heap for reduction? work in progress, to be sure.
#if 0
class NCPolyHeap
{
  using Poly = PolynomialAlgebra::Poly;
  const PolynomialAlgebra& mRing;  // Our elements will be vectors in here
  Poly mHeap[GEOHEAP_SIZE];
  Poly::Iterator mLead[GEOHEAP_SIZE];
  int mTop; // largest index into mHeap which has a polynomial in it.

  bool leadTermComputed;
  ring_elem mLeadCoefficient;
  ConstMonomial mLeadMonomial;
 public:
  NCPolyHeap(const PolynomialAlgebra& F);
  ~NCPolyHeap();

  void add(const Poly& f);

  void subtractMultiple(ring_elem coeff, Word& left, const Poly* g, Word& right);

  bool computeLeadTerm();
  ring_elem leadTermCoefficient();
  ConstMonomial leadTermMonomial();
  
    Poly value();  // Returns the linearized value, and resets the NCPolyHeap.

  ring_elem getValue();
  void add(ring_elem f1);
  
  const Poly& debug_list(int i) const
  {
    return mHeap[i];
  }  // DO NOT USE, except for debugging purposes!
};

// will eventually use this in the reduction code below.
class PolyWithPosition
{
public:
  PolyWithPosition(std::unique_ptr<Poly>(f));
private:
  Poly* mPoly;
  Poly::iterator mLead;
};
#endif

auto NCGroebner::twoSidedReduction(const FreeAlgebra* A,
                                   const Poly* reducee,
                                   const ConstPolyList& reducers,
                                   const WordTable& W) -> const Poly*
{
  // pair will be (i,j) where the ith word in wordtable appears in word in position j
  std::pair<int,int> subwordPos; 
  Poly tmp1,tmp2,reduceeSoFar;
  Poly* remainder = new Poly;
  Word leftWord, rightWord;

  A->copy(reduceeSoFar,*reducee);

  while (!A->is_zero(reduceeSoFar))
    {
      // Find (left, right, index) s.t. left*reducers[index]*right == leadMonomial(reduceeSoFar).
      Word reduceeLM(reduceeSoFar.cbegin().monom().begin()+2,
                     reduceeSoFar.cbegin().monom().end());
      if (W.subword(reduceeLM,subwordPos))
        {
          // If one, perform reduceeSoFar -= coef * left * reducers[index] * right
          A->lead_word_prefix(leftWord, reduceeSoFar, subwordPos.second);
          A->lead_word_suffix(rightWord, reduceeSoFar, W[subwordPos.first].size()+subwordPos.second);
          A->setZero(tmp1);
          A->setZero(tmp2);
          A->mult_by_term_left_and_right(tmp1,
                                         *reducers[subwordPos.first],
                                         reduceeSoFar.cbegin().coeff(),
                                         leftWord,
                                         rightWord);
          A->subtract(tmp2,reduceeSoFar,tmp1);
          A->copy(reduceeSoFar,tmp2); // swap
        }
      else
        {
          // If none, copy that term to the remainder (use add_to_end), and subtract that term
          A->setZero(tmp1);
          A->setZero(tmp2);
          A->lead_term_as_poly(tmp1,reduceeSoFar);
          A->add_to_end(*remainder,tmp1);
          A->subtract(tmp2,reduceeSoFar,tmp1);
          A->copy(reduceeSoFar,tmp2);  // swap
        }
    }
  A->clear(tmp1);
  A->clear(tmp2);
  A->clear(reduceeSoFar);
  return remainder;
}

auto NCGroebner::twoSidedReduction(const PolynomialAlgebra* A,
                       const Poly* reducee,
                       const ConstPolyList& reducers,
                       const WordTable& W) -> const Poly*
{
  return twoSidedReduction(A->freeAlgebra(),reducee,reducers,W);
}


auto NCGroebner::twoSidedReduction(const FreeAlgebra* A,
                                   const ConstPolyList& reducees,
                                   const ConstPolyList& reducers) -> ConstPolyList
{
  WordTable W;
  // Build the word table for the reduction
  for (auto& f : reducers)
    {
      auto i = f->cbegin();
      W.insert(Word(i.monom().begin()+2, i.monom().end()));
    }
  ConstPolyList result;
  for (auto i = reducees.cbegin(); i != reducees.cend(); ++i)
    result.push_back(twoSidedReduction(A, *i, reducers, W));
  return result;
}

auto NCGroebner::twoSidedReduction(const PolynomialAlgebra* A,
                                   const ConstPolyList& reducees,
                                   const ConstPolyList& reducers) -> ConstPolyList
{
  return twoSidedReduction(A->freeAlgebra(),reducees,reducers);
}

auto NCGroebner::createOverlapPoly(const FreeAlgebra* A,
                                   const ConstPolyList& polyList,
                                   int polyIndex1,
                                   int overlapIndex,
                                   int polyIndex2) -> const Poly*
{
  // here, polyIndex1 and 2 are indices into polyList, and overlapIndex
  // is the index where the overlap starts in the polynomial pointed in
  // by *polyIndex1*.
  Poly* result = new Poly;
  Poly tmp1, tmp2;
  Word prefix, suffix;
  A->lead_word_prefix(prefix, *polyList[polyIndex1], overlapIndex);
  A->lead_word_suffix(suffix, *polyList[polyIndex2], *(polyList[polyIndex1]->cbegin().monom().begin()) - 2 - overlapIndex);
  A->mult_by_term_right(tmp1, *polyList[polyIndex1], A->coefficientRing()->from_long(1), suffix);
  A->mult_by_term_left(tmp2, *polyList[polyIndex2], A->coefficientRing()->from_long(1), prefix);
  A->subtract(*result, tmp1, tmp2);
  return result;
}
                          


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
