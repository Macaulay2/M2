#include "NCGroebner.hpp"

void NCGroebner::compute(int softDegreeLimit)
{
  std::vector<Overlap> newOverlaps;
  Word tmpWord;
  
  // process input polynomials first
  for (auto i = 0; i < mInput.size(); i++)
    {
      freeAlgebra().lead_word(tmpWord,*mInput[i]);
      mOverlapTable.insert(tmpWord.size(),
                           true,
                           std::make_tuple(i,-1,-1));
    }
  if (M2_gbTrace >= 2)
    {
      std::cout << "Overlap table after including generators:" << std::endl;
      mOverlapTable.dump(std::cout, true);
    }
  while (!mOverlapTable.isFinished(softDegreeLimit))
    {
      auto toBeProcessed = mOverlapTable.nextDegreeOverlaps().second;
      while(!toBeProcessed->empty())
        {
          auto overlap = toBeProcessed->front();
          auto overlapPoly = createOverlapPoly(overlap);
          auto redOverlapPoly = twoSidedReduction(overlapPoly);
          delete overlapPoly;
          if (!freeAlgebra().is_zero(*redOverlapPoly))
            {
              // if reduction is nonzero
              if (M2_gbTrace >= 2)
                {
                  buffer o;
                  freeAlgebra().elem_text_out(o,*redOverlapPoly,true,true,true);
                  std::cout << o.str() << std::endl;
                  mOverlapTable.dump(std::cout,true);
                }
              freeAlgebra().makeMonicInPlace(*redOverlapPoly);
              if (M2_gbTrace >= 2)
                {
                  buffer o;
                  freeAlgebra().elem_text_out(o,*redOverlapPoly,true,false,false);
                  std::cout << "After makeMonic: " << o.str() << std::endl;
                }

              mGroebner.push_back(redOverlapPoly);

              newOverlaps.clear();
              freeAlgebra().lead_word(tmpWord,*redOverlapPoly);
              mWordTable.insert(tmpWord,newOverlaps);
              for (auto newOverlap : newOverlaps)
                {
                  mOverlapTable.insert(overlapWordLength(newOverlap),
                                       false,
                                       newOverlap);
                }
              newOverlaps.clear();
              mWordTable.leftOverlaps(newOverlaps);
              for (auto newOverlap : newOverlaps)
                {
                  mOverlapTable.insert(overlapWordLength(newOverlap),
                                       false,
                                       newOverlap);
                }
              if (M2_gbTrace >= 2)
                {
                  buffer o;
                  freeAlgebra().elem_text_out(o,*redOverlapPoly,true,true,true);
                  std::cout << o.str() << std::endl;
                  mOverlapTable.dump(std::cout,true);
                }
            }
          else
            {
              // if reduction is zero
            }
          toBeProcessed->pop_front();
          if (M2_gbTrace >= 2)
            {
              std::cout << "table after pop:";
              mOverlapTable.dump(std::cout,true);
            }
        }
      // remove the lowest degree overlaps from the overlap table
      mOverlapTable.removeLowestDegree();
    }
}

const ConstPolyList* NCGroebner::currentValue()
{
  return &mGroebner;
}

auto NCGroebner::twoSidedReduction(const FreeAlgebra* A,
                                   const Poly* reducee,
                                   const ConstPolyList& reducers,
                                   const WordTable& W) -> Poly*
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

auto NCGroebner::twoSidedReduction(const Poly* reducee) const -> Poly*
{
  return twoSidedReduction(& freeAlgebra(),
                           reducee,
                           mGroebner,
                           mWordTable);
}
 
auto NCGroebner::createOverlapPoly(const FreeAlgebra* A,
                                   const ConstPolyList& polyList,
                                   int polyIndex1,
                                   int overlapIndex,
                                   int polyIndex2) -> Poly*
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
                          
auto NCGroebner::createOverlapPoly(Overlap overlap) const -> Poly*
{
  if (std::get<1>(overlap) == -1)
    {
      const Poly* f = mInput[std::get<0>(overlap)];
      Poly * result = new Poly;
      freeAlgebra().copy(*result, *f);
      return result;
    }
  else return createOverlapPoly(& freeAlgebra(),
                                mGroebner,
                                std::get<0>(overlap),
                                std::get<1>(overlap),
                                std::get<2>(overlap));
}

auto NCGroebner::overlapWordLength(Overlap o) const -> int
{
  Word tmp;
  freeAlgebra().lead_word(tmp,*mGroebner[std::get<2>(o)]);
  return std::get<1>(o) + tmp.size();
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

// will eventually use this in the reduction code.
class PolyWithPosition
{
public:
  PolyWithPosition(std::unique_ptr<Poly>(f));
private:
  Poly* mPoly;
  Poly::iterator mLead;
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
