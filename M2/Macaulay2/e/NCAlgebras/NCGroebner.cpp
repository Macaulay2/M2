#include "NCGroebner.hpp"

void NCGroebner::compute(int softDegreeLimit)
{
  std::vector<Overlap> newOverlaps;
  Word tmpWord;
  
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
          //if this is a `real' overlap, and the overlap is not necessary, then move
          //on to the next overlap.
          if (std::get<1>(overlap) != -1 && !isOverlapNecessary(overlap))
            {
              toBeProcessed->pop_front();
              if (M2_gbTrace >= 2)
                {
                  std::cout << "Reduction avoided using 2nd criterion." << std::endl;
                  std::cout << "t.able after pop:";
                  mOverlapTable.dump(std::cout,true);
                }
            }
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
              if (M2_gbTrace >= 4)
                {
                  std::cout << "SuffixTree before inserting : " << tmpWord << std::endl;
                  std::cout << mWordTable << std::endl;
                }
              mWordTable.insert(tmpWord,newOverlaps);
              if (M2_gbTrace >= 4)
                {
                  std::cout << "SuffixTree after insert: " << std::endl;
                  std::cout << mWordTable << std::endl;
                }
              
              // TODO: Fix bug here!
              // std::cout << "Here1" << std::endl << std::flush;
              insertNewOverlaps(newOverlaps);
              // std::cout << "Here2" << std::endl << std::flush;

              newOverlaps.clear();
              mWordTable.leftOverlaps(newOverlaps);
              insertNewOverlaps(newOverlaps);

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
              if (M2_gbTrace >= 4)
                {
                  std::cout << "Overlap " << overlap << " reduced to zero."
                            << std::endl;
                }
            }
          toBeProcessed->pop_front();
          if (M2_gbTrace >= 2)
            {
              std::cout << "Table after pop:";
              mOverlapTable.dump(std::cout,true);
            }
        }
      // remove the lowest degree overlaps from the overlap table
      mOverlapTable.removeLowestDegree();
    }
}

const ConstPolyList& NCGroebner::currentValue()
{
  return mGroebner;
}

auto NCGroebner::twoSidedReduction(const FreeAlgebra& A,
                                    const Poly* reducee,
                                    const ConstPolyList& reducers,
                                    const WordTable& W) -> Poly*
// auto NCGroebner::twoSidedReduction(const FreeAlgebra& A,
//                                    const Poly* reducee,
//                                    const ConstPolyList& reducers,
//                                    const SuffixTree& W) -> Poly*
{
  // pair will be (i,j) where the ith word in wordtable appears in word in position j
  std::pair<int,int> subwordPos; 
  Poly tmp1,tmp2,reduceeSoFar;
  Poly* remainder = new Poly;
  Word leftWord, rightWord;

  A.copy(reduceeSoFar,*reducee);

  while (!A.is_zero(reduceeSoFar))
    {
      // Find (left, right, index) s.t. left*reducers[index]*right == leadMonomial(reduceeSoFar).
      Word reduceeLM(reduceeSoFar.cbegin().monom());
      if (W.subword(reduceeLM,subwordPos))
        {
          // If there is one, perform reduceeSoFar -= coef * left * reducers[index] * right
          A.lead_word_prefix(leftWord, reduceeSoFar, subwordPos.second);
          A.lead_word_suffix(rightWord, reduceeSoFar, W[subwordPos.first].size()+subwordPos.second);
          A.setZero(tmp1);
          A.setZero(tmp2);
          auto c = reduceeSoFar.cbegin().coeff();
          auto d = reducers[subwordPos.first]->cbegin().coeff();
          // TODO: Check to see if d is a unit before inverting.
          auto coeffNeeded = A.coefficientRing()->divide(c,d);
          A.mult_by_term_left_and_right(tmp1,
                                         *reducers[subwordPos.first],
                                        coeffNeeded,
                                        //reduceeSoFar.cbegin().coeff(),
                                         leftWord,
                                         rightWord);
          A.subtract(tmp2,reduceeSoFar,tmp1);
          A.swap(reduceeSoFar,tmp2);
        }
      else
        {
          // If none, copy that term to the remainder (use add_to_end), and subtract that term
          A.setZero(tmp1);
          A.setZero(tmp2);
          A.lead_term_as_poly(tmp1,reduceeSoFar);
          A.add_to_end(*remainder,tmp1);
          A.subtract(tmp2,reduceeSoFar,tmp1);
          A.swap(reduceeSoFar,tmp2);
        }
    }
  A.clear(tmp1);
  A.clear(tmp2);
  A.clear(reduceeSoFar);
  return remainder;
}

auto NCGroebner::twoSidedReduction(const FreeAlgebra& A,
                                   const ConstPolyList& reducees,
                                   const ConstPolyList& reducers) -> ConstPolyList
{
  WordTable W;
  //SuffixTree W;
  // Build the word table for the reduction
  for (auto& f : reducers)
    {
      auto i = f->cbegin();
      W.insert(Word(i.monom()));
    }
  ConstPolyList result;
  for (auto i = reducees.cbegin(); i != reducees.cend(); ++i)
    result.push_back(twoSidedReduction(A, *i, reducers, W));
  return result;
}

auto NCGroebner::twoSidedReduction(const Poly* reducee) const -> Poly*
{
  return twoSidedReduction(freeAlgebra(),
                           reducee,
                           mGroebner,
                           mWordTable);
}
 
auto NCGroebner::createOverlapPoly(const FreeAlgebra& A,
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
  A.lead_word_prefix(prefix, *polyList[polyIndex1], overlapIndex);
  A.lead_word_suffix(suffix, *polyList[polyIndex2], *(polyList[polyIndex1]->cbegin().monom().begin()) - 2 - overlapIndex);
  A.mult_by_term_right(tmp1, *polyList[polyIndex1], A.coefficientRing()->from_long(1), suffix);
  A.mult_by_term_left(tmp2, *polyList[polyIndex2], A.coefficientRing()->from_long(1), prefix);
  A.subtract(*result, tmp1, tmp2);
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
  else return createOverlapPoly(freeAlgebra(),
                                mGroebner,
                                std::get<0>(overlap),
                                std::get<1>(overlap),
                                std::get<2>(overlap));
}

auto NCGroebner::createOverlapLeadWord(Poly& wordAsPoly, Overlap o) const -> void
{
  auto A = freeAlgebra();
  Poly tmp;
  Word prefix;
  A.lead_word_prefix(prefix, *mGroebner[std::get<0>(o)], std::get<1>(o));
  A.lead_term_as_poly(tmp, *mGroebner[std::get<2>(o)]);
  A.mult_by_term_left(wordAsPoly, tmp, A.coefficientRing()->from_long(1), prefix);
}

auto NCGroebner::overlapWordLength(Overlap o) const -> int
{
  Word tmp;
  freeAlgebra().lead_word(tmp,*mGroebner[std::get<2>(o)]);
  return std::get<1>(o) + tmp.size();
}

auto NCGroebner::printOverlapData(std::ostream& o, Overlap overlap) const -> void
{
  buffer b1,b2;
  freeAlgebra().elem_text_out(b1,*mGroebner[std::get<0>(overlap)], true, true, true);
  freeAlgebra().elem_text_out(b2,*mGroebner[std::get<2>(overlap)], true, true, true);
  o << "Left Poly   : " << b1.str() << std::endl;
  o << "Overlap Pos : " << std::get<1>(overlap) << std::endl;
  o << "Right Poly  : " << b2.str() << std::endl;
  return;
}

auto NCGroebner::insertNewOverlaps(std::vector<Overlap>& newOverlaps) -> void
{
   for (auto newOverlap : newOverlaps)
     {
       // check to see if the overlap is necessary before insertion
       // FM: not sure if we should do this here, or in the loop.
       // std::cout << "Checking overlap: " << newOverlap << std::endl;
       // printOverlapData(std::cout, newOverlap);
       if (isOverlapNecessary(newOverlap))
         {
           mOverlapTable.insert(overlapWordLength(newOverlap),
                                false,
                                newOverlap);
         }
       else
         {
           if (M2_gbTrace >= 3)
             {
               std::cout << "Reduction avoided using 2nd criterion." << std::endl;
             }
         }
       // std::cout << "Overlap check complete." << std::endl;
     }  
}

auto NCGroebner::isOverlapNecessary(Overlap o) const -> bool
{
  // this function tests if the lead word of the overlap polynomial
  // of o is a multiple of another pattern in the word table.

  // need to be careful, however, since an overlap lead word is trivially
  // a multiple of the words used to build it.  These possibilities must be discarded
  bool retval;

  auto A = freeAlgebra();
  Poly tmp;
  Word w;
  
  createOverlapLeadWord(tmp,o);
  A.lead_word(w,tmp);
  retval = !mWordTable.isNontrivialSuperword(w, std::get<0>(o), std::get<2>(o));
  return retval;
}

// Heap for reduction? work in progress, to be sure.
#if 0
class NCPolyHeap
{
  using Poly = M2FreeAlgebra::Poly;
  const M2FreeAlgebra& mRing;  // Our elements will be vectors in here
  Poly mHeap[GEOHEAP_SIZE];
  Poly::Iterator mLead[GEOHEAP_SIZE];
  int mTop; // largest index into mHeap which has a polynomial in it.

  bool leadTermComputed;
  ring_elem mLeadCoefficient;
  ConstMonomial mLeadMonomial;
 public:
  NCPolyHeap(const M2FreeAlgebra& F);
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
