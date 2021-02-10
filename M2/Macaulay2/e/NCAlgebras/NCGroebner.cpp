#include "NCAlgebras/NCGroebner.hpp"

#include "NCAlgebras/FreeAlgebra.hpp"   // for FreeAlgebra
#include "NCAlgebras/FreeMonoid.hpp"    // for FreeMonoid, operator<<, FreeM...
#include "NCAlgebras/NCReduction.hpp"   // for PolynomialHeap, getHeapType
#include "NCAlgebras/OverlapTable.hpp"  // for OverlapTable, operator<<
#include "NCAlgebras/Word.hpp"          // for Word
#include "NCAlgebras/WordTable.hpp"     // for Overlap, WordTable
#include "buffer.hpp"                   // for buffer
#include "engine-exports.h"             // for M2_gbTrace, newline
#include "myalloc.hpp"                  // for operator<<, AllocLogger
#include "ring.hpp"                     // for Ring
#include "ringelem.hpp"                 // for ring_elem
#include "text-io.hpp"                  // for emit_line, emit

#include <deque>                        // for deque
#include <tuple>                        // for get, make_tuple
#include <utility>                      // for pair

NCGroebner::NCGroebner(const FreeAlgebra& A,
                       const ConstPolyList& input,
                       int hardDegreeLimit,
                       int strategy
                       )
  : mFreeAlgebra(A),
    mInput(input),
    mHeap(makePolynomialHeap(getHeapType(strategy), A)),
    mTopComputedDegree(-1),
    mHardDegreeLimit(hardDegreeLimit)
{
  if (M2_gbTrace >= 1)
    {
      buffer o;
      o << "[NCGB] reduction heap:  "
        << getHeapName(getHeapType(strategy))
        << newline;
      emit_line(o.str());
    }
  Word tmpWord;
  // process input polynomials
  mIsGraded = true;
  for (auto i = 0; i < mInput.size(); ++i)
    {
      auto d = freeAlgebra().heft_degree(*mInput[i]);
      mGeneratorDegrees.push_back(d.first);
      if (not d.second)
        mIsGraded = false;
      tmpWord = freeAlgebra().lead_word(*mInput[i]);
      mOverlapTable.insert(d.first, // previously: freeAlgebra().monoid().wordHeft(tmpWord),
                           true,
                           std::make_tuple(i,-1,-1,true));
    }
  if (M2_gbTrace >= 1)
    {
      buffer o;
      o << "[NCGB] input is " << (mIsGraded ? "homogeneous" : "inhomogeneous") << newline;
      emit_line(o.str());
    }
}

void NCGroebner::compute(int softDegreeLimit)
{
  if (mIsGraded)
    computeHomogeneous(softDegreeLimit);
  else
    computeInhomogeneous(softDegreeLimit);
}

void NCGroebner::computeHomogeneous(int softDegreeLimit)
{
  size_t n_spairs = 0;
  while (!mOverlapTable.isFinished(softDegreeLimit))
    {
      auto degSet = mOverlapTable.nextDegreeOverlaps();
      auto toBeProcessed = degSet.second;
      if (M2_gbTrace >= 1)
        {
          buffer o;
          o << "[" << degSet.first << "](" << toBeProcessed->size() << ")";
          emit(o.str());
        }
      while(!toBeProcessed->empty())
        {
          auto overlap = toBeProcessed->front();
          //if this is a `real' overlap, and the overlap is not necessary, then move
          //on to the next overlap.
          if (std::get<1>(overlap) != -1 && !isOverlapNecessary(overlap))
            {
              toBeProcessed->pop_front();
              if (M2_gbTrace >= 3)
                {
                  std::cout << "Reduction avoided using 2nd criterion." << std::endl;
                  std::cout << "table after pop:";
                  mOverlapTable.dump(std::cout,true);
                }
              // TODO: is this logic correct?  Is the overlap actually skipped?
              continue;
            }
          auto overlapPoly = createOverlapPoly(overlap);

          n_spairs++;
          auto redOverlapPoly = twoSidedReduction(overlapPoly);
          delete overlapPoly;

          if (!freeAlgebra().is_zero(*redOverlapPoly))
            {
              addToGroebnerBasis(redOverlapPoly);
              autoreduceByLastElement();
              updateOverlaps(redOverlapPoly);
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
        }
      // remove the lowest degree overlaps from the overlap table
      mOverlapTable.removeLowestDegree();
    }
  if (M2_gbTrace >= 1)
    {
      buffer o;
      o << "[NCGB] number of spair reductions: " << n_spairs;
      emit_line(o.str());
    }
}

void NCGroebner::computeInhomogeneous(int softDegreeLimit)
{
  size_t n_spairs = 0;
  while (!mOverlapTable.isFinished(softDegreeLimit))
    {
      auto degSet = mOverlapTable.nextDegreeOverlaps();
      auto toBeProcessed = degSet.second;
      if (M2_gbTrace >= 1)
        {
          buffer o;
          o << "[" << degSet.first << "](" << toBeProcessed->size() << ")";
          emit(o.str());
        }
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
                  std::cout << "table after pop:";
                  mOverlapTable.dump(std::cout,true);
                }
              // TODO: is this logic correct?  Is the overlap actually skipped?
              continue;
            }
          auto overlapPoly = createOverlapPoly(overlap);

          n_spairs++;
          auto redOverlapPoly = twoSidedReduction(overlapPoly);
          delete overlapPoly;

          if (!freeAlgebra().is_zero(*redOverlapPoly))
            {
              addToGroebnerBasis(redOverlapPoly);
              autoreduceByLastElement();
              updateOverlaps(redOverlapPoly);
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
        }
      // remove the lowest degree overlaps from the overlap table
      mOverlapTable.removeLowestDegree();
    }
  if (M2_gbTrace >= 1)
    {
      buffer o;
      o << "[NCGB] number of spair reductions: " << n_spairs;
      emit_line(o.str());
    }
}

void NCGroebner::addToGroebnerBasis(Poly * toAdd)
{
  // add in auto-reduction here?
  freeAlgebra().makeMonicInPlace(*toAdd);
  mGroebner.push_back(toAdd);
}

void NCGroebner::autoreduceByLastElement()
{
  if (mGroebner.size() <= 1) return;
  const Poly& lastPoly = *(mGroebner[mGroebner.size()-1]);
  const Monom& leadMon = lastPoly.cbegin().monom();
  for (auto fPtr = mGroebner.begin(); fPtr != mGroebner.end() - 1; ++fPtr)
  {
    ring_elem foundCoeff = getCoeffOfMonom(**fPtr,leadMon);
    if (!freeAlgebra().coefficientRing()->is_zero(foundCoeff))
    {
      Poly* result = new Poly;
      freeAlgebra().subtractScalarMultipleOf(*result,**fPtr,lastPoly,foundCoeff);
      freeAlgebra().swap(**fPtr,*result);
    }
  }
}

ring_elem NCGroebner::getCoeffOfMonom(const Poly& f, const Monom& m)
{
  for (auto t = f.cbegin(); t != f.cend(); ++t)
  {
    if (freeAlgebra().monoid().isEqual(t.monom(),m))
      return t.coeff();
  }
  return freeAlgebra().coefficientRing()->zero();
}

void NCGroebner::updateOverlaps(const Poly * toAdd)
{
  std::vector<Overlap> newOverlaps;
  Word newLeadWord = freeAlgebra().lead_word(*toAdd);

  // the word table insert places the right overlaps into newOverlaps
  mWordTable.insert(newLeadWord,newOverlaps);
  insertNewOverlaps(newOverlaps);

  newOverlaps.clear();
  // this function finds the left overlaps with the most recently
  // inserted word.
  mWordTable.leftOverlaps(newOverlaps);
  insertNewOverlaps(newOverlaps);
}

const PolyList& NCGroebner::currentValue() const
{
  //return reinterpret_cast<const ConstPolyList&>(mGroebner);
  return mGroebner;
}

// new version of reduction code which uses a heap structure
auto NCGroebner::twoSidedReduction(const Poly* reducee) const -> Poly*
{
  // stats for benchmarking, debug only...
  size_t loop_count = 0;
  size_t nterms = 0; // number of terms added in to the heap

  // easy access to variables in the class
  const FreeAlgebra& A{ freeAlgebra() };
  const PolyList& reducers{ currentValue() };
  const WordTable& W{ mWordTable };
  //const SuffixTree& W{ mWordTable };
  
  // pair will be (i,j) where the ith word in wordtable appears in word in position j
  std::pair<int,int> subwordPos; 
  Word leftWord, rightWord;

  Poly* remainder = new Poly;
  Poly tmp; // temp polynomial for seeing what is being added to heap.

  mHeap->clear();
  nterms += reducee->numTerms();
  mHeap->addPolynomial(*reducee);

  FreeMonoidLogger::reset();

  while (not mHeap->isZero())
    {
      loop_count++;
      
      // Find (left, right, index) s.t. left*reducers[index]*right == leadMonomial(reduceeSoFar).
      Word reduceeLeadWord;
      std::pair<Monom, ring_elem> LT { mHeap->viewLeadTerm() };

      A.monoid().wordFromMonom(reduceeLeadWord, LT.first);

      if (W.subword(reduceeLeadWord,subwordPos))
        {
          // If there is one, perform reduceeSoFar -= coef * left * reducers[index] * right
          A.monoid().wordPrefixFromMonom(leftWord, LT.first, subwordPos.second);
          A.monoid().wordSuffixFromMonom(rightWord, LT.first, W[subwordPos.first].size()+subwordPos.second);

          ring_elem c = A.coefficientRing()->negate(LT.second);
          ring_elem d = reducers[subwordPos.first]->cbegin().coeff();
          // TODO: Check to see if d is a unit before inverting.
          auto coeffNeeded = A.coefficientRing()->divide(c,d);
          
          A.clear(tmp);
          A.mult_by_term_left_and_right(tmp,
                                        *reducers[subwordPos.first],
                                        coeffNeeded, 
                                        leftWord, 
                                        rightWord);
          nterms += tmp.numTerms();
          mHeap->addPolynomial(tmp);
        }
      else
        {
          // If none, copy that term to the remainder (use add_to_end)
          // and subtract that term
          A.add_to_end(*remainder, LT.second, LT.first);
          mHeap->removeLeadTerm();
        }
    }
  if (M2_gbTrace >= 5)
    {
      std::cout << "reduction: " << "#steps: " << loop_count << " " << FreeMonoidLogger() << std::endl;
      std::cout << "           " << "#terms: " << nterms << std::endl;
      std::cout << "           " << AllocLogger() << std::endl;
    }
  return remainder;
}

auto NCGroebner::twoSidedReduction(const ConstPolyList& reducees) const -> ConstPolyList
{
  ConstPolyList result;
  for (auto i = reducees.cbegin(); i != reducees.cend(); ++i)
    result.push_back(twoSidedReduction(*i));
  return result;
}

auto NCGroebner::initReductionOnly() -> void
{
  // this function clears out mWordTable, places mInput in mGroebner,
  // and builds the word table.
  mWordTable.clear();
  for (auto& f : mInput)
    {
      Poly *fCopy = new Poly;
      freeAlgebra().copy(*fCopy,*f);
      mGroebner.push_back(fCopy);
      auto i = f->cbegin();
      Word tmp;
      freeAlgebra().monoid().wordFromMonom(tmp,i.monom());
      mWordTable.insert(tmp);
    }  
}

auto NCGroebner::createOverlapPoly(const FreeAlgebra& A,
                                   const PolyList& polyList,
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
  prefix = A.lead_word_prefix(*polyList[polyIndex1], overlapIndex);
  suffix = A.lead_word_suffix(*polyList[polyIndex2], *(polyList[polyIndex1]->cbegin().monom().begin()) - A.monoid().numWeights() - 1 - overlapIndex);
  A.mult_by_term_right(tmp1, *polyList[polyIndex1], A.coefficientRing()->from_long(1), suffix);
  A.mult_by_term_left(tmp2, *polyList[polyIndex2], A.coefficientRing()->from_long(1), prefix);
  A.subtract(*result, tmp1, tmp2);
  return result;
}
                          
auto NCGroebner::createOverlapPoly(Overlap overlap) const -> Poly*
{
  if (std::get<1>(overlap) == -1)
    {
      // if we are here, then 'overlap' represents a generator
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
  Word prefix = A.lead_word_prefix(*mGroebner[std::get<0>(o)], std::get<1>(o));
  A.lead_term_as_poly(tmp, *mGroebner[std::get<2>(o)]);
  A.mult_by_term_left(wordAsPoly, tmp, A.coefficientRing()->from_long(1), prefix);
}

// not what we need - need an overlapDegree function
auto NCGroebner::overlapWordLength(Overlap o) const -> int
{
  Word tmp = freeAlgebra().lead_word(*mGroebner[std::get<2>(o)]);
  return std::get<1>(o) + tmp.size();
}

auto NCGroebner::overlapHeft(Overlap o) const -> int
// overlap: of a pair of words v, w, v = a*s, w = s*b, returns the
// heft degree of a*s*b.
// o = triple (index of left GB element, pos, index of right GB element,
//   pos is the location in left GB element where s starts.
{
  Word tmpL = freeAlgebra().lead_word(*mGroebner[std::get<0>(o)]);
  Word tmpR = freeAlgebra().lead_word(*mGroebner[std::get<2>(o)]);
  int len_of_s = tmpL.size() - std::get<1>(o);
  return freeAlgebra().monoid().wordHeft(tmpL) +
    freeAlgebra().monoid().wordHeft(tmpR, len_of_s);
}

auto NCGroebner::printOverlapData(std::ostream& o, Overlap overlap) const -> void
{
  buffer b1,b2;
  freeAlgebra().elem_text_out(b1,*mGroebner[std::get<0>(overlap)], true, true, true);
  freeAlgebra().elem_text_out(b2,*mGroebner[std::get<2>(overlap)], true, true, true);
  o << "Left Poly   : " << b1.str() << std::endl;
  o << "Overlap Pos : " << std::get<1>(overlap) << std::endl;
  o << "Right Poly  : " << b2.str() << std::endl;
}

auto NCGroebner::displayGroebnerBasis(std::ostream& o) const -> void
{
  o << "Current GB:" << std::endl;
  for (auto f : mGroebner)
  {
    buffer b1;
    freeAlgebra().elem_text_out(b1,*f, true, true, true);
    o << b1.str() << std::endl;
  }
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
           mOverlapTable.insert(overlapHeft(newOverlap),
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
  w = A.lead_word(tmp);
  retval = !mWordTable.isNontrivialSuperword(w, std::get<0>(o), std::get<2>(o));
  return retval;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
