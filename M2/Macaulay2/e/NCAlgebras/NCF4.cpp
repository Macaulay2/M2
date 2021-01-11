#include "NCAlgebras/NCF4.hpp"

#include "text-io.hpp"                   // for emit_wrapped
#include "NCAlgebras/FreeAlgebra.hpp"       // for FreeAlgebra
#include "NCAlgebras/OverlapTable.hpp"      // for OverlapTable
#include "NCAlgebras/VectorArithmetic.hpp"  // for VectorArithmetic
#include "VectorArithmetic2.hpp"            // for VectorArithmetic2
#include "NCAlgebras/WordTable.hpp"         // for Overlap, WordTable
#include "buffer.hpp"                       // for buffer
#include "engine-exports.h"                 // for M2_gbTrace
#include "ring.hpp"                         // for Ring
#include "ringelem.hpp"                     // for ring_elem

#include <cassert>                          // for assert
#include <cstdlib>                          // for exit, size_t
#include <algorithm>                        // for copy
#include <iostream>                         // for operator<<, basic_ostream

#include <tbb/tbb.h>                        // for tbb 
#include <tbb/enumerable_thread_specific.h> // for enumerable_thread_specific
#include <tbb/combinable.h>                 // for combinable
#include <tbb/spin_rw_mutex.h>

NCF4::NCF4(const FreeAlgebra& A,
           const ConstPolyList& input,
           int hardDegreeLimit,
           int strategy
           )
    : mFreeAlgebra(A),
      mInput(input),
      mTopComputedDegree(-1),
      mHardDegreeLimit(hardDegreeLimit),
      mMonomEq(A.monoid()),
      mMonomHashEqual(A.monoid()),
      mColumnMonomials(10,mMonomHash,mMonomHashEqual),
      mPreviousColumnMonomials(10,mMonomHash,mMonomHashEqual),
      mVectorArithmetic(vectorArithmetic2(A.coefficientRing()))
{
  if (M2_gbTrace >= 1)
    {
      buffer o;
      o << "[NCGB F4]";
      emit_wrapped(o.str());
    }
  
  // process input polynomials
  mIsGraded = true;
  for (int i = 0; i < mInput.size(); ++i)
    {
      auto d = freeAlgebra().heft_degree(*mInput[i]);
      if (not d.second)
        mIsGraded = false;
      mOverlapTable.insert(d.first,
                           true,
                           std::make_tuple(i,-1,-1));
    }
  if (M2_gbTrace >= 1)
    {
      buffer o;
      o << (mIsGraded ? " homogeneous " : " inhomogeneous ");
      emit_wrapped(o.str());
    }
  if (mVectorArithmetic == nullptr) std::cout << "OOoooooops" << std::endl;
}

void NCF4::compute(int softDegreeLimit)
{
  while (!mOverlapTable.isFinished(softDegreeLimit))
    {
      auto degSet = mOverlapTable.nextDegreeOverlaps();
      auto toBeProcessed = degSet.second;
      if (M2_gbTrace >= 1)
        {
          buffer o;
          o << "{" << degSet.first << "}(" << toBeProcessed->size() << ")";
          emit_wrapped(o.str());
        }
      process(*toBeProcessed);
      mOverlapTable.removeLowestDegree(); // TODO: suspect line.
      // we really want to just delete toBeProcessed...
    }
}

void NCF4::process(const std::deque<Overlap>& overlapsToProcess)
{
#if 0  
  if (M2_gbTrace >= 2)
    {
      buffer o;
      o << newline << "F4 processing: # overlaps (spairs): " << overlapsToProcess.size() << newline;
      emit(o.str());
    }
#endif
  
  // build the F4 matrix
  buildF4Matrix(overlapsToProcess);

  if (M2_gbTrace >= 2)
    {
      std::cout << "NC F4 GB: matrix size: ";
      displayF4MatrixSize(std::cout);
    }
  if (M2_gbTrace >= 200) displayFullF4Matrix(std::cout);

  // sort the columns so that the matrix is `upper triangular'
  sortF4Matrix();

  if (M2_gbTrace >= 100) displayFullF4Matrix(std::cout);
  else if (M2_gbTrace >= 50) displayF4Matrix(std::cout);

  // reduce the matrix
  reduceF4Matrix();
  //parallelReduceF4Matrix();

  if (M2_gbTrace >= 2)
    {
      std::cout << "NC F4 GB: matrix size: ";
      displayF4MatrixSize(std::cout);
    }

  if (M2_gbTrace >= 100) displayFullF4Matrix(std::cout);
  else if (M2_gbTrace >= 50) displayF4Matrix(std::cout);

  std::cout << "Entering: " << mColumnMonomials.size() << std::endl;
  // convert back to GB elements...
  PolyList newElems = newGBelements();

  for (auto& f : newElems)
    {
      addToGroebnerBasis(f);
      updateOverlaps(f);
    }

  std::cout << "Entering: " << mColumnMonomials.size() << std::endl;

  // prepare reduced matrix for use in the next degree
  processPreviousF4Matrix();
  
#if 0
  if (M2_gbTrace >= 2)
    {
      buffer o;
      o << "F4 processing: # GB added: " << newElems.size() << newline;
      o << "F4 processing: # GB total: " << mGroebner.size() << newline;
      emit(o.str());
    }
#endif

}

void NCF4::processPreviousF4Matrix()
{
  // if (M2_gbTrace >= 2)
  // {
  //   // output some statistics
  //   // the hash is good -- it seems to have a max_load_factor of 1
  //   std::cout << "Monomial Hash Max Load : " << mColumnMonomials.max_load_factor() << std::endl;
  // }
  
  // flag the columns correspond to lead terms of new GB elements as reducers
  for (int i = mFirstOverlap; i < mRows.size(); ++i)
  {
    if (mRows[i].second.size() == 0) continue;
    int newReducerCol = mRows[i].second[0];
    mColumns[newReducerCol].second = i;
    mPreviousColumnMonomials[mColumns[newReducerCol].first].second = i;
  } 
  
  // copy the finished rows and columns into the holding areas
  mPreviousRows.clear();
  mPreviousColumns.clear();
  mPreviousColumnMonomials.clear();
  mPreviousMonomialSpace.deallocateAll();
  mPreviousRows = std::move(mRows);
  mPreviousColumns = std::move(mColumns); 
  mPreviousColumnMonomials = std::move(mColumnMonomials);
  // need to move mMonomialSpace to a holding area since all the monomials
  // and int arrays in mPrevious data types are allocated there.
  mPreviousMonomialSpace.swap(mMonomialSpace);
}

void NCF4::addToGroebnerBasis(Poly * toAdd)
{
  mGroebner.push_back(toAdd);
}

void NCF4::updateOverlaps(const Poly * toAdd)
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

auto NCF4::overlapHeft(Overlap o) const -> int
// overlap: of a pair of words v, w, v = a*s, w = s*b, returns the
// heft degree of a*s*b.
// o = triple (index of left GB element, pos, index of right GB element,
//   pos is the location in left GB element where s starts.
{
  Word tmpL = mWordTable[std::get<0>(o)];
  Word tmpR = mWordTable[std::get<2>(o)];
  int len_of_s = tmpL.size() - std::get<1>(o);
  return freeAlgebra().monoid().wordHeft(tmpL) +
    freeAlgebra().monoid().wordHeft(tmpR, len_of_s);
}

auto NCF4::insertNewOverlaps(std::vector<Overlap>& newOverlaps) -> void
{
   for (auto newOverlap : newOverlaps)
   {
     if (std::get<1>(newOverlap) != -1 && !isOverlapNecessary(newOverlap))
       {
         if (M2_gbTrace >= 3)
           std::cout << "Reduction avoided using 2nd criterion." << std::endl;
         // TODO: is this logic correct?  Is the overlap actually skipped?
         continue;
       }
     mOverlapTable.insert(overlapHeft(newOverlap),
                          false,
                          newOverlap);
   }
}

auto NCF4::isOverlapNecessary(Overlap o) -> bool
{
  // this function tests if the lead word of the overlap polynomial
  // of o is a multiple of another pattern in the word table.

  // need to be careful, however, since an overlap lead word is trivially
  // a multiple of the words used to build it.  These possibilities must be discarded
  bool retval;
  Word w;
  
  w = createOverlapLeadWord(o);
  retval = !mWordTable.isNontrivialSuperword(w, std::get<0>(o), std::get<2>(o));
  return retval;
}

Word NCF4::createOverlapLeadWord(Overlap o) 
{
  // this function adds the return value to mMemoryBlock, so should only be used
  // when running a GB since it will be subsequently cleared.
  Word left(mWordTable[std::get<0>(o)].begin(),
            mWordTable[std::get<0>(o)].begin()+std::get<1>(o));
  Word right = mWordTable[std::get<2>(o)];
  int sz = left.size() + right.size();
  auto rg = mMonomialSpace.allocateArray<int>(sz);
  std::copy(left.begin(),left.end(),rg.first);
  std::copy(right.begin(),right.end(),rg.first+left.size());
  return Word(rg.first, rg.second);
}

PolyList NCF4::newGBelements() const // From current F4 matrix.
{
  PolyList result;
  for (int i = mFirstOverlap; i < mRows.size(); i++)
    {
      if (mRows[i].second.size() == 0) continue;
      Poly* f = new Poly;
      reducedRowToPoly(f,mRows,mColumns,i);
      result.push_back(f);
    }
  return result;
}

void NCF4::reducedRowToPoly(Poly* result,
                            const std::vector<Row>& rows,
                            const std::vector<Column>& cols,
                            int i) const
{
  // this function places the elements of the ith row of the
  // (reduced!) F4 matrix in result.  This assumes that the first
  // term of the ith row is after the last entry of result (or that result is empty).
  auto resultCoeffInserter = result->getCoeffInserter();
  auto resultMonomInserter = result->getMonomInserter();
  mVectorArithmetic->appendSparseVectorToContainer(rows[i].first,resultCoeffInserter);
  
  for (const auto& col : rows[i].second)
    resultMonomInserter.insert(resultMonomInserter.end(),
                               cols[col].first.begin(),
                               cols[col].first.end());
}

ring_elem NCF4::getCoeffOfMonom(const Poly& f, const Monom& m)
{
  for (auto t = f.cbegin(); t != f.cend(); ++t)
  {
    if (freeAlgebra().monoid().isEqual(t.monom(),m))
      return t.coeff();
  }
  return freeAlgebra().coefficientRing()->zero();
}

void NCF4::autoreduceByLastElement()
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

void NCF4::matrixReset()
{
  mReducersTodo.clear();
  mOverlapsTodo.clear();
  mColumns.clear();
  mColumnMonomials.clear();
  mRows.clear();
  mOverlaps.clear();
  mFirstOverlap = 0;
  mMonomialSpace.deallocateAll(); 
}

void NCF4::preRowsFromOverlap(const Overlap& o)
{
  // o = (gbLeftIndex, overLapPos, gbRightIndex).
  // BUT: if overlapPos < 0, then gbRightIndex is also < 0 (and is ignored), and gbLeftIndex
  //   refers to a generator, and we add to mOverlapsTodo in only (1,gbLeftIndex,1).
  // where 1 = empty word.

  int gbLeftIndex = std::get<0>(o);
  int overlapPos = std::get<1>(o);
  int gbRightIndex = std::get<2>(o);

  if (overlapPos < 0)
    {
      // Sneaky trick: a PreRow with index a < 0 refers to generator with index -a-1
      mOverlapsTodo.emplace_back(PreRow(Word(), - gbLeftIndex - 1, Word(), false));
      return;
    }
  
  // LM(gbLeft) = x^a x^b
  // LM(gbRight) = x^b x^c
  // overlapPos = starting position of x^b in gbLeft.
  // one prerow will be: (1, gbLeftIndex, x^c)
  // another prerow will be: (x^a, gbRightIndex, 1)
      
  Word leadWordLeft = mWordTable[gbLeftIndex];
  Word leadWordRight = mWordTable[gbRightIndex];
  int overlapLen = leadWordLeft.size() - overlapPos;

  Word suffix2 {}; // trivial word
  Word prefix2(leadWordLeft.begin(), leadWordLeft.begin() + overlapPos);
  
  Word suffix1(leadWordRight.begin() + overlapLen, leadWordRight.end());
  Word prefix1 {}; // trivial word
  
  // need to add in the lead monomial to the mColumnMonomials list now
  // so they know which row reduces them
  Monom newmon = freeAlgebra().monoid().wordProductAsMonom(leadWordLeft, suffix1, mMonomialSpace);

  // This overlap may already have lead term in table.
  // Only have to add it in if it is not yet present.
  auto it = mColumnMonomials.find(newmon);
  if (it == mColumnMonomials.cend())
    mColumnMonomials.emplace(newmon, std::make_pair(mColumnMonomials.size(), mReducersTodo.size()));

  // it *matters* which one is a reducer and which one is an overlap.
  // this is due to how the word table lookup works -- it searches them
  // in the order that they were entered into the word table, which may
  // not be sorted in term order.
  if (gbLeftIndex > gbRightIndex)
    {
      mReducersTodo.emplace_back(PreRow(prefix2, gbRightIndex, suffix2, false));
      mOverlapsTodo.emplace_back(PreRow(prefix1, gbLeftIndex, suffix1, false));
    }
  else
    {  
      mReducersTodo.emplace_back(PreRow(prefix1, gbLeftIndex, suffix1, false));
      mOverlapsTodo.emplace_back(PreRow(prefix2, gbRightIndex, suffix2, false));
    }
}

void NCF4::buildF4Matrix(const std::deque<Overlap>& overlapsToProcess)
{
  matrixReset();

  for (auto o : overlapsToProcess)
    {
      preRowsFromOverlap(o);
    }

  // can't do this loop as a range-based for loop since we are adding to it
  // during the for loop
  // process each element in mReducersTodo
  for (int i=0 ; i < mReducersTodo.size(); ++i)
  {
    mRows.emplace_back(processPreRow(mReducersTodo[i])); // this often adds new elements to mReducersTodo
  }
  
  int numReducersAtFirst = mReducersTodo.size();
  
  for (auto over : mOverlapsTodo)
  {
    mOverlaps.emplace_back(processPreRow(over));  // this often adds new elements to mReducersTodo
  }

  // can't do this loop as a range-based for loop since we are adding
  // to mReducersTodo during the for loop
  for (int i=numReducersAtFirst ; i < mReducersTodo.size(); ++i)
  {
    mRows.emplace_back(processPreRow(mReducersTodo[i])); // this often adds new elements to mReducersTodo
  }

  // Now we move the overlaps into mRows, and set mFirstOverlap.
  // double range, so use a for loop
  mFirstOverlap = mRows.size();
  for (int i=0; i < mOverlapsTodo.size(); ++i)
    {
      mRows.emplace_back(mOverlaps[i]);
      mReducersTodo.emplace_back(mOverlapsTodo[i]);
    }
}

std::pair<bool,int> NCF4::findPreviousReducerPrefix(const Monom& m)
{
  // build the largest proper prefix of m and look it up in previous
  // mColumnMonomials table.  we will return (true, row index) if
  // found and (false, -1) if not.

  // note that just because the second coordinate of an
  // mColumnMonomials entry is -1 does not mean there isn't a reducer
  // in the row -- in this case it is a new GB element and already
  // added to the mGroebner list.  In this case, we will be using that
  // entry anyway so no need to make the additional effort to find it
  // in the mRows table.

  // get prefix of m as a monom
  // search for prefix in mPreviousColumnMonomials
  // if it.second.second != -1, then return (true,it.second.first)
  // else return (false, -1)

  // if the monom is the empty monomial, return false
  if (freeAlgebra().monoid().is_one(m)) return std::make_pair(false,-1);
  
  std::vector<int> prefixInserter;
  std::pair<bool,int> retval;
  
  freeAlgebra().monoid().monomPrefixFromMonom(prefixInserter,m,1);
  Monom prefixM(prefixInserter.data());
  auto it = mPreviousColumnMonomials.find(prefixM);
  if (it == mPreviousColumnMonomials.end()) // not in table
    retval = std::make_pair(false,-1);
  else
  {
    int colNum = (*it).second.first;
    if (mPreviousColumns[colNum].second == -1)  // in column table and not a reducer monomial
      retval = std::make_pair(false,-1);
    else  // in table and a reducer monomial
      retval = std::make_pair(true,mPreviousColumns[colNum].second);
  }
  return retval;
}


std::pair<bool,int> NCF4::findPreviousReducerSuffix(const Monom& m)
{
  // basically the same as above, except with suffixes

  // get suffix of m
  // search for suffix in mPreviousColumnMonomials
  // if it.second.second != -1, then return (true,it.second.first)
  // else return (false, -1)
  if (freeAlgebra().monoid().is_one(m)) return std::make_pair(false,-1);
  
  std::vector<int> suffixInserter;

  std::pair<bool,int> retval;
  
  freeAlgebra().monoid().monomSuffixFromMonom(suffixInserter,m,1);
  Monom suffixM(suffixInserter.data());
  auto it = mPreviousColumnMonomials.find(suffixM);
  if (it == mPreviousColumnMonomials.end()) //not in table
    retval = std::make_pair(false,-1);
  else
  {
    int colNum = (*it).second.first;
    if (mPreviousColumns[colNum].second == -1)  // in column table and not a reducer monomial
      retval = std::make_pair(false,-1);
    else  // in table and a reducer monomial
      retval = std::make_pair(true,mPreviousColumns[colNum].second);
  }
  return retval;
}

void NCF4::processMonomInPreRow(Monom& m, int* nextcolloc) 
{
  auto it = mColumnMonomials.find(m);
  if (it == mColumnMonomials.end())
    { 
      // if we are here, this monomial is not yet accounted for in the matrix
      // so, check if the monomial is a multiple of a lead term of a gb element
      auto divresult = findDivisor(m);
      int divisornum = -1; // -1 indicates no divisor was found
      if (divresult.first)
        {
          // if it is a divisor of a gb element (or a left/right
          // variable multiple of a previous reducer), add that
          // multiple of the GB element to mReducersToDo for
          // processing
          divisornum = mReducersTodo.size();
          mReducersTodo.push_back(divresult.second);
        }
      // insert column information into mColumnMonomials,
      // which provides us search/indexing of columns
      int newColumnIndex = mColumnMonomials.size();
      mColumnMonomials.emplace(m, std::make_pair(newColumnIndex, divisornum));
      *nextcolloc = newColumnIndex;
    }
  else
    {
      // in this case, the monomial already has an associated column
      *nextcolloc = (*it).second.first;
    }
}

NCF4::Row NCF4::processPreRow(PreRow r)
{
  // note: left and right should be the empty word if gbIndex < 0 indicating
  // an input polynomial.
  Word left = std::get<0>(r);
  int gbIndex = std::get<1>(r);
  Word right = std::get<2>(r);
  bool prevReducer = std::get<3>(r);

  if (M2_gbTrace >= 100) 
    std::cout << "Processing PreRow: ("
              << left << "," << gbIndex << "," << right << ")"
              << std::endl;

  // construct the elem corresponding to this prerow
  // it will either be:
  //  a multiple of a previously reduced row (if prevReducer)
  //  an element of the input (if gbIndex < 0 and not prevReducer)
  //  a multiple of a gb element (if gbIndex >= 0 and not prevReducer)
  const Poly* elem;
  if (prevReducer)
  {
    // in this case, we construct the poly locally for processing in this
    // function.  We will destroy it at the end, as the new monomials for reduction
    // are created and inserted into mMonomialSpace
    Poly* tempelem = new Poly;
    reducedRowToPoly(tempelem,mPreviousRows,mPreviousColumns,gbIndex);
    elem = tempelem;
  }
  else
  {
    if (gbIndex < 0)
      elem = mInput[-gbIndex-1];
    else
      elem = mGroebner[gbIndex];
  }

  // loop through all monomials of the product
  // for each monomial:
  //  (the below steps are now in processMonomInPreRow)
  //  is its prefix or suffix the lead term of a row from the previous degree?
  //    if so: insert the information from this row in the appropriate places
  //    if not: is the monomial in the hash table?
  //      if so: return the column index into the component for the new row.
  //      if not: insert it, and return the new column index into same place
  //          and place this monomial into mColumns.
  //          and search for divisor for it.
  //        
  int nterms = elem->numTerms();
  auto componentRange = mMonomialSpace.allocateArray<int>(nterms);
  int* nextcolloc = componentRange.first;
  for (auto i = elem->cbegin(); i != elem->cend(); ++i)
    {
      Monom m = freeAlgebra().monoid().wordProductAsMonom(left,i.monom(),right,mMonomialSpace);
      processMonomInPreRow(m,nextcolloc);
      nextcolloc++;
    }
  CoefficientVector2 coeffs = mVectorArithmetic->sparseVectorFromContainer(elem->getCoeffVector());

  // delete the Poly created for prevReducer case, if necessary.
  if (prevReducer) delete elem;

  return(Row(coeffs, componentRange));
}

// this function is meant for debugging only
// prerows should not be inserted twice
int NCF4::prerowInReducersTodo(PreRow pr) const
{
  int retval = -1;
  for (int i = 0; i < mReducersTodo.size(); i++)
    {
      if (pr == mReducersTodo[i])
        {
          retval = i;
          break;
        }
    }
  return retval;
}

static long findDivisorCalls = 0;
static long previousPrefixesFound = 0;
static long previousSuffixesFound = 0;

std::pair<bool, NCF4::PreRow> NCF4::findDivisor(Monom mon)
{
  Word newword;

  findDivisorCalls++;
  if (M2_gbTrace >= 2 && findDivisorCalls % 100000 == 0)
     std::cout << "Total findDivisorCalls      : " << findDivisorCalls << std::endl
               << "  Previous prefixes found   : " << previousPrefixesFound << std::endl
               << "  Previous suffixes found   : " << previousSuffixesFound << std::endl       
               << "  Elements in Previous Rows : " << mPreviousRows.size() << std::endl; 
  // look in previous F4 matrix for Monom before checking mWordTable

  auto usePreviousSuffix = findPreviousReducerSuffix(mon);
  if (usePreviousSuffix.first)
    {
      // here, we use a multiple of a previously reduced row
      previousSuffixesFound++;
      Word tmpWord = freeAlgebra().monoid().firstVar(mon);
      return std::make_pair(true, PreRow(tmpWord,
                                         usePreviousSuffix.second,
                                         Word(),
                                         true));
    }
  auto usePreviousPrefix = findPreviousReducerPrefix(mon);
  if (usePreviousPrefix.first)
    {
      // here, we use a multiple of a previously reduced row
      previousPrefixesFound++;
      Word tmpWord = freeAlgebra().monoid().lastVar(mon);
      return std::make_pair(true, PreRow(Word(),
                                         usePreviousPrefix.second,
                                         tmpWord,
                                         true));
    }

  // if we are here, then the Monom does not have a prefix/suffix
  // that was processed previously.

  // look in mWordTable for Monom
  freeAlgebra().monoid().wordFromMonom(newword, mon);
  std::pair<int,int> divisorInfo;
  
  // TODO: for certain inputs (e.g. Monoms that we *know* are of
  //       the form xm or mx for m a standard monomial and x a
  //       variable, one needs only to check prefixes or
  //       suffixes, not all subwords.  Not sure how to utilize that
  //       just yet.

  bool found = mWordTable.subword(newword, divisorInfo);
  
  // if newword = x^a x^b x^c, with x^b in the word table, then:
  //  divisorInfo.first = index of the GB element with x^b as lead monomial.
  //  divisorInfo.second = position of the start of x^b in newword
  //   (that is, the length of x^a).
  if (not found)
    return std::make_pair(false, PreRow(Word(), 0, Word(), false));
  // if found, then return this information to caller
  Word prefix = Word(newword.begin(), newword.begin() + divisorInfo.second);
  Word divisorWord = mWordTable[divisorInfo.first];
  Word suffix = Word(newword.begin() + divisorInfo.second + divisorWord.size(),
                     newword.end());
  return std::make_pair(true, PreRow(prefix, divisorInfo.first, suffix, false));
}

// Besides sorting the columns (using 'perm'), this also sets the
// pivot rows of each column index (in the new sorted order).
void NCF4::sortF4Matrix()
{
  size_t sz = mColumnMonomials.size();
  std::vector<int> indices;
  std::vector<Monom> tempMonoms;
  tempMonoms.reserve(sz);
  indices.reserve(sz);
  for (int i = 0; i < sz; ++i) indices.emplace_back(i);
 
  // store all the column monomials in a temporary to sort them
  for (auto& i : mColumnMonomials)
    tempMonoms.emplace_back(i.first);

  // create the monomial sorter object
  MonomSort<std::vector<Monom>> monomialSorter(&freeAlgebra().monoid(),&tempMonoms);
  std::stable_sort(indices.begin(),indices.end(),monomialSorter);
  
  std::vector<int> perm (static_cast<size_t>(mColumnMonomials.size()), -1);
  int count = 0;
  for (int i = 0; i < sz; ++i)
    {
      auto& val = mColumnMonomials[tempMonoms[indices[i]]];
      perm[val.first] = count;
      mColumns.emplace_back(Column(tempMonoms[indices[i]],val.second));
      val.first = count;
      ++count;
    }

  // Now we run through the rows
  for (auto& r : mRows)
    {
      auto& comps = r.second;
      for (int i=0; i < comps.size(); ++i)
        comps[i] = perm[comps[i]];
    }
}

void NCF4::reduceF4Row(int index,
                       int first,
                       int firstcol,
                       long& numCancellations,
                       DenseCoefficientVector2& dense)
{
  int sz = mRows[index].second.size();
  //assert(sz > 0);  this may be zero when autoreducing the new gb elements
  if (sz <= 1 && firstcol != -1) return;

  int last = mRows[index].second[sz-1];

  mVectorArithmetic->sparseRowToDenseRow(dense, mRows[index].first, mRows[index].second);
  do {
    int pivotrow = mColumns[first].second;
    if (pivotrow >= 0)
      {
        numCancellations++;
        mVectorArithmetic->denseRowCancelFromSparse(dense,
                                                   mRows[pivotrow].first,
                                                   mRows[pivotrow].second);
        // last component in the row corresponding to pivotrow
        int last1 = mRows[pivotrow].second.cend()[-1];
        last = (last1 > last ? last1 : last);
      }
    else if (firstcol == -1)
      {
        firstcol = first;
      }
    first = mVectorArithmetic->denseRowNextNonzero(dense, first+1, last);
  } while (first <= last);
  mVectorArithmetic->denseRowToSparseRow(dense,
                        mRows[index].first,
                        mRows[index].second,
                        firstcol,
                        last,
                        mMonomialSpace);
  if (mVectorArithmetic->size(mRows[index].first) > 0)
    {
      mVectorArithmetic->sparseRowMakeMonic(mRows[index].first);
      mColumns[firstcol].second = index;
    }
}

#if 0
void NCF4::parallelReduceF4Matrix()
{
  //auto numThreads = tbb::task_scheduler_init::default_num_threads();
  ring_elem zero = freeAlgebra().coefficientRing()->zero();

  long numCancellations = 0;
  using threadLocalDense_t = tbb::enumerable_thread_specific<DenseCoefficientVector2>;
  using threadLocalLong_t = tbb::enumerable_thread_specific<long>;
  using rwmutex_t = tbb::spin_rw_mutex;

  //tbb::task_scheduler_init init(numThreads);
  rwmutex_t my_mutex;

  // create a dense array for each thread
  threadLocalDense_t threadLocalDense([&]);
  for (auto tlDense : threadLocalDense)
    tlDense = & mVectorArithmetic->allocateDenseCoefficientVector(mColumnMonomials.size());

  threadLocalLong_t numCancellationsLocal{numThreads};
  
  // do we want to backsolve?  This slows things down a lot, but
  // we are not yet re-using this information for the next iteration at all.
  #if 0
  for (auto i = mFirstOverlap - 1; i >= 0; --i)
    reduceF4Row(i,mRows[i].second[1],mRows[i].second[0],numCancellations,*(threadLocalDense.begin()));
  #endif

  // reduce each overlap row by mRows.

  // make this parallel by creating a dense row for each thread?
  // Should see how this does before going further in terms of parallelization

  // really want a parallel_for here.  If we want to parallelize the reduction
  // of a row as well, we would also want to use parallel_reduce, but this may be
  // too much overhead.  We will see once we get it actually working.
   
  std::cout << "Number of threads: " << numThreads << std::endl;

  tbb::parallel_for(tbb::blocked_range<size_t>{(size_t)(mFirstOverlap),mRows.size()},
                    [&](const tbb::blocked_range<size_t>& r)
                    {
                      threadLocalDense_t::reference my_dense = threadLocalDense.local();
                      threadLocalLong_t::reference my_accum = numCancellationsLocal.local();
                      for (size_t i = r.begin(); i != r.end(); ++i)
                      {
                        rwmutex_t::scoped_lock my_lock{my_mutex,false};
                        reduceF4Row(i,mRows[i].second[0],-1,my_accum,*my_dense);
                      }
                    });

  for (auto i : numCancellationsLocal)
    numCancellations += i;

  // interreduce the matrix with respect to these overlaps.  This needs to be serial
  // at this point.

  for (int i = mRows.size()-1; i >= mFirstOverlap; --i)
    reduceF4Row(i,mRows[i].second[1],mRows[i].second[0],numCancellations,**(threadLocalDense.begin()));

  for (auto tlDense : threadLocalDense)
    mVectorArithmetic->deallocateDenseCoefficientVector(*tlDense);

  // std::cout << "Number of cancellations: " << numCancellations << std::endl;
}
#endif

void NCF4::reduceF4Matrix()
{
  long numCancellations = 0;

  auto denseVector = mVectorArithmetic->allocateDenseCoefficientVector(mColumnMonomials.size());

  // do we want to backsolve?  This slows things down a lot, but
  // we are not yet re-using this information for the next iteration at all.
  #if 0
  for (int i = mFirstOverlap - 1; i >= 0; --i)
    reduceF4Row(i,mRows[i].second[1],mRows[i].second[0],numCancellations,denseVector);
  #endif

  // reduce each overlap row by mRows.
  for (int i = mFirstOverlap; i < mRows.size(); ++i)
  {
    reduceF4Row(i,mRows[i].second[0],-1,numCancellations,denseVector);
  } 

  // interreduce the matrix with respect to these overlaps.
  //#if 0
  //for (int i = mRows.size()-1; i >= 0; --i)
  for (int i = mRows.size()-1; i >= mFirstOverlap; --i)
    reduceF4Row(i,mRows[i].second[1],mRows[i].second[0],numCancellations,denseVector);
  //#endif

  //std::cout << "Number of cancellations: " << numCancellations << std::endl;
}

void NCF4::displayF4MatrixSize(std::ostream & o) const
{
  // Display sizes:
  o << "(#cols, #reducer rows, #spair rows) = ("
    << mColumnMonomials.size() << ", "
    << mFirstOverlap << ", "
    << mRows.size() - mFirstOverlap << ")"
    << "  ";
  long numReducerEntries = 0;
  long numSPairEntries = 0;
 
  for (long i = 0; i < mRows.size(); ++i)
  {
    if (i < mFirstOverlap) 
       numReducerEntries += mVectorArithmetic->size(mRows[i].first);
    else
       numSPairEntries += mVectorArithmetic->size(mRows[i].first);
    if (mVectorArithmetic->size(mRows[i].first) != mRows[i].second.size())
      o << "***ERROR*** ring_elem and component ranges do not match!" << std::endl;
  }
  o << "#entries: (" << numReducerEntries << "," << numSPairEntries << ")"
    << std::endl;
}

void NCF4::displayF4Matrix(std::ostream& o) const
{
  displayF4MatrixSize(o);
  // Now column monomials
  for (auto i : mColumnMonomials)
    {
      buffer b;
      freeAlgebra().monoid().elem_text_out(b, i.first);
      o << b.str() << "(" << i.second.first << ", " << i.second.second << ") ";
      // each i is a pair (const Monom, pair(int,int)).
    }
  o << std::endl;
  
  // For each row, and each overlap row, display the non-zero comps, non-zero coeffs.
  if (mRows.size() != mReducersTodo.size())
    {
      o << "***ERROR*** expected mRows and mReducersTodo to have the same length!" << std::endl;
      exit(1);
    }
  const Ring* kk = freeAlgebra().coefficientRing();
  for (int count = 0; count < mRows.size(); ++count)
    {
      PreRow pr = mReducersTodo[count];
      o << count << " ("<< std::get<0>(pr) << ", "
        << std::get<1>(pr) << ", "
        << std::get<2>(pr) << ", "
        << std::get<3>(pr) << ") "
        << mRows[count].second.size() << ": ";
      if (mVectorArithmetic->size(mRows[count].first) != mRows[count].second.size())
        {
          o << "***ERROR*** expected coefficient array and components array to have the same length" << std::endl;
          exit(1);
        }
      for (int i=0; i < mVectorArithmetic->size(mRows[count].first); ++i)
        {
          buffer b;
          kk->elem_text_out(b, mVectorArithmetic->ringElemFromSparseVector(mRows[count].first,i));
          o << "[" << mRows[count].second[i] << "," << b.str() << "] ";
        }
      o << std::endl;
    }
}

void NCF4::displayFullF4Matrix(std::ostream& o) const
{
  displayF4MatrixSize(o);
  // Now column monomials
  for (auto i : mColumnMonomials)
    {
      buffer b;
      freeAlgebra().monoid().elem_text_out(b, i.first);
      o << b.str() << "(" << i.second.first << ", " << i.second.second << ") ";
      // each i is a pair (const Monom, pair(int,int)).
    }
  o << std::endl;  
  // For each row, and each overlap row, display the non-zero comps, non-zero coeffs.
  if (mRows.size() != mReducersTodo.size())
    {
      o << "***ERROR*** expected mRows and mReducersTodo to have the same length!" << std::endl;
      exit(1);
    }
  const Ring* kk = freeAlgebra().coefficientRing();
  for (int count = 0; count < mRows.size(); ++count)
    {
      PreRow pr = mReducersTodo[count];
      o << count << " ("<< std::get<0>(pr) << ", "
        << std::get<1>(pr) << ", "
        << std::get<2>(pr) << ")";
      int count2 = 0;
      for (int i=0; i < mColumnMonomials.size(); i++)
        {
          if (count2 == mVectorArithmetic->size(mRows[count].first) or mRows[count].second[count2] != i)
            {
              o << " 0 ";
            }
          else
            {
              buffer b;
              kk->elem_text_out(b,mVectorArithmetic->ringElemFromSparseVector(mRows[count].first,count2));
              o << " " << b.str() << " ";
              count2++;
            }
        }
      o << std::endl;
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
