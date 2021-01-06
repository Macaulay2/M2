#include "NCAlgebras/NCF4.hpp"

#include "text-io.hpp"                   // for emit_wrapped
#include "NCAlgebras/FreeAlgebra.hpp"       // for FreeAlgebra
#include "NCAlgebras/OverlapTable.hpp"      // for OverlapTable
#include "NCAlgebras/VectorArithmetic.hpp"  // for VectorArithmetic
#include "NCAlgebras/WordTable.hpp"         // for Overlap, WordTable
#include "buffer.hpp"                       // for buffer
#include "engine-exports.h"                 // for M2_gbTrace
#include "ring.hpp"                         // for Ring
#include "ringelem.hpp"                     // for ring_elem

#include <cassert>                         // for assert
#include <cstdlib>                         // for exit, size_t
#include <algorithm>                        // for copy
#include <iostream>                         // for operator<<, basic_ostream

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
      mColumnMonomials(mMonomEq),
      mPreviousColumnMonomials(mMonomEq)
{
  if (M2_gbTrace >= 1)
    {
      buffer o;
      o << "[NCGB F4]";
      emit_wrapped(o.str());
    }
  
  // process input polynomials
  mIsGraded = true;
  for (auto i = 0; i < mInput.size(); ++i)
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

  if (M2_gbTrace >= 2)
    {
      std::cout << "NC F4 GB: matrix size: ";
      displayF4MatrixSize(std::cout);
    }

  if (M2_gbTrace >= 100) displayFullF4Matrix(std::cout);
  else if (M2_gbTrace >= 50) displayF4Matrix(std::cout);

  // convert back to GB elements...
  PolyList newElems = newGBelements();

  for (auto& f : newElems)
    {
      addToGroebnerBasis(f);
      //autoreduceByLastElement();
      updateOverlaps(f);
    }

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
  // not sure how to do a move of an arena
  //mPreviousMonomialSpace.moveFrom(mMonomialSpace);
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
                            const VECTOR(Row)& rows,
                            const std::vector<Column>& cols,
                            int i) const
{
  // this function places the elements of the ith row of the
  // (reduced!) F4 matrix in result.  This assumes that the first
  // term of the ith row is after the last entry of result (or that result is empty).
  for (int j = 0; j < rows[i].second.size(); j++)
    freeAlgebra().add_to_end(*result, rows[i].first[j], cols[rows[i].second[j]].first);  
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
      mOverlapsTodo.push_back(PreRow(Word(), - gbLeftIndex - 1, Word(), false));
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
    mColumnMonomials.insert({newmon, {mColumnMonomials.size(), mReducersTodo.size()}});

  // it *matters* which one is a reducer and which one is an overlap.
  // this is due to how the word table lookup works -- it searches them
  // in the order that they were entered into the word table, which may
  // not be sorted in term order.
  if (gbLeftIndex > gbRightIndex)
    {
      mReducersTodo.push_back(PreRow(prefix2, gbRightIndex, suffix2, false));
      mOverlapsTodo.push_back(PreRow(prefix1, gbLeftIndex, suffix1, false));
    }
  else
    {  
      mReducersTodo.push_back(PreRow(prefix1, gbLeftIndex, suffix1, false));
      mOverlapsTodo.push_back(PreRow(prefix2, gbRightIndex, suffix2, false));
    }
}

void NCF4::buildF4Matrix(const std::deque<Overlap>& overlapsToProcess)
{
  matrixReset();

  for (auto o : overlapsToProcess)
    {
      preRowsFromOverlap(o);
    }

  // process each element in mReducersTodo
  for (int i=0 ; i < mReducersTodo.size(); ++i)
    {
      Row r = processPreRow(mReducersTodo[i]); // this often adds new elements to mReducersTodo
      mRows.push_back(r);
    }
  int numReducersAtFirst = mReducersTodo.size();

  for (int i=0; i < mOverlapsTodo.size(); ++i)
    {
      Row r = processPreRow(mOverlapsTodo[i]); // this often adds new elements to mReducersTodo
      mOverlaps.push_back(r);
    }

  for (int i=numReducersAtFirst ; i < mReducersTodo.size(); ++i)
    {
      Row r = processPreRow(mReducersTodo[i]); // this often adds new elements to mReducersTodo
      mRows.push_back(r);
    }

  // Now we move the overlaps into mRows, and set mFirstOverlap.
  mFirstOverlap = mRows.size();
  for (int i=0; i < mOverlapsTodo.size(); ++i)
    {
      mRows.push_back(mOverlaps[i]);
      mReducersTodo.push_back(mOverlapsTodo[i]);
    }
}

std::pair<bool,int> NCF4::findPreviousReducerPrefix(const Monom& m) const
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

  // get prefix of m
  // search for prefix in mPreviousColumnMonomials
  // if it.second.second != -1, then return (true,it.second.first)
  // else return (false, -1)
  return std::make_pair(false,-1);
}


std::pair<bool,int> NCF4::findPreviousReducerSuffix(const Monom& m) const
{
  // basically the same as above, except with suffixes

  // get suffix of m
  // search for suffix in mPreviousColumnMonomials
  // if it.second.second != -1, then return (true,it.second.first)
  // else return (false, -1)
  return std::make_pair(false,-1);
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
          // if it is a divisor of a gb element,
          // add that multiple of the GB element to mReducersToDo for processing
          divisornum = mReducersTodo.size();
          mReducersTodo.push_back(divresult.second);
        }
      // insert column information into mColumnMonomials,
      // which provides us search/indexing of columns
      int newColumnIndex = mColumnMonomials.size();
      mColumnMonomials.insert({m, {newColumnIndex, divisornum}});
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
  ring_elem* ptr = newarray(ring_elem, elem->getCoeffVector().size());
  Range<ring_elem> coeffrange(ptr, ptr + elem->getCoeffVector().size());
  std::copy(elem->getCoeffVector().cbegin(), elem->getCoeffVector().cend(), coeffrange.begin());
  // delete the poly created for prevReducer case, if necessary.
  if (prevReducer) delete elem;
  return(Row(coeffrange, componentRange));
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
     std::cout << "Total findDivisorCalls    : " << findDivisorCalls << std::endl
               << "  Prevoius prefixes found : " << previousPrefixesFound << std::endl
               << "  Prevoius suffixes found : " << previousSuffixesFound << std::endl;       

  // look in previous F4 matrix for Monom before checking mWordTable
  auto usePreviousPrefix = findPreviousReducerPrefix(mon);
  if (usePreviousPrefix.first)
    {
      // here, we use a multiple of a previously reduced row
      previousPrefixesFound++;
      return std::make_pair(true, PreRow(Word(),0,Word(),true));
      // will eventually be:
      //return std::make_pair(true, PreRow(Word(),
      //                                   usePreviousPrefix.second,
      //                                   freeAlgebra().monoid().lastVar(mon),
      //                                   true));
    }
  auto usePreviousSuffix = findPreviousReducerSuffix(mon);
  if (usePreviousSuffix.first)
    {
      // here, we use a multiple of a previously reduced row
      previousPrefixesFound++;
      return std::make_pair(true, PreRow(Word(),0,Word(),true));
      // will eventually be:
      //return std::make_pair(true, PreRow(freeAlgebra().monoid().firstVar(mon),
      //                                   usePreviousSuffix.second,
      //                                   Word(),
      //                                   true));
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

void NCF4::sortF4Matrix()
// Besides sorting the columns (using 'perm'), this also sets the
// pivot rows of each column index (in the new sorted order).
{
  std::vector<int> perm (static_cast<size_t>(mColumnMonomials.size()), -1);
  int count = 0;
  for (auto& i : mColumnMonomials)
    {
      int origIndex = i.second.first;
      perm[origIndex] = count;
      mColumns.push_back(Column(i.first, i.second.second));
      i.second.first = count;
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
                       Range<ring_elem>& dense)
{
  VectorArithmetic V(freeAlgebra().coefficientRing());

  int sz = mRows[index].second.size();
  //assert(sz > 0);  this may be zero when autoreducing the new gb elements
  if (sz <= 1 && firstcol != -1) return;

  int last = mRows[index].second[sz-1];

  V.sparseRowToDenseRow(dense, mRows[index].first, mRows[index].second);
  do {
    int pivotrow = mColumns[first].second;
    if (pivotrow >= 0)
      {
        numCancellations++;
        V.denseRowCancelFromSparse(dense, mRows[pivotrow].first, mRows[pivotrow].second);
        // last component in the row corresponding to pivotrow
        int last1 = mRows[pivotrow].second.cend()[-1];
        last = (last1 > last ? last1 : last);
      }
    else if (firstcol == -1)
      {
        firstcol = first;
      }
    first = V.denseRowNextNonzero(dense, first+1, last);
  } while (first <= last);
  V.denseRowToSparseRow(dense,
                        mRows[index].first,
                        mRows[index].second,
                        firstcol,
                        last);
  if (mRows[index].first.size() > 0)
    {
      V.sparseRowMakeMonic(mRows[index].first);
      mColumns[firstcol].second = index;
    }
}

void NCF4::reduceF4Matrix()
{
  long numCancellations = 0;

  // Create the dense array.
  ring_elem zero = freeAlgebra().coefficientRing()->zero();
  VECTOR(ring_elem) denseVector(mColumnMonomials.size(), zero);
  Range<ring_elem> dense(denseVector);

  // do we want to backsolve?  This slows things down a lot, but
  // we are not yet re-using this information for the next iteration at all.
  #if 0
  for (int i = mFirstOverlap - 1; i >= 0; --i)
    reduceF4Row(i,mRows[i].second[1],mRows[i].second[0],numCancellations,dense);
  #endif

  // reduce each overlap row by mRows.
  for (int i = mFirstOverlap; i < mRows.size(); ++i)
    reduceF4Row(i,mRows[i].second[0],-1,numCancellations,dense); 

  // interreduce the overlaps
  //#if 0
  for (int i = mRows.size()-1; i >= mFirstOverlap; --i)
    reduceF4Row(i,mRows[i].second[1],mRows[i].second[0],numCancellations,dense);
  //#endif

  std::cout << "Number of cancellations: " << numCancellations << std::endl;
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
 
  for (int i = 0; i < mRows.size(); ++i)
  {
    if (i < mFirstOverlap) 
       numReducerEntries += mRows[i].first.size();
    else
       numSPairEntries += mRows[i].first.size();
    if (mRows[i].first.size() != mRows[i].second.size())
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
        << std::get<2>(pr) << ")";
      if (mRows[count].first.size() != mRows[count].second.size())
        {
          o << "***ERROR*** expected coefficient array and components array to have the same length" << std::endl;
          exit(1);
        }
      for (int i=0; i < mRows[count].first.size(); ++i)
        {
          buffer b;
          kk->elem_text_out(b, mRows[count].first[i]);
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
          if (count2 == mRows[count].first.size() or mRows[count].second[count2] != i)
            {
              o << " 0 ";
            }
          else
            {
              buffer b;
              kk->elem_text_out(b,mRows[count].first[count2]);
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
