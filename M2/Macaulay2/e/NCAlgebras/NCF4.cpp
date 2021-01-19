#include "NCAlgebras/NCF4.hpp"

#include "text-io.hpp"                      // for emit_wrapped
#include "NCAlgebras/FreeAlgebra.hpp"       // for FreeAlgebra
#include "NCAlgebras/OverlapTable.hpp"      // for OverlapTable
#include "VectorArithmetic.hpp"             // for VectorArithmetic
#include "NCAlgebras/WordTable.hpp"         // for Overlap, WordTable
#include "buffer.hpp"                       // for buffer
#include "engine-exports.h"                 // for M2_gbTrace
#include "ring.hpp"                         // for Ring
#include "ringelem.hpp"                     // for ring_elem

#include <cassert>                          // for assert
#include <cstdlib>                          // for exit, size_t
#include <algorithm>                        // for copy
#include <iostream>                         // for operator<<, basic_ostream

NCF4::NCF4(const FreeAlgebra& A,
           const ConstPolyList& input,
           int hardDegreeLimit,
           int strategy,
           bool isParallel
           )
    : mFreeAlgebra(A),
      mInput(input),
      mTopComputedDegree(-1),
      mHardDegreeLimit(hardDegreeLimit),
      mMonomEq(A.monoid()),
      mMonomHashEqual(A.monoid()),
      mColumnMonomials(10,mMonomHash,mMonomHashEqual),
      mPreviousColumnMonomials(10,mMonomHash,mMonomHashEqual),
//      mVectorArithmetic(vectorArithmetic(A.coefficientRing())),
      mVectorArithmetic(new VectorArithmetic2(A.coefficientRing())),
      mIsParallel(isParallel)
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
                           std::make_tuple(i,-1,-1,true));
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
  tbb::tick_count t0 = tbb::tick_count::now();
  //if (mIsParallel)
  //  parallelBuildF4Matrix(overlapsToProcess);
  //else
  buildF4Matrix(overlapsToProcess);
  tbb::tick_count t1 = tbb::tick_count::now();
  if (M2_gbTrace >= 2) 
    std::cout << "Time spent on build step: " << (t1-t0).seconds() << std::endl;
    
  if (M2_gbTrace >= 2)
    {
      std::cout << "NC F4 GB: matrix size: ";
      displayF4MatrixSize(std::cout);
    }

  // sort the columns so that the matrix is `upper triangular'
  labelAndSortF4Matrix();

  if (M2_gbTrace >= 100) displayFullF4Matrix(std::cout);
  else if (M2_gbTrace >= 50) displayF4Matrix(std::cout);

  // reduce the matrix
  t0 = tbb::tick_count::now();
  if (mIsParallel)
    parallelReduceF4Matrix();
  else
    reduceF4Matrix();
  t1 = tbb::tick_count::now();
  if (M2_gbTrace >= 2) 
    std::cout << "Time spent on reduction step: " << (t1-t0).seconds() << std::endl;

  if (M2_gbTrace >= 100) displayFullF4Matrix(std::cout);
  else if (M2_gbTrace >= 50) displayF4Matrix(std::cout);

  // convert back to GB elements...
  PolyList newElems = newGBelements();

  for (auto& f : newElems)
    {
      addToGroebnerBasis(f);
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
  // flag the columns correspond to lead terms of new GB elements as reducers
  for (int i = mFirstOverlap; i < mRows.size(); ++i)
  {
    if (mRows[i].columnIndices.size() == 0) continue;
    int newReducerCol = mRows[i].columnIndices[0];
    mColumns[newReducerCol].pivotRow = i;
    mPreviousColumnMonomials[mColumns[newReducerCol].word].second = i;
  } 
  
  // copy the finished rows and columns into the holding areas
  mPreviousRows.clear();
  mPreviousColumns.clear();
  mPreviousColumnMonomials.clear();
  mPreviousMonomialSpace.deallocateAll();

  for (auto& p : mPreviousMemoryBlocks)
  {
    if (p != nullptr)
      p->deallocateAll();
    p = nullptr;
  }
  mPreviousMemoryBlocks.clear();

  mPreviousRows = std::move(mRows);
  mPreviousColumns = std::move(mColumns); 
  mPreviousColumnMonomials = std::move(mColumnMonomials);
  mPreviousMemoryBlocks = std::move(mMemoryBlocks);
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
   std::vector<Overlap> newLeftOverlaps;
   std::vector<Overlap> newRightOverlaps;

   Word newLeadWord = freeAlgebra().lead_word(*toAdd);

   // the word table insert places the right overlaps into newOverlaps
   mWordTable.insert(newLeadWord,newRightOverlaps);

   // this function finds the left overlaps with the most recently
   // inserted word.
   mWordTable.leftOverlaps(newLeftOverlaps);

   // can *also* remove previously added overlaps based on poly using
   // the same criterion from before
   checkOldOverlaps(newLeadWord);

   // finally, insert the new overlaps
   insertNewOverlaps(newRightOverlaps);
   insertNewOverlaps(newLeftOverlaps);

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
           std::cout << "Reduction avoided using eager 2nd criterion." << std::endl;
         continue;
       }
     mOverlapTable.insert(overlapHeft(newOverlap),
                          false,
                          newOverlap);
   }
}

auto NCF4::isOverlapNecessary(const Overlap& o) -> bool
{
  // this function tests if the lead word of the overlap polynomial
  // of o is a multiple of another pattern in the word table.

  // need to be careful, however, since an overlap lead word is trivially
  // a multiple of the words used to build it.  These possibilities must be discarded
  bool retval;
  Word w;
  
  // not optimal.  Should pass information to wordTable for checking.
  w = createOverlapLeadWord(o);
  retval = !mWordTable.isNontrivialSuperword(w, std::get<0>(o), std::get<2>(o));
  return retval;
}

auto NCF4::checkOldOverlaps(Word& newLeadWord) -> void
{
  // this function flags any previous overlaps that properly
  // contain newLeadWord as no longer necessary.

  // the newest overlaps are not yet added at this point
  std::vector<int> startIndices;

  for (auto& p : mOverlapTable.overlapMap())
  {
    for (auto& o : p.second)
    {
      if (not std::get<3>(o)) continue;
      if (std::get<1>(o) == -1) continue;
      int overlapLen = std::get<1>(o) + mWordTable[std::get<2>(o)].size();
      if (overlapLen <= newLeadWord.size()) continue;
      startIndices.clear();
      // this is not optimal.  Should pass info to word table to avoid creation of another word
      auto w = createOverlapLeadWord(o);
      WordTable::subwordPositions(newLeadWord,w,startIndices);
      for (auto j : startIndices)
      {
        if (j != 0 or j != w.size() - newLeadWord.size())
        {
          if (M2_gbTrace >= 3)
             std::cout << "Reduction avoided using lazy 2nd criterion." << std::endl;        
          std::get<3>(o) = false;
          break;
        }
      }
    }
  }
}

Word NCF4::createOverlapLeadWord(const Overlap& o) 
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
      if (mRows[i].columnIndices.size() == 0) continue;
      Poly* f = new Poly;
      reducedRowToPoly(f,mRows,mColumns,i);
      result.push_back(f);
    }
  return result;
}

void NCF4::reducedRowToPoly(Poly* result,
                            const RowsVector& rows,
                            const ColumnsVector& cols,
                            int i) const
{
  // this function places the elements of the ith row of the
  // (reduced!) F4 matrix in result.  This assumes that the first
  // term of the ith row is after the last entry of result (or that result is empty).
  auto& resultCoeffInserter = result->getCoeffInserter();
  auto& resultMonomInserter = result->getMonomInserter();
  
  //mVectorArithmetic->appendSparseVectorToContainer(rows[i].first,resultCoeffInserter);
  using ContainerType = decltype(resultCoeffInserter);
  mVectorArithmetic->appendSparseVectorToContainer<ContainerType>(rows[i].coeffVector,resultCoeffInserter);
  
  for (const auto& col : rows[i].columnIndices)
    freeAlgebra().monoid().monomInsertFromWord(resultMonomInserter,cols[col].word);
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

  // the MemoryBlock objects that were in this vector
  // have been passed to mPreviousMemoryBlocks at this point.
  mMemoryBlocks.clear();
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
      mOverlapsTodo.emplace_back(PreRow {Word(),
                                         - gbLeftIndex - 1,
                                         Word(),
                                         OverlapPreRow});
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
  Word newWord = freeAlgebra().monoid().wordProductAsWord(leadWordLeft, suffix1, mMonomialSpace);

  // This overlap may already have lead term in table.
  // Only have to add it in if it is not yet present.
  auto it = mColumnMonomials.find(newWord);
  if (it == mColumnMonomials.cend())
    mColumnMonomials.emplace(newWord, std::make_pair(-1,-1));

  // it *matters* which one is a reducer and which one is an overlap.
  // this is due to how the word table lookup works -- it searches them
  // in the order that they were entered into the word table, which may
  // not be sorted in term order.
  if (gbLeftIndex > gbRightIndex)
    {
      mReducersTodo.emplace_back(PreRow {prefix2,
                                         gbRightIndex,
                                         suffix2,
                                         ReducerPreRow});
      mOverlapsTodo.emplace_back(PreRow {prefix1,
                                         gbLeftIndex,
                                         suffix1,
                                         OverlapPreRow});
    }
  else
    {  
      mReducersTodo.emplace_back(PreRow {prefix1,
                                         gbLeftIndex,
                                         suffix1,
                                         ReducerPreRow});
      mOverlapsTodo.emplace_back(PreRow {prefix2,
                                         gbRightIndex,
                                         suffix2,
                                         OverlapPreRow});
    }
}

void NCF4::buildF4Matrix(const std::deque<Overlap>& overlapsToProcess)
{
  matrixReset();

  mColumnMonomials.rehash(pow(2, ceil(log2(mPreviousColumnMonomials.size()))));

  for (auto o : overlapsToProcess)
    {
      auto stillValid = std::get<3>(o);
      if (stillValid) preRowsFromOverlap(o);
    }

  // can't do this loop as a range-based for loop since we are adding to it
  // during the for loop
  // process each element in mReducersTodo (false indicates a reducer row)
  for (int i=0 ; i < mReducersTodo.size(); ++i)
    processPreRow(mReducersTodo[i],mRows); // this often adds new elements to mReducersTodo
  
  int numReducersAtFirst = mReducersTodo.size();
  
  // process the overlap rows (true indicates an overlap row)
  for (auto over : mOverlapsTodo)
    processPreRow(over,mOverlaps);  // this often adds new elements to mReducersTodo

  // can't do this loop as a range-based for loop since we are adding
  // to mReducersTodo during the for loop
  for (int i=numReducersAtFirst ; i < mReducersTodo.size(); ++i)
    processPreRow(mReducersTodo[i],mRows); // this often adds new elements to mReducersTodo

  // Now we move the overlaps into mRows, and set mFirstOverlap.
  // double range, so use a for loop
  mFirstOverlap = mRows.size();
  for (int i=0; i < mOverlapsTodo.size(); ++i)
    {
      mRows.emplace_back(mOverlaps[i]);
      mReducersTodo.emplace_back(mOverlapsTodo[i]);
    }
}

// this is currently not used in the process function
// since it is not as fast as the serial version due
// concurrency overhead.
void NCF4::parallelBuildF4Matrix(const std::deque<Overlap>& overlapsToProcess)
{
  matrixReset();

  mColumnMonomials.rehash(pow(2, ceil(log2(mPreviousColumnMonomials.size()))));

  for (auto o : overlapsToProcess)
    {
      auto stillValid = std::get<3>(o);
      if (stillValid) preRowsFromOverlap(o);
    }

  struct ThreadData {
    RowsVector rowsVector;
    MemoryBlock* memoryBlock;
  };

  tbb::enumerable_thread_specific<ThreadData> threadData([&](){  
      tbb::queuing_mutex::scoped_lock myColumnLock(mColumnMutex);
      ThreadData data;
      data.memoryBlock = new MemoryBlock;
      mMemoryBlocks.push_back(data.memoryBlock);
      return data;
    });

  // can't do this loop as a range-based for loop since we are adding to it
  // during the for loop
  // process each element in mReducersTodo
  tbb::parallel_do(mReducersTodo.begin(), mReducersTodo.end(),
      [&](const PreRow& prerow, PreRowFeeder& feeder)
      {
        auto& data = threadData.local();
        processPreRow(prerow,
                      data.rowsVector,
                      *data.memoryBlock,
                      &feeder);
      });

  // combine the thread local rows into mRows
  for (const auto& data : threadData)
  {
    mRows.reserve(mRows.size() + data.rowsVector.size());
    std::move(data.rowsVector.begin(),
              data.rowsVector.end(),
              std::back_inserter(mRows));
  }

  // WARNING: The feeder doesn't actually add things to mReducersTodo
  //          so in this algorithm there is now a disconnect between the
  //          sizes of mReducersTodo and mRows.

  int numReducersAtFirst = mReducersTodo.size();
  
  // this can be a parallel_for
  for (auto over : mOverlapsTodo)
    processPreRow(over,mOverlaps);  // this often adds new elements to mReducersTodo

  // this must be a parallel_do
  // can't do this loop as a range-based for loop since we are adding
  // to mReducersTodo during the for loop
  for (int i=numReducersAtFirst ; i < mReducersTodo.size(); ++i)
    processPreRow(mReducersTodo[i],mRows); // this often adds new elements to mReducersTodo

  // Now we move the overlaps into mRows, and set mFirstOverlap.
  mFirstOverlap = mRows.size();
  for (int i=0; i < mOverlapsTodo.size(); ++i)
    {
      mRows.emplace_back(mOverlaps[i]);
      mReducersTodo.emplace_back(mOverlapsTodo[i]);
    }
}

void NCF4::processPreRow(PreRow r,
                         RowsVector& rowsVector,
                         MemoryBlock& memoryBlock,
                         PreRowFeeder* feeder)
{
  // note: left and right should be the empty word if gbIndex < 0 indicating
  // an input polynomial.
  Word left = r.left;
  int gbIndex = r.preRowIndex;
  Word right = r.right;
  PreRowType preRowType = r.preRowType;
  
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
  if (preRowType == PreviousReducerPreRow)
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

  auto wordRange = memoryBlock.allocateArray<Word>(nterms);
  auto columnRange = memoryBlock.allocateArray<int>(nterms);
  // int* componentAlloc = (int*)mMemoryPool.malloc(nterms*sizeof(int));
  // Range<int> componentRange(componentAlloc,componentAlloc + nterms);
  // for (auto& i : componentRange) i = 0;

  Word* nextColWord = wordRange.first;

  for (auto i = elem->cbegin(); i != elem->cend(); ++i)
  {
    Word mid;
    freeAlgebra().monoid().wordFromMonom(mid,i.monom());
    Word w = freeAlgebra().monoid().wordProductAsWord(left,mid,right,memoryBlock);
    processWordInPreRow(w,feeder);
    *nextColWord = w;
    ++nextColWord;
  }
  CoeffVector coeffs = mVectorArithmetic->sparseVectorFromContainer(elem->getCoeffVector());

  // delete the Poly created for prevReducer case, if necessary.
  if (preRowType == PreviousReducerPreRow) delete elem;

  // add the processed row to the appropriate list
  rowsVector.emplace_back(Row {coeffs, columnRange, wordRange});
}

void NCF4::processPreRow(PreRow r,
                         RowsVector& rowsVector)
{
  processPreRow(r,
                rowsVector,
                mMonomialSpace,
                nullptr);
}

void NCF4::processWordInPreRow(Word& w,
                               PreRowFeeder* feeder)
{
  const auto it = mColumnMonomials.find(w);
  if (it == mColumnMonomials.end())
  {
    // if we are here, this monomial is not yet accounted for in the matrix
    // so, check if the monomial is a multiple of a lead term of a gb element

    // insert column information into mColumnMonomials,
    // which provides us search (and eventually) indexing of columns
    mColumnMonomials.emplace(w,std::make_pair(-1,-1));
    auto divresult = findDivisor(w);
    if (divresult.first)
    {
      // if m is a divisor of a gb element (or a left/right
      // variable multiple of a previous reducer), add that
      // multiple of the GB element to mReducersToDo for
      // processing

      // if feeder is nullptr, then this is a computation on a list
      // not being added to during the loop.
      // e.g. either F4 serial or overlaps processing
      // otherwise, we must use the feeder to add more work.
      if (feeder == nullptr)
        mReducersTodo.push_back(divresult.second);
      else
        feeder->add(divresult.second);
    }
  }
}

std::pair<bool, NCF4::PreRow> NCF4::findDivisor(Word word)
{
  Word newword;

  // look in previous F4 matrix for Monom before checking mWordTable

  auto usePreviousSuffix = findPreviousReducerSuffix(word);
  if (usePreviousSuffix.first)
    {
      // here, we use a multiple of a previously reduced row
      Word tmpWord;
      if (word.size() != 0)
        tmpWord.init(word.begin(), word.begin() + 1);
      return std::make_pair(true, PreRow {tmpWord,
                                          usePreviousSuffix.second,
                                          Word(),
                                          PreviousReducerPreRow});
    }
  auto usePreviousPrefix = findPreviousReducerPrefix(word);
  if (usePreviousPrefix.first)
    {
      // here, we use a multiple of a previously reduced row
      Word tmpWord;
      if (word.size() != 0)
        tmpWord.init(word.end()-1, word.end());
      return std::make_pair(true, PreRow {Word(),
                                          usePreviousPrefix.second,
                                          tmpWord,
                                          PreviousReducerPreRow});
    }

  // if we are here, then the Monom does not have a prefix/suffix
  // that was processed previously.

  // look in mWordTable for Monom
  std::pair<int,int> divisorInfo;
  
  // TODO: for certain inputs (e.g. Monoms that we *know* are of
  //       the form xm or mx for m a standard monomial and x a
  //       variable, one needs only to check prefixes or
  //       suffixes, not all subwords.  Not sure how to utilize that
  //       just yet.

  bool found = mWordTable.subword(word, divisorInfo);
  
  // if newword = x^a x^b x^c, with x^b in the word table, then:
  //  divisorInfo.first = index of the GB element with x^b as lead monomial.
  //  divisorInfo.second = position of the start of x^b in newword
  //   (that is, the length of x^a).
  if (not found)
    return std::make_pair(false, PreRow {Word(), 0, Word(), ReducerPreRow});
  // if found, then return this information to caller
  Word prefix = Word(word.begin(), word.begin() + divisorInfo.second);
  Word divisorWord = mWordTable[divisorInfo.first];
  Word suffix = Word(word.begin() + divisorInfo.second + divisorWord.size(),
                     word.end());
  return std::make_pair(true, PreRow {prefix, divisorInfo.first, suffix, ReducerPreRow});
}

std::pair<bool,int> NCF4::findPreviousReducerPrefix(const Word& w)
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
  if (w.size() == 0) return std::make_pair(false,-1);
  
  std::pair<bool,int> retval;
  
  Word prefix(w.begin(),w.end()-1);
  const auto it = mPreviousColumnMonomials.find(prefix);
  if (it == mPreviousColumnMonomials.end()) // not in table
    retval = std::make_pair(false,-1);
  else
  {
    int colNum = (*it).second.first;
    if (mPreviousColumns[colNum].pivotRow == -1)  // in column table and not a reducer monomial
      retval = std::make_pair(false,-1);
    else  // in table and a reducer monomial
      retval = std::make_pair(true,mPreviousColumns[colNum].pivotRow);
  }
  return retval;
}


std::pair<bool,int> NCF4::findPreviousReducerSuffix(const Word& w)
{
  // basically the same as above, except with suffixes

  // get suffix of m
  // search for suffix in mPreviousColumnMonomials
  // if it.second.second != -1, then return (true,it.second.first)
  // else return (false, -1)
  if (w.size() == 0) return std::make_pair(false,-1);
  
  std::pair<bool,int> retval;
  
  Word suffix(w.begin()+1,w.end());
  const auto it = mPreviousColumnMonomials.find(suffix);
  if (it == mPreviousColumnMonomials.end()) //not in table
    retval = std::make_pair(false,-1);
  else
  {
    int colNum = (*it).second.first;
    if (mPreviousColumns[colNum].pivotRow == -1)  // in column table and not a reducer monomial
      retval = std::make_pair(false,-1);
    else  // in table and a reducer monomial
      retval = std::make_pair(true,mPreviousColumns[colNum].pivotRow);
  }
  return retval;
}

// Besides sorting the columns (using 'perm'), this also sets the
// pivot rows of each column index (in the new sorted order).
void NCF4::labelAndSortF4Matrix()
{
  size_t sz = mColumnMonomials.size();
  std::vector<int> columnIndices;
  std::vector<Word> tempWords;
  
  tempWords.reserve(sz);
  columnIndices.resize(sz);
  std::iota(columnIndices.begin(), columnIndices.end(), 0);
  
  // store all the column monomials in a temporary to sort them
  // and also set the reducer column for them on the same pass
  for (auto& i : mColumnMonomials)
    tempWords.emplace_back(i.first);

  // create the monomial sorter object
  MonomSort<std::vector<Word>> monomialSorter(&freeAlgebra().monoid(),&tempWords);
  // stable sort was here before, but this sort is based on a total ordering
  // with no ties so we can use an unstable (and hence parallel!) sort.
  tbb::parallel_sort(columnIndices.begin(),columnIndices.end(),monomialSorter);
  
  // apply the sorted labeling to the columns
  mColumns.resize(sz);
  tbb::parallel_for(tbb::blocked_range<int>{0,(int)sz},
         [&](const tbb::blocked_range<int>& r)
         {
           for (auto count = r.begin(); count != r.end(); ++count)
             {
               auto& val = mColumnMonomials[tempWords[columnIndices[count]]];
               val.first = count;
               mColumns[count].word = tempWords[columnIndices[count]];
               mColumns[count].pivotRow = -1;
             }
         });

  // now fix the column labels in the rows and set pivot rows in columns
  tbb::parallel_for(tbb::blocked_range<int>{0,(int)mRows.size()},
        [&](const tbb::blocked_range<int>& r)
        {
          for (auto i = r.begin(); i != r.end(); ++i)
          {
            auto& comps = mRows[i].columnIndices;
            auto& words = mRows[i].columnWords;
            // sets the pivot row in the column if this is a reducer row
            if (i < mFirstOverlap)
            {
              mColumns[mColumnMonomials[words[0]].first].pivotRow = i;
              mColumnMonomials[words[0]].second = i;
            }
            for (int j = 0; j < words.size(); ++j)
              comps[j] = mColumnMonomials[words[j]].first;
          }
        });
}

// both reduceF4Row and parallelReduceF4Row call this function
// with the appropriate mutexes inserted for lock.
//         reduceF4Row: tbb::null_mutex (a do-nothing mutex)
// parallelReduceF4Row: tbb::queuing_mutex (for now)
template<typename LockType>
void NCF4::generalReduceF4Row(int index,
                              int first,
                              int firstcol,
                              long& numCancellations,
                              DenseCoeffVector& dense,
                              bool updateColumnIndex,
                              LockType& lock)
{
  int sz = mRows[index].columnIndices.size();
  //assert(sz > 0);  this may be zero when autoreducing the new gb elements
  if (sz == 0) return;
  if (sz == 1 && firstcol != -1) return;

  int last = mRows[index].columnIndices[sz-1];

  mVectorArithmetic->sparseRowToDenseRow(dense,
                                         mRows[index].coeffVector,
                                         mRows[index].columnIndices);
  do {
    int pivotrow = mColumns[first].pivotRow;
    if (pivotrow >= 0)
      {
        numCancellations++;
        mVectorArithmetic->denseRowCancelFromSparse(dense,
                                                    mRows[pivotrow].coeffVector,
                                                    mRows[pivotrow].columnIndices);
        // last component in the row corresponding to pivotrow
        int last1 = mRows[pivotrow].columnIndices.cend()[-1];
        last = (last1 > last ? last1 : last);
      }
    else if (firstcol == -1)
      {
        firstcol = first;
      }
    first = mVectorArithmetic->denseRowNextNonzero(dense, first+1, last);
  } while (first <= last);
  
  mVectorArithmetic->safeDenseRowToSparseRow(dense,
                                             mRows[index].coeffVector,
                                             mRows[index].columnIndices,
                                             firstcol,
                                             last,
                                             mMonomialSpace,
                                             lock);
  if (mVectorArithmetic->size(mRows[index].coeffVector) > 0)
    {
      mVectorArithmetic->sparseRowMakeMonic(mRows[index].coeffVector);
      // don't do this in the parallel version
      if (updateColumnIndex) mColumns[firstcol].pivotRow = index;
    }
}

void NCF4::parallelReduceF4Matrix()
{
  long numCancellations = 0;
  using threadLocalDense_t = tbb::enumerable_thread_specific<DenseCoeffVector>;
  using threadLocalLong_t = tbb::enumerable_thread_specific<long>;

  // create a dense array for each thread
  threadLocalDense_t threadLocalDense([&]() { 
    return mVectorArithmetic->allocateDenseCoeffVector(mColumnMonomials.size());
  });
  auto denseVector = mVectorArithmetic->allocateDenseCoeffVector(mColumnMonomials.size());
  
  threadLocalLong_t numCancellationsLocal;
  
  // reduce each overlap row by mRows.

  tbb::queuing_mutex lock;
  tbb::parallel_for(tbb::blocked_range<int>{mFirstOverlap,(int)mRows.size()},
                    [&](const tbb::blocked_range<int>& r)
                    {
                      threadLocalDense_t::reference my_dense = threadLocalDense.local();
                      threadLocalLong_t::reference my_accum = numCancellationsLocal.local();
                      for (auto i = r.begin(); i != r.end(); ++i)
                        parallelReduceF4Row(i,
                                            mRows[i].columnIndices[0],
                                            -1,
                                            my_accum,
                                            my_dense,
                                            lock);
                    });
  
  int numThreads = 0;
  for (auto i : numCancellationsLocal)
  {
    ++numThreads;
    numCancellations += i;
  }

  // sequentially perform one more pass to reduce the spair rows down 
  for (int i = mFirstOverlap; i < mRows.size(); ++i)
    reduceF4Row(i,
                mRows[i].columnIndices[0],
                -1,
                numCancellations,
                denseVector);

  // interreduce the matrix with respect to these overlaps.  This needs to be sequential.
  for (int i = mRows.size()-1; i >= mFirstOverlap; --i)
    reduceF4Row(i,
                mRows[i].columnIndices[1],
                mRows[i].columnIndices[0],
                numCancellations,
                denseVector);

  for (auto tlDense : threadLocalDense)
    mVectorArithmetic->deallocateCoeffVector(tlDense);

  mVectorArithmetic->deallocateCoeffVector(denseVector);
  // std::cout << "Number of cancellations: " << numCancellations << std::endl;
  // std::cout << "Number of threads used: " << numThreads << std::endl;
}

void NCF4::reduceF4Matrix()
{
  long numCancellations = 0;

  auto denseVector = mVectorArithmetic->allocateDenseCoeffVector(mColumnMonomials.size());

  // reduce each overlap row by mRows.
  for (int i = mFirstOverlap; i < mRows.size(); ++i)
    reduceF4Row(i,
                mRows[i].columnIndices[0],
                -1,
                numCancellations,
                denseVector);

  // interreduce the matrix with respect to these overlaps.
  for (int i = mRows.size()-1; i >= mFirstOverlap; --i)
    reduceF4Row(i,
                mRows[i].columnIndices[1],
                mRows[i].columnIndices[0],
                numCancellations,
                denseVector);

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
 
  // if (mRows.size() != mReducersTodo.size())
  //   {
  //     o << std::endl;
  //     o << "***ERROR*** expected mRows and mReducersTodo to have the same length!" << std::endl;
  //     o << "        mRows size: " << mRows.size() << std::endl;
  //     o << "mReducersTodo size: " << mReducersTodo.size() << std::endl;
  //     exit(1);
  //   }
  for (long i = 0; i < mRows.size(); ++i)
  {
    if (i < mFirstOverlap) 
       numReducerEntries += mVectorArithmetic->size(mRows[i].coeffVector);
    else
       numSPairEntries += mVectorArithmetic->size(mRows[i].coeffVector);
    if (mVectorArithmetic->size(mRows[i].coeffVector) != mRows[i].columnIndices.size())
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
      // each i is a pair (const Monom, pair(int,int)).
      buffer b;
      FreeMonoid::MonomialInserter monomInserter;
      freeAlgebra().monoid().monomInsertFromWord(monomInserter,i.first);
      Monom m(monomInserter.data());
      freeAlgebra().monoid().elem_text_out(b, m);
      o << b.str() << "(" << i.second.first << ", " << i.second.second << ") ";
      if (i.second.second != -1 and
          (mRows[i.second.second].columnIndices.begin() == nullptr or
           mRows[i.second.second].columnIndices[0] != i.second.first))
      {
        std::cout << "Oops (" << mRows[i.second.second].columnIndices.begin()
                  << "," << mRows[i.second.second].columnIndices[0] << ","
                  << i.second.first << ")";
      }
    }
  o << std::endl;
  
  // For each row, and each overlap row, display the non-zero comps, non-zero coeffs.
  // if (mRows.size() != mReducersTodo.size())
  //   {
  //     o << std::endl;
  //     o << "***ERROR*** expected mRows and mReducersTodo to have the same length!" << std::endl;
  //     o << "        mRows size: " << mRows.size() << std::endl;
  //     o << "mReducersTodo size: " << mReducersTodo.size() << std::endl;
  //     exit(1);
  //   }
  const Ring* kk = freeAlgebra().coefficientRing();
  for (int count = 0; count < mRows.size(); ++count)
    {
      // PreRow pr = mReducersTodo[count];
      // o << count << " ("<< std::get<0>(pr) << ", "
      //   << std::get<1>(pr) << ", "
      //   << std::get<2>(pr) << ", "
      //   << std::get<3>(pr) << ") "
      if (mRows[count].columnIndices.begin() == nullptr)
      {
        o << "Row " << count
          << " is empty.  This may indicate an error depending on the current state."
          << std::endl;
        continue;
      }
      o << "Row " << count << ";" << mRows[count].columnIndices.size() << ": ";
      if (mVectorArithmetic->size(mRows[count].coeffVector) != mRows[count].columnIndices.size())
        {
          o << "***ERROR*** expected coefficient array and components array to have the same length" << std::endl;
          exit(1);
        }
      for (int i=0; i < mVectorArithmetic->size(mRows[count].coeffVector); ++i)
        {
          buffer b;
          kk->elem_text_out(b, mVectorArithmetic->ringElemFromSparseVector(mRows[count].coeffVector,i));
          o << "[" << mRows[count].columnIndices[i] << "," << b.str() << "] ";
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
      FreeMonoid::MonomialInserter monomInserter;
      freeAlgebra().monoid().monomInsertFromWord(monomInserter,i.first);
      Monom m(monomInserter.data());
      freeAlgebra().monoid().elem_text_out(b, m);
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
      o << count << " ("<< pr.left << ", "
        << pr.preRowIndex << ", "
        << pr.right << ")";
      int count2 = 0;
      for (int i=0; i < mColumnMonomials.size(); i++)
        {
          if (count2 == mVectorArithmetic->size(mRows[count].coeffVector) or 
              mRows[count].columnIndices[count2] != i)
            {
              o << " 0 ";
            }
          else
            {
              buffer b;
              kk->elem_text_out(b,mVectorArithmetic->ringElemFromSparseVector(mRows[count].coeffVector,count2));
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
