#include "../text-io.hpp"
#include "NCAlgebras/NCF4.hpp"

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
      mColumnMonomials(mMonomEq)
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
  buildF4Matrix(overlapsToProcess);
  if (M2_gbTrace >= 200) displayFullF4Matrix(std::cout);
  sortF4Matrix();
  if (M2_gbTrace >= 100) displayFullF4Matrix(std::cout);
  else if (M2_gbTrace >= 50) displayF4Matrix(std::cout);
  else if (M2_gbTrace >= 2) displayF4MatrixSize(std::cout);
  reduceF4Matrix();
  if (M2_gbTrace >= 100) displayFullF4Matrix(std::cout);
  else if (M2_gbTrace >= 50) displayF4Matrix(std::cout);

  // auto-reduce the new elements
  
  // convert back to GB elements...
  ConstPolyList newElems = newGBelements();
  for (auto& f : newElems)
    {
      addToGroebnerBasis(f);
      updateOverlaps(f);
    }
  matrixReset();
}

void NCF4::addToGroebnerBasis(const Poly * toAdd)
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

ConstPolyList NCF4::newGBelements()  // From current F4 matrix.
{
  ConstPolyList result;
  for (int i = mFirstOverlap; i < mRows.size(); i++)
    {
      if (mRows[i].second.size() == 0) continue;
      Poly* f = new Poly;
      for (int j = 0; j < mRows[i].second.size(); j++)
        freeAlgebra().add_to_end(*f, mRows[i].first[j], mColumns[mRows[i].second[j]].first);
      result.push_back(f);
    }
  return result;
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
      mOverlapsTodo.push_back(PreRow(Word(), - gbLeftIndex - 1, Word()));
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
      mReducersTodo.push_back(PreRow(prefix2, gbRightIndex, suffix2));
      mOverlapsTodo.push_back(PreRow(prefix1, gbLeftIndex, suffix1));
    }
  else
    {  
      mReducersTodo.push_back(PreRow(prefix1, gbLeftIndex, suffix1));
      mOverlapsTodo.push_back(PreRow(prefix2, gbRightIndex, suffix2));
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

NCF4::Row NCF4::processPreRow(PreRow r)
{
  // note: left and right should be the empty word if gbIndex < 0 indicating
  // an input polynomial.
  Word left = std::get<0>(r);
  int gbIndex = std::get<1>(r);
  Word right = std::get<2>(r);

  if (M2_gbTrace >= 100) std::cout << "Processing PreRow: (" << left << "," << gbIndex << "," << right << ")" << std::endl;

  const Poly* elem;
  if (gbIndex < 0)
    {
      elem = mInput[-gbIndex-1];
    }
  else
    {
      elem = mGroebner[gbIndex];
    }

  // loop through all monomials of the product
  // for each monomial:
  //  is it in the hash table?
  //    if so: return the column index into the component for the new row.
  //    if not: insert it, and return the new column index into same place
  //        and place this monomial into mColumns.
  //        and search for divisor for it.
  //        
  int nterms = elem->numTerms();
  auto componentRange = mMonomialSpace.allocateArray<int>(nterms);
  int* nextcolloc = componentRange.first;
  for (auto i = elem->cbegin(); i != elem->cend(); ++i)
    {
      Monom m = freeAlgebra().monoid().wordProductAsMonom(left,i.monom(),right,mMonomialSpace);
      auto it = mColumnMonomials.find(m);
      if (it == mColumnMonomials.end())
        { 
          auto divresult = findDivisor(m);
          int divisornum = -1; // -1 indicates no divisor was found
          if (divresult.first)
            {
              divisornum = mReducersTodo.size();
              mReducersTodo.push_back(divresult.second);
            }
          int newColumnIndex = mColumnMonomials.size();
          mColumnMonomials.insert({m, {newColumnIndex, divisornum}});
          *nextcolloc++ = newColumnIndex;
        }
      else
        {
          *nextcolloc++ = (*it).second.first;
        }
    }
  ring_elem* ptr = newarray(ring_elem, elem->getCoeffVector().size());
  Range<ring_elem> coeffrange(ptr, ptr + elem->getCoeffVector().size());
  std::copy(elem->getCoeffVector().cbegin(), elem->getCoeffVector().cend(), coeffrange.begin());
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

std::pair<bool, NCF4::PreRow> NCF4::findDivisor(Monom mon)
{
  Word newword;
  freeAlgebra().monoid().wordFromMonom(newword, mon);
  std::pair<int,int> divisorInfo;
  bool found = mWordTable.subword(newword, divisorInfo);
  // if newword = x^a x^b x^c, with x^b in the word table, then:
  //  divisorInfo.first = index of the GB element with x^b as lead monomial.
  //  divisorInfo.second = position of the start of x^b in newword
  //   (that is, the length of x^a).
  if (not found)
    return std::make_pair(false, PreRow(Word(), 0, Word()));
  Word prefix = Word(newword.begin(), newword.begin() + divisorInfo.second);
  Word divisorWord = mWordTable[divisorInfo.first];
  Word suffix = Word(newword.begin() + divisorInfo.second + divisorWord.size(),
                     newword.end());
  return std::make_pair(true, PreRow(prefix, divisorInfo.first, suffix));
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

void NCF4::reduceF4Matrix()
{
  VectorArithmetic V(freeAlgebra().coefficientRing());

  // Create the dense array.
  ring_elem zero = freeAlgebra().coefficientRing()->zero();
  std::vector<ring_elem> denseVector(mColumnMonomials.size(), zero);
  Range<ring_elem> dense(denseVector);

  // reduce each overlap row by mRows.
  for (int i=mFirstOverlap; i < mRows.size(); ++i)
    {
      int sz = mRows[i].second.size();
      assert(sz > 0);
      int firstcol = -1; // will be set to the first non-zero value in the result
      int first = mRows[i].second[0];
      int last = mRows[i].second[sz-1];

      V.sparseRowToDenseRow(dense, mRows[i].first, mRows[i].second);
      do {
        int pivotrow = mColumns[first].second;
        if (pivotrow >= 0)
          {
            V.denseRowCancelFromSparse(dense, mRows[pivotrow].first, mRows[pivotrow].second);
            int last1 = mRows[pivotrow].second.cend()[-1]; // last component in the row corresponding to pivotrow
            last = (last1 > last ? last1 : last);
          }
        else if (firstcol == -1)
          {
            firstcol = first;
          }
        first = V.denseRowNextNonzero(dense, first+1, last);
      } while (first <= last);
      V.denseRowToSparseRow(dense,
                            mRows[i].first,
                            mRows[i].second,
                            firstcol,
                            last);
      if (mRows[i].first.size() > 0)
        {
          V.sparseRowMakeMonic(mRows[i].first);
          mColumns[firstcol].second = i;
        }
    }
}

void NCF4::displayF4MatrixSize(std::ostream & o) const
{
  // Display sizes:
  o << "(#cols, #reducer rows, #spair rows) = ("
    << mColumnMonomials.size() << ", "
    << mFirstOverlap << ", "
    << mRows.size() - mFirstOverlap << ")"
    << std::endl
    << "  ";
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
