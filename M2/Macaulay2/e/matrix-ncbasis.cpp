// Copyright 2018  Michael E. Stillman

#include "matrix-ncbasis.hpp"

#include <memory>

#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/FreeMonoid.hpp"
#include "NCAlgebras/Word.hpp"
#include "NCAlgebras/WordTable.hpp"

#include "interrupted.hpp"
#include "monoid.hpp"

std::unique_ptr<WordTable> constructWordTable(const FreeAlgebra& A, const ConstPolyList& gb)
{
  std::unique_ptr<WordTable> W { new WordTable };
  
  // Build the word table for the reduction
  for (auto& f : gb)
    {
      auto i = f->cbegin();
      Word tmp;
      A.monoid().wordFromMonom(tmp,i.monom());
      W->insert(tmp);
    }
  return W;
}

class NCBasis : public our_new_delete
{
private:
  const FreeAlgebra& mFreeAlgebra;
  const FreeMonoid& mMonoid;

  std::vector<int> mVariables; // the variable indices being used.
  std::vector<int> mVariableHefts; // matches mVariables array.

  // Word table of all lead terms from the input GB
  std::unique_ptr<WordTable> mWordTable;
  
  // Construction of monomials: these are used in the recursion.
  std::vector<int> mMonomial; // word being constructed.
  int mCurrentIndex; // into mMonomial... this is where we place next variable.
  int mCurrentHeftValue; // heft value so far, of monomial being constructed.

  // Result
  PolyList mBasis;

  // Input degree bounds, and input options
  int mLimit; // if >= 0, then number of monomials to collect.  <0 means no limit.
  int mLoHeft; // 0 if -infinity was given
  int mHiHeft; // -1 if infinity was given

  // do we need these?  Keep for now.
  // // const int* mLoDegree; // either nullptr (if -infinity), or an array of length = degreeRank
  // // const int* mHiDegree; // an array of length = degreeRank
  // // enum { KB_SINGLE, KB_MULTI } mComputationType;
  // // const int* mCurrentMultiDegree; // only used in multi-grading case?
  // // const int* mMultiDegree; // target multi-degree in the multi-graded case.

  // // std::vector<int> mHeftVector; // length: degreeRank := degreeMonoid().numVars()
  // // std::vector<int> mLoDegree; // either nullptr (if -infinity), or an array of length = degreeRank
  // // std::vector<int> mHiDegree; // an array of length = degreeRank

public:
  // TODO: add in a list of variable indices.
  //       add in heft vectors, multi-degree.
  NCBasis(const FreeAlgebra& A,
          const ConstPolyList& gb,
          const std::vector<int>& lo_degree,
          const std::vector<int>& hi_degree,
          int limit
          )
    :
    mFreeAlgebra(A),
    mMonoid(A.monoid()),
    mVariableHefts(A.monoid().flattenedDegrees()),
    mWordTable(constructWordTable(A,gb)),
    mCurrentIndex(-1),
    mCurrentHeftValue(0),
    mLimit(limit),
    mLoHeft(0),
    mHiHeft(-1)
  {
    // TODO: set mVariables and their hefts from mMonoid info.
    for (auto i = 0; i < mMonoid.numVars(); ++i)
        mVariables.push_back(i);    
    
    if (lo_degree.size() > 0) mLoHeft = lo_degree[0];
    if (hi_degree.size() > 0) mHiHeft = hi_degree[0];
  }

  PolyList& compute()
  {
    if (mLimit != 0) basis0();
    return mBasis;
  }

private:  
  void insert(); // takes mMonomial, and makes a polynomial from it (but with new memory), appending to mBasis.
  void basis0(); // the main recursion step
};

void NCBasis::insert()
{
  Poly* result = new Poly;
  mFreeAlgebra.from_word(*result, Word(mMonomial.data(), mMonomial.data() + mCurrentIndex + 1));
  mBasis.push_back(result);
  if (mLimit > 0) mLimit--;
}

void NCBasis::basis0()
{
  if (system_interrupted()) return;

  // order of events:
  // 1. check if the degree is greater than hi_degree.  If so, exit.
  // 2. check if a suffix of the current word is a pattern in the WordTable.  If so, exit.
  // 3. if the degree is >= lo_degree, then call insert.
  // 4. if the degree is equal to hi_degree, then exit (no recursion necessary)
  // 5. for each variable, append the variable to the state monomial, and make recursive call.

  Word tmpWord(mMonomial.data(),mMonomial.data() + mCurrentIndex + 1);
  int notUsed;
  
  if (mHiHeft != -1 and mCurrentHeftValue > mHiHeft) return;

  if (mWordTable->isSuffix(tmpWord,notUsed)) return;

  if (mCurrentHeftValue >= mLoHeft) insert();

  if (mHiHeft != -1 and mCurrentHeftValue == mHiHeft) return;

  // this ensures that we do not move past allocated memory in the loop below
  if (mMonomial.size() <= mCurrentIndex + 1) mMonomial.push_back(0);

  mCurrentIndex++;
  for (int i = 0; i < mVariables.size(); ++i)
    {
      mMonomial[mCurrentIndex] = mVariables[i];
      mCurrentHeftValue += mVariableHefts[i];
      
      basis0();

      mCurrentHeftValue -= mVariableHefts[i];

      if (mLimit == 0) return;
    }
  mCurrentIndex--;
}

bool ncBasis(
             const FreeAlgebra& A,
             const ConstPolyList& gb, // actually, only the lead terms are ever considered
             const std::vector<int>& lo_degree, // length 0: means -infinity, i.e. 0.
             const std::vector<int>& hi_degree, // length 0: +infinity
             int limit, // <0 means no limit
             PolyList &result
                 )
{
  if (A.monoid().degreeMonoid().n_vars() != 1)
    {
      ERROR("expected singly graded algebra");
      return false;
    }
  if (lo_degree.size() > 1 or hi_degree.size() > 1)
    {
      ERROR("expected singly graded algebra");
      return false;
    }
  NCBasis computation(A, gb, lo_degree, hi_degree, limit);
  std::swap(result, computation.compute());
  return true;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// indent-tabs-mode: nil
// End:
