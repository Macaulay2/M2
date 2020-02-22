#ifndef __nc_f4_hpp__
#define __nc_f4_hpp__

#include "../newdelete.hpp"
#include "NCAlgebras/MemoryBlock.hpp"
#include "FreeAlgebra.hpp"
#include "WordTable.hpp"
#include "OverlapTable.hpp"

#include <vector>
#include <utility>

class NCF4 : public our_new_delete
{
private:
  const FreeAlgebra& mFreeAlgebra;
  const ConstPolyList mInput;
  
  WordTable mWordTable;
  OverlapTable mOverlapTable;
  ConstPolyList mGroebner;

  bool mIsGraded;
  int mTopComputedDegree;
  int mHardDegreeLimit;

  // Data structures for construction of each spair matrix
  // and data for the matrix itself.

  // memory space for monomials and words for F4 matrix.
  using ColumnIndices = std::pair<int*, int*>;
  using PreRow = std::tuple<Word, int, Word>;
  using Row = std::pair<std::vector<ring_elem>*, // pointer to existing vector in a GB polynomial
                         ColumnIndices>;
  using Column = std::pair<Monom, int>;
  
  MemoryBlock mMonomialSpace;
  std::deque<PreRow> mReducerTodo;
  std::deque<PreRow> mOverlapsTodo;
  int mCurrentReducer;
  int mCurrentOverlap;
  std::vector<Column> mColumns;
  std::vector<Row> mReducers;
  std::vector<Row> mOverlaps;

  // build matrix, from std::vector of overlap pairs.
  void fromOverlapPairs(std::deque<PreRow>&result, const std::deque<Overlap>& tobeProcessed);
  void buildMatrix(const std::vector<PreRow>);
  void sortColumns();

  // Gaussian elimination

  // Translate a Row to a Poly.

  // Overall algorithm logic
public:
  NCF4(const FreeAlgebra& A,
       const ConstPolyList& input,
       int hardDegreeLimit,
       int strategy
       );

  const FreeAlgebra& freeAlgebra() const { return mFreeAlgebra; }
};
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
