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
  using Row = std::pair<VECTOR(ring_elem), // copy of existing coeff vector
                        ColumnIndices>; // components corresponding to monomials appearing
  using Column = std::pair<Monom, int>;

  // Where is the actual data stored (i.e. ring_elem's and monomials?)
  // ring_elems: each Poly has a VECTOR(ring_elem).
  // Word's in PreRow's: These are pointer pairs to parts of lead monomials in the GB as constructed
  //    These are constant for the life time of the matrix construction, then we don't need them.
  // Monom's for columns of the F4 matrix:
  //    These are stored in mMonomialSpace, are cleared once the non-zero reduced overlap pairs
  //      are placed back into new Poly's. (Their monomials are copied at that point into the Poly.)
  // ColumnIndices: are pointers into memory in a backing store, perhaps mMonmoialSpace,
  //    perhaps another one (we need to decide).
  //    
  
  MemoryBlock mMonomialSpace;
  MonomEq mMonomEq;
  std::map<Monom, std::pair<int,int>, MonomEq> mColumnMonomials;
  std::vector<PreRow> mReducersTodo;
  std::vector<PreRow> mOverlapsTodo;
  std::vector<Column> mColumns;
  std::vector<Row> mReducers;
  std::vector<Row> mOverlaps;
  int mCurrentReducer;
  int mCurrentOverlap;
  
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

  const ConstPolyList& currentValue() const { return mGroebner; }

  void compute(int softDegreeLimit);

  void displayF4Matrix(std::ostream& o) const;
private:
  void process(const std::deque<Overlap>& overlapsToProcess);

  void buildF4Matrix(const std::deque<Overlap>& overlapsToProcess);

  void sortF4Matrix();
  
  void matrixReset();

  Row processPreRow(PreRow r);

  void preRowsFromOverlap(const Overlap& o);

  std::pair<bool, PreRow> findDivisor(Monom mon);
};
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
