#ifndef __nc_f4_hpp__
#define __nc_f4_hpp__

#include "../newdelete.hpp"
#include "NCAlgebras/MemoryBlock.hpp"
#include "FreeAlgebra.hpp"
#include "WordTable.hpp"
#include "OverlapTable.hpp"
#include "VectorArithmetic.hpp"
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
  using PreRow = std::tuple<Word, int, Word>;
  using Row = std::pair<Range<ring_elem>,
                        Range<int>>; // components corresponding to monomials appearing
  using Column = std::pair<Monom, int>; // monomial, pivot row for this monomial (if not -1).

  MemoryBlock mMonomialSpace;
  MonomEq mMonomEq;
  std::map<Monom, std::pair<int,int>, MonomEq> mColumnMonomials;
  std::vector<PreRow> mReducersTodo;
  std::vector<PreRow> mOverlapsTodo;
  std::vector<Column> mColumns; // mColumns[c].second is the row which will reduce the c'th monomial (unless it is -1).
  std::vector<Row> mRows;
  std::vector<Row> mOverlaps;
  int mFirstOverlap; // First non pivot row row (and all later ones are also non-pivot rows).
  
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

  void reduceF4Matrix();
  
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
