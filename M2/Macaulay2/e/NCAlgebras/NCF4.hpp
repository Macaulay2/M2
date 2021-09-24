#ifndef __nc_f4_hpp__
#define __nc_f4_hpp__

#include <tbb/queuing_mutex.h>                // for queuing_mutex
#include <tbb/null_mutex.h>                   // for null_mutex
#include <tbb/parallel_do.h>                  // for parallel_do_feeder
#include <tbb/concurrent_unordered_map.h>     // for concurrent_unordered_map
//#include <tbb/concurrent_vector.h>          // for concurrent_vector (no longer needed)

#include "NCAlgebras/FreeMonoid.hpp"      // for MonomEq
#include "MemoryBlock.hpp"                // for MemoryBlock
#include "NCAlgebras/Range.hpp"           // for Range
#include "NCAlgebras/Word.hpp"            // for Word
#include "NCAlgebras/OverlapTable.hpp"    // for OverlapTable
#include "NCAlgebras/WordTable.hpp"       // for Overlap, WordTable
#include "NCAlgebras/SuffixTree.hpp"      // for experimental suffix tree code
#include "NCAlgebras/FreeAlgebra.hpp"     // for FreeAlgebra
#include "VectorArithmetic.hpp"           // for VectorArithmetic, CoeffVector, etc
#include "Polynomial.hpp"                 // for Monom, ConstPolyList, Poly
#include "newdelete.hpp"                  // for VECTOR, our_new_delete

#include <deque>                       // for deque
#include <iosfwd>                      // for ostream
#include <map>                         // for map
#include <unordered_map>               // for unordered_map
#include <tuple>                       // for tuple
#include <utility>                     // for pair
#include <vector>                      // for vector

union ring_elem;

// this class contains an NCGB calculation using the F4 algorithm.
// subclasses needed:
//   WordTable/SuffixTree (used for this class and Naive algorithm)
//   OverlapTable         (used for this class and Naive algorithm)
//   F4MatrixBuilder
//   F4Matrix

class NCF4 : public our_new_delete
{
private:
  // Data structures for construction of each spair matrix
  // and data for the matrix itself.

  // memory space for monomials and words for F4 matrix.
  // PreRow is left, index of poly, right, prevReducer
  // where the entries depend on the value of prevReducer.
  // prevReducer = false: if index \geq 0, then left*mGroebner[index]*right
  //                      is the lead term of a reducer.
  //                      if index < 0, then left*mInput[-index-1]*right
  //                      is the lead term of a reducer.
  // prevReducer = true : left*mRows[i]*right is the lead term of a reducer
  // the int at the end is the index of the PreRow in the
  // corresponding vector it belongs to, which will eventually
  // be the corresponding row.

  enum PreRowType { ReducerPreRow, OverlapPreRow, PreviousReducerPreRow };

  struct PreRow
  {
    Word left;
    int preRowIndex;
    Word right;
    PreRowType preRowType;
  };

  struct Row
  {
    CoeffVector coeffVector;     // vector of coefficients
    Range<int> columnIndices;    // column indices used in the row.  Valid *only* after labelAndSortF4Matrix, 
                                 // as the indices are not known during creation.
    Range<Word> columnWords;     // monoms used in the row.  Valid only *before* reduction begins, as reduction
                                 // does not update this field
  };

  struct Column
  {
    Word word;                 // Monom corresponding to the column
    int pivotRow;              // pivot row corresponding to this monomial
  };
  // the index of a Column in a ColumnsVector is the column index and is used in Row.columnIndices.

  //using ColumnsVector = tbb::concurrent_vector<Column>;
  using ColumnsVector = std::vector<Column>;

  // unfortunately we must use the GC allocator here for now
  using RowsVector = std::vector<Row,gc_allocator<Row>>;
  //using RowsVector = tbb::concurrent_vector<Row>;

  using PreRowFeeder = tbb::parallel_do_feeder<PreRow>;
  // The pair in this unordered_map is (i,j) where:
  //    i is the column number
  //    j is the row that reduces it
  //      (and -1 if there is no such row).
  using MonomialHash = tbb::concurrent_unordered_map<Word,std::pair<int,int>,MonomHash,MonomHashEqual>;

  // data
  const FreeAlgebra& mFreeAlgebra;
  const ConstPolyList mInput;
  
  WordTable mWordTable;
  //SuffixTree mWordTable;
  OverlapTable mOverlapTable;
  PolyList mGroebner;

  bool mIsGraded;
  //int mTopComputedDegree;  // not used yet
  //int mHardDegreeLimit;    // not used yet

  MemoryBlock mMonomialSpace;
  MemoryBlock mPreviousMonomialSpace;

  MonomEq mMonomEq;
  MonomHashEqual mMonomHashEqual;
  MonomHash mMonomHash;

  MonomialHash mColumnMonomials;
  MonomialHash mPreviousColumnMonomials;

  std::vector<PreRow> mReducersTodo;
  std::vector<PreRow> mOverlapsTodo;
  ColumnsVector mColumns;
  ColumnsVector mPreviousColumns;

  RowsVector mRows;
  RowsVector mPreviousRows;
  RowsVector mOverlaps;

  int mFirstOverlap; // First non pivot row (and all later ones are also non-pivot rows).

  // vector arithmetic class for reduction
  const VectorArithmetic* mVectorArithmetic;

  bool mIsParallel;
 
  // these are pointers to the MemoryBlocks used in creating the various structures.
  // only used in parallelBuildF4Matrix, which is currently not used.
  std::vector<MemoryBlock*> mMemoryBlocks;
  std::vector<MemoryBlock*> mPreviousMemoryBlocks;
  tbb::queuing_mutex mColumnMutex;

public:
  NCF4(const FreeAlgebra& A,
       const ConstPolyList& input,
       int hardDegreeLimit,
       int strategy,
       bool isParallel
       );

  ~NCF4() {
    for (auto f : mGroebner) {
      mFreeAlgebra.clear(*f);
      delete f;
    }
    clearRows(mRows);
    clearRows(mPreviousRows);
    delete mVectorArithmetic;
  };

  [[nodiscard]] const FreeAlgebra& freeAlgebra() const { return mFreeAlgebra; }

  const PolyList& currentValue() const
  { 
    //return reinterpret_cast<const ConstPolyList&>(mGroebner);
    return mGroebner;
  }

  void compute(int softDegreeLimit);

  void displayF4Matrix(std::ostream& o) const;

  void displayFullF4Matrix(std::ostream& o) const;

  void displayF4MatrixSize(std::ostream & o) const;

private:
  void process(const std::deque<Overlap>& overlapsToProcess);

  void buildF4Matrix(const std::deque<Overlap>& overlapsToProcess);
  void parallelBuildF4Matrix(const std::deque<Overlap>& overlapsToProcess);

  void labelAndSortF4Matrix();

  void reduceF4Matrix();
  void parallelReduceF4Matrix();
  
  // FM : I had to discard const qualifiers here because I used mMonomialSpace.
  // Should I be doing this?  Two questions:  Should we make mMonomialSpace
  // mutable so changes don't trigger const errors, and should I really be
  // using mMonomialSpace anyway?
  Word createOverlapLeadWord(const Overlap& o);
  auto isOverlapNecessary(const Overlap& o) -> bool;
  auto checkOldOverlaps(Word& newLeadWord) -> void;

  void matrixReset();

  // These functions are essentially from NCGroebner
  void addToGroebnerBasis(Poly * toAdd);
  void updateOverlaps(const Poly * toAdd);
  auto overlapHeft(Overlap o) const -> int;
  auto insertNewOverlaps(std::vector<Overlap>& newOverlaps) -> void;

  void reducedRowToPoly(Poly* result,
                        const RowsVector& rows,
                        const ColumnsVector& cols,
                        int i) const;
  PolyList newGBelements() const;  // From current F4 matrix.

  void processPreRow(PreRow r,
                     RowsVector& rowsVector,
                     MemoryBlock& memoryBlock,
                     PreRowFeeder* feeder);
  void processPreRow(PreRow r,
                     RowsVector& rowsVector);

  void processWordInPreRow(Word& w,
                           PreRowFeeder* feeder);

  void preRowsFromOverlap(const Overlap& o);

  std::pair<bool, PreRow> findDivisor(Word w);

  void autoreduceByLastElement();
  ring_elem getCoeffOfMonom(const Poly& f, const Monom& m) const;

  template<typename LockType>
  void generalReduceF4Row(int index,
                          int first,
                          int firstcol,
                          long &numCancellations,
                          DenseCoeffVector& dense,
                          bool updateColumnIndex,
                          LockType& lock);

  void reduceF4Row(int index,
                   int first,
                   int firstcol,
                   long &numCancellations,
                   DenseCoeffVector& dense)
  {
    tbb::null_mutex noLock;
    generalReduceF4Row<tbb::null_mutex>(index,
                                        first,
                                        firstcol,
                                        numCancellations,
                                        dense,
                                        true,
                                        noLock);
  }
  
  void parallelReduceF4Row(int index,
                           int first,
                           int firstcol,
                           long &numCancellations,
                           DenseCoeffVector& dense,
                           tbb::queuing_mutex& lock)
  {
    generalReduceF4Row<tbb::queuing_mutex>(index,
                                           first,
                                           firstcol,
                                           numCancellations,
                                           dense,
                                           false,
                                           lock);
  }

  
  // return value is isFound, columnIndexOfFound
  // discard const qualifier here again because this creates a monom in mMonomialSpace
  std::pair<bool,int> findPreviousReducerPrefix(const Word& w);
  std::pair<bool,int> findPreviousReducerSuffix(const Word& w);

  void clearRows(RowsVector& rowsVector);

  void processPreviousF4Matrix();
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
