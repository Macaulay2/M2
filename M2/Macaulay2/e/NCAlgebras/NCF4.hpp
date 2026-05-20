#ifndef __nc_f4_hpp__
#define __nc_f4_hpp__

/**
 * @file NCAlgebras/NCF4.hpp
 * @brief `NCF4` --- non-commutative F4 Gröbner-basis driver building a per-degree Macaulay matrix.
 *
 * Declares the F4-style alternative to the one-overlap-at-a-time
 * `NCGroebner` reduction loop. Each iteration collects every
 * overlap of the current degree from `OverlapTable mOverlapTable`,
 * assembles a Macaulay matrix whose rows are the overlap
 * polynomials plus their tail-reducers (drawn from
 * `mGroebner`, the original inputs `mInput`, and
 * previously-built reducer rows --- corresponding to the three
 * `PreRowType` flavours `ReducerPreRow` / `OverlapPreRow` /
 * `PreviousReducerPreRow`) and whose columns are the distinct
 * monomials seen in any row sorted by the non-commutative
 * monomial order, then row-reduces the whole matrix via
 * `mVectorArithmetic`. Echelon rows whose leading column was
 * not already a basis-element leading column become new GB
 * elements (added to `mGroebner`); their overlaps feed the next
 * degree. Two `RowsVector` matrices (`mRows`/`mPreviousRows`)
 * and a third `mOverlaps` track current vs prior cohorts so
 * `PreviousReducerPreRow` references can be resolved without
 * rebuilding from scratch.
 *
 * Monomial / word storage is owned by an internal pair of
 * `MemoryBlock`s (`mMonomialSpace` / `mPreviousMonomialSpace`).
 * Word divisibility uses the live `WordTable` (the
 * commented-out `SuffixTree mWordTable;` alternative remains
 * available as a swap-in experiment). `m2tbb.hpp` is included
 * for the column-monomial `mtbb::unordered_map MonomialHash`,
 * the `PreRowFeeder = mtbb::feeder<PreRow>` worker queue, and a
 * held `mtbb::task_arena mScheduler` ready for the parallel
 * matrix-build path; the explicit comment "only used in
 * parallelBuildF4Matrix, which is currently not used" flags
 * that the parallel build is dormant scaffolding, and the
 * `concurrent_vector` row / column container choices are
 * commented out in favour of `std::vector` for now.
 *
 * @see NCGroebner.hpp
 * @see FreeAlgebra.hpp
 * @see OverlapTable.hpp
 * @see WordTable.hpp
 * @see SuffixTree.hpp
 * @see VectorArithmetic.hpp
 */

#include "m2tbb.hpp"                      // for tbb interface

#include "NCAlgebras/FreeMonoid.hpp"      // for MonomEq
#include "MemoryBlock.hpp"                // for MemoryBlock
#include "NCAlgebras/Range.hpp"           // for Range
#include "NCAlgebras/Word.hpp"            // for Word
#include "NCAlgebras/OverlapTable.hpp"    // for OverlapTable
#include "NCAlgebras/WordTable.hpp"       // for Overlap, WordTable
#include "NCAlgebras/SuffixTree.hpp"      // for experimental suffix tree code
#include "NCAlgebras/FreeAlgebra.hpp"     // for FreeAlgebra
#include "VectorArithmetic.hpp"           // for VectorArithmetic, ElementArray, etc
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

/**
 * @brief Non-commutative F4 Groebner-basis driver: builds a per-degree
 * Macaulay matrix from overlaps and reduces it in bulk.
 *
 * @details Replaces the one-overlap-at-a-time `NCGroebner` loop with the F4
 * pattern. Each degree, the driver pulls every overlap of that
 * degree from `mOverlapTable`, builds rows from the overlaps plus
 * their tail-reducers (drawn from `mGroebner`, the original
 * `mInput`, or `mRows` --- the three `PreRowType` flavours), labels
 * the columns by the distinct monomials seen, sorts them under the
 * non-commutative monomial order, then echelon-reduces via
 * `mVectorArithmetic`. Echelon rows whose leading column is new
 * become new GB elements and feed the next degree's overlaps.
 *
 * Monomial / word storage is owned by an internal `MemoryBlock` pair
 * (`mMonomialSpace` / `mPreviousMonomialSpace`); divisibility uses
 * `mWordTable`. TBB scaffolding (`PreRowFeeder`, `mScheduler`) is
 * wired up but the parallel build path is currently dormant.
 */
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

  /**
   * @brief Symbolic description of one row before it is materialised in the
   * matrix: a `left * (something) * right` triple.
   *
   * @details `preRowType` decides what `preRowIndex` points at:
   * `ReducerPreRow` indexes either `mGroebner` (`>= 0`) or `mInput`
   * (`< 0`, decoded as `-preRowIndex - 1`); `OverlapPreRow` is a
   * freshly added overlap whose polynomial lives in the current
   * cohort's `mRows`; `PreviousReducerPreRow` refers back into
   * `mPreviousRows`. The `Word`s `left` and `right` are the leading
   * monomial decomposition needed to materialise the row.
   */
  struct PreRow
  {
    Word left;
    int preRowIndex;
    Word right;
    PreRowType preRowType;
  };

  /**
   * @brief A materialised row of the Macaulay matrix: parallel coefficient and
   * monomial arrays.
   *
   * @details `coeffVector` carries the coefficients via the `VectorArithmetic`
   * abstraction. `columnWords` holds the monomial of each non-zero
   * entry and is only valid before reduction starts; once columns are
   * labelled, `columnIndices` carries the integer column indices that
   * reduction actually uses.
   */
  struct Row
  {
    ElementArray coeffVector;     // vector of coefficients
    Range<int> columnIndices;    // column indices used in the row.  Valid *only* after labelAndSortF4Matrix,
                                 // as the indices are not known during creation.
    Range<Word> columnWords;     // monoms used in the row.  Valid only *before* reduction begins, as reduction
                                 // does not update this field
  };

  /**
   * @brief A column of the Macaulay matrix: the monomial that names it plus
   * the row currently acting as its pivot (or -1 if none).
   *
   * @details A `Column`'s position in the enclosing `ColumnsVector` is its
   * column index, so `Row::columnIndices` indexes back into this
   * vector.
   */
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

  using PreRowFeeder = mtbb::feeder<PreRow>;

  // The pair in this unordered_map is (i,j) where:
  //    i is the column number
  //    j is the row that reduces it
  //      (and -1 if there is no such row).
  using MonomialHash = mtbb::unordered_map<Word,std::pair<int,int>,MonomHash,MonomHashEqual>;
  
  /**
   * @brief Per-thread counters tracking how much work the F4 reduction did.
   *
   * @details `numCancellations` is the number of coefficient cancellations
   * the reducer applied, and `numRows` is the number of rows it
   * processed. Stored thread-locally so the parallel build path can
   * tally without contention.
   */
  // thread local information
  struct NCF4Stats {
    NCF4Stats() : numCancellations(0), numRows(0) {}
    long numCancellations;
    long numRows;
  };

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

  int mNumThreads;
  bool mIsParallel;
  
  mtbb::task_arena mScheduler;

 
  // these are pointers to the MemoryBlocks used in creating the various structures.
  // only used in parallelBuildF4Matrix, which is currently not used.
  std::vector<MemoryBlock*> mMemoryBlocks;
  std::vector<MemoryBlock*> mPreviousMemoryBlocks;
  mtbb::queuing_mutex mColumnMutex;

public:
  NCF4(const FreeAlgebra& A,
       const ConstPolyList& input,
       int hardDegreeLimit,
       int strategy,
       int numThreads // 0 for tbb::info::default_concurrency(), for now
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
                          NCF4Stats &ncF4Stats,
                          ElementArray& dense,
                          bool updateColumnIndex,
                          LockType& lock);

  void reduceF4Row(int index,
                   int first,
                   int firstcol,
                   NCF4Stats &ncF4Stats,
                   ElementArray& dense)
  {
    mtbb::null_mutex noLock;
    generalReduceF4Row<mtbb::null_mutex>(index,
                                        first,
                                        firstcol,
                                        ncF4Stats,
                                        dense,
                                        true,
                                        noLock);
  }
  
  void parallelReduceF4Row(int index,
                           int first,
                           int firstcol,
                           NCF4Stats &ncF4Stats,
                           ElementArray& dense,
                           mtbb::queuing_mutex& lock)
  {
    generalReduceF4Row<mtbb::queuing_mutex>(index,
                                           first,
                                           firstcol,
                                           ncF4Stats,
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
