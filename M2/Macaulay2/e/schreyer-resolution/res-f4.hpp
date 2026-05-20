// Copyright 2014-2016 Michael E. Stillman

#ifndef _res_f4_hpp_
#define _res_f4_hpp_

/**
 * @file schreyer-resolution/res-f4.hpp
 * @brief `F4Res` --- F4-style matrix-reduction worker over a `SchreyerFrame`.
 *
 * Declares the per-cell algorithm class `F4ResComputation`
 * invokes to advance the resolution one `(level, degree)` step
 * at a time. `construct(lev, degree)` reads the frame's pending
 * syzygies into one matrix and collects the matching
 * tail-reducers from lower levels and earlier degrees into a
 * second matrix sharing the same columns; the column set is
 * the union of monomials at level `lev - 2` reachable from
 * those rows, sorted by Schreyer order, and the hashtable
 * `MonomialHashTable<ResMonomialsWithComponent> mHashTable`
 * keys those monomials back to their column index.
 * `gaussReduce` row-reduces the S-pair matrix against the
 * reducer matrix via the shared `VectorArithmetic` backend,
 * and the resulting echelon rows are written back into the
 * frame as new syzygies. The matrices live in two parallel
 * `std::vector<Row>` --- `mReducers` (the square reducer
 * matrix) and `mSPairs` (one row per element at this
 * `(lev, degree)`) --- where each `Row` carries
 * `mLeadTerm` / `mComponents` / `mCoeffs`. Monomials at this
 * cell live in `MonomialMemorySpace mMonomSpace2` and are
 * freed in bulk between cells.
 *
 * Construction calls `processMonomialProduct` (creates or
 * looks up a column for `m*n` with skew-sign tracking) and
 * `findDivisor` (which reducer covers a given monomial) before
 * `loadRow` / `reorderColumns` / `makeMatrix` / `gaussReduce`
 * snap the matrix together. `F4Res` holds a reference to its
 * `SchreyerFrame` rather than owning it, so the frame outlives
 * any single reduction; `friend class ResColumnsSorter` gives
 * the column-sorting helper access to the column-index
 * map. `construct(lev, degree)` requires that both
 * `(lev, degree-1)` and `(lev-1, degree-1)` are already
 * complete, **but not** `(lev-1, degree)` (per the in-source
 * NOTE comment). Sibling of `f4/f4.hpp` for the commutative
 * F4.
 *
 * @see res-schreyer-frame.hpp
 * @see res-f4-computation.hpp
 * @see res-poly-ring.hpp
 * @see f4/f4.hpp
 * @see VectorArithmetic.hpp
 */

#include "VectorArithmetic.hpp"
#include "f4/monhashtable.hpp"
#include "schreyer-resolution/res-memblock.hpp"
#include "schreyer-resolution/res-poly-ring.hpp"
#include "monomial-sets.hpp"

#include <cassert>

class SchreyerFrame;

/////////////////////////////////////////////////////////////////////////////

/**
 * @brief F4-style engine that computes one `(level, degree)` slice of a
 * free resolution into the host `SchreyerFrame`.
 *
 * @details `construct(lev, degree)` builds the Macaulay matrix whose rows
 * are the symbolic reducers for each frame element of that
 * `(level, degree)` cell, runs `gaussReduce`, and writes the
 * resulting syzygy polynomials back into the matching
 * `FrameElement::mSyzygy` slots. The matrix construction
 * (`processCurrentMonomial` / `processMonomialProduct` /
 * `loadRow`) and column-sorting passes (`reorderColumns`,
 * `ResColumnsSorter` friend) live inside this class; the
 * `debugOutput*` helpers exist for printing the partial state of
 * the F4 matrix when investigating reduction bugs.
 */
class F4Res
{
  friend class ResColumnsSorter;

 public:
  F4Res(SchreyerFrame& res);

  ~F4Res();

  SchreyerFrame& frame() { return mFrame; }
  const SchreyerFrame& frame() const { return mFrame; }
  // Constructs the elements of the GB at level 'lev', in the given degree.
  // The following must have been done:
  //    construct(lev, degree-1)
  // and
  //    construct(lev-1, degree-1)
  // NOTE: it is not needed to have done: construct(lev-1,degree)
  void construct(int lev, int degree);

  const VectorArithmetic& vectorArithmetic() const { return mRing.vectorArithmetic(); }
  const ResMonoid& monoid() const { return mRing.monoid(); }
  const ResPolyRing& ring() const { return mRing; }
 private:
  /**
   * @brief One row of the Macaulay matrix built by `F4Res::construct`.
   *
   * @details `mLeadTerm` is the previous-level monomial that contributed
   * this row, used after reduction to slot the resulting syzygy
   * back into the right `FrameElement`. `mComponents` and
   * `mCoeffs` are parallel arrays: column index and coefficient
   * for each non-zero entry. `loadRow` populates the arrays;
   * `gaussReduce` consumes them.
   */
  struct Row
  {
    res_packed_monomial
        mLeadTerm;  // monomial (level lev-1) giving rise to this row
    // The following two should have the same length.
    std::vector<ComponentIndex> mComponents;  // indices into mColumns
    ElementArray mCoeffs;
    Row() : mLeadTerm(nullptr) {}
  };

  ////////////////////////////////////
  // Functions for construction //////
  ////////////////////////////////////
  void resetMatrix(int lev,
                   int degree);  // remember to clearMatrix before calling this.
  void clearMatrix();
  bool findDivisor(res_const_packed_monomial m, res_packed_monomial result);
  ComponentIndex processCurrentMonomial(res_packed_monomial thisMonom);  // subroutine of processMonomialProduct
  ComponentIndex processMonomialProduct(res_const_packed_monomial m,
                                        res_const_packed_monomial n,
                                        int& result_sign_if_skew);
  // if result_sign_if_skew is set to 0, then result is set to -1.
  void loadRow(Row& r);
  void reorderColumns();
  void makeMatrix();
  void gaussReduce();
  void gaussReduceRow(int index,
                       ElementArray &dense,
                       bool onlyConstantMaps,
                       const std::vector<bool>& track);

  void debugOutputReducers();
  void debugOutputColumns();
  void debugOutputMatrix(std::vector<Row>&);
  void debugOutputMatrixSparse(std::vector<Row>&);
  void debugOutputReducerMatrix();
  void debugOutputSPairMatrix();
  ////////////////////////////////////
  // Data for construct(lev,degree) //
  ////////////////////////////////////

  SchreyerFrame& mFrame;

  const ResPolyRing& mRing;

  // Data used to construct the next matrix
  int mThisLevel;
  int mThisDegree;
  long mNextReducerToProcess;
  //  res_packed_monomial mNextMonom;

  std::unique_ptr<const ResMonomialsWithComponent>
      mSchreyerRes;  // Support structure for mHashTable
  //  const ResMonomialsWithComponent* mSchreyerRes; // Support structure for
  //  mHashTable
  MonomialHashTable<ResMonomialsWithComponent> mHashTable;  // keys: monomials
                                                            // at level lev-2,
                                                            // values: indices
                                                            // into mColumns.
  // or: -1: means is determined to not need to be a column.

  std::vector<Row> mReducers;  // columns: mColumns.  This is a square matrix.
  std::vector<Row>
      mSPairs;  // columns: also mColumns  One row per element at (lev,degree).
  std::vector<long> mSPairComponents;  // index into mFrame.level(mThisLevel)
  std::vector<res_packed_monomial>
      mColumns;  // all the monomials at level lev-2 we need to consider
  //  ResMemoryBlock<res_monomial_word> mMonomSpace;  // for monomials stored in this
                                               // (lev,degree) in mColumns and
                                               // the lead terms in Row.
  MonomialMemorySpace mMonomSpace2;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
