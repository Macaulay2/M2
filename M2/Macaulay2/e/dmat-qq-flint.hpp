// Copyright 2013  Michael E. Stillman

#ifndef _dmat_qq_flint_hpp_
#define _dmat_qq_flint_hpp_

#include <assert.h>            // for assert
#include <utility>             // for swap
#include "aring-qq-flint.hpp"  // for ARingQQFlint

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fmpq_mat.h>  // for fmpq_mat_t, fmpq_mat_entry, fmpq_mat_init, fmpq_ma...
#pragma GCC diagnostic pop

template <typename ACoeffRing>
class DMat;

////////////////////////////////////////////////////
// Dense matrices for Flint type M2::ARingQQFlint //
////////////////////////////////////////////////////

// Warning: objects of this class should *not* go to the front end.
// fmpz_t's might be garbage collected out from under you...
template <>
class DMat<M2::ARingQQFlint>
{
 public:
  typedef M2::ARingQQFlint ACoeffRing;
  typedef ACoeffRing CoeffRing;
  typedef ACoeffRing::ElementType ElementType;
  // typedef ElementType elem;
  // typedef ACoeffRing::Element Element;

  DMat() : mRing(0) {}
  DMat(const ACoeffRing& R, size_t nrows, size_t ncols) : mRing(&R)
  {
    fmpq_mat_init(mArray, nrows, ncols);
  }

  DMat(const DMat<ACoeffRing>& M) : mRing(&M.ring())
  {
    fmpq_mat_init(mArray, M.numRows(), M.numColumns());
    fmpq_mat_set(mArray, M.mArray);
  }

  ~DMat() { fmpq_mat_clear(mArray); }

    // swap the actual matrices of 'this' and 'M'.
  void swap(DMat<ACoeffRing>& M)
  {
    std::swap(mRing, M.mRing);
    std::swap(*mArray, *M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return fmpq_mat_nrows(mArray); }
  size_t numColumns() const { return fmpq_mat_ncols(mArray); }

  ElementType& entry(size_t row, size_t column)
  {
    assert(row < numRows());
    assert(column < numColumns());
    return *fmpq_mat_entry(mArray, row, column);
  }
  const ElementType& entry(size_t row, size_t column) const
  {
    assert(row < numRows());
    assert(column < numColumns());
    return *fmpq_mat_entry(mArray, row, column);
  }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }

  // These are labelled 'unsafe', as it s possible the rows
  // are out of order (which happens in particular if
  // certain flint functions created this.
  const ElementType* unsafeArray() const { return mArray->entries; }
  ElementType*& unsafeArray() { return mArray->entries; }
  
 public:
  // Other routines from flint nmod_mat interface
  const fmpq_mat_t& fmpq_mat() const { return mArray; }
  fmpq_mat_t& fmpq_mat() { return mArray; }
 private:
  const ACoeffRing* mRing;
  fmpq_mat_t mArray;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
