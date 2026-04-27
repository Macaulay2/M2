// Copyright 2013  Michael E. Stillman

#ifndef _dmat_zz_flint_hpp_
#define _dmat_zz_flint_hpp_

#include <assert.h>            // for assert
#include <utility>             // for swap
#include "aring-zz-flint.hpp"  // for ARingZZ

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/fmpz_mat.h>  // for fmpz_mat_t, fmpz_mat_entry, fmpz_mat_clear, fmpz_m...
#pragma GCC diagnostic pop

template <typename ACoeffRing>
class DMat;

////////////////////////////////////////////////////
// Dense matrices using Flint...
////////////////////////////////////////////////////

template <>
class DMat<M2::ARingZZ>
// Warning: objects of this class should *not* go to the front end.
// fmpz_t's might be garbage collected out from under you...
{
 public:
  typedef M2::ARingZZ ACoeffRing;
  typedef ACoeffRing CoeffRing;
  typedef ACoeffRing::ElementType ElementType;
  // typedef ElementType elem;
  // typedef ACoeffRing::Element Element;

  DMat() : mRing(0) {}
  DMat(const ACoeffRing& R, size_t nrows, size_t ncols) : mRing(&R)
  {
    fmpz_mat_init(mArray, nrows, ncols);
  }

  DMat(const DMat<ACoeffRing>& M) : mRing(&M.ring())
  {
    fmpz_mat_init_set(mArray, M.mArray);
  }

  ~DMat() { fmpz_mat_clear(mArray); }
  // storage for these rings is row-major, which is reflected in these iterator
  // functions

  // swap the actual matrices of 'this' and 'M'.
  void swap(DMat<ACoeffRing>& M)
  {
    std::swap(mRing, M.mRing);
    fmpz_mat_swap(mArray, M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return fmpz_mat_nrows(mArray); }
  size_t numColumns() const { return fmpz_mat_ncols(mArray); }

  ElementType& entry(size_t row, size_t column)
  {
    assert(row < numRows());
    assert(column < numColumns());
    return *fmpz_mat_entry(mArray, row, column);
  }
  const ElementType& entry(size_t row, size_t column) const
  {
    assert(row < numRows());
    assert(column < numColumns());
    return *fmpz_mat_entry(mArray, row, column);
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
  const fmpz_mat_t& fmpz_mat() const { return mArray; }
  fmpz_mat_t& fmpz_mat() { return mArray; }
 private:
  const ACoeffRing* mRing;
  fmpz_mat_t mArray;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
