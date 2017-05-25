// Copyright 2013  Michael E. Stillman

#ifndef _dmat_zzp_flint_hpp_
#define _dmat_zzp_flint_hpp_

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/arith.h>
#include <flint/nmod_mat.h>
#include <flint/fmpq_mat.h>
#pragma GCC diagnostic pop

#include "aring-zzp-flint.hpp"

template<typename ACoeffRing> class DMat;

//////////////////////////////////////////////////////////////
// Flint: use nmod_mat for implementation of dense matrices //
//////////////////////////////////////////////////////////////

template<>
class DMat<M2::ARingZZpFlint>
{
public:
  typedef M2::ARingZZpFlint ACoeffRing;
  typedef ACoeffRing CoeffRing;
  typedef ACoeffRing::ElementType ElementType;
  typedef ElementType elem;

  typedef DMatIterator<ACoeffRing> Iterator;
  typedef DMatConstIterator<ACoeffRing> ConstIterator;

  DMat() : mRing(0) {}

  DMat(const ACoeffRing& R, size_t nrows, size_t ncols)
    : mRing(&R)
  {
    nmod_mat_init(mArray, nrows, ncols, R.characteristic());
  }

  DMat(const DMat<ACoeffRing>& M)
    : mRing(& M.ring())
  {
    nmod_mat_init_set(mArray, M.mArray);
  }

  ~DMat() 
  {
    nmod_mat_clear(mArray);
  }

  // storage for these rings is row-major, which is reflected in these iterator functions
  Iterator rowBegin(size_t row) { return Iterator(array() + row * numColumns(), 1); }
  ConstIterator rowBegin(size_t row) const { return ConstIterator(array() + row * numColumns(), 1); }
  ConstIterator rowEnd(size_t row) const { return ConstIterator(array() + (row+1) * numColumns(), 1); }

  Iterator columnBegin(size_t col) { return Iterator(array() + col, numColumns()); }
  ConstIterator columnBegin(size_t col) const { return ConstIterator(array() + col, numColumns()); }
  ConstIterator columnEnd(size_t col) const { return ConstIterator(array() + col + numRows() * numColumns(), numColumns()); }

  // swap the actual matrices of 'this' and 'M'.
  // The rings must be the same.
  void swap(DMat<ACoeffRing>& M) 
  {
    std::swap(mRing, M.mRing);
    std::swap(mArray, M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return nmod_mat_nrows(mArray); }
  size_t numColumns() const { return nmod_mat_ncols(mArray); }

  const ElementType* array() const { return mArray->entries; }
  ElementType*&  array() { return mArray->entries; }

  ElementType& entry(size_t row, size_t column) { return nmod_mat_entry(mArray, row, column); }
  const ElementType& entry(size_t row, size_t column) const { return nmod_mat_entry(mArray, row, column); }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }
public:
  // Access routines so that the flint nmod_mat interface may be used
  const nmod_mat_t& nmod_mat() const { return mArray; }
  nmod_mat_t& nmod_mat() { return mArray; }
private:
  const ACoeffRing* mRing;
  nmod_mat_t mArray;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
