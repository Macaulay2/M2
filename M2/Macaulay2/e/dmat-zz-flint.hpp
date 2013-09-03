// Copyright 2013  Michael E. Stillman

#ifndef _dmat_zz_flint_hpp_
#define _dmat_zz_flint_hpp_

#ifdef HAVE_FLINT
#include <flint/arith.h>
#include <flint/nmod_mat.h>
#include <flint/fmpq_mat.h>
#include "aring-zz-flint.hpp"
#endif

template<typename ACoeffRing> class DMat;

#ifdef HAVE_FLINT
template<>
class DMat<M2::ARingZZ>
{
public:
  typedef M2::ARingZZ ACoeffRing;
  typedef ACoeffRing CoeffRing;
  typedef typename ACoeffRing::ElementType ElementType;
  typedef ElementType elem;

  typedef DMatIterator<ACoeffRing> Iterator;
  typedef DMatConstIterator<ACoeffRing> ConstIterator;

  DMat() : mRing(0) {}

  DMat(const ACoeffRing& R, size_t nrows, size_t ncols)
    : mRing(&R)
  {
    fmpz_mat_init(mArray, nrows, ncols);
  }

  DMat(const DMat<ACoeffRing>& M)
    : mRing(& M.ring())
  {
    fmpz_mat_init_set(mArray, M.mArray);
  }

  ~DMat() 
  {
    fmpz_mat_clear(mArray);
  }

  // storage for these rings is row-major, which is reflected in these iterator functions
  Iterator rowBegin(size_t row) { return Iterator(array() + row * numColumns(), 1); }
  ConstIterator rowBegin(size_t row) const { return ConstIterator(array() + row * numColumns(), 1); }
  ConstIterator rowEnd(size_t row) const { return ConstIterator(array() + (row+1) * numColumns(), 1); }

  Iterator columnBegin(size_t col) { return Iterator(array() + col, numColumns()); }
  ConstIterator columnBegin(size_t col) const { return ConstIterator(array() + col, numColumns()); }
  ConstIterator columnEnd(size_t col) const { return ConstIterator(array() + col + numRows() * numColumns(), numColumns()); }

  // swap the actual matrices of 'this' and 'M'.
  void swap(DMat<ACoeffRing>& M) 
  {
    std::swap(mRing, M.mRing);
    fmpz_mat_swap(mArray, M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return fmpz_mat_nrows(mArray); }
  size_t numColumns() const { return fmpz_mat_ncols(mArray); }


  const ElementType* array() const { return mArray->entries; }
  ElementType*& array() { return mArray->entries; }

  ElementType& entry(size_t row, size_t column) { 
    M2_ASSERT(row < numRows());
    M2_ASSERT(column < numColumns());
    return * fmpz_mat_entry(mArray, row, column); 
  }
  const ElementType& entry(size_t row, size_t column) const { 
    M2_ASSERT(row < numRows());
    M2_ASSERT(column < numColumns());
    return * fmpz_mat_entry(mArray, row, column); 
  }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }
public:
  // Other routines from flint nmod_mat interface
  const fmpz_mat_t& fmpz_mat() const { return mArray; }
  fmpz_mat_t& fmpz_mat() { return mArray; }
private:
  const ACoeffRing* mRing;
  fmpz_mat_t mArray;
};
#endif


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
