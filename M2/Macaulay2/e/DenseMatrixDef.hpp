// Copyright 2013  Michael E. Stillman

#ifndef _dmat_new_try_hpp_
#define _dmat_new_try_hpp_

/**
 * \ingroup matrices
 */

#include <flint/arith.h>
#include <flint/nmod_mat.h>
#include "aring-zz-flint.hpp"
#include "aring-zzp-flint.hpp"

template<typename ACoeffRing>
class DenseMatrixDef
{
public:
  typedef typename ACoeffRing::elem ElementType;

  DenseMatrixDef() : mRing(0), mNumRows(0), mNumColumns(0), mArray(0) {}

  DenseMatrixDef(const ACoeffRing& R, size_t nrows, size_t ncols)
    : mRing(&R), mNumRows(nrows), mNumColumns(ncols)
  {
    size_t len = mNumRows * mNumColumns;
    if (len == 0)
      mArray = 0;
    else
      {
        mArray = new ElementType[len];
        for (size_t i=0; i<len; i++)
          {
            ring().init(mArray[i]);
            ring().set_zero(mArray[i]);
          }
      }
  }
  DenseMatrixDef(const DenseMatrixDef<ACoeffRing>& M)
    : mRing(& M.ring()), mNumRows(M.numRows()), mNumColumns(M.numColumns())
  {
    size_t len = mNumRows * mNumColumns;
    if (len == 0)
      mArray = 0;
    else
      {
        mArray = new ElementType[len];
        for (size_t i=0; i<len; i++)
          ring().init_set(mArray[i], M.array()[i]);
      }
  }
  ~DenseMatrixDef()
  {
    size_t len = mNumRows * mNumColumns;
    for (size_t i=0; i<len; i++)
      ring().clear(mArray[i]);
    delete [] mArray;
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return mNumRows; }
  size_t numColumns() const { return mNumColumns; }

  const ElementType* array() const { return mArray; }
  ElementType* array() { return mArray; }
  ElementType& entry(size_t row, size_t column) { return mArray[mNumColumns * row + column]; }
private:
  const ACoeffRing* mRing;
  size_t mNumRows;
  size_t mNumColumns;
  ElementType* mArray;
};


template<>
class DenseMatrixDef<M2::ARingZZ>
{
public:
  typedef M2::ARingZZ ACoeffRing;
  typedef typename ACoeffRing::ElementType ElementType;

  DenseMatrixDef() : mRing(0) {}

  DenseMatrixDef(const ACoeffRing& R, size_t nrows, size_t ncols)
    : mRing(&R)
  {
    fmpz_mat_init(mArray, nrows, ncols);
  }

  DenseMatrixDef(const DenseMatrixDef<ACoeffRing>& M)
    : mRing(& M.ring())
  {
    fmpz_mat_init_set(mArray, M.mArray);
  }

  ~DenseMatrixDef() 
  {
    fmpz_mat_clear(mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return fmpz_mat_nrows(mArray); }
  size_t numColumns() const { return fmpz_mat_ncols(mArray); }

  const ElementType* array() const { return mArray->entries; }
  ElementType* array() { return mArray->entries; }
  ElementType& entry(size_t row, size_t column) { return * fmpz_mat_entry(mArray, row, column); }
public:
  // Other routines from flint nmod_mat interface
  const fmpz_mat_t& fmpz_mat() const { return mArray; }
  fmpz_mat_t& fmpz_mat() { return mArray; }
private:
  const ACoeffRing* mRing;
  fmpz_mat_t mArray;
};

//////////////////////////////////////////////////////////////
// Flint: use nmod_mat for implementation of dense matrices //
//////////////////////////////////////////////////////////////
template<>
class DenseMatrixDef<M2::ARingZZpFlint>
{
public:
  typedef M2::ARingZZpFlint ACoeffRing;
  typedef typename ACoeffRing::ElementType ElementType;

  DenseMatrixDef() : mRing(0) {}

  DenseMatrixDef(const ACoeffRing& R, size_t nrows, size_t ncols)
    : mRing(&R)
  {
    nmod_mat_init(mArray, nrows, ncols, R.characteristic());
  }

  DenseMatrixDef(const DenseMatrixDef<ACoeffRing>& M)
    : mRing(& M.ring())
  {
    nmod_mat_init_set(mArray, M.mArray);
  }

  ~DenseMatrixDef() 
  {
    nmod_mat_clear(mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return nmod_mat_nrows(mArray); }
  size_t numColumns() const { return nmod_mat_ncols(mArray); }

  const ElementType* array() const { return mArray->entries; }
  ElementType* array() { return mArray->entries; }
  ElementType& entry(size_t row, size_t column) { return nmod_mat_entry(mArray, row, column); }
public:
  // Other routines from flint nmod_mat interface
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
