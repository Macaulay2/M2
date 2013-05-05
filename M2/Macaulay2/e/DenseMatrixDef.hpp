// Copyright 2013  Michael E. Stillman

#ifndef _dmat_new_try_hpp_
#define _dmat_new_try_hpp_

/**
 * \ingroup matrices
 */

#include <flint/arith.h>
#include <flint/nmod_mat.h>
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
private:
  const ACoeffRing* mRing;
  nmod_mat_t mArray;
};

#if 0
template<typename ACoeffRing, typename MatDef>
class DMat
{
public:
  typedef ACoeffRing CoeffRing;
  typedef typename CoeffRing::ElementType ElementType;

  typedef typename EigenvalueMatrixType< DMat<ACoeffRing, MatDef> >::type EigenvalueType;

  //  typedef DMat<typename EigenvalueType<ACoeffRing>::Ring> EigenvalueMatrixType;

  //  DMat(): mGeneralRing(0), mRing(0), array_(0) {} // Makes a zero matrix

  // Makes a zero matrix of size nrows x ncols
  DMat(const Ring* R, const ACoeffRing& R0, size_t nrows, size_t ncols)
    : mGeneralRing(R),
      mRing(R0),
      mMatrix(R0,nrows,ncols)
  {
  }

  //  DMat(const DMat<ACoeffRing> &M, size_t nrows, size_t ncols); // Makes a zero matrix, same ring.

  // Copies (clones) M
  DMat(const DMat<ACoeffRing> &M)
    : mGeneralRing(M.get_ring()),
      mRing(M.ring()),
      mMatrix(M.clone(M.ring()))
  {
  }

  bool is_dense() const { return true; }

  size_t n_rows() const { return mMatrix.numRows(); }
  size_t n_cols() const { return mMatrix.numColumns(); }
  const Ring * get_ring() const { return mGeneralRing; }
  const CoeffRing* get_CoeffRing() const { return &mRing; }
  const CoeffRing& ring() const { return mRing; }

  const ElementType* array() const { return mMatrix.array(); }
  ElementType* array() { return mMatrix.array(); }

private:
  const Ring* mGeneralRing;
  const CoeffRing& mRing;
  MatType mMatrix;
};
#endif
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
