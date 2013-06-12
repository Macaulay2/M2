// Copyright 2005-2012  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

#include "engine-includes.hpp"

#ifdef HAVE_FLINT
#include <flint/arith.h>
#include <flint/nmod_mat.h>
#include <flint/fmpq_mat.h>
#include "aring-zz-flint.hpp"
#include "aring-qq-flint.hpp"
#include "aring-zzp-flint.hpp"
#endif

template<typename ACoeffRing> class DMat;

template<typename ACoeffRing>
class DMatConstIterator
{
public:
  typedef DMat<ACoeffRing> Mat;
  typedef typename Mat::ElementType ElementType;

  DMatConstIterator(const ElementType* start, size_t stride)
    : mCurrent(start),
      mStride(stride)
  {
  }

  void operator++() { mCurrent += mStride; }
  const ElementType& operator*() { return *mCurrent; }
  bool operator==(DMatConstIterator& i) const { return(&(*i) == mCurrent); }
  bool operator!=(DMatConstIterator& i) const { return(&(*i) != mCurrent); }
private:
  const ElementType* mCurrent;
  size_t mStride;
};

template<typename ACoeffRing>
class DMatIterator
{
public:
  typedef DMat<ACoeffRing> Mat;
  typedef typename Mat::ElementType ElementType;

  DMatIterator(ElementType* start, size_t stride)
    : mCurrent(start),
      mStride(stride)
  {
  }

  void operator++() { mCurrent += mStride; }
  ElementType& operator*() { return *mCurrent; }
  bool operator==(DMatConstIterator<ACoeffRing>& i) const { return(&(*i) == mCurrent); }
  bool operator!=(DMatConstIterator<ACoeffRing>& i) const { return(&(*i) != mCurrent); }
private:
  ElementType* mCurrent;
  size_t mStride;
};

template<typename ACoeffRing>
class DMat
{
public:
  typedef ACoeffRing CoeffRing;
  typedef typename ACoeffRing::ElementType ElementType;
  typedef ElementType elem;

  typedef DMatIterator<ACoeffRing> Iterator;
  typedef DMatConstIterator<ACoeffRing> ConstIterator;

  DMat() : mRing(0), mNumRows(0), mNumColumns(0), mArray(0) {}

  DMat(const ACoeffRing& R, size_t nrows, size_t ncols)
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
  DMat(const DMat<ACoeffRing>& M)
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
  ~DMat()
  {
    size_t len = mNumRows * mNumColumns;
    for (size_t i=0; i<len; i++)
      ring().clear(mArray[i]);
    if (mArray != 0) 
      delete [] mArray;
  }

  // swap the actual matrices of 'this' and 'M'.
  void swap(DMat<ACoeffRing>& M) 
  {
    std::swap(mRing, M.mRing);
    std::swap(mNumRows, M.mNumRows);
    std::swap(mNumColumns, M.mNumColumns);
    std::swap(mArray, M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return mNumRows; }
  size_t numColumns() const { return mNumColumns; }

  Iterator rowBegin(size_t row) { return Iterator(array() + row, numRows()); }
  ConstIterator rowBegin(size_t row) const { return ConstIterator(array() + row, numRows()); }
  ConstIterator rowEnd(size_t row) const { return ConstIterator(array() + row + numRows() * numColumns(), numRows()); }

  Iterator columnBegin(size_t col) { return Iterator(array() + col * numRows(), 1); }
  ConstIterator columnBegin(size_t col) const { return ConstIterator(array() + col * numRows(), 1); }
  ConstIterator columnEnd(size_t col) const { return ConstIterator(array() + (col+1) * numRows(), 1); }


  // When we store in row major order, we can change to these values:
  //  ElementType& entry(size_t row, size_t column) { return mArray[mNumColumns * row + column]; }
  //  const ElementType& entry(size_t row, size_t column) const { return mArray[mNumColumns * row + column]; }

  ElementType& entry(size_t row, size_t column) { return mArray[mNumRows * column + row]; }
  const ElementType& entry(size_t row, size_t column) const { return mArray[mNumRows * column + row]; }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }

  const ElementType* array() const { return mArray; }
  ElementType*& array() { return mArray; }

private:
  const ACoeffRing* mRing;
  size_t mNumRows;
  size_t mNumColumns;
  ElementType* mArray;
};


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

////////////////////////////////////////////////////
// Dense matrices for Flint type M2::ARingQQFlint //
////////////////////////////////////////////////////
#ifdef HAVE_FLINT
template<>
class DMat<M2::ARingQQFlint>
{
public:
  typedef M2::ARingQQFlint ACoeffRing;
  typedef ACoeffRing CoeffRing;
  typedef typename ACoeffRing::ElementType ElementType;
  typedef ElementType elem;

  typedef DMatIterator<ACoeffRing> Iterator;
  typedef DMatConstIterator<ACoeffRing> ConstIterator;

  DMat() : mRing(0) {}

  DMat(const ACoeffRing& R, size_t nrows, size_t ncols)
    : mRing(&R)
  {
    fmpq_mat_init(mArray, nrows, ncols);
  }

  DMat(const DMat<ACoeffRing>& M)
    : mRing(& M.ring())
  {
    fmpq_mat_init(mArray, M.numRows(), M.numColumns());
    fmpq_mat_set(mArray, M.mArray);
  }

  ~DMat() 
  {
    fmpq_mat_clear(mArray);
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
    std::swap(*mArray, *M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return fmpq_mat_nrows(mArray); }
  size_t numColumns() const { return fmpq_mat_ncols(mArray); }


  const ElementType* array() const { return mArray->entries; }
  ElementType*& array() { return mArray->entries; }

  ElementType& entry(size_t row, size_t column) { 
    M2_ASSERT(row < numRows());
    M2_ASSERT(column < numColumns());
    return * fmpq_mat_entry(mArray, row, column); 
  }
  const ElementType& entry(size_t row, size_t column) const { 
    M2_ASSERT(row < numRows());
    M2_ASSERT(column < numColumns());
    return * fmpq_mat_entry(mArray, row, column); 
  }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }
public:
  // Other routines from flint nmod_mat interface
  const fmpq_mat_t& fmpq_mat() const { return mArray; }
  fmpq_mat_t& fmpq_mat() { return mArray; }
private:
  const ACoeffRing* mRing;
  fmpq_mat_t mArray;
};
#endif

//////////////////////////////////////////////////////////////
// Flint: use nmod_mat for implementation of dense matrices //
//////////////////////////////////////////////////////////////
#ifdef HAVE_FLINT
template<>
class DMat<M2::ARingZZpFlint>
{
public:
  typedef M2::ARingZZpFlint ACoeffRing;
  typedef ACoeffRing CoeffRing;
  typedef typename ACoeffRing::ElementType ElementType;
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

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
