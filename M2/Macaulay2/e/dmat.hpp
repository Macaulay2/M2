// Copyright 2005-2012  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

#include "engine-includes.hpp"
#include "mat-util.hpp"

#include <algorithm>
#include <utility>

template <typename ACoeffRing>
class DMat;

template <typename ACoeffRing>
class DMatConstIterator
{
 public:
  typedef DMat<ACoeffRing> Mat;
  typedef typename Mat::ElementType ElementType;

  DMatConstIterator(const ElementType* start, size_t stride)
      : mCurrent(start), mStride(stride)
  {
  }

  void operator++() { mCurrent += mStride; }
  const ElementType& operator*() { return *mCurrent; }
  bool operator==(const DMatConstIterator& i) const { return (&(*i.mCurrent) == mCurrent); }
  bool operator!=(const DMatConstIterator& i) const { return (&(*i.mCurrent) != mCurrent); }
 private:
  const ElementType* mCurrent;
  size_t mStride;
};

template <typename ACoeffRing>
class DMatIterator
{
 public:
  typedef DMat<ACoeffRing> Mat;
  typedef typename Mat::ElementType ElementType;

  DMatIterator(ElementType* start, size_t stride)
      : mCurrent(start), mStride(stride)
  {
  }

  void operator++() { mCurrent += mStride; }
  ElementType& operator*() { return *mCurrent; }
  bool operator==(DMatConstIterator<ACoeffRing>& i) const
  {
    return (&(*i) == mCurrent);
  }
  bool operator!=(DMatConstIterator<ACoeffRing>& i) const
  {
    return (&(*i) != mCurrent);
  }

 private:
  ElementType* mCurrent;
  size_t mStride;
};

// Special instantiations of DMat class
#include "dmat-zz-flint.hpp"
#include "dmat-qq-flint.hpp"
#include "dmat-zzp-flint.hpp"
#include "dmat-gf-flint-big.hpp"
#include "dmat-gf-flint.hpp"

template <typename ACoeffRing>
class DMat
{
 public:
  typedef ACoeffRing CoeffRing;
  typedef typename ACoeffRing::ElementType ElementType;
  typedef ElementType elem;

  typedef DMatIterator<ACoeffRing> Iterator;
  typedef DMatConstIterator<ACoeffRing> ConstIterator;

  DMat() : mRing(nullptr), mNumRows(0), mNumColumns(0), mArray(nullptr) {}
  DMat(const ACoeffRing& R, size_t nrows, size_t ncols)
      : mRing(&R), mNumRows(nrows), mNumColumns(ncols)
  {
    size_t len = mNumRows * mNumColumns;
    if (len == 0)
      mArray = nullptr;
    else
      {
        mArray = newarray(ElementType,len);
        //        mArray = new ElementType[len];
        for (size_t i = 0; i < len; i++)
          {
            ring().init(mArray[i]);
            ring().set_zero(mArray[i]);
          }
      }
  }
  DMat(const DMat<ACoeffRing>& M)
      : mRing(&M.ring()), mNumRows(M.numRows()), mNumColumns(M.numColumns())
  {
    size_t len = mNumRows * mNumColumns;
    if (len == 0)
      mArray = nullptr;
    else
      {
        mArray = newarray(ElementType,len);
        //        mArray = new ElementType[len];
        for (size_t i = 0; i < len; i++)
          ring().init_set(mArray[i], M.array()[i]);
      }
  }
  ~DMat()
  {
    size_t len = mNumRows * mNumColumns;
    for (size_t i = 0; i < len; i++) ring().clear(mArray[i]);
    //    if (mArray != 0) delete[] mArray;
    if (mArray != nullptr) freemem(mArray);
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
  // column-major order (old)
  // Iterator rowBegin(size_t row) { return Iterator(array() + row, numRows());
  // }
  // ConstIterator rowBegin(size_t row) const { return ConstIterator(array() +
  // row, numRows()); }
  // ConstIterator rowEnd(size_t row) const { return ConstIterator(array() + row
  // + numRows() * numColumns(), numRows()); }

  // Iterator columnBegin(size_t col) { return Iterator(array() + col *
  // numRows(), 1); }
  // ConstIterator columnBegin(size_t col) const { return ConstIterator(array()
  // + col * numRows(), 1); }
  // ConstIterator columnEnd(size_t col) const { return ConstIterator(array() +
  // (col+1) * numRows(), 1); }

  // row-major order
  Iterator rowBegin(size_t row)
  {
    return Iterator(array() + row * numColumns(), 1);
  }
  ConstIterator rowBegin(size_t row) const
  {
    return ConstIterator(array() + row * numColumns(), 1);
  }
  ConstIterator rowEnd(size_t row) const
  {
    return ConstIterator(array() + (row + 1) * numColumns(), 1);
  }

  Iterator columnBegin(size_t col)
  {
    return Iterator(array() + col, numColumns());
  }
  ConstIterator columnBegin(size_t col) const
  {
    return ConstIterator(array() + col, numColumns());
  }
  ConstIterator columnEnd(size_t col) const
  {
    return ConstIterator(array() + col + numRows() * numColumns(),
                         numColumns());
  }

  // column-major order (old)
  // ElementType& entry(size_t row, size_t column) { return mArray[mNumRows *
  // column + row]; }
  // const ElementType& entry(size_t row, size_t column) const { return
  // mArray[mNumRows * column + row]; }

  // row-major order
  ElementType& entry(size_t row, size_t column)
  {
    return mArray[mNumColumns * row + column];
  }
  const ElementType& entry(size_t row, size_t column) const
  {
    return mArray[mNumColumns * row + column];
  }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }

  const ElementType* array() const { return mArray; }
  ElementType*& array() { return mArray; }
  const ElementType* rowMajorArray() const { return mArray; }
  ElementType*& rowMajorArray() { return mArray; }
 private:
  const ACoeffRing* mRing;
  size_t mNumRows;
  size_t mNumColumns;
  ElementType* mArray;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
