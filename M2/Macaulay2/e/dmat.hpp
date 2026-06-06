// Copyright 2005-2012  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

#include "engine-includes.hpp"
#include "mat-util.hpp"

#include <algorithm>
#include <utility>
#include <vector>

template <typename ACoeffRing>
class DMat;

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
  typedef typename ACoeffRing::Element Element;

  DMat() : mRing(nullptr), mNumRows(0), mNumColumns(0), mArray(nullptr) {}
  DMat(const ACoeffRing& R,
       size_t nrows,
       size_t ncols)
      : mRing(&R),
        mNumRows(nrows),
        mNumColumns(ncols)
  {
    size_t len = mNumRows * mNumColumns;
    if (len == 0)
      {
        mArray = nullptr;
      }
    else
      {
        mArray = newarray(ElementType,len);
        for (size_t i = 0; i < len; i++)
          {
            ring().init(mArray[i]);
            ring().set_zero(mArray[i]);
          }
        ElementType* next = mArray;
        for (size_t r=0; r < mNumRows; ++r)
          {
            mRowPointers.push_back(next);
            next += mNumColumns;
          }
      }
  }
  DMat(const DMat<ACoeffRing>& M)
      : mRing(&M.ring()),
        mNumRows(M.numRows()),
        mNumColumns(M.numColumns())
  {
    size_t len = mNumRows * mNumColumns;
    if (len == 0)
      mArray = nullptr;
    else
      {
        mArray = newarray(ElementType,len);
        for (size_t i = 0; i < len; i++)
          {
            ring().init(mArray[i]);
            ring().set_zero(mArray[i]);
          }
        ElementType* next = mArray;
        for (size_t r=0; r < mNumRows; ++r)
          {
            mRowPointers.push_back(next);
            next += mNumColumns;
          }
        for (size_t r = 0; r < mNumRows; ++r)
          for (size_t c = 0; c < mNumColumns; ++c)
            ring().init_set(entry(r,c), M.entry(r,c));
      }
  }

  ~DMat()
  {
    size_t len = mNumRows * mNumColumns;
    for (size_t i = 0; i < len; i++) ring().clear(mArray[i]);
    if (mArray != nullptr) freemem(mArray);
    // don't need to free mRowPointers (they are pointers into mArray...)
  }

  // swap the actual matrices of 'this' and 'M'.
  void swap(DMat<ACoeffRing>& M)
  {
    std::swap(mRing, M.mRing);
    std::swap(mNumRows, M.mNumRows);
    std::swap(mNumColumns, M.mNumColumns);
    std::swap(mArray, M.mArray);
    std::swap(mRowPointers, M.mRowPointers);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return mNumRows; }
  size_t numColumns() const { return mNumColumns; }

  // default placement: elements in row-major order
  ElementType& entry(size_t row, size_t column)
  {
    return *(mRowPointers[row] + column);
  }
  const ElementType& entry(size_t row, size_t column) const
  {
    return *(mRowPointers[row] + column);
  }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }

  // Get rid of these too!
  const ElementType* rowMajorArray() const { return mArray; }
  ElementType*& rowMajorArray() { return mArray; }

  // These are labelled 'unsafe', as it s possible the rows
  // are out of order (which happens in particular if
  // certain flint functions created this.
  const ElementType* unsafeArray() const { return mArray; }
  ElementType*& unsafeArray() { return mArray; }
  
 private:
  const ACoeffRing* mRing;
  size_t mNumRows;
  size_t mNumColumns;
  ElementType* mArray;
  std::vector<ElementType*> mRowPointers;
};



#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
