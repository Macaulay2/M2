// Copyright 2005-2012  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

#include "engine-includes.hpp"

#ifdef HAVE_FLINT
#include <flint/arith.h>
#include <flint/nmod_mat.h>
#include "aring-zz-flint.hpp"
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
  ElementType* array() { return mArray; }

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

  // swap the actual matrices of 'this' and 'M'.
  void swap(DMat<ACoeffRing>& M) 
  {
    std::swap(mRing, M.mRing);
    fmpz_mat_swap(mArray, M.mArray);
  }

  const ACoeffRing& ring() const { return *mRing; }
  size_t numRows() const { return fmpz_mat_nrows(mArray); }
  size_t numColumns() const { return fmpz_mat_ncols(mArray); }

  Iterator rowBegin(size_t row) { return Iterator(array() + row, numRows()); }
  ConstIterator rowBegin(size_t row) const { return ConstIterator(array() + row, numRows()); }
  ConstIterator rowEnd(size_t row) const { return ConstIterator(array() + row + numRows() * numColumns(), numRows()); }

  Iterator columnBegin(size_t col) { return Iterator(array() + col * numRows(), 1); }
  ConstIterator columnBegin(size_t col) const { return ConstIterator(array() + col * numRows(), 1); }
  ConstIterator columnEnd(size_t col) const { return ConstIterator(array() + (col+1) * numRows(), 1); }

  const ElementType* array() const { return mArray->entries; }
  ElementType* array() { return mArray->entries; }

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

  Iterator rowBegin(size_t row) { return Iterator(array() + row, numRows()); }
  ConstIterator rowBegin(size_t row) const { return ConstIterator(array() + row, numRows()); }
  ConstIterator rowEnd(size_t row) const { return ConstIterator(array() + row + numRows() * numColumns(), numRows()); }

  Iterator columnBegin(size_t col) { return Iterator(array() + col * numRows(), 1); }
  ConstIterator columnBegin(size_t col) const { return ConstIterator(array() + col * numRows(), 1); }
  ConstIterator columnEnd(size_t col) const { return ConstIterator(array() + (col+1) * numRows(), 1); }

  const ElementType* array() const { return mArray->entries; }
  ElementType* array() { return mArray->entries; }

  ElementType& entry(size_t row, size_t column) { return nmod_mat_entry(mArray, row, column); }
  const ElementType& entry(size_t row, size_t column) const { return nmod_mat_entry(mArray, row, column); }

  void resize(size_t new_nrows, size_t new_ncols)
  {
    DMat newMatrix(ring(), new_nrows, new_ncols);
    swap(newMatrix);
  }
public:
  // Other routines from flint nmod_mat interface
  const nmod_mat_t& nmod_mat() const { return mArray; }
  nmod_mat_t& nmod_mat() { return mArray; }
private:
  const ACoeffRing* mRing;
  nmod_mat_t mArray;
};
#endif


#if 0
// Below this is the "OLD" version of dmat.hpp
// The "NEW" version is coming from DenseMatrixDef
// We will see what we need to pha
#include "DenseMatrixDef.hpp"
#include "DenseMatrixLinAlg.hpp"

class MutableMatrix;

template <typename RingType>
class EigenvalueType
{
public:
  typedef RingType Ring;
};

template<>
class EigenvalueType<Ring_RRR>
{
public:
  typedef CoefficientRingCCC Ring;
};

template <typename MT> class MatElementaryOps;
template <typename MT> class MatArithmetic;
template <typename RT> class DMatLU;

/**
 * \ingroup matrices
 */
template<typename ACoeffRing>
class DMat : public our_new_delete
{
  friend class Lapack;
  friend class MatElementaryOps<DMat>;
  friend class MatArithmetic<DMat>;
  friend class DMatLU<ACoeffRing>;

  typedef typename EigenvalueType<ACoeffRing>::Ring EigenvalueRing;
public:
  typedef ACoeffRing CoeffRing;
  typedef typename CoeffRing::elem ElementType;
  typedef ElementType elem; // same as ElementType.  Will possibly remove 'elem' later.

  typedef DMat<EigenvalueRing> EigenvalueMatrixType;

  //  typedef ApproximateLinAlg<ACoeffRing> ApproximateLinAlg;

  DMat() {} // Makes an unusable matrix, over no ring.
  DMat(const ACoeffRing *R0, size_t nrows, size_t ncols); // Makes a zero matrix
  DMat(const DMat<ACoeffRing> &M, size_t nrows, size_t ncols); // Makes a zero matrix, same ring.
  DMat(const DMat<ACoeffRing> &M); // Copies (clones) M into 'this'


  const CoeffRing& ring() const { return mMatrix.ring(); }

  size_t numRows() const { return mMatrix.numRows(); }
  size_t numColumns() const { return mMatrix.numColumns(); }

  bool is_dense() const { return true; }

  void text_out(buffer& o) const;

  void resize(size_t nrows, size_t ncols);

  // Warning: iterator cannot be used on a matrix with no entries
  class iterator : public our_new_delete
  {
    const DMat<CoeffRing> *M;
    size_t col;
    const ElementType *begin;
    const ElementType *end;
    void to_next_valid() {
      const CoeffRing& R = M->ring();
      --begin;
      while (begin >= end && R.is_zero(*begin)) --begin;
    }
  public:
    void set(size_t col0) {
      M2_ASSERT(M->array() != 0);
      col = col0;
      begin = M->array() + (col0+1) * (M->numRows());
      end = begin-M->numRows();
      to_next_valid();
    }
    iterator(const DMat<CoeffRing> *M0) : M(M0),
                                          col(-1),
                                          begin(0),
                                          end(0) { }
    const ElementType &value() { return *begin; }
    void next() { to_next_valid(); }
    bool valid() { return begin >= end; }
    size_t row() { return (begin-end); }

    void copy_elem(ring_elem &result) {
      M->ring().to_ring_elem(result, value());
    }
  };


public:
  // The following routines return false if one of the row or columns given
  // is out of range.

  bool get_entry(size_t r, size_t c, ElementType &result) const;
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.

  void set_entry(size_t r, size_t c, const ElementType &a);
  // Returns false if (r,c) is out of range, or the ring of a is wrong.

  ///////////////////////////////////
  /// Fast linear algebra routines //
  ///////////////////////////////////

  void setFromSubmatrix(const DMat &A, M2_arrayint rows, M2_arrayint cols);

  void setFromSubmatrix(const DMat &A, M2_arrayint cols);

  size_t rank() const;
  void determinant(ElementType &result) const;

  size_t new_rank() const
  {
    return LinAlg::rank(mMatrix);
  }

  void new_determinant(ElementType &result_det) const
  {
    LinAlg::determinant(mMatrix, result_det);
  }

  // Set 'inverse' with the inverse of 'this'.  If the matrix is not square, or 
  // the matrix is not invertible, or
  // the ring is one in which the matrix cannot be inverted,
  // then false is returned, and an error message is set.
  bool invert(DMat<ACoeffRing> &result_inverse) const
  {
    return LinAlg::inverse(mMatrix, result_inverse.mMatrix);
  }

  // Returns an array of increasing integers {n_1, n_2, ...}
  // such that if M is the matrix with rows (resp columns, if row_profile is false)
  // then rank(M_{0..n_i-1}) + 1 = rank(M_{0..n_i}).
  // NULL is returned, and an error is set, if this function is not available
  // for the given choice of ring and dense/sparseness.
  M2_arrayintOrNull rankProfile(bool row_profile) const;
  
  // Find a spanning set for the null space.  If M = this,
  // and right_side is true, return a matrix whose rows span {x |  xM = 0},
  // otherwise return a matrix whose columns span {x | Mx = 0}
  void nullSpace(DMat<ACoeffRing> &nullspace, bool right_side) const;

  // X is set to  a matrix whose rows or columns solve either AX = B (right_side=true)
  // or XA = B (right_side=false). If no solutions are found, false is returned.
  bool solveLinear(DMat<ACoeffRing> &X, const DMat<ACoeffRing> &B, bool right_side) const;

  /** C=this,A,B should be mutable matrices over the same ring, and a,b
     elements of this ring. AND of the same density type.
     C = b*C + a * op(A)*op(B),
     where op(A) = A or transpose(A), depending on transposeA
     where op(B) = B or transpose(B), depending on transposeB
  */  
  void addMultipleTo(const DMat<ACoeffRing> &A,
                     const DMat<ACoeffRing> &B,
                     bool transposeA,
                     bool transposeB,
                     const ElementType& a,
                     const ElementType& b);

  /** if 'this' is a square n x n matrix, return
      an array, {a0,a1,a2,...,an} such that
      the characteristic polynomial is
      det(t*I - this) = a0 + a1 * t + ... + an * t^n .
  */

  void mult(const DMat<ACoeffRing>& B,
            DMat<ACoeffRing>& result_product) const
  {
    return DenseMatrixLinAlg<CoeffRing>::mult(mMatrix, B.mMatrix, result_product.mMatrix);
  }

  void copy_elems(size_t n_to_copy, ElementType *target, size_t target_stride, const ElementType *source, size_t stride) const;

  const ElementType* array() const { return mMatrix.array(); }
  ElementType* array() { return mMatrix.array(); }
private:

  DenseMatrixDef<ACoeffRing>& getInternalMatrix() { return mMatrix; }
  const DenseMatrixDef<ACoeffRing>& getInternalMatrix() const { return mMatrix; }

  DenseMatrixDef<CoeffRing> mMatrix;
};

/////////////////////////
// Implementation ///////
/////////////////////////

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const CoeffRing *coeffR0, size_t nrows, size_t ncols)
  : mMatrix(*coeffR0, nrows, ncols)
{
}

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const DMat<CoeffRing> &m, size_t nrows, size_t ncols)
  : mMatrix(m.ring(), nrows, ncols)
{
}

template<typename CoeffRing>
DMat<CoeffRing>::DMat(const DMat<CoeffRing> &m)
  : mMatrix(m.mMatrix)
{
}

template<typename CoeffRing>
void DMat<CoeffRing>::resize(size_t new_nrows, size_t new_ncols)
{
  DenseMatrixDef<CoeffRing> newMatrix(ring(), new_nrows, new_ncols);
  mMatrix.swap(newMatrix); // when newMatrix is removed, it frees elements of matrix too
}

template<typename CoeffRing>
void DMat<CoeffRing>::text_out(buffer& o) const
{
  buffer *p = new buffer[numRows()];
  for (size_t c=0; c<numColumns(); c++)
    {
      size_t maxcount = 0;
      for (size_t r=0; r<numRows(); r++)
        {
          const ElementType& f = mMatrix.entry(r,c);
          if (!ring().is_zero(f))
            {
              //#warning "don't forget to write elem_text_out for CoeffRing's"
              //              ring().elem_text_out(p[r], f);
            }
          else
            p[r] << ".";
          if (p[r].size() > maxcount)
            maxcount = p[r].size();
        }
      for (size_t r=0; r<numRows(); r++)
        for (size_t k=maxcount+1-p[r].size(); k > 0; k--)
          p[r] << ' ';
    }
  for (size_t r=0; r<numRows(); r++)
    {
      p[r] << '\0';
      char *s = p[r].str();
      o << s << newline;
    }
  delete[] p;
}

template<typename CoeffRing>
bool DMat<CoeffRing>::get_entry(size_t r, size_t c, ElementType &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  ring().set(result, mMatrix.entry(r,c));
  return !ring().is_zero(result);
}

template<typename CoeffRing>
void DMat<CoeffRing>::set_entry(size_t r, size_t c, const ElementType &a)
{
  ring().set(mMatrix.entry(r,c), a);
}

template<typename CoeffRing>
void DMat<CoeffRing>::copy_elems(size_t n_to_copy, 
                                 ElementType *target, 
                                 size_t target_stride, 
                                 const ElementType *source, 
                                 size_t stride) const
{
  for (size_t i=0; i<n_to_copy; i++)
    {
      ring().set(*target, *source);
      target += target_stride;
      source += stride;
    }
}

template<typename CoeffRing>
void DMat<CoeffRing>::setFromSubmatrix(const DMat& A, 
                                       M2_arrayint rows,
                                       M2_arrayint cols)
{
  resize(rows->len, cols->len); // resets 'this' to zero matrix
  for (size_t r = 0; r < rows->len; r++)
    for (size_t c = 0; c < cols->len; c++)
      ring().set(mMatrix.entry(r,c), A.mMatrix.entry(rows->array[r],cols->array[c]));
}

template<typename CoeffRing>
void DMat<CoeffRing>::setFromSubmatrix(const DMat& A, 
                                       M2_arrayint cols)
{
  resize(numRows(), cols->len); // resets 'this' to zero matrix
  for (size_t r = 0; r < numRows(); r++)
    for (size_t c = 0; c < cols->len; c++)
      ring().set(mMatrix.entry(r,c), A.mMatrix.entry(r,cols->array[c]));
}
#endif // #if 0 starting "OLD" dmat code

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
