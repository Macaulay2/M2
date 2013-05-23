// Copyright 2005-2012  Michael E. Stillman

#ifndef _dmat_hpp_
#define _dmat_hpp_

union ring_elem;
class Ring;

#include "newdelete.hpp"
#include "ring.hpp"
#include "coeffrings.hpp"

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
  typedef DenseMatrixLinAlg<ACoeffRing> LinAlg;
  //  typedef DenseMatrixArithmetic<CoeffRing> Arithmetic;
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
  //  const CoeffRing* get_CoeffRing() const { return & ring(); }

  size_t numRows() const { return mMatrix.numRows(); }
  size_t numColumns() const { return mMatrix.numColumns(); }
  size_t n_rows() const { return numRows(); }
  size_t n_cols() const { return numColumns(); }


  bool is_dense() const { return true; }

  void text_out(buffer& o) const;

  void set_matrix(const DMat<CoeffRing>& mat0);
  void resize(size_t nrows, size_t ncols);

  double *get_lapack_array() const; // redefined by RR,CC
  double *make_lapack_array() const; // creates an array of doubles (or 0, if not applicable)
  void fill_from_lapack_array(double *lapack_array);  // The size of the array should match the size of this.

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

  bool is_zero() const;

  bool is_equal(const DMat& B) const;

  ///////////////////////////////////
  /// Fast linear algebra routines //
  ///////////////////////////////////

  // this -= B, assertion failure on bad ring or bad sizes
  void subtractInPlace(const DMat& B);

  // this = -this
  void negateInPlace();

  // this = f * this
  void scalarMultInPlace(const ElementType &f);

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

  engine_RawRingElementArrayOrNull characteristicPolynomial() const;

  /** if 'this' is a square n x n matrix, return
      an array, {a0,a1,a2,...,am} such that
      the minimal monic polynomial of 'this' is
      a0 + a1 * t + ... + am * t^m .
  */
  engine_RawRingElementArrayOrNull minimalPolynomial() const;

  void copy_elems(size_t n_to_copy, ElementType *target, size_t target_stride, const ElementType *source, size_t stride) const;


  const ElementType* array() const { return mMatrix.array(); }
  ElementType* array() { return mMatrix.array(); }
  // These functions are used for interface with e.g. lapack, ffpack.
  const ElementType * get_array() const { return array(); }
  ElementType * get_array() { return array(); }
private:

  DenseMatrixDef<ACoeffRing>& getInternalMatrix() { return mMatrix; }
  const DenseMatrixDef<ACoeffRing>& getInternalMatrix() const { return mMatrix; }

  const Ring* mGeneralRing; // To interface to the outside world
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
double * DMat<CoeffRing>::get_lapack_array() const
{
  return 0;
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
void DMat<CoeffRing>::set_matrix(const DMat<CoeffRing>& mat0)
{
  DenseMatrixDef<CoeffRing> newMatrix(mat0.mMatrix);
  mMatrix.swap(newMatrix); // when newMatrix is removed, it frees elements of matrix too
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
bool DMat<CoeffRing>::is_zero() const
{
  size_t len = numRows() * numColumns();
  if (len == 0) return true;
  for (const ElementType *t = array() + len - 1; t >= array(); t--)
    if (!ring().is_zero(*t))
      return false;
  return true;
}

template<typename CoeffRing>
void DMat<CoeffRing>::setFromSubmatrix(const DMat& A, 
                                       M2_arrayint rows,
                                       M2_arrayint cols)
{
  resize(rows->len, cols->len); // resets 'this' to zero matrix
  for (size_t r = 0; r < numRows(); r++)
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

template <typename CoeffRing>
void DMat<CoeffRing>::subtractInPlace(const DMat<CoeffRing>& B)
  // this -= B.
  // assumption:the assert statements below:
{
  M2_ASSERT(&B.ring() == &ring());
  M2_ASSERT(B.numRows() == numRows());
  M2_ASSERT(B.numColumns() == numColumns());
  
  for (size_t i=0; i<numRows()*numColumns(); i++)
    {
      ring().subtract(array()[i], array()[i], B.array()[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::negateInPlace()
  // this = -this
{
  for (size_t i=0; i<numRows()*numColumns(); i++)
    {
      ring().negate(array()[i], array()[i]);
    }
}

template <typename CoeffRing>
void DMat<CoeffRing>::scalarMultInPlace(const ElementType &f)
  // this = f * this
{
  for (size_t i=0; i<numRows()*numColumns(); i++)
    {
      ring().mult(array()[i], f, array()[i]);
    }
}

template <typename CoeffRing>
bool DMat<CoeffRing>::is_equal(const DMat& B) const
{
  M2_ASSERT(&ring() == &B.ring());
  if (B.numRows() != numRows()) return false;
  if (B.numColumns() != numColumns()) return false;
  size_t top = numRows() * numColumns();
  const ElementType * elemsA = get_array();
  const ElementType * elemsB = B.get_array();
  for (size_t i = 0; i < top; i++)
    if (!ring().is_equal(*elemsA++, *elemsB++))
      return false;
  return true;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
