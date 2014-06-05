// This file should only be included once, by what?

  //////////////////////
  // ZZpFlint //////////
  //////////////////////

#include "dmat-qq-interface-flint.hpp"

template<>
class DMatLinAlg<M2::ARingQQ>
{
public:
  typedef M2::ARingQQ RingType;
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;

public:
  DMatLinAlg(const Mat& A) : mInputMatrix(A) {}

  size_t rank() 
  { 
    FlintQQMat A(mInputMatrix);
    fmpz_mat_t m1;
    fmpz_mat_init(m1, A.numRows(), A.numColumns());
    fmpq_mat_get_fmpz_mat_rowwise(m1, NULL, A.value());
    size_t rk = fmpz_mat_rank(m1);
    fmpz_mat_clear(m1);
    return rk;
  }

  void determinant(ElementType& result_det) 
  { 
    FlintQQMat A(mInputMatrix);
    fmpq_t det;
    fmpq_init(det);
    fmpq_mat_det(det, A.value());
    fmpq_get_mpq(&result_det, det);
    fmpq_clear(det);
  }

  void columnRankProfile(std::vector<size_t>& profile) 
  { 
    FlintQQMat A(mInputMatrix);
    FlintQQMat B(A.numRows(), A.numColumns());
    fmpq_mat_rref(B.value(), A.value());

    profile.clear();
    size_t thiscol = 0;
    size_t thisrow = 0;
    while (thisrow < B.numRows() 
           and thiscol < B.numColumns())
      {
        if (not fmpq_is_zero(fmpq_mat_entry(B.value(),thisrow,thiscol)))
          {
            profile.push_back(thiscol);
            thisrow++;
          }
        thiscol++;
      }
  }
  
  void matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
  {
    printf("dmat lu qq PLU\n");
    FlintQQMat A(mInputMatrix);
    FlintZZMat LU(A.numRows(), A.numColumns());
    fmpz_t den, den2;
    fmpz_init(den);
    fmpz_init(den2);
    long* perm = newarray_atomic(long, LU.numRows());
    for (long i=0; i<LU.numRows(); i++)
      perm[i] = i;
    fmpq_mat_get_fmpz_mat_matwise(LU.value(), den, A.value());
    fmpz_mat_fflu(LU.value(), den2, perm, LU.value(), 0); // the 0 means do not abort if rank is not maximal
    P.resize(0);
    for (long i=0; i<LU.numRows(); i++)
      P.push_back(perm[i]);
    deletearray(perm);

    fmpz_mul(den, den, den2); // total denominator
    // Now we need to set L and U.
    // Sigh, this code is taken and hacked from LUUtil<RingType>::setUpperLower.
    size_t min = std::min(LU.numRows(), LU.numColumns());
    L.resize(LU.numRows(), min);
    U.resize(min, LU.numColumns());
    
    // At this point, lower and upper should be zero matrices.
    M2_ASSERT(MatrixOps::isZero(L));
    M2_ASSERT(MatrixOps::isZero(U));

    fmpq_t b;
    fmpq_init(b);
    for (size_t c=0; c<LU.numColumns(); c++)
      {
        if (c < min)
          L.ring().set_from_long(L.entry(c,c), 1);
        for (size_t r=0; r<LU.numRows(); r++)
          {
            if (r <= c)
              {
                mpq_t a;
                // Set the value in U by dividing the fmpz by the denom
                // Note: the elements in LU have type fmpz_t
                // the elements in L or U: have type mpq_t
                
                fmpq_set_fmpz_frac(b, fmpz_mat_entry(LU.value(),r,c), den);
                flint_mpq_init_set_readonly(a, b);
                U.ring().set_from_mpq(U.entry(r,c), a);
                flint_mpq_clear_readonly(a);
              }
            else if (c < L.numRows())
              {
                mpz_t a;
                flint_mpz_init_set_readonly(a, fmpz_mat_entry(LU.value(),r,c));
                L.ring().set_from_mpz(L.entry(r,c), a);
                flint_mpz_clear_readonly(a);
              }
          }
      }

    fmpq_clear(b);
    fmpz_clear(den);
    fmpz_clear(den2);
#if 0
    // order of events:
    // take input matrix, make it over ZZ (element wise, or column wise?)
    // do lu in that case.  get a denom.
    // grab P, L directly, but for U, mult it by corresponding value...
    fmpq_mat_lu(perm, A.value(), false);
    P.resize(0);
    for (long i=0; i<LU.numRows(); i++)
      P.push_back(perm[i]);
    deletearray(perm);
    Mat LU;
    A.toDMat(LU);
    LUUtil<RingType>::setUpperLower(LU, L, U);

    // TODO
    Mat LU(mMatrix); // copy
    long* perm = newarray_atomic(long, LU.numRows());
    fmpq_mat_lu(perm, LU.fmpq_mat(), false);
    P.resize(0);
    for (long i=0; i<LU.numRows(); i++)
      P.push_back(perm[i]);
    deletearray(perm);
    LUUtil<RingType>::setUpperLower(LU, L, U);
#endif
  }

  bool solve(const Mat& B, Mat& X) 
  { 
    // TODO
#if 0
    Mat& A1 = const_cast<Mat&>(mMatrix); // needed because fmpq_mat_solve doesn't declare params const
    Mat& B1 = const_cast<Mat&>(B);
    return fmpq_mat_solve(X.fmpq_mat(), B1.fmpq_mat(), A1.fmpq_mat());
#endif
    return false;
  }

  bool solveInvertible(const Mat& B, Mat& X) 
  { 
    // TODO
#if 0
    Mat& A1 = const_cast<Mat&>(mMatrix); // needed because fmpq_mat_solve doesn't declare params const
    Mat& B1 = const_cast<Mat&>(B);
    return fmpq_mat_solve(X.fmpq_mat(), B1.fmpq_mat(), A1.fmpq_mat());
#endif
    return false;
  }

  bool inverse(Mat& result_inv) 
  { 
    FlintQQMat A(mInputMatrix);
    // This seems like alot of futzing around to get the inverse of a matrix over QQ...
    // printf("in dmat-lu-qq inverse\n");
    long nrows = A.numRows();
    FlintQQMat inv(nrows, nrows);
    fmpz_t D, den;
    fmpz_mat_t matZZ, invZZ;
    fmpz_init(D);
    fmpz_init(den);
    fmpz_mat_init(matZZ, nrows, nrows);
    fmpz_mat_init(invZZ, nrows, nrows);

    fmpq_mat_get_fmpz_mat_matwise(matZZ, D, A.value());
    int result = fmpz_mat_inv(invZZ, den, matZZ); // sets invZZ, den
    if (result != 0) 
      {
        fmpq_mat_set_fmpz_mat_div_fmpz(inv.value(), invZZ, den);
        fmpq_mat_scalar_mul_fmpz(inv.value(), inv.value(), D);
        inv.toDMat(result_inv);
      }
    fmpz_clear(D);
    fmpz_clear(den);
    fmpz_mat_clear(matZZ);
    fmpz_mat_clear(invZZ);
    return (result != 0);
  }

  size_t kernel(Mat& result_nullspace) 
  {
    FlintQQMat A(mInputMatrix);
    // printf("in dmat-lu-qq kernel\n");
    fmpz_mat_t matZZ;
    fmpz_mat_t kerZZ;
    fmpz_mat_init(matZZ, A.numRows(), A.numColumns());
    fmpz_mat_init(kerZZ, A.numColumns(), A.numColumns());

    fmpq_mat_get_fmpz_mat_rowwise(matZZ, NULL, A.value());
    long nullity = fmpz_mat_nullspace(kerZZ, matZZ);
    FlintQQMat kerQQ(A.numColumns(), nullity);
    for (long r=0; r<A.numColumns(); r++)
      for (long c=0; c<nullity; c++)
        {
          // There is no function which sets an fmpq from an fmpz!
          kerQQ.set_from_fmpz(r,c, fmpz_mat_entry(kerZZ, r, c));
        }
    //fmpq_mat_set_fmpz_mat(kerQQ.value(), kerZZ);
    kerQQ.toDMat(result_nullspace);
    fmpz_mat_clear(matZZ);
    fmpz_mat_clear(kerZZ);
    return nullity;
  }

private:
  const Mat& mInputMatrix;
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
