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
  DMatLinAlg(const Mat& A) : mMatrix(A) {}

  size_t rank() 
  { 
    fmpz_mat_t m1;
    fmpz_mat_init(m1, mMatrix.numRows(), mMatrix.numColumns());
    fmpq_mat_get_fmpz_mat_rowwise(m1, NULL, mMatrix.value());
    size_t rk = fmpz_mat_rank(m1);
    fmpz_mat_clear(m1);
    return rk;
  }

  void determinant(ElementType& result_det) 
  { 
    fmpq_t det;
    fmpq_init(det);
    fmpq_mat_det(det, mMatrix.value());
    fmpq_get_mpq(&result_det, det);
    fmpq_clear(det);
  }

  void columnRankProfile(std::vector<size_t>& profile) 
  { 
#if 0
    // TODO
    Mat LU(mMatrix); // copy
    long* perm = newarray_atomic(long, LU.numRows());
    fmpq_mat_lu(perm, LU.fmpq_mat(), false);
    deletearray(perm);
    LUUtil<RingType>::computePivotColumns(LU, profile);
#endif
  }

  void matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
  {
#if 0
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

  bool inverse(Mat& result_inv) 
  { 
    // This seems like alot of futzing around to get the inverse of a matrix over QQ...
    printf("in dmat-lu-qq inverse\n");
    long nrows = mMatrix.numRows();
    FlintQQMat inv(nrows, nrows);
    fmpz_t D, den;
    fmpz_mat_t matZZ, invZZ;
    fmpz_init(D);
    fmpz_init(den);
    fmpz_mat_init(matZZ, nrows, nrows);
    fmpz_mat_init(invZZ, nrows, nrows);

    fmpq_mat_get_fmpz_mat_matwise(matZZ, D, mMatrix.value());
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
    printf("in dmat-lu-qq kernel\n");
    fmpz_mat_t matZZ;
    fmpz_mat_t kerZZ;
    fmpz_mat_init(matZZ, mMatrix.numRows(), mMatrix.numColumns());
    fmpz_mat_init(kerZZ, mMatrix.numColumns(), mMatrix.numColumns());

    fmpq_mat_get_fmpz_mat_rowwise(matZZ, NULL, mMatrix.value());
    printf("made it past rowwise\n");
    long nullity = fmpz_mat_nullspace(kerZZ, matZZ);
    FlintQQMat kerQQ(mMatrix.numColumns(), nullity);
    printf("made it past nullspace\n");
    for (long r=0; r<mMatrix.numColumns(); r++)
      for (long c=0; c<nullity; c++)
        {
          // There is no function which sets an fmpq from an fmpz!
          kerQQ.set_from_fmpz(r,c, fmpz_mat_entry(kerZZ, r, c));
        }
    printf("made it past copy\n");
    fmpq_mat_set_fmpz_mat(kerQQ.value(), kerZZ);
    kerQQ.toDMat(result_nullspace);
    printf("made it past toDMat\n");
    fmpz_mat_clear(matZZ);
    fmpz_mat_clear(kerZZ);
    return nullity;
  }

private:
  FlintQQMat mMatrix;
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
