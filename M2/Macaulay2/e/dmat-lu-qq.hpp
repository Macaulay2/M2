// This file should only be included once, by what?

//////////////////////
// ZZpFlint //////////
//////////////////////

#include "dmat-qq-interface-flint.hpp"

template <>
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

  static void findColumnRankProfileFromLU(const fmpq_mat_t B,
                                          std::vector<size_t>& profile)
  {
    profile.clear();
    long nrows = fmpq_mat_nrows(B);
    long ncols = fmpq_mat_ncols(B);
    long thiscol = 0;
    long thisrow = 0;
    while (thisrow < nrows and thiscol < ncols)
      {
        if (not fmpq_is_zero(fmpq_mat_entry(B, thisrow, thiscol)))
          {
            profile.push_back(thiscol);
            thisrow++;
          }
        thiscol++;
      }
  }
  void columnRankProfile(std::vector<size_t>& profile)
  {
    FlintQQMat A(mInputMatrix);
    FlintQQMat B(A.numRows(), A.numColumns());
    fmpq_mat_rref(B.value(), A.value());

    findColumnRankProfileFromLU(B.value(), profile);
  }

  void matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
  {
    printf("dmat lu qq PLU\n");
    FlintQQMat A(mInputMatrix);
    FlintZZMat LU(A.numRows(), A.numColumns());
    fmpz_t den, den2;
    fmpz_init(den);
    fmpz_init(den2);
    mp_limb_signed_t* perm = newarray_atomic(mp_limb_signed_t, LU.numRows());
    for (long i = 0; i < LU.numRows(); i++) perm[i] = i;
    fmpq_mat_get_fmpz_mat_matwise(LU.value(), den, A.value());
    fmpz_mat_fflu(LU.value(),
                  den2,
                  perm,
                  LU.value(),
                  0);  // the 0 means do not abort if rank is not maximal
    P.resize(0);
    for (long i = 0; i < LU.numRows(); i++) P.push_back(perm[i]);
    deletearray(perm);

    fmpz_mul(den, den, den2);  // total denominator
    // Now we need to set L and U.
    // Sigh, this code is taken and hacked from LUUtil<RingType>::setUpperLower.
    size_t min = std::min(LU.numRows(), LU.numColumns());
    L.resize(LU.numRows(), min);
    U.resize(min, LU.numColumns());

    // At this point, lower and upper should be zero matrices.
    assert(MatrixOps::isZero(L));
    assert(MatrixOps::isZero(U));

    fmpq_t b;
    fmpq_init(b);
    for (size_t c = 0; c < LU.numColumns(); c++)
      {
        if (c < min) L.ring().set_from_long(L.entry(c, c), 1);
        for (size_t r = 0; r < LU.numRows(); r++)
          {
            if (r <= c)
              {
                mpq_t a;
                // Set the value in U by dividing the fmpz by the denom
                // Note: the elements in LU have type fmpz_t
                // the elements in L or U: have type mpq_t

                fmpq_set_fmpz_frac(b, fmpz_mat_entry(LU.value(), r, c), den);
                flint_mpq_init_set_readonly(a, b);
                assert(U.ring().set_from_mpq(U.entry(r, c), a));
                U.ring().set_from_mpq(U.entry(r, c), a);  // ignore the result
                                                          // boolean: this
                                                          // operation should
                                                          // not fail
                flint_mpq_clear_readonly(a);
              }
            else if (c < L.numRows())
              {
                mpz_t a;
                flint_mpz_init_set_readonly(a,
                                            fmpz_mat_entry(LU.value(), r, c));
                L.ring().set_from_mpz(L.entry(r, c), a);
                flint_mpz_clear_readonly(a);
              }
          }
      }

    fmpq_clear(b);
    fmpz_clear(den);
    fmpz_clear(den2);
  }

  bool solve(const Mat& B1, Mat& X1)
  {
    // This is the version where A = this isn't nec square or full rank...
    // Plan for now (we will see how efficient this version is): put A|B into a
    // single matrix.
    // rref
    // get column rank profile
    // from that, either return false (inconsistent), or copy the correct values
    // into X

    // Too bad flint doesn't do this automatically...
    // 1. copy A, B to flint objects
    // 2. concatenate them
    // 3. rref
    // 4. pivot columns
    // 5. if pivot column is in B columns, return false.
    // 6. otherwise:
    //   resize X
    //   populate X with the solution (only non-zero entries are in pivot
    //   columns
    // 6. copy
    // TODO

    long nrows = mInputMatrix.numRows();
    long ncols = mInputMatrix.numColumns();
    std::vector<size_t> profile;
    Mat AB1(mInputMatrix.ring(), nrows, ncols + B1.numColumns());
    concatenateMatrices<Mat>(mInputMatrix, B1, AB1);
    FlintQQMat AB(AB1);
    fmpq_mat_rref(AB.value(), AB.value());
    findColumnRankProfileFromLU(AB.value(), profile);
    if (profile[profile.size() - 1] >= ncols)
      return false;  // system is inconsistent
    // At this point, we know the solutions.  Should we go through FlintQQMat,
    // or go directly to Mat?
    FlintQQMat X(ncols, B1.numColumns());
    for (long c = 0; c < B1.numColumns(); c++)
      {
        // Fill in this column
        for (long r = 0; r < profile.size(); r++)
          {
            fmpq_set(fmpq_mat_entry(X.value(), profile[r], c),
                     fmpq_mat_entry(AB.value(), r, ncols + c));
          }
      }
    X.toDMat(X1);
    return true;
  }

  bool solveInvertible(const Mat& B1, Mat& X1)
  {
    FlintQQMat A(mInputMatrix);
    FlintQQMat B(B1);
    FlintQQMat X(mInputMatrix.numColumns(), B1.numColumns());
    //    int isfullrank = fmpq_mat_solve_fraction_free(X.value(), A.value(),
    //    B.value());
    int isfullrank = fmpq_mat_solve_dixon(X.value(), A.value(), B.value());
    if (isfullrank == 0) return false;
    X.toDMat(X1);
    return true;
  }

  bool inverse(Mat& result_inv)
  {
    FlintQQMat A(mInputMatrix);
    // This seems like a lot of fuzzing around to get the inverse of a matrix
    // over QQ...
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
    int result = fmpz_mat_inv(invZZ, den, matZZ);  // sets invZZ, den
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
    for (long r = 0; r < A.numColumns(); r++)
      for (long c = 0; c < nullity; c++)
        {
          // There is no function which sets an fmpq from an fmpz!
          kerQQ.set_from_fmpz(r, c, fmpz_mat_entry(kerZZ, r, c));
        }
    // fmpq_mat_set_fmpz_mat(kerQQ.value(), kerZZ);
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
