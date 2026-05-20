/**
 * @file dmat-lu-zzp-flint.hpp
 * @brief `DMatLinAlg<M2::ARingZZpFlint>` --- dense Z/p linear algebra routed through FLINT `nmod_mat_*`.
 *
 * Specialises `DMatLinAlg` for the FLINT-backed Z/p aring,
 * with each public method forwarding to the matching
 * `nmod_mat_*` routine on the underlying `nmod_mat_t`:
 * `rank()` to `nmod_mat_rank`, `determinant` to `nmod_mat_det`,
 * `inverse` to `nmod_mat_inv`, `kernel` to `nmod_mat_nullspace`,
 * and `solveInvertible(B, X)` to `nmod_mat_solve`. `matrixPLU`
 * copies the input matrix, runs `nmod_mat_lu` on the copy, and
 * then calls `LUUtil<RingType>::setUpperLower` to split the
 * packed result into separate L and U. The general-shape
 * `solve(B, X)` concatenates `[A | B]` via
 * `concatenateMatrices<Mat>`, runs `nmod_mat_rref`, reads the
 * column-rank profile via `LUUtil::computePivotColumns`, and
 * either rejects the system as inconsistent or copies the
 * solution out of the trailing columns of `AB`.
 *
 * The header is `#include`d from `dmat-lu.hpp`;
 * `M2/gc-include.h` precedes the FLINT include so
 * `flint_malloc` routes through bdwgc, and the diagnostic
 * pragmas silence FLINT-internal conversion warnings. The
 * FFLAS-FFPACK alternative is `dmat-lu-zzp-ffpack.hpp`.
 *
 * @see dmat-lu.hpp
 * @see dmat-lu-zzp-ffpack.hpp
 * @see aring-zzp-flint.hpp
 */

// The following needs to be included before any flint files are included.
#include <M2/gc-include.h>

#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wconversion"
#include <flint/nmod_mat.h>  // for nmod_mat_lu, nmod_mat_rank, nmod_mat_det
#pragma GCC diagnostic pop

//////////////////////
// ZZpFlint //////////
//////////////////////

/**
 * @brief Specialisation of `DMatLinAlg` for `ARingZZpFlint` dense matrices,
 * routing rank / determinant / kernel / solve / inverse calls to
 * FLINT's `nmod_mat_*` routines.
 *
 * @details `DMat<ARingZZpFlint>` already wraps an `nmod_mat_t`, so the
 * linear-algebra layer just hands the underlying FLINT matrix to
 * the right `nmod_mat_*` entry point and converts the result
 * back. Gives word-prime `Z/p` matrices the same FLINT speed-up
 * the FFPACK specialisation provides for primes that fit in a
 * FFPACK word.
 */
template <>
class DMatLinAlg<M2::ARingZZpFlint>
{
 public:
  typedef M2::ARingZZpFlint RingType;
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;

 public:
  DMatLinAlg(const Mat& A) : mMatrix(A) {}
  size_t rank() { return nmod_mat_rank(mMatrix.nmod_mat()); }
  void determinant(ElementType& result_det)
  {
    result_det = nmod_mat_det(mMatrix.nmod_mat());
  }

  void columnRankProfile(std::vector<size_t>& profile)
  {
    Mat LU(mMatrix);  // copy
    mp_limb_signed_t* perm = newarray_atomic(mp_limb_signed_t, LU.numRows());
    nmod_mat_lu(perm, LU.nmod_mat(), false);
    freemem(perm);
    LUUtil<RingType>::computePivotColumns(LU, profile);
  }

  void matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
  {
    //    std::cout << "calling matrixPLU zzp-flint\n";
    Mat LU(mMatrix);  // copy
    mp_limb_signed_t* perm = newarray_atomic(mp_limb_signed_t, LU.numRows());
    nmod_mat_lu(perm, LU.nmod_mat(), false);
    P.resize(0);
    for (long i = 0; i < LU.numRows(); i++) P.push_back(perm[i]);
    freemem(perm);
    LUUtil<RingType>::setUpperLower(LU, L, U);
  }

  bool solveInvertible(const Mat& B, Mat& X)
  {
    // printf("called mat-linalg zzp flint solveInvertible\n");
    assert(mMatrix.numRows() == mMatrix.numColumns());
    assert(mMatrix.numRows() == B.numRows());
    X.resize(mMatrix.numColumns(), B.numColumns());
    int isinvertible =
        nmod_mat_solve(X.nmod_mat(), mMatrix.nmod_mat(), B.nmod_mat());
    return (isinvertible != 0);
  }

  bool solve(const Mat& B, Mat& X)
  {
    // printf("in dmat lu zzp flint solve\n");
    long nrows = mMatrix.numRows();
    long ncols = mMatrix.numColumns();
    std::vector<size_t> profile;
    Mat AB(mMatrix.ring(), nrows, ncols + B.numColumns());
    concatenateMatrices<Mat>(mMatrix, B, AB);
    nmod_mat_rref(AB.nmod_mat());
    LUUtil<RingType>::computePivotColumns(AB, profile);
    if (profile.size() >= 1 and profile[profile.size() - 1] >= ncols)
      return false;  // system is inconsistent
    // At this point, we know the solutions.  Should we go through FlintQQMat,
    // or go directly to Mat?
    X.resize(ncols, B.numColumns());
    for (long c = 0; c < B.numColumns(); c++)
      {
        // Fill in this column
        for (long r = 0; r < profile.size(); r++)
          {
            mMatrix.ring().set(X.entry(profile[r], c), AB.entry(r, ncols + c));
          }
      }
    return true;
  }

  bool inverse(Mat& result_inv)
  {
    assert(mMatrix.numRows() == mMatrix.numColumns());
    Mat& A = const_cast<Mat&>(mMatrix);
    result_inv.resize(mMatrix.numRows(), mMatrix.numColumns());
    return nmod_mat_inv(result_inv.nmod_mat(), A.nmod_mat());
  }

  size_t kernel(Mat& result_nullspace)
  {
    long rank = nmod_mat_rank(mMatrix.nmod_mat());
    result_nullspace.resize(
        mMatrix.numColumns(),
        mMatrix.numColumns() - rank);  // the largest the answer could be
    long nullity =
        nmod_mat_nullspace(result_nullspace.nmod_mat(), mMatrix.nmod_mat());
    assert(rank == mMatrix.numColumns() - nullity);
    return nullity;
  }

 private:
  const Mat& mMatrix;  // reference to the original matrix
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
