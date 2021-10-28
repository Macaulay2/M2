// This file should only be included once, by what?

//////////////////////
// ZZpFlint //////////
//////////////////////

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
