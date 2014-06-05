// This file should only be included once, by what?

  //////////////////////
  // ZZpFlint //////////
  //////////////////////

template<>
class DMatLinAlg<M2::ARingZZpFlint>
{
public:
  typedef M2::ARingZZpFlint RingType;
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;

public:
  DMatLinAlg(const Mat& A) : mMatrix(A) {}

  size_t rank() { return nmod_mat_rank(mMatrix.nmod_mat()); }

  void determinant(ElementType& result_det) { result_det = nmod_mat_det(mMatrix.nmod_mat()); }

  void columnRankProfile(std::vector<size_t>& profile) 
  { 
    Mat LU(mMatrix); // copy
    long* perm = newarray_atomic(long, LU.numRows());
    nmod_mat_lu(perm, LU.nmod_mat(), false);
    deletearray(perm);
    LUUtil<RingType>::computePivotColumns(LU, profile);
  }

  void matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
  {
    Mat LU(mMatrix); // copy
    long* perm = newarray_atomic(long, LU.numRows());
    nmod_mat_lu(perm, LU.nmod_mat(), false);
    P.resize(0);
    for (long i=0; i<LU.numRows(); i++)
      P.push_back(perm[i]);
    deletearray(perm);
    LUUtil<RingType>::setUpperLower(LU, L, U);
  }

  bool solveInvertible(const Mat& B, Mat& X) 
  { 
    printf("called mat-linalg zzp flint solveInvertible\n");
    M2_ASSERT(A.numRows() == A.numColumns());
    M2_ASSERT(A.numRows() == B.numRows());
    X.resize(mMatrix.numColumns(), B.numColumns());
    int isinvertible = nmod_mat_solve(X.nmod_mat(), mMatrix.nmod_mat(), B.nmod_mat());
    return (isinvertible != 0);
  }

  bool solve(const Mat& B, Mat& X) 
  { 
    Mat& A1 = const_cast<Mat&>(mMatrix); // needed because nmod_mat_solve doesn't declare params const
    Mat& B1 = const_cast<Mat&>(B);
    X.resize(mMatrix.numColumns(), B.numColumns());
    return nmod_mat_solve(X.nmod_mat(), A1.nmod_mat(), B1.nmod_mat());
  }

  bool inverse(Mat& result_inv) { 
    Mat& A = const_cast<Mat&>(mMatrix);
    return nmod_mat_inv(result_inv.nmod_mat(), A.nmod_mat());
  }

  size_t kernel(Mat& result_nullspace) {
    long rank = nmod_mat_rank(mMatrix.nmod_mat());
    result_nullspace.resize(mMatrix.numColumns(), mMatrix.numColumns() - rank); // the largest the answer could be
    long nullity = nmod_mat_nullspace(result_nullspace.nmod_mat(), mMatrix.nmod_mat());
    M2_ASSERT(rank == mMatrix.numColumns() - nullity);
    return nullity;
  }

private:
  const Mat& mMatrix; // reference to the original matrix
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
