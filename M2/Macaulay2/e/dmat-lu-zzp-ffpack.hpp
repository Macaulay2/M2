//////////////////////
// ZZpFFPACK /////////
//////////////////////
namespace ffpackInterface {
// Functions for DMatZZpFFPACK, in dmat.cpp
size_t rank(const DMatZZpFFPACK& A);

void determinant(const DMatZZpFFPACK& A, ZZpFFPACK::ElementType& result_det);

M2_arrayintOrNull rankProfile(const DMatZZpFFPACK& A, bool row_profile);

void rankProfile(const DMatZZpFFPACK& A,
                 bool row_profile,
                 std::vector<size_t>& result_profile);

bool solveLinear(const DMatZZpFFPACK& A,
                 const DMatZZpFFPACK& B,
                 DMatZZpFFPACK& X);

bool inverse(const DMatZZpFFPACK& A, DMatZZpFFPACK& result_inv);

size_t nullSpace(const DMatZZpFFPACK& A, DMatZZpFFPACK& result_nullspace);

};  // namespace ffpackInterface

template <>
class DMatLinAlg<M2::ARingZZpFFPACK>
{
 public:
  typedef M2::ARingZZpFFPACK RingType;
  typedef typename RingType::ElementType ElementType;
  typedef DMat<RingType> Mat;

 public:
  DMatLinAlg(const Mat& A) : mLU(A) {}
  size_t rank() { return ffpackInterface::rank(mLU); }
  void determinant(ElementType& result)
  {
    ffpackInterface::determinant(mLU, result);
  }

  void columnRankProfile(std::vector<size_t>& profile)
  {
    ffpackInterface::rankProfile(mLU, false, profile);
  }

  void matrixPLU(std::vector<size_t>& P, Mat& L, Mat& U)
  {
    DMatLUinPlace<RingType> C(mLU);

    const Mat& LU = C.LUinPlace();

    LUUtil<RingType>::setUpperLower(LU, L, U);
    P = C.permutation();
  }

  bool solve(const Mat& B, Mat& X)
  {
    return ffpackInterface::solveLinear(mLU, B, X);
  }

  bool solveInvertible(const Mat& B, Mat& X)
  {
    return ffpackInterface::solveLinear(mLU, B, X);
  }

  bool inverse(Mat& X) { return ffpackInterface::inverse(mLU, X); }
  size_t kernel(Mat& X) { return ffpackInterface::nullSpace(mLU, X); }
 private:
  const Mat& mLU;  // reference to the original matrix
};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
