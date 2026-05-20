/**
 * @file dmat-lu-zzp-ffpack.hpp
 * @brief `DMatLinAlg<ARingZZpFFPACK>` and the `ffpackInterface` namespace --- FFLAS-FFPACK-backed Z/p linear algebra.
 *
 * Declares the `ffpackInterface` namespace whose free functions
 * (`rank`, `determinant`, two `rankProfile` overloads,
 * `solveLinear`, `inverse`, `nullSpace`) operate on
 * `DMatZZpFFPACK` --- the declarations are here so callers
 * include only this header, while the bodies live in
 * `dmat.cpp` to keep the FFLAS-FFPACK include surface
 * contained. Each entry point delegates to FFLAS-FFPACK, which
 * represents Z/p entries as `double`s and reinterprets the
 * matrix so multiplications dispatch into BLAS; the result is
 * reduced modulo `p` afterwards. For primes whose intermediate
 * products fit in the 53-bit mantissa (roughly `p <= 2^25` at
 * typical matrix sizes), this is by far the fastest dense Z/p
 * path in the engine.
 *
 * The same file defines the `DMatLinAlg<M2::ARingZZpFFPACK>`
 * specialisation: `rank` / `determinant` / `columnRankProfile`
 * / `solve` / `solveInvertible` / `inverse` / `kernel` forward
 * directly to `ffpackInterface`, while `matrixPLU` falls back
 * to a generic `DMatLUinPlace<RingType>` + `LUUtil::setUpperLower`
 * to split L and U (FFPACK does not return an "honest" LU).
 *
 * @see dmat-lu.hpp
 * @see dmat-lu-zzp-flint.hpp
 * @see aring-zzp-ffpack.hpp
 */

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

/**
 * @brief Specialisation of `DMatLinAlg` for `ARingZZpFFPACK` dense matrices,
 * delegating rank / determinant / kernel / solve / inverse to the
 * FFPACK library's tuned `Z/p` routines.
 *
 * @details FFPACK gives near-BLAS speed for dense linear algebra over `Z/p`
 * by laying entries out as native floats. The specialisation
 * forwards each query through the `ffpackInterface` shims
 * declared just above and returns the result as a fresh
 * `DMat<ARingZZpFFPACK>`. The preferred backend for primes that
 * fit comfortably in a single-precision float.
 */
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
    //    std::cout << "calling matrixPLU ffpack\n";
    
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
