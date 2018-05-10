#define mpfr foo
#include "mpreal.h"
#include <Eigen/MPRealSupport>
#undef mpfr

#include "eigen.hpp"
#include <Eigen/SVD>
#include "mpfr.h"

using Real = foo::mpreal;
using Complex = std::complex<Real>;
using MatrixXmp = Eigen::Matrix<Real,Eigen::Dynamic,Eigen::Dynamic>;
using MatrixXmpCC = Eigen::Matrix<Complex,Eigen::Dynamic,Eigen::Dynamic>;

void test(const MatrixXmp& m)
{
  foo::mpreal::set_default_prec(256);

  //  MatrixXmp m = MatrixXmp::Random(30,50);
  std::cout << "Here is the matrix m:" << std::endl << m << std::endl;

  Eigen::JacobiSVD<MatrixXmp> svd(m, Eigen::ComputeThinU | Eigen::ComputeThinV);
  std::cout << "Its singular values are:" << std::endl << svd.singularValues() << std::endl;
  std::cout << "Its left singular vectors are the columns of the thin U matrix:" << std::endl << svd.matrixU() << std::endl;
  std::cout << "Its right singular vectors are the columns of the thin V matrix:" << std::endl << svd.matrixV() << std::endl;
}

namespace EigenM2 {

void fill_from_MatrixXmp(const MatrixXmp& orig, LMatrixRRR& result)
{
  int numrows = orig.rows();
  int numcols = orig.cols();
  result.resize(numrows, numcols);
  for (int r=0; r<numrows; r++)
    for (int c=0; c<numcols; c++)
      result.ring().set(result.entry(r,c), * orig(r,c).mpfr_srcptr());
}
  
void fill_from_MatrixXmp(const MatrixXmpCC& orig, LMatrixCCC& result)
{
  int numrows = orig.rows();
  int numcols = orig.cols();
  result.resize(numrows, numcols);
  for (int r=0; r<numrows; r++)
    for (int c=0; c<numcols; c++)
      result.ring().set_from_complex_mpfr(result.entry(r,c),
                                          orig(r,c).real().mpfr_srcptr(),
                                          orig(r,c).imag().mpfr_srcptr());
}

bool SVD(const LMatrixRRR *A,
         LMatrixRRR *Sigma,
         LMatrixRRR *U,
         LMatrixRRR *VT
         )
{
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());
  std::cout << "Eigen::SVD (real)" << std::endl;

  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  //int min = (rows <= cols) ? rows : cols;

  // Create the correct matrices: A, Sigma, U, VT perhaps.
  // call eigen
  // Transform matrices back.
  MatrixXmp AXmp(A->numRows(), A->numColumns());

  //  fill_to_MatrixXmp(A, AXmp);

  for (int r=0; r<A->numRows(); r++)
    for (int c=0; c<A->numColumns(); c++)
      AXmp(r,c) = Real(& A->entry(r,c), true);

  Eigen::JacobiSVD<MatrixXmp> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  //  U->resize(rows, rows);
  //  VT->resize(cols, cols);
  //  Sigma->resize(min, 1);
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  Real::set_default_prec(old_prec);
  return true;
}

bool SVD(const LMatrixCCC *A,
         LMatrixRRR *Sigma,
         LMatrixCCC *U,
         LMatrixCCC *VT
         )
{
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  int rows = static_cast<int>(A->numRows());
  int cols = static_cast<int>(A->numColumns());
  //int min = (rows <= cols) ? rows : cols;

  // Create the correct matrices: A, Sigma, U, VT perhaps.
  // call eigen
  // Transform matrices back.
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());

  //  fill_to_MatrixXmp(A, AXmp);

  for (int r=0; r<A->numRows(); r++)
    for (int c=0; c<A->numColumns(); c++)
      AXmp(r,c) = Complex(Real(& A->entry(r,c).re, true),Real(& A->entry(r,c).im, true));

  Eigen::JacobiSVD<MatrixXmpCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  //  U->resize(rows, rows);
  //  VT->resize(cols, cols);
  //  Sigma->resize(min, 1);
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  Real::set_default_prec(old_prec);
  return true;
}
} // end of namespace Eigen 

/*
kk = RR_100
M = random(kk^3, kk^3)
SVD M
M53 = sub(M, RR_53)
SVD M53

kk = CC_100
M = random(kk^3, kk^3)
SVD M
M53 = sub(M, CC_53)
SVD M53

 */

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
*/
