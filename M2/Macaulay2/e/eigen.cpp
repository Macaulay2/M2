#include <cstdlib>

#include <M2/math-include.h>

// the reason for the following is that mpreal.h defines a namespace mpfr,
// which conflicts with mpfr defined by us
#define mpfr eigen_mpfr
#include "mpreal.h"
#include <unsupported/Eigen/MPRealSupport>
#undef mpfr

#include <Eigen/SVD>
#include <Eigen/Eigenvalues>
#include "eigen.hpp"

using Real = eigen_mpfr::mpreal;
using Complex = std::complex<Real>;
using MatrixXmp = Eigen::Matrix<Real,Eigen::Dynamic,Eigen::Dynamic>;
using MatrixXmpCC = Eigen::Matrix<Complex,Eigen::Dynamic,Eigen::Dynamic>;


namespace EigenM2 {

void fill_to_MatrixXmp(const LMatrixRRR& orig, MatrixXmp& result)
{
  for (int r=0; r<orig.numRows(); r++)
    for (int c=0; c<orig.numColumns(); c++)
      result(r,c) = Real(& orig.entry(r,c), false); // false = make a copy
}

void fill_to_MatrixXmp(const LMatrixCCC& orig,  MatrixXmpCC& result)
{
  for (int r=0; r<orig.numRows(); r++)
    for (int c=0; c<orig.numColumns(); c++)
      result(r,c) = Complex(Real(& orig.entry(r,c).re, false),Real(& orig.entry(r,c).im, false)); // false = make a copy
}

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

  // Create the correct matrices: A, Sigma, U, VT perhaps.
  // call eigen
  // Transform matrices back.

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::JacobiSVD<MatrixXmp> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);

  auto& eigenU = svd.matrixU();
  auto& eigenVT = svd.matrixV().adjoint();
  auto& eigenSigma = svd.singularValues();
  
  fill_from_MatrixXmp(eigenU, *U);
  fill_from_MatrixXmp(eigenVT, *VT);
  fill_from_MatrixXmp(eigenSigma, *Sigma);

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

  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::JacobiSVD<MatrixXmpCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  Real::set_default_prec(old_prec);
  return true;
}

bool SVD_divide_conquer(const LMatrixRRR *A,
  LMatrixRRR *Sigma,
  LMatrixRRR *U,
  LMatrixRRR *VT
)
{
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::BDCSVD<MatrixXmp> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  Real::set_default_prec(old_prec);
  return true;
}

bool SVD_divide_conquer(const LMatrixCCC *A,
  LMatrixRRR *Sigma,
  LMatrixCCC *U,
  LMatrixCCC *VT
)
{
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::BDCSVD<MatrixXmpCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues(const LMatrixRRR *A, LMatrixCCC *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::EigenSolver<MatrixXmp> es(AXmp,false/*no eigenvectors*/);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues(const LMatrixCCC *A, LMatrixCCC *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::ComplexEigenSolver<MatrixXmpCC> ces(AXmp, false/*no eigenvectors*/);
  fill_from_MatrixXmp(ces.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues_hermitian(const LMatrixRRR *A, LMatrixRRR *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmp> es(AXmp, false);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmp> es(AXmp, false);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors(const LMatrixRRR *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::EigenSolver<MatrixXmp> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors(const LMatrixCCC *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::ComplexEigenSolver<MatrixXmpCC> ces(AXmp);
  fill_from_MatrixXmp(ces.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(ces.eigenvectors(), *eigenvecs);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors_hermitian(const LMatrixRRR *A, LMatrixRRR *eigenvals, LMatrixRRR *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmp> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals, LMatrixCCC *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmp> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  Real::set_default_prec(old_prec);
  return true;
}

bool least_squares(const LMatrixRRR *A,
  const LMatrixRRR *B,
  LMatrixRRR *X
)
{
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);
  MatrixXmp BXmp(B->numRows(), B->numColumns());
  fill_to_MatrixXmp(*B, BXmp);

  Eigen::BDCSVD<MatrixXmp> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.solve(BXmp), *X);

  Real::set_default_prec(old_prec);
  return true;
}

bool least_squares(const LMatrixCCC *A,
  const LMatrixCCC *B,
  LMatrixCCC *X
)
{
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);
  MatrixXmpCC BXmp(B->numRows(), B->numColumns());
  fill_to_MatrixXmp(*B, BXmp);

  Eigen::BDCSVD<MatrixXmpCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.solve(BXmp), *X);

  Real::set_default_prec(old_prec);
  return true;
}

} // end of namespace EigenM2 

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e eigen.o "
// indent-tabs-mode: nil
// End:
*/
