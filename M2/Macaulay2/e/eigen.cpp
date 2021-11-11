//#define NO_LAPACK
/*
Uncommenting the above has two consequences:
  (1) This effectively eliminates the use of LAPACK (eigen is used instead for machine precision).
  (2) This slows down the compilation (at the moment) due to heavy templating in eigen. 
*/

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
using MatrixXmpRRR = Eigen::Matrix<Real,Eigen::Dynamic,Eigen::Dynamic>;
using MatrixXmpCCC = Eigen::Matrix<Complex,Eigen::Dynamic,Eigen::Dynamic>;
#ifdef NO_LAPACK
using MatrixXmpRR = Eigen::Matrix<double,Eigen::Dynamic,Eigen::Dynamic>;
using MatrixXmpCC = Eigen::Matrix<std::complex<double>,Eigen::Dynamic,Eigen::Dynamic>;
#endif

namespace EigenM2 {

#ifdef NO_LAPACK  
// RR/CC

// Need to rewrite matrix conversion functions

void fill_to_MatrixXmp(const LMatrixRR& orig, MatrixXmpRR& result)
{
  for (int r=0; r<orig.numRows(); r++)
    for (int c=0; c<orig.numColumns(); c++)
      result(r,c) = orig.entry(r,c);
}

void fill_to_MatrixXmp(const LMatrixCC& orig,  MatrixXmpCC& result)
{
  for (int r=0; r<orig.numRows(); r++)
    for (int c=0; c<orig.numColumns(); c++)
      //~ result(r,c) = Complex(Real(& orig.entry(r,c).re, false),Real(& orig.entry(r,c).im, false));
      result(r,c) = std::complex<double>(orig.entry(r,c).re, orig.entry(r,c).im);
}

void fill_from_MatrixXmp(const MatrixXmpRR& orig, LMatrixRR& result)
{
  int numrows = orig.rows();
  int numcols = orig.cols();
  result.resize(numrows, numcols);
  for (int r=0; r<numrows; r++)
    for (int c=0; c<numcols; c++)
      result.ring().set(result.entry(r,c), orig(r,c));
}

void fill_from_MatrixXmp(const MatrixXmpCC& orig, LMatrixCC& result)
{
  int numrows = orig.rows();
  int numcols = orig.cols();
  result.resize(numrows, numcols);
  for (int r=0; r<numrows; r++)
    for (int c=0; c<numcols; c++)
      result.ring().set_from_doubles(result.entry(r,c),
        orig(r,c).real(),
        orig(r,c).imag());
}

bool SVD(const LMatrixRR *A,
  LMatrixRR *Sigma,
  LMatrixRR *U,
  LMatrixRR *VT
)
{
  // Create the correct matrices: A, Sigma, U, VT perhaps.
  // call eigen
  // Transform matrices back.

  MatrixXmpRR AXmp(A->numRows(), A->numColumns());
  
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::JacobiSVD<MatrixXmpRR> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);

  auto& eigenU = svd.matrixU();
  auto& eigenVT = svd.matrixV().adjoint();
  auto& eigenSigma = svd.singularValues();
  
  fill_from_MatrixXmp(eigenU, *U);
  fill_from_MatrixXmp(eigenVT, *VT);
  fill_from_MatrixXmp(eigenSigma, *Sigma);

  return true;
}

bool SVD(const LMatrixCC *A,
  LMatrixRR *Sigma,
  LMatrixCC *U,
  LMatrixCC *VT
)
{
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::JacobiSVD<MatrixXmpCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  return true;
}

bool SVD_divide_conquer(const LMatrixRR *A,
  LMatrixRR *Sigma,
  LMatrixRR *U,
  LMatrixRR *VT
)
{
  MatrixXmpRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::BDCSVD<MatrixXmpRR> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  return true;
}

bool SVD_divide_conquer(const LMatrixCC *A,
  LMatrixRR *Sigma,
  LMatrixCC *U,
  LMatrixCC *VT
)
{
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::BDCSVD<MatrixXmpCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  return true;
}

bool eigenvalues(const LMatrixRR *A, LMatrixCC *eigenvals) {
  MatrixXmpRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::EigenSolver<MatrixXmpRR> es(AXmp,false/*no eigenvectors*/);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  return true;
}

bool eigenvalues(const LMatrixCC *A, LMatrixCC *eigenvals) {
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::ComplexEigenSolver<MatrixXmpCC> ces(AXmp, false);
  fill_from_MatrixXmp(ces.eigenvalues(), *eigenvals);

  return true;
}

bool eigenvalues_hermitian(const LMatrixRR *A, LMatrixRR *eigenvals) {
  MatrixXmpRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpRR> es(AXmp, false);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  return true;
}

bool eigenvalues_hermitian(const LMatrixCC *A, LMatrixRR *eigenvals) {
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpCC> es(AXmp, false);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  return true;
}

bool eigenvectors(const LMatrixRR *A, LMatrixCC *eigenvals, LMatrixCC *eigenvecs) {
  MatrixXmpRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::EigenSolver<MatrixXmpRR> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  return true;
}

bool eigenvectors(const LMatrixCC *A, LMatrixCC *eigenvals, LMatrixCC *eigenvecs) {
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::ComplexEigenSolver<MatrixXmpCC> ces(AXmp);
  fill_from_MatrixXmp(ces.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(ces.eigenvectors(), *eigenvecs);

  return true;
}

bool eigenvectors_hermitian(const LMatrixRR *A, LMatrixRR *eigenvals, LMatrixRR *eigenvecs) {
  MatrixXmpRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpRR> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  return true;
}

bool eigenvectors_hermitian(const LMatrixCC *A, LMatrixRR *eigenvals, LMatrixCC *eigenvecs) {
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpCC> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  return true;
}

bool least_squares(const LMatrixRR *A,
  const LMatrixRR *B,
  LMatrixRR *X
)
{
  MatrixXmpRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);
  MatrixXmpRR BXmp(B->numRows(), B->numColumns());
  fill_to_MatrixXmp(*B, BXmp);

  Eigen::BDCSVD<MatrixXmpRR> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.solve(BXmp), *X);

  return true;
}

bool least_squares(const LMatrixCC *A,
  const LMatrixCC *B,
  LMatrixCC *X
)
{
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);
  MatrixXmpCC BXmp(B->numRows(), B->numColumns());
  fill_to_MatrixXmp(*B, BXmp);

  Eigen::BDCSVD<MatrixXmpCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.solve(BXmp), *X);

  return true;
}
#endif

// RRR/CCC

void fill_to_MatrixXmp(const LMatrixRRR& orig, MatrixXmpRRR& result)
{
  for (int r=0; r<orig.numRows(); r++)
    for (int c=0; c<orig.numColumns(); c++)
      result(r,c) = Real(& orig.entry(r,c), false);
}

void fill_to_MatrixXmp(const LMatrixCCC& orig,  MatrixXmpCCC& result)
{
  for (int r=0; r<orig.numRows(); r++)
    for (int c=0; c<orig.numColumns(); c++)
      result(r,c) = Complex(Real(& orig.entry(r,c).re, false),Real(& orig.entry(r,c).im, false));
}

void fill_from_MatrixXmp(const MatrixXmpRRR& orig, LMatrixRRR& result)
{
  int numrows = orig.rows();
  int numcols = orig.cols();
  result.resize(numrows, numcols);
  for (int r=0; r<numrows; r++)
    for (int c=0; c<numcols; c++)
      result.ring().set(result.entry(r,c), * orig(r,c).mpfr_srcptr());
}

void fill_from_MatrixXmp(const MatrixXmpCCC& orig, LMatrixCCC& result)
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

  MatrixXmpRRR AXmp(A->numRows(), A->numColumns());
  
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::JacobiSVD<MatrixXmpRRR> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);

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

  MatrixXmpCCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::JacobiSVD<MatrixXmpCCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
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

  MatrixXmpRRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::BDCSVD<MatrixXmpRRR> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
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

  MatrixXmpCCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::BDCSVD<MatrixXmpCCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
  fill_from_MatrixXmp(svd.matrixU(), *U);
  fill_from_MatrixXmp(svd.matrixV().adjoint(), *VT);
  fill_from_MatrixXmp(svd.singularValues(), *Sigma);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues(const LMatrixRRR *A, LMatrixCCC *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpRRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::EigenSolver<MatrixXmpRRR> es(AXmp,false);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues(const LMatrixCCC *A, LMatrixCCC *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::ComplexEigenSolver<MatrixXmpCCC> ces(AXmp, false);
  fill_from_MatrixXmp(ces.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues_hermitian(const LMatrixRRR *A, LMatrixRRR *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpRRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpRRR> es(AXmp, false);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvalues_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpCCC> es(AXmp, false);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors(const LMatrixRRR *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpRRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::EigenSolver<MatrixXmpRRR> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors(const LMatrixCCC *A, LMatrixCCC *eigenvals, LMatrixCCC *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::ComplexEigenSolver<MatrixXmpCCC> ces(AXmp);
  fill_from_MatrixXmp(ces.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(ces.eigenvectors(), *eigenvecs);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors_hermitian(const LMatrixRRR *A, LMatrixRRR *eigenvals, LMatrixRRR *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpRRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpRRR> es(AXmp);
  fill_from_MatrixXmp(es.eigenvalues(), *eigenvals);
  fill_from_MatrixXmp(es.eigenvectors(), *eigenvecs);

  Real::set_default_prec(old_prec);
  return true;
}

bool eigenvectors_hermitian(const LMatrixCCC *A, LMatrixRRR *eigenvals, LMatrixCCC *eigenvecs) {
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  MatrixXmpCCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);

  Eigen::SelfAdjointEigenSolver<MatrixXmpCCC> es(AXmp);
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

  MatrixXmpRRR AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);
  MatrixXmpRRR BXmp(B->numRows(), B->numColumns());
  fill_to_MatrixXmp(*B, BXmp);

  Eigen::BDCSVD<MatrixXmpRRR> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
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

  MatrixXmpCCC AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);
  MatrixXmpCCC BXmp(B->numRows(), B->numColumns());
  fill_to_MatrixXmp(*B, BXmp);

  Eigen::BDCSVD<MatrixXmpCCC> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);
  
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
