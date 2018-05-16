#define mpfr foo
#include "mpreal.h"
#include <Eigen/MPRealSupport>
#undef mpfr

#include "eigen.hpp"
#include <Eigen/SVD>
#include "mpfr.h"
#include <cstdlib>

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
  
class SetMemoryFunctions {
  void *(*alloc_func_ptr) (size_t);
  void *(*realloc_func_ptr) (void *, size_t, size_t); 
  void (*free_func_ptr) (void *, size_t);
public:
  SetMemoryFunctions()
  {
    mp_get_memory_functions(&alloc_func_ptr,&realloc_func_ptr,&free_func_ptr);
    after();
  } 
  ~SetMemoryFunctions()
  {
    before();
  }
  void after() 
  {
    mp_set_memory_functions((void *(*) (size_t)) malloc,
                            (void *(*) (void *, size_t, size_t)) realloc,
                            (void (*)(void *, size_t)) free);
  }
  void before()
  {
    mp_set_memory_functions(alloc_func_ptr,realloc_func_ptr,free_func_ptr);    
  }
};
  
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
  SetMemoryFunctions smf; // gc off
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());
  std::cout << "Eigen::SVD (real)" << std::endl;

  // Create the correct matrices: A, Sigma, U, VT perhaps.
  // call eigen
  // Transform matrices back.

  MatrixXmp AXmp(A->numRows(), A->numColumns());
  fill_to_MatrixXmp(*A, AXmp);
  Eigen::JacobiSVD<MatrixXmp> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);

  auto eigenU = svd.matrixU();
  auto eigenVT = svd.matrixV().adjoint();
  auto eigenSigma = svd.singularValues();
  
  smf.before(); // gc on
  fill_from_MatrixXmp(eigenU, *U);
  fill_from_MatrixXmp(eigenVT, *VT);
  fill_from_MatrixXmp(eigenSigma, *Sigma);

  smf.after(); // gc off
  Real::set_default_prec(old_prec);
  return true;
  // svd destructed
  // AXmp destructed
  // smf destructed => gc on
}

bool SVD(const LMatrixCCC *A,
         LMatrixRRR *Sigma,
         LMatrixCCC *U,
         LMatrixCCC *VT
         )
{
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  //int rows = static_cast<int>(A->numRows());
  //int cols = static_cast<int>(A->numColumns());
  //int min = (rows <= cols) ? rows : cols;

  // Create the correct matrices: A, Sigma, U, VT perhaps.
  // call eigen
  // Transform matrices back.
  MatrixXmpCC AXmp(A->numRows(), A->numColumns());

  fill_to_MatrixXmp(*A, AXmp);

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
