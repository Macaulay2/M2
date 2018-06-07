#include <cstdlib>

//void* my_malloc( std::size_t size );
//void my_free( void* ptr );
namespace std {
void* my_malloc( std::size_t size );
void my_free( void* ptr );
}
using std::my_malloc;
using std::my_free;

#define malloc my_malloc
#define free my_free
#define mpfr foo
#include "mpreal.h"
#include <Eigen/MPRealSupport>
#undef mpfr
#include "eigen.hpp"
#include <Eigen/SVD>
#include "mpfr.h"
#undef free
#undef malloc

#include <gc/gc_allocator.h>
// void* my_malloc( std::size_t size )
// {
//   return GC_MALLOC_UNCOLLECTABLE(size);
// }
// void my_free( void* ptr )
// {
//   GC_FREE(ptr);
// }
namespace std {
void* my_malloc( std::size_t size )
{
  return GC_MALLOC_UNCOLLECTABLE(size);
}
void my_free( void* ptr )
{
  GC_FREE(ptr);
}
}


/***********************
 * An attempt to overwrite memory management globally
 * (i.e., without "#define my_free", etc.)
 * ... failed!!!
 * Is there a function that I'm missing???
  
void* malloc( std::size_t size )
{
  std::cout << "-- std::malloc call!!!" << std::endl;
  return GC_MALLOC_UNCOLLECTABLE(size);
}
void* realloc( void* ptr, std::size_t new_size )
{
  std::cout << "-- std::realloc call!!!" << std::endl;
  GC_FREE(ptr);
  return GC_MALLOC_UNCOLLECTABLE(new_size);
}
void* calloc( std::size_t num, std::size_t size )
{
  std::cout << "-- std::calloc call!!!" << std::endl;
  return GC_MALLOC_UNCOLLECTABLE(num*size);
}
void free( void* ptr )
{
  std::cout << "-- std::free call!!!" << std::endl;
  GC_FREE(ptr);
}
*/

using Real = foo::mpreal;
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
  std::cerr << "Eigen::SVD (real)" << std::endl;
  auto old_prec = Real::get_default_prec(); 
  Real::set_default_prec(A->ring().get_precision());

  // Create the correct matrices: A, Sigma, U, VT perhaps.
  // call eigen
  // Transform matrices back.

  std::cerr << "  starting Eigen code space..." << std::endl;  
  MatrixXmp AXmp(A->numRows(), A->numColumns());
    
  fill_to_MatrixXmp(*A, AXmp);
  std::cout << AXmp.cols() << std::endl;

  Eigen::JacobiSVD<MatrixXmp> svd(AXmp, Eigen::ComputeThinU | Eigen::ComputeThinV);

  auto eigenU = svd.matrixU();
  auto eigenVT = svd.matrixV().adjoint();
  auto eigenSigma = svd.singularValues();
  
  std::cerr << "  about to fill M2 matrices..." << std::endl ; 
  fill_from_MatrixXmp(eigenU, *U);
  fill_from_MatrixXmp(eigenVT, *VT);
  fill_from_MatrixXmp(eigenSigma, *Sigma);

  Real::set_default_prec(old_prec);
  std::cerr << "  leaving SVD..." << std::endl;
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
