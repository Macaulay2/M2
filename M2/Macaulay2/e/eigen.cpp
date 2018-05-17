#define mpfr foo
#include "mpreal.h"
#include <Eigen/MPRealSupport>
#undef mpfr

// This line is here to get def of gmp allocation functions
#include "../d/M2inits.h"

#include "eigen.hpp"
#include <Eigen/SVD>
#include "mpfr.h"
#include <cstdlib>

// This function is a hack to change allocator for gmp
// (and therefore mpfr) so that Eigen will not use garb age collected memory
void
our_mp_set_memory_functions (void *(*alloc_func) (size_t),
			 void *(*realloc_func) (void *, size_t, size_t),
			 void (*free_func) (void *, size_t))
{
 if (alloc_func == 0)
   alloc_func = __gmp_default_allocate;
 if (realloc_func == 0)
   realloc_func = __gmp_default_reallocate;
 if (free_func == 0)
   free_func = __gmp_default_free;
 __gmp_allocate_func = alloc_func;
 __gmp_reallocate_func = realloc_func;
 __gmp_free_func = free_func;
}

using Real = foo::mpreal;
using Complex = std::complex<Real>;
using MatrixXmp = Eigen::Matrix<Real,Eigen::Dynamic,Eigen::Dynamic>;
using MatrixXmpCC = Eigen::Matrix<Complex,Eigen::Dynamic,Eigen::Dynamic>;


namespace EigenM2 {

static void freeFunc(void *p, size_t sz)
  {
  }
  
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
    our_mp_set_memory_functions((void *(*) (size_t)) malloc,
                            (void *(*) (void *, size_t, size_t)) realloc,
                            (void (*)(void *, size_t)) freeFunc);
  }
  void before()
  {
    our_mp_set_memory_functions(alloc_func_ptr,realloc_func_ptr,free_func_ptr);    
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
  std::cerr << "Eigen::SVD (real)" << std::endl;
  SetMemoryFunctions smf; // gc off
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
  
  smf.before(); // gc on
  std::cerr << "  about to fill M2 matrices..." << std::endl ; 
  fill_from_MatrixXmp(eigenU, *U);
  fill_from_MatrixXmp(eigenVT, *VT);
  fill_from_MatrixXmp(eigenSigma, *Sigma);

  smf.after(); // gc off
  Real::set_default_prec(old_prec);
  std::cerr << "  leaving SVD..." << std::endl;
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
