#include "engine.h"
#include "error.h"
#include "lapack.h"
#include <stdio.h>
#include "matrixRR.hpp"
#include "matrixCC.hpp"

M2_CC LP_make_M2_Comp(double* w)
{
  M2_CC z = (M2_CC) getmem_atomic(sizeof(M2_CC_struct));
  z->re = w[0];
  z->im = w[1];
  return z;
}

M2_CC LP_make_M2_Complex(double re, double im)
{
  M2_CC z = (M2_CC) getmem_atomic(sizeof(M2_CC_struct));
  z->re = re;
  z->im = im;
  return z;
}

LMatrixRR *LP_LMatrixRR_make(int nrows, int ncols)
{
  return new LMatrixRR(nrows, ncols);
}

LMatrixCC *LP_LMatrixCC_make(int nrows, int ncols)
{
  return new LMatrixCC(nrows, ncols);
}

LMatrixCC *LP_LMatrixCC_from_LMatrixRR(LMatrixRR *N)
{
  return new LMatrixCC(N);
}

void LP_LMatrixRR_resize(LMatrixRR *M, int nrows, int ncols)
{
  M->resize(nrows, ncols);
}

void LP_LMatrixCC_resize(LMatrixCC *M, int nrows, int ncols)
{
  M->resize(nrows, ncols);
}

LMatrixCC *LP_LMatrixCC_copy(LMatrixCC *M)
{
  return M->copy();
}

int LP_LMatrixRR_nrows(const LMatrixRR *M)
{
  return M->nrows();
}

int LP_LMatrixRR_ncols(const LMatrixRR *M)
{
  return M->ncols();
}

int LP_LMatrixCC_nrows(const LMatrixCC *M)
{
  return M->nrows();
}

int LP_LMatrixCC_ncols(const LMatrixCC *M)
{
  return M->ncols();
}

void LP_LMatrixRR_get_entry(const LMatrixRR *M, int r, int c, double *re)
{
  M->get_entry(r,c,re);
}

void LP_LMatrixCC_get_entry(const LMatrixCC *M, int r, int c, 
			    M2_CC result)
{
  M->get_entry(r,c,result);
}

void LP_LMatrixRR_set_entry(LMatrixRR *M, int r, int c, double re)
{
  M->set_entry(r,c,re);
}

void LP_LMatrixCC_set_entry(LMatrixCC *M, int r, int c, M2_CC val)
{
  M->set_entry(r,c,val);
}

void LP_LMatrixRR_set_values(LMatrixRR *M, M2_arrayint rowcols, M2_double_array *vals)
{
   M->set_values(rowcols,vals);
}

void LP_LMatrixCC_set_values(LMatrixCC *M, M2_arrayint rowcols, M2_complex_array *vals)
{
   M->set_values(rowcols,vals);
}

LMatrixRR * LP_LMatrixRR_get_submatrix(LMatrixRR *M, M2_arrayint rows, 
					     M2_arrayint cols)
{
  return M->sub_matrix(rows, cols);
}

LMatrixCC * LP_LMatrixCC_get_submatrix(LMatrixCC *M, M2_arrayint rows, 
					     M2_arrayint cols)
{
  return M->sub_matrix(rows, cols);
}

M2_string LP_LMatrixRR_to_string(const LMatrixRR *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

M2_string LP_LMatrixCC_to_string(const LMatrixCC *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

LMatrixCCOrNull *LP_LMatrixRR_eigenvalues(LMatrixRR *M, LMatrixCC *eigenvalues)
{
  return M->eigenvalues(eigenvalues);
}

LMatrixCCOrNull *LP_LMatrixRR_eigenvectors(LMatrixRR *M, LMatrixCC *eigenvalues,
					   LMatrixCC *eigenvectors)
{
  return M->eigenvectors(eigenvalues, eigenvectors);
}

LMatrixCCOrNull *LP_LMatrixCC_eigenvalues(LMatrixCC *M, LMatrixCC *eigenvalues)
{
  return M->eigenvalues(eigenvalues);
}

LMatrixCCOrNull *LP_LMatrixCC_eigenvectors(LMatrixCC *M, LMatrixCC *eigenvalues, 
					   LMatrixCC *eigenvectors)
{
  return M->eigenvectors(eigenvalues, eigenvectors);
}

LMatrixRROrNull *LP_LMatrixRR_eigenvalues_symmetric(LMatrixRR *M, 
					     LMatrixRR *eigenvalues)
{
  return M->eigenvalues_symmetric(eigenvalues);
}

LMatrixRROrNull *LP_LMatrixRR_eigenvectors_symmetric(LMatrixRR *M, 
						     LMatrixRR *eigenvalues, 
						     LMatrixRR *eigenvectors)
{
  return M->eigenvectors_symmetric(eigenvalues, eigenvectors);
}

LMatrixRROrNull *LP_LMatrixCC_eigenvalues_hermitian(LMatrixCC *M, 
					     LMatrixRR *eigenvalues)
{
  return M->eigenvalues_hermitian(eigenvalues);
}

LMatrixCCOrNull *LP_LMatrixCC_eigenvectors_hermitian(LMatrixCC *M, 
						     LMatrixRR *eigenvalues, 
						     LMatrixCC *eigenvectors)
{
  return M->eigenvectors_hermitian(eigenvalues, eigenvectors);
}

LMatrixRROrNull *LP_LMatrixRR_solve(LMatrixRR *A, LMatrixRR *b, LMatrixRR *x)
{
  return A->solve(b, x);
}

LMatrixCCOrNull *LP_LMatrixCC_solve(LMatrixCC *A, LMatrixCC *b, LMatrixCC *x)
{
  return A->solve(b,x);
}

LMatrixRROrNull *LP_LMatrixRR_LU(LMatrixRR *M, LMatrixRR *L, LMatrixRR *U, LMatrixRR *P)
{
  return M->LU(L, U, P);
}

LMatrixCCOrNull *LP_LMatrixCC_LU(LMatrixCC *M, LMatrixCC *L, LMatrixCC *U, LMatrixRR *P)
{
  return M->LU(L, U, P);
}

LMatrixRROrNull *LP_LMatrixRR_SVD(LMatrixRR *M, LMatrixRR *Sigma, 
			    LMatrixRR *U, LMatrixRR *VT)
{
  return M->SVD(Sigma, U, VT);
}

LMatrixRROrNull *LP_LMatrixRR_SVD_divide_conquer(LMatrixRR *M, LMatrixRR *Sigma, 
					   LMatrixRR *U, LMatrixRR *VT)
{
  return M->SVD_divide_conquer(Sigma, U, VT);
}

LMatrixRROrNull *LP_LMatrixCC_SVD(LMatrixCC *M, LMatrixRR *Sigma, 
				  LMatrixCC *U, LMatrixCC *VT)
{
  return M->SVD(Sigma, U, VT);
}

LMatrixRROrNull *LP_LMatrixCC_SVD_divide_conquer(LMatrixCC *M, LMatrixRR *Sigma, 
						 LMatrixCC *U, LMatrixCC *VT)
{
  return M->SVD_divide_conquer(Sigma, U, VT);
}

LMatrixRROrNull *LP_LMatrixRR_least_squares(LMatrixRR *M, LMatrixRR *b, 
					    LMatrixRR *x)
{
  return M->least_squares(b, x);
}

LMatrixCCOrNull *LP_LMatrixCC_least_squares(LMatrixCC *M, LMatrixCC *b, 
					    LMatrixCC *x)
{
  return M->least_squares(b, x);
}

LMatrixRROrNull *LP_LMatrixRR_least_squares_deficient(LMatrixRR *M, LMatrixRR *b, 
						      LMatrixRR *x)
{
  return M->least_squares_deficient(b, x);
}

LMatrixCCOrNull *LP_LMatrixCC_least_squares_deficient(LMatrixCC *M, LMatrixCC *b, 
						      LMatrixCC *x)
{
  return M->least_squares_deficient(b, x);
}


LMatrixRROrNull *LP_LMatrixRR_add(LMatrixRR *M, LMatrixRR *N)
{
  return (*M) + N;
}

LMatrixCCOrNull *LP_LMatrixRC_add(LMatrixRR *M, LMatrixCC *N)
{
  return (*M) + N;
}

LMatrixCCOrNull *LP_LMatrixCR_add(LMatrixCC *M, LMatrixRR *N)
{
  return (*M) + N;
}

LMatrixCCOrNull *LP_LMatrixCC_add(LMatrixCC *M, LMatrixCC *N)
{
  return (*M) + N;
}

LMatrixRROrNull *LP_LMatrixRR_subtract(LMatrixRR *M, LMatrixRR *N)
{
  return (*M) - N;
}

LMatrixCCOrNull *LP_LMatrixRC_subtract(LMatrixRR *M, LMatrixCC *N)
{
  return (*M) - N;
}

LMatrixCCOrNull *LP_LMatrixCR_subtract(LMatrixCC *M, LMatrixRR *N)
{
  return (*M) - N;
}

LMatrixCCOrNull *LP_LMatrixCC_subtract(LMatrixCC *M, LMatrixCC *N)
{
  return (*M) - N;
}

LMatrixRR *LP_LMatrixRR_negate(LMatrixRR *M)
{
  return - (*M);
}

LMatrixCC *LP_LMatrixCC_negate(LMatrixCC *M)
{
  return - (*M);
}

LMatrixRROrNull *LP_LMatrixRR_mult(LMatrixRR *M, LMatrixRR *N)
{
  return (*M) * N;
}

LMatrixCCOrNull *LP_LMatrixCC_mult(LMatrixCC *M, LMatrixCC *N)
{
  return (*M) * N;
}

LMatrixCCOrNull *LP_LMatrixRC_mult(LMatrixRR *M, LMatrixCC *N)
{
  return (*M) * N;
}

LMatrixCCOrNull *LP_LMatrixCR_mult(LMatrixCC *M, LMatrixRR *N)
{
  return (*M) * N;
}

double * LP_LMatrixRR_get_epsilon()
{
  double *result = (double *) getmem(sizeof(double));
  *result = LMatrixRR::get_epsilon();
  return result;
}
double * LP_LMatrixCC_get_epsilon()
{
  double *result = (double *) getmem(sizeof(double));
  *result = LMatrixCC::get_epsilon();
  return result;
}
void LP_LMatrixRR_set_epsilon(double epsilon)
{
  LMatrixRR::set_epsilon(epsilon);
}
void LP_LMatrixCC_set_epsilon(double epsilon)
{
  LMatrixCC::set_epsilon(epsilon);
}

const M2_bool LP_LMatrixRR_is_equal(const LMatrixRR *M, const LMatrixRR *N)
{
  return M->is_equal(*N);
}

const M2_bool LP_LMatrixCC_is_equal(const LMatrixCC *M, const LMatrixCC *N)
{
  return M->is_equal(*N);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
