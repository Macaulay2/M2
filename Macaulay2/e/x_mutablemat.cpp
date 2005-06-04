// Copyright 2004 Michael E. Stillman

#include "engine.h"
#include "relem.hpp"
#include "ring.hpp"
#include "QQ.hpp"

#include "coeffrings.hpp"
#include "mat.hpp"
#include "fractionfreeLU.hpp"
#include "LLL.hpp"
#include "dmat-LU.hpp"

//#include "mutablemat.hpp"
//#include "sparsemat.hpp"
//#include "dmatrix.hpp"
//#include "lapack.hpp"
//#include "ntl_interface.hpp"

typedef MutableMatrixXXX MutableMatrixXXXOrNull;

MutableMatrixXXX * IM2_MutableMatrix_identity(const Ring *R,
						 int n,
						 M2_bool is_dense)
{
  return MutableMatrixXXX::identity(R, n, is_dense);
}

MutableMatrixXXXOrNull * IM2_MutableMatrix_make(const Ring *R,
					    int nrows,
					    int ncols,
					    M2_bool is_dense)
{
  return MutableMatrixXXX::zero_matrix(R,nrows,ncols,is_dense);
}

MutableMatrixXXX * IM2_MutableMatrix_from_matrix(const Matrix *M, M2_bool is_dense)
{
  return MutableMatrixXXX::from_matrix(M, is_dense);
}

const Matrix * IM2_MutableMatrix_to_matrix(const MutableMatrixXXX *M)
{
  return M->to_matrix();
}

const M2_string IM2_MutableMatrix_to_string(const MutableMatrixXXX *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

unsigned long IM2_MutableMatrix_hash(const MutableMatrixXXX *M)
{
  return M->get_hash_value();
}

int IM2_MutableMatrix_n_rows(const MutableMatrixXXX *M)
{
  return M->n_rows();
}

int IM2_MutableMatrix_n_cols(const MutableMatrixXXX *M)
{
  return M->n_cols();
}

const RingElementOrNull * IM2_MutableMatrix_get_entry(const MutableMatrixXXX *M, int r, int c)
{
  if (r < 0 || r >= M->n_rows())
    {
      ERROR("matrix row index %d out of range 0 .. %d", r, M->n_rows()-1);
      return 0;
    }
  if (c < 0 || c >= M->n_cols())
    {
      ERROR("matrix column index %d out of range 0 .. %d", c, M->n_cols()-1);
      return 0;
    }
  ring_elem result;
  M->get_entry(r,c,result);
  return RingElement::make_raw(M->get_ring(), result);
}

M2_bool IM2_MutableMatrix_set_entry(MutableMatrixXXX *M, 
					int r, 
					int c, 
					const RingElement *a)
{
  const Ring *R = M->get_ring();
  if (R != a->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  if (r < 0 || r >= M->n_rows())
    {
      ERROR("row index %d out of range 0..%d",r,M->n_rows()-1);
      return 0;
    }
  if (c < 0 || c >= M->n_cols())
    {
      ERROR("column index %d out of range 0..%d",c,M->n_cols()-1);
      return 0;
    }

  M->set_entry(r,c,a->get_value());
  return 1;
}

M2_bool IM2_MutableMatrix_row_swap(MutableMatrixXXX *M, int i, int j)
  /* swap rows: row(i) <--> row(j) */
{
  if (i < 0 || j < 0 || i >= M->n_rows() || j >= M->n_rows())
    {
      ERROR("row index out of range");
      return 0;
    }

  M->interchange_rows(i,j);
  return 1;
}

M2_bool IM2_MutableMatrix_column_swap(MutableMatrixXXX *M, int i, int j)
  /* swap columns: column(i) <--> column(j) */
{
  if (i < 0 || j < 0 || i >= M->n_cols() || j >= M->n_cols())
    {
      ERROR("column index out of range");
      return 0;
    }

  M->interchange_columns(i,j);
  return 1;
}

M2_bool IM2_MutableMatrix_row_operation(MutableMatrixXXX *M, 
					int i,
					const RingElement *r, 
					int j,
					M2_bool opposite_mult)
  /* row(i) <- row(i) + r * row(j) */
{
  const Ring *R = M->get_ring();
  if (R != r->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  if (i < 0 || j < 0 || i >= M->n_rows() || j >= M->n_rows())
    {
      ERROR("row index out of range");
      return 0;
    }

  M->row_op(i,r->get_value(),j);
  return 1;
}

M2_bool IM2_MutableMatrix_column_operation(MutableMatrixXXX *M, 
					   int i,
					   const RingElement *r, 
					   int j,
					   M2_bool opposite_mult)
  /* column(i) <- column(i) + r * column(j) */
{
  const Ring *R = M->get_ring();
  if (R != r->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  if (i < 0 || j < 0 || i >= M->n_cols() || j >= M->n_cols())
    {
      ERROR("column index out of range");
      return 0;
    }

  M->column_op(i,r->get_value(),j);
  return 1;
}

M2_bool IM2_MutableMatrix_row_scale(MutableMatrixXXX *M, 
				    const RingElement *r, 
				    int i,
				    M2_bool opposite_mult)
  /* row(i) <- r * row(i) */
{
  const Ring *R = M->get_ring();
  if (R != r->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  if (i < 0 || i >= M->n_rows())
    {
      ERROR("row index out of range");
      return 0;
    }
  M->scale_row(r->get_value(),i);
  return 1;
}

M2_bool IM2_MutableMatrix_column_scale(MutableMatrixXXX *M, 
    const RingElement *r, 
    int i,
    M2_bool opposite_mult)
  /* column(i) <- r * column(i) */
{
  const Ring *R = M->get_ring();
  if (R != r->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }
  if (i < 0 || i >= M->n_cols())
    {
      ERROR("column index out of range");
      return 0;
    }
  M->scale_column(r->get_value(),i);
  return 1;
}

M2_bool IM2_MutableMatrix_insert_columns(MutableMatrixXXX *M, int i, int n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. */
{
  return M->insert_columns(i, n_to_add);
}

M2_bool IM2_MutableMatrix_insert_rows(MutableMatrixXXX *M, int i, int n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
{
  return M->insert_rows(i, n_to_add);
}

M2_bool IM2_MutableMatrix_delete_columns(MutableMatrixXXX *M, int i, int j)
  /* Delete columns i .. j from M */
{
  return M->delete_columns(i, j);
}


M2_bool IM2_MutableMatrix_delete_rows(MutableMatrixXXX *M, int i, int j)
  /* Delete rows i .. j from M */
{
  return M->delete_rows(i, j);
}

M2_bool IM2_MutableMatrix_column_2by2(MutableMatrixXXX *M,
				      int c1, int c2, 
				      const RingElement *a1, const RingElement *a2,
				      const RingElement *b1, const RingElement *b2,
				      M2_bool opposite_mult)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2)
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  const Ring *R = M->get_ring();
  if (a1->get_ring() != R || a2->get_ring() != R || b1->get_ring() != R || b2->get_ring() != R)
    {
      ERROR("expected elements in the same ring");
      return 0;
    }
  return M->column2by2(c1,c2,
		       a1->get_value(),a2->get_value(),
		       b1->get_value(),b2->get_value());
}


M2_bool IM2_MutableMatrix_row_2by2(MutableMatrixXXX *M,
				   int r1, int r2, 
				   const RingElement *a1, const RingElement *a2,
				   const RingElement *b1, const RingElement *b2,
				   M2_bool opposite_mult)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2)
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  const Ring *R = M->get_ring();
  if (a1->get_ring() != R || a2->get_ring() != R || b1->get_ring() != R || b2->get_ring() != R)
    {
      ERROR("expected elements in the same ring");
      return 0;
    }
  return M->row2by2(r1,r2,
		    a1->get_value(),a2->get_value(),
		    b1->get_value(),b2->get_value());
}

M2_bool IM2_MutableMatrix_sort_columns(MutableMatrixXXX *M, int lo, int hi)
  /* Returns false if M is not mutable, or lo, or hi are out of range */
{
  ERROR("not re-implemented yet");
  return false;
}

M2_bool IM2_MutableMatrix_row_permute(MutableMatrixXXX *M,
				      int start, 
				      M2_arrayint perm)
  /* if perm = [p0 .. pr], then row(start + i) --> row(start + pi), and
     all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */
{
  int nrows = M->n_rows();
  if (start < 0 || start + perm->len > nrows)
    {
      ERROR("row indices out of range");
      return false;
    }
  for (int i=0; i<perm->len; i++)
    {
      int r = start + perm->array[i];
      if (r < 0 || r >= nrows)
	{
	  ERROR("row indices out of range");
	  return false;
	}
    }
  return M->row_permute(start,perm);
}

M2_bool IM2_MutableMatrix_column_permute(MutableMatrixXXX *M,
					 int start, 
					 M2_arrayint perm)
  /* if perm = [p0 .. pr], then column(start + i) --> column(start + pi), and
     all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */
{
  int ncols = M->n_cols();
  if (start < 0 || start + perm->len > ncols)
    {
      ERROR("column indices out of range");
      return false;
    }
  for (int i=0; i<perm->len; i++)
    {
      int r = start + perm->array[i];
      if (r < 0 || r >= ncols)
	{
	  ERROR("column indices out of range");
	  return false;
	}
    }
  return M->column_permute(start,perm);
}


const RingElement * IM2_Matrix_dot_product(const MutableMatrixXXX *M, int c1, int c2)
{
  ring_elem a;
  M->dot_product(c1,c2,a);
  return RingElement::make_raw(M->get_ring(), a);
}

const M2_bool IM2_MutableMatrix_is_zero(const MutableMatrixXXX *M)
{
  return M->is_zero();
}

const M2_bool IM2_MutableMatrix_is_equal(const MutableMatrixXXX *M, 
					 const MutableMatrixXXX *N)
/* This checks that the entries of M,N are the same */
{
  return M->is_equal(N);
}

MutableMatrixXXX * IM2_MutableMatrix_copy(MutableMatrixXXX *M, M2_bool prefer_dense)
{
  return M->copy(prefer_dense);
}

M2_bool IM2_MutableMatrix_set_values(MutableMatrixXXX *M, 
				     M2_arrayint rows,
				     M2_arrayint cols,
				     RingElement_array *values)
{
  return M->set_values(rows,cols,values);
}

M2_bool IM2_MutableMatrix_reduce_by_pivots(MutableMatrixXXX *M)
/* Using row and column operations, use unit pivots to reduce the matrix */
{
  //  SparseMutableMatrix *N = M->cast_to_SparseMutableMatrix();
  //  if (N == 0)
  //    {
  //      ERROR("expected sparse mutable matrix");
  //      return false;
  //    }
  //  N->reduce_pivots();
  return true;
}

MutableMatrixXXXOrNull * IM2_MutableMatrix_add(const MutableMatrixXXX *M, const MutableMatrixXXX *N)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the addition is performed.  If the targets are not equal, the target 
   of the result is set to have each degree zero.  Similarly with the
   source, and also with the degree of the matrix. */
{
  return M->add(N);
}

MutableMatrixXXXOrNull * IM2_MutableMatrix_subtract(const MutableMatrixXXX *M, const MutableMatrixXXX *N)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the addition is performed.  If the targets are not equal, the target 
   of the result is set to have each degree zero.  Similarly with the
   source, and also with the degree of the matrix. */
{
  return M->subtract(N);
}

MutableMatrixXXX * IM2_MutableMatrix_negate(const MutableMatrixXXX *M)
{
  return M->negate();
}

MutableMatrixXXXOrNull * IM2_MutableMatrix_mult(const MutableMatrixXXX *M, 
					     const MutableMatrixXXX *N, 
					     M2_bool opposite_mult)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the multiplication is performed, and the source and target are taken from N,M
   respectively.  The degree of the result is the sum of the two degrees */
{
  return M->mult(N);
}

MutableMatrixXXXOrNull * IM2_MutableMatrix_scalar_mult(const RingElement *f,
						    const MutableMatrixXXX *M, 
						    M2_bool opposite_mult)
{
  return M->mult(f);
}

MutableMatrixXXXOrNull * IM2_MutableMatrix_submatrix(const MutableMatrixXXX *M,
						  const M2_arrayint rows,
						  const M2_arrayint cols)
{
  return M->submatrix(rows,cols);
}

bool IM2_MutableMatrix_set_submatrix(MutableMatrixXXX *M,
				     const M2_arrayint rows,
				     const M2_arrayint cols,
				     const MutableMatrixXXX *N)
{
  return M->set_submatrix(rows,cols,N);
}

MutableMatrixXXXOrNull * IM2_MutableMatrix_submatrix1(const MutableMatrixXXX *M,
						   const M2_arrayint cols)
{
  return M->submatrix(cols);
}

/*******************************
 ** Cmputations ****************
 *******************************/

M2_arrayint_OrNull IM2_FF_LU(MutableMatrixXXX *M)
{
  return FF_LUComputation::DO(M);
}

M2_bool IM2_LLL(MutableMatrixXXX *M, const M2_Rational threshold, int strategy)
{
  if (strategy == 0)
    {
      return LLLoperations::LLL(M,threshold);
    }

  long a = mpz_get_si(mpq_numref(threshold));
  long b = mpz_get_si(mpq_denref(threshold));
  return ntl_LLL(M,a,b,strategy);
}

M2_bool IM2_SmithNormalForm(MutableMatrixXXX *M)
{
#warning "implement smith"
  ERROR("not implemented yet");
  return 0;
}

M2_bool IM2_HermiteNormalForm(MutableMatrixXXX *M)
{
#warning "implement hermite"
  ERROR("not implemented yet");
  return 0;
}

  /***************************************************
   ***** Lapack routines for dense mutable matrices **
   ***************************************************/

  /* Each of the following routines accepts honest MutableMatrix arguments,
     and returns false if there is an error.  The return values are placed into
     some of the (already existing) parameters of the routine */
//typedef DMat<CoefficientRingRR> LMatrixRR;
//typedef DMat<CoefficientRingCC> LMatrixCC;

M2_arrayint_OrNull rawLU(MutableMatrixXXX *A)
{
#if 0
  const Ring *R = A->get_ring();
  if (R == globalRR)
    {
      LMatrixRR *A2 = A->get_DMatRR();
      if (A2 == 0)
	{
	  ERROR("requires dense mutable matrix over RR");
	  return 0;
	}
      return Lapack::LU(A2);
    }
  if (R == globalCC)
    {
      LMatrixCC *A2 = A->get_DMatCC();
      if (A2 == 0)
	{
	  ERROR("requires dense mutable matrix over CC");
	  return 0;
	}
      return Lapack::LU(A2);
    }
  const Z_mod *KZZp = R->cast_to_Z_mod();
  if (KZZp != 0)
    {
      DMat<CoefficientRingZZp> *A2 = A->get_DMatZZp();
      if (A2 == 0)
	{
	  ERROR("requires dense mutable matrix over ZZ/p");
	  return 0;
	}
      return DMatLU<CoefficientRingZZp>::LU(A2);
    }
#endif
  ERROR("not re-implemented yet");
  return false;
}

M2_bool rawNullspaceU(MutableMatrixXXX *U,
		      MutableMatrixXXX *x)
  /* U should be a matrix in LU 'U' format.
     x is set to the matrix whose columns form a basis of Ux=0,
     which is the same as Ax=0. if A = PLU is the LU-decomp of A.
  */
{
  return U->nullspaceU(x);
}

M2_bool rawSolve(MutableMatrixXXX *A,
		 MutableMatrixXXX *b,
		 MutableMatrixXXX *x)
{
  /* Check: A, b, x all have the same ring, either RR or CC */
  /* Check: if all of these are dense mutable matrices, then 
     call the correct routine */
  /* Otherwise: give error: 
     OR: make mutable matrices of the correct size, call the correct routine
     and afterwords, copy to x. */
  return A->solve(b,x);
}

M2_bool rawEigenvalues(MutableMatrixXXX *A,
		       MutableMatrixXXX *eigenvalues,
		       M2_bool is_symm_or_hermitian)
{
  return A->eigenvalues(eigenvalues,is_symm_or_hermitian);
}

M2_bool rawEigenvectors(MutableMatrixXXX *A,
			MutableMatrixXXX *eigenvalues,
			MutableMatrixXXX *eigenvectors,
			M2_bool is_symm_or_hermitian)
{
  return A->eigenvectors(eigenvalues, eigenvectors, is_symm_or_hermitian);
}

M2_bool rawSVD(MutableMatrixXXX *A,
	       MutableMatrixXXX *Sigma,
	       MutableMatrixXXX *U,
	       MutableMatrixXXX *VT,
	       M2_bool use_divide_and_conquer)
{
  return A->SVD(Sigma,U,VT,use_divide_and_conquer);
}

M2_bool rawLeastSquares(MutableMatrixXXX *A, 
			MutableMatrixXXX *b, 
			MutableMatrixXXX *x, /* return value: argument modified */
			M2_bool assume_full_rank)
/* Case 1: A is a dense matrix over RR.  Then so are b,x.
   Case 2: A is a dense matrix over CC.  Then so are b,x. */
{
  return A->least_squares(b,x,assume_full_rank);
}

#if 0

  LMatrixCCOrNull * eigenvalues(LMatrixCC *eigenvalues);
  // returns NULL if an error occurs, else returns eigenvalues matrix,
  // which is a complex matrix.  'eigenvalues' must be initialized first.  
  // It does not need to have the correct size.

  LMatrixCC * eigenvectors(LMatrixCC *eigenvalues, LMatrixCC *eigenvectors);
  // returns NULL if an error occurs, else returns eigenvalues matrix,
  // which is a complex matrix.  'eigenvalues' must be initialized first.  
  // It does not need to have the correct size.

  LMatrixRROrNull * eigenvalues_symmetric(LMatrixRR *eigenvalues);
  // returns NULL if an error occurs, else returns eigenvalues matrix
  // for a symmetric matrix.  'eigenvalues' needs to be initialized first.  
  // It does not need to have the correct size.
  // Assumes symmetric matrix by using upper triangular part only.

  LMatrixRROrNull * eigenvectors_symmetric(LMatrixRR *eigenvalues, 
					   LMatrixRR *eigenvectors); 
  // returns NULL if an error occurs, else returns eigenvector matrix
  // and eigenvalues of a symmetric matrix.  
  // 'eigenvalues' and 'eigenvectors' need to be initialized first. 
  // They do not need to have the correct size.
  // Assumes symmetric matrix by using upper triangular part only.

  LMatrixRROrNull * SVD(LMatrixRR *Sigma, LMatrixRR *U, LMatrixRR *VT);
  // returns NULL if an error occurs, else returns the singular values
  // 'Sigma' in the singular value decomposition 'A = U*Diag(Sigma)*VT'.  
  // Note the routine returns VT, which is the transpose of V.

  LMatrixRROrNull * SVD_divide_conquer(LMatrixRR *Sigma, LMatrixRR *U, 
				 LMatrixRR *VT);
  // better algorithm for SVD, especially for large matrices
  // might fail on hexadecimal / decimal machines?


  LMatrixRROrNull * least_squares(LMatrixRR *b, LMatrixRR *x);
  // This routine assumes the matrix has full rank.
  // return NULL if an error occurs, else returns the solutions to the
  // linear least squares problem which minimizes |Ax-b| if A has more
  // rows than columns and minimizes |x| satisfying Ax=b if A has more
  // columns than rows.

  LMatrixRROrNull * least_squares_deficient(LMatrixRR *b, LMatrixRR *x);
  // This routine can handle matrices with deficient rank.  It uses SVD.
  // return NULL if an error occurs, else returns the minimum norm
  // solution to the linear least squares problem which minimizes |Ax-b|
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
