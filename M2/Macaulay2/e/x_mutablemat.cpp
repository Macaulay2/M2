// Copyright 2004 Michael E. Stillman

#include "engine.h"
#include "mutablemat.hpp"
#include "relem.hpp"
#include "LLL.hpp"
#include "fractionfreeLU.hpp"
#include "ring.hpp"
#include "densematRR.hpp"
#include "sparsemat.hpp"

typedef MutableMatrix MutableMatrixOrNull;

MutableMatrix * IM2_MutableMatrix_identity(const Ring *R,
						 int n,
						 M2_bool is_dense)
{
  return MutableMatrix::identity(R, n, is_dense);
}

MutableMatrixOrNull * IM2_MutableMatrix_make(const Ring *R,
					    int nrows,
					    int ncols,
					    M2_bool is_dense)
{
  if (nrows < 0 | ncols < 0)
    {
      ERROR("expected non-negative number of rows or columns");
      return 0;
    }
  return MutableMatrix::zero_matrix(R,nrows,ncols,is_dense);
}

MutableMatrix * IM2_MutableMatrix_from_matrix(const Matrix *M, M2_bool is_dense)
{
  return MutableMatrix::from_matrix(M, is_dense);
}

const Matrix * IM2_MutableMatrix_to_matrix(const MutableMatrix *M)
{
  return M->to_matrix();
}

const M2_string IM2_MutableMatrix_to_string(const MutableMatrix *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

unsigned long IM2_MutableMatrix_hash(const MutableMatrix *M)
{
  return M->get_hash_value();
}

int IM2_MutableMatrix_n_rows(const MutableMatrix *M)
{
  return M->n_rows();
}

int IM2_MutableMatrix_n_cols(const MutableMatrix *M)
{
  return M->n_cols();
}


MutableMatrixOrNull * IM2_MutableMatrix_get_row_change(MutableMatrix *M)
{
  return M->getRowChangeMatrix();
}

MutableMatrixOrNull * IM2_MutableMatrix_get_col_change(MutableMatrix *M)
{
  return M->getColumnChangeMatrix();
}

M2_bool IM2_MutableMatrix_set_row_change(MutableMatrix *M,
					 MutableMatrix *rowChange)
{
  return M->setRowChangeMatrix(rowChange);
}

M2_bool IM2_MutableMatrix_set_col_change(MutableMatrix *M,
					 MutableMatrix *colChange)
{
  return M->setColumnChangeMatrix(colChange);
}

const RingElementOrNull * IM2_MutableMatrix_get_entry(const MutableMatrix *M, int r, int c)
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

M2_bool IM2_MutableMatrix_set_entry(MutableMatrix *M, 
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

M2_bool IM2_MutableMatrix_row_swap(MutableMatrix *M, int i, int j)
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

M2_bool IM2_MutableMatrix_column_swap(MutableMatrix *M, int i, int j)
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

M2_bool IM2_MutableMatrix_row_operation(MutableMatrix *M, 
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

  M->row_op(i,r->get_value(),j, opposite_mult);
  return 1;
}

M2_bool IM2_MutableMatrix_column_operation(MutableMatrix *M, 
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

  M->column_op(i,r->get_value(),j, opposite_mult);
  return 1;
}

M2_bool IM2_MutableMatrix_row_scale(MutableMatrix *M, 
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
  M->scale_row(r->get_value(),i, opposite_mult);
  return 1;
}

M2_bool IM2_MutableMatrix_column_scale(MutableMatrix *M, 
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
  M->scale_column(r->get_value(),i,opposite_mult);
  return 1;
}

M2_bool IM2_MutableMatrix_insert_columns(MutableMatrix *M, int i, int n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. */
{
  ERROR("not yet implemented");
  return false;
}

M2_bool IM2_MutableMatrix_insert_rows(MutableMatrix *M, int i, int n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
{
  ERROR("not yet implemented");
  return false;
}

M2_bool IM2_MutableMatrix_delete_columns(MutableMatrix *M, int i, int j)
  /* Delete columns i .. j from M */
{
  ERROR("not yet implemented");
  return false;
}


M2_bool IM2_MutableMatrix_delete_rows(MutableMatrix *M, int i, int j)
  /* Delete rows i .. j from M */
{
  ERROR("not yet implemented");
  return false;
}

M2_bool IM2_MutableMatrix_column_2by2(MutableMatrix *M,
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
		       b1->get_value(),b2->get_value(),
		       opposite_mult);
}


M2_bool IM2_MutableMatrix_row_2by2(MutableMatrix *M,
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
		    b1->get_value(),b2->get_value(),
		    opposite_mult);
}

M2_bool IM2_MutableMatrix_sort_columns(MutableMatrix *M, int lo, int hi)
  /* Returns false if M is not mutable, or lo, or hi are out of range */
{
  ERROR("not re-implemented yet");
  return false;
}

M2_bool IM2_MutableMatrix_row_permute(MutableMatrix *M,
				      int start, 
				      M2_arrayint perm)
  /* if perm = [p0 .. pr], then row(start + i) --> row(start + pi), and
     all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */
{
  ERROR("not re-implemented yet");
  return false;
}

M2_bool IM2_MutableMatrix_column_permute(MutableMatrix *M,
					 int start, 
					 M2_arrayint perm)
  /* if perm = [p0 .. pr], then column(start + i) --> column(start + pi), and
     all other rows are unchanged.  p0 .. pr should be a permutation of 0..r */
{
  ERROR("not re-implemented yet");
  return false;
}


const RingElement * IM2_Matrix_dot_product(const MutableMatrix *M, int c1, int c2)
{
  ring_elem a;
  M->dot_product(c1,c2,a);
  return RingElement::make_raw(M->get_ring(), a);
}

const M2_bool IM2_MutableMatrix_is_zero(const MutableMatrix *M)
{
  return M->is_zero();
}

const M2_bool IM2_MutableMatrix_is_equal(const MutableMatrix *M, 
					 const MutableMatrix *N)
/* This checks that the entries of M,N are the same */
{
  return M->is_equal(N);
}

MutableMatrix * IM2_MutableMatrix_copy(MutableMatrix *M, M2_bool prefer_dense)
{
  return M->copy(prefer_dense);
}

M2_bool IM2_MutableMatrix_set_values(MutableMatrix *M, 
				     M2_arrayint rows,
				     M2_arrayint cols,
				     RingElement_array *values)
{
  return M->set_values(rows,cols,values);
}

M2_bool IM2_MutableMatrix_reduce_by_pivots(MutableMatrix *M)
/* Using row and column operations, use unit pivots to reduce the matrix */
{
  SparseMutableMatrix *N = M->cast_to_SparseMutableMatrix();
  if (N == 0)
    {
      ERROR("expected sparse mutable matrix");
      return false;
    }
  N->reduce_pivots();
  return true;
}

MutableMatrixOrNull * IM2_kernel_of_GB(const MutableMatrix *G)
  /* Assuming that the columns of G form a GB, this computes
     a Groebner basis of the kernel of these elements, using an appropriate Schreyer order on the
     source of G. */
{
#warning "put the Eschreyer call here"
  return const_cast<MutableMatrix *>(G);
}

MutableMatrixOrNull * IM2_MutableMatrix_add(const MutableMatrix *M, const MutableMatrix *N)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the addition is performed.  If the targets are not equal, the target 
   of the result is set to have each degree zero.  Similarly with the
   source, and also with the degree of the matrix. */
{
  return M->add(N);
}

MutableMatrixOrNull * IM2_MutableMatrix_subtract(const MutableMatrix *M, const MutableMatrix *N)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the addition is performed.  If the targets are not equal, the target 
   of the result is set to have each degree zero.  Similarly with the
   source, and also with the degree of the matrix. */
{
  return M->subtract(N);
}

MutableMatrix * IM2_MutableMatrix_negate(const MutableMatrix *M)
{
  return M->negate();
}

MutableMatrixOrNull * IM2_MutableMatrix_mult(const MutableMatrix *M, 
					     const MutableMatrix *N, 
					     M2_bool opposite_mult)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the multiplication is performed, and the source and target are taken from N,M
   respectively.  The degree of the result is the sum of the two degrees */
{
  return M->mult(N, opposite_mult);
}

MutableMatrixOrNull * IM2_MutableMatrix_scalar_mult(const RingElement *f,
						    const MutableMatrix *M, 
						    M2_bool opposite_mult)
{
  return M->mult(f, opposite_mult);
}

MutableMatrixOrNull * IM2_MutableMatrix_submatrix(const MutableMatrix *M,
						  const M2_arrayint rows,
						  const M2_arrayint cols)
{
  return M->submatrix(rows,cols);
}

MutableMatrixOrNull * IM2_MutableMatrix_submatrix1(const MutableMatrix *M,
						   const M2_arrayint cols)
{
  return M->submatrix(cols);
}

/*******************************
 ** Cmputations ****************
 *******************************/

M2_arrayint_OrNull IM2_FF_LU(MutableMatrix *M)
{
  return FF_LUComputation::DO(M);
}

M2_bool IM2_LLL(MutableMatrix *M, const M2_Rational threshold)
{
  return LLLoperations::LLL(M,threshold);
}

M2_bool IM2_SmithNormalForm(MutableMatrix *M)
{
#warning "implement smith"
  ERROR("not implemented yet");
  return 0;
}

M2_bool IM2_HermiteNormalForm(MutableMatrix *M)
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

M2_bool rawSolve(MutableMatrix *A,
		 MutableMatrix *b,
		 MutableMatrix *x)
{
  /* Check: A, b, x all have the same ring, either RR or CC */
  /* Check: if all of these are dense mutable matrices, then 
     call the correct routine */
  /* Otherwise: give error: 
     OR: make mutable matrices of the correct size, call the correct routine
     and afterwords, copy to x. */

  const Ring *R = A->get_ring();
  if (b->get_ring() != R || x->get_ring() != R)
    {
      ERROR("expected same ring");
      return false;
    }
  if (R->cast_to_RR())
    {
      DenseMutableMatrixRR *A1 = A->cast_to_DenseMutableMatrixRR();
      DenseMutableMatrixRR *b1 = b->cast_to_DenseMutableMatrixRR();
      DenseMutableMatrixRR *x1 = x->cast_to_DenseMutableMatrixRR();
      if (!A1 || !b1 || !x1)
	{
	  ERROR("expected dense matrices");
	  return false;
	}
      DenseMutableMatrixRR *result_x = A1->solve(b1,x1);
      return result_x != 0;
    }
  if (R->cast_to_CC())
    {
      return true;
    }
  ERROR("expected base ring to be RR or CC");
  return false;
}

M2_bool rawLU(MutableMatrix *A,
	      MutableMatrix *L,
	      MutableMatrix *U,
	      MutableMatrix *P)
{
  ERROR("not re-implemented yet");
  return false;
}

M2_bool rawEigenvalues(MutableMatrix *A,
		       MutableMatrix *eigenvalues,
		       M2_bool is_symm_or_hermitian)
{
  ERROR("not re-implemented yet");
  return false;
}

M2_bool rawEigenvectors(MutableMatrix *A,
			MutableMatrix *eigenvalues,
			MutableMatrix *eigenvectors,
			M2_bool is_symm_or_hermitian)
{
  ERROR("not re-implemented yet");
  return false;
}

M2_bool rawSVD(MutableMatrix *A,
	       MutableMatrix *Sigma,
	       MutableMatrix *U,
	       MutableMatrix *VT,
	       M2_bool use_divide_and_conquer)
{
  ERROR("not re-implemented yet");
  return false;
}

M2_bool rawLeastSquares(MutableMatrix *A, 
			MutableMatrix *b, 
			MutableMatrix *x, /* return value: argument modified */
			M2_bool assume_full_rank)
/* Case 1: A is a dense matrix over RR.  Then so are b,x.
   Case 2: A is a dense matrix over CC.  Then so are b,x. */
{
  ERROR("not re-implemented yet");
  return false;
}

#if 0
M2_bool 
rawMutableMatrixEigenvalues(MutableMatrix *M, MutableMatrix *result_eigenvalues)
// M must be a dense mutable matrix over RR or over CC
// result_eigenvalues must be a dense mutable matrix over CC
{
  DenseMutableMatrixCC *eig = result_eigenvalues->cast_to_DenseMutableMatrixCC();
  if (eig == 0)
    {
      ERROR("expected dense MutableMatrix over RR");
      return false;
    }
  DenseMutableMatrixRR *MR = M->cast_to_DenseMutableMatrixRR();
  if (MR)
    {
      DenseMutableMatrixCC *result = MR->eigenvalues(eig);
      return (result != 0);
    }
  DenseMutableMatrixCC *MC = M->cast_to_DenseMutableMatrixCC();
  if (MC)
    {
      DenseMutableMatrixCC *result = MC->eigenvalues(eig);
      return (result != 0);
    }
  return 0;
}
#endif

#if 0
const MutableMatrixOrNull *

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
