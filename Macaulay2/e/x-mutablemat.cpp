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
#include "exceptions.hpp"

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

M2_string IM2_MutableMatrix_to_string(const MutableMatrix *M)
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

void rawMutableMatrixFillRandom(MutableMatrix *M, long nelems)
{
  int nrows = M->n_rows();
  int ncols = M->n_cols();
  const Ring *R = M->get_ring();
  for (long i=0; i<nelems; i++)
    {
      int r = rawRandomInt(nrows);
      int c = rawRandomInt(ncols);
      ring_elem a = R->random();
      if (!R->is_zero(a))
	M->set_entry(r,c,R->random());
    }
}

void rawMutableMatrixFillRandomDensity(MutableMatrix *M, double density, int special_type)
/* special_type: 0 is general, 1 is (strictly) upper triangular. */
{
  bool doing_fraction = false;
  int threshold = 0;

  int nrows = M->n_rows();
  int ncols = M->n_cols();
  const Ring *R = M->get_ring();

  if (density != 1.0)
    {
      doing_fraction = true;
      threshold = static_cast<int>(density * 10000);
    }

  if (special_type == 0)
    {
      for (int i=0; i<ncols; i++)
	for (int j=0; j<nrows; j++)
	  {
	    if (doing_fraction)
	      {
		int32_t u = rawRandomInt((int32_t)10000);
		if (u > threshold) continue;
	      }
	    ring_elem a = R->random();
	    if (!R->is_zero(a))
	      M->set_entry(j,i,a);
	  }
    }
  else if (special_type == 1)
    {
      for (int i=0; i<ncols; i++)
	{
	  int top = (i>=nrows ? nrows : i);
	  for (int j=0; j<top; j++)
	    {
	      if (doing_fraction)
		{
		  int32_t u = rawRandomInt((int32_t)10000);
		  if (u > threshold) continue;
		}
	      ring_elem a = R->random();
	      if (!R->is_zero(a))
		M->set_entry(j,i,a);
	    }
	}
    }
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

  M->row_op(i,r->get_value(),j);
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

  M->column_op(i,r->get_value(),j);
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
  M->scale_row(i,r->get_value());
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
  M->scale_column(i,r->get_value());
  return 1;
}

M2_bool IM2_MutableMatrix_insert_columns(MutableMatrix *M, int i, int n_to_add)
  /* Insert n_to_add columns directly BEFORE column i. */
{
  return M->insert_columns(i, n_to_add);
}

M2_bool IM2_MutableMatrix_insert_rows(MutableMatrix *M, int i, int n_to_add)
  /* Insert n_to_add rows directly BEFORE row i. */
{
  return M->insert_rows(i, n_to_add);
}

M2_bool IM2_MutableMatrix_delete_columns(MutableMatrix *M, int i, int j)
  /* Delete columns i .. j from M */
{
  return M->delete_columns(i, j);
}


M2_bool IM2_MutableMatrix_delete_rows(MutableMatrix *M, int i, int j)
  /* Delete rows i .. j from M */
{
  return M->delete_rows(i, j);
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
		       b1->get_value(),b2->get_value());
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
		    b1->get_value(),b2->get_value());
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

M2_bool IM2_MutableMatrix_column_permute(MutableMatrix *M,
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


const RingElement * IM2_Matrix_dot_product(const MutableMatrix *M, int c1, int c2)
{
  ring_elem a;
  M->dot_product(c1,c2,a);
  return RingElement::make_raw(M->get_ring(), a);
}

M2_bool IM2_MutableMatrix_is_zero(const MutableMatrix *M)
{
  return M->is_zero();
}

M2_bool IM2_MutableMatrix_is_equal(const MutableMatrix *M, 
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


void perform_reduction(MutableMatrix *M, 
		       int r, int c, 
		       int nr, int nc,
		       int pivot_type)
  // Subroutine of reduce_pivots()
  // pivot_type: 1 means pivot is 1, -1 means pivot is -1, 0 means pivot is unit
{
  // Flip rows r, nr
  // Flip cols c, nc
  // Use (nr,nc) location to remove all terms in columns 0..nc-1
  //   and in row nr.
  // Replace column nc with all zeros, except 1 in nr row.
  const Ring *R = M->get_ring();
  M->interchange_columns(c,nc);
  M->interchange_rows(r,nr);
  ring_elem pivot;
  long pivotrow = M->lead_row(nc, pivot);
  if (pivot_type == -1) // pivot is -1
    M->scale_column(nc,pivot);    
  else if (pivot_type == 0)
    M->divide_column(nc, pivot);
  for (int i=0; i<nc; i++)
    {
      ring_elem coef;
      pivotrow = M->lead_row(i,coef);
      if (pivotrow < 0) continue;
      if (pivotrow == nr)
	{
	  // Do the reduction
	  ring_elem f = R->negate(coef);
	  M->column_op(i, f, nc);
	}
    }
  M->scale_column(nc, R->zero());
  M->set_entry(nr,nc,R->one());
}

void reduce_pivots(MutableMatrix *M)
{
  int nr = M->n_rows()-1;
  int nc = M->n_cols()-1;
  if (nr < 0 || nc < 0) return;
  const Ring *K = M->get_ring();
  ring_elem one = K->one();
  ring_elem minus_one = K->minus_one();

  // After using the pivot element, it is moved to [nrows-1,ncols-1]
  // and nrows and ncols are decremented.

  MutableMatrix::iterator *p = M->begin();
  for (int i=0; i<=nc; i++)
    {
      p->set(i);
      for (; p->valid(); p->next())
	{
	  int pivot_type = 0;
	  ring_elem coef;
	  p->copy_ring_elem(coef);
	  if (K->is_equal(one, coef))
	    pivot_type = 1;
	  else if (K->is_equal(minus_one, coef))
	    pivot_type = -1;
	  if (pivot_type != 0)
	    {
	      perform_reduction(M, p->row(), i, nr--, nc--, pivot_type);
	      if (nr < 0 || nc < 0) return;
	      // restart loop with the (new) column i
	      i = -1;
	      break;
	    }
	}
    }

  // Now search for other possible pivots
  for (int i=0; i<=nc; i++)
    {
      p->set(i);
      for (; p->valid(); p->next())
	{
	  ring_elem coef;
	  p->copy_ring_elem(coef);
	  if (!K->is_unit(coef)) continue;
	  int pivot_type = 0;
	  if (K->is_equal(one, coef))
	    pivot_type = 1;
	  else if (K->is_equal(minus_one, coef))
	    pivot_type = -1;

	  perform_reduction(M, p->row(), i, nr--, nc--, pivot_type);
	  if (nr < 0 || nc < 0) return;
	  // restart loop with the (new) column i
	  i = -1;
	  break;
	}
    }
}

////////////////////////////
//
//void SparseMutableMatrix::perform_reduction(int r, int c, int nr, int nc,
//					    int pivot_type)
//  // Subroutine of reduce_pivots()
//  // pivot_type: 1 means pivot is 1, -1 means pivot is -1, 0 means pivot is unit
//{
//  // Flip rows r, nr
//  // Flip cols c, nc
//  // Use (nr,nc) location to remove all terms in columns 0..nc-1
//  //   and in row nr.
//  // Replace column nc with all zeros, except 1 in nr row.
//  interchange_columns(c,nc);
//  interchange_rows(r,nr);
//  ring_elem pivot = columns_[nc]->coeff;
//  if (pivot_type == -1) // pivot is -1
//    scale_column(pivot,nc,false);    
//  else if (pivot_type == 0)
//    divide_column(nc, pivot);
//  for (int i=0; i<nc; i++)
//    {
//      vec p = columns_[i];
//      if (p == 0) continue;
//      if (p->comp == nr)
//	{
//	  // Do the reduction
//	  ring_elem f = R->negate(p->coeff);
//	  column_op(i, f, nc, false);
//	}
//    }
//  R->remove_vec(columns_[nc]);
//  columns_[nc] = R->e_sub_i(nr);
//}
//
//void SparseMutableMatrix::reduce_pivots()
//{
//  int nr = n_rows()-1;
//  int nc = n_cols()-1;
//  if (nr < 0 || nc < 0) return;
//  const Ring *K = get_ring();
//  ring_elem one = K->one();
//  ring_elem minus_one = K->minus_one();
//
//  // After using the pivot element, it is moved to [nrows-1,ncols-1]
//  // and nrows and ncols are decremented.
//
//  for (int i=0; i<=nc; i++)
//    {
//      for (vec p = columns_[i]; p != 0; p = p->next) 
//	{
//	  int pivot_type = 0;
//	  if (K->is_equal(one, p->coeff))
//	    pivot_type = 1;
//	  else if (K->is_equal(minus_one, p->coeff))
//	    pivot_type = -1;
//	  if (pivot_type != 0)
//	    {
//	      perform_reduction(p->comp, i, nr--, nc--, pivot_type);
//	      if (nr < 0 || nc < 0) return;
//	      // restart loop with the (new) column i
//	      i = -1;
//	      break;
//	    }
//	}
//    }
//
//  // Now search for other possible pivots
//  for (int i=0; i<=nc; i++)
//    {
//      for (vec p = columns_[i]; p != 0; p = p->next) 
//	{
//	  if (!K->is_unit(p->coeff)) continue;
//	  int pivot_type = 0;
//	  if (K->is_equal(one, p->coeff))
//	    pivot_type = 1;
//	  else if (K->is_equal(minus_one, p->coeff))
//	    pivot_type = -1;
//
//	  perform_reduction(p->comp, i, nr--, nc--, pivot_type);
//	  if (nr < 0 || nc < 0) return;
//	  // restart loop with the (new) column i
//	  i = -1;
//	  break;
//	}
//    }
//}




M2_bool IM2_MutableMatrix_reduce_by_pivots(MutableMatrix *M)
/* Using row and column operations, use unit pivots to reduce the matrix */
{
  reduce_pivots(M);
  return true;
}

MutableMatrixOrNull * IM2_MutableMatrix_add(const MutableMatrix *M, const MutableMatrix *N)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the addition is performed.  If the targets are not equal, the target 
   of the result is set to have each degree zero.  Similarly with the
   source, and also with the degree of the matrix. */
{
  ERROR("not implemented for mutable matrices");
  return 0;
  return M->add(N);
}

MutableMatrixOrNull * IM2_MutableMatrix_subtract(const MutableMatrix *M, const MutableMatrix *N)
/* If the sizes do not match, then NULL is returned.  If they do match,
   the addition is performed.  If the targets are not equal, the target 
   of the result is set to have each degree zero.  Similarly with the
   source, and also with the degree of the matrix. */
{
  ERROR("not implemented for mutable matrices");
  return 0;
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
  ERROR("not implemented for mutable matrices");
  return 0;
  return M->mult(N);
}

MutableMatrixOrNull * IM2_MutableMatrix_scalar_mult(const RingElement *f,
						    const MutableMatrix *M, 
						    M2_bool opposite_mult)
{
  return M->mult(f);
}

MutableMatrixOrNull * IM2_MutableMatrix_submatrix(const MutableMatrix *M,
						  M2_arrayint rows,
						  M2_arrayint cols)
{
  return M->submatrix(rows,cols);
}

bool IM2_MutableMatrix_set_submatrix(MutableMatrix *M,
				     M2_arrayint rows,
				     M2_arrayint cols,
				     const MutableMatrix *N)
{
  return M->set_submatrix(rows,cols,N);
}

MutableMatrixOrNull * IM2_MutableMatrix_submatrix1(const MutableMatrix *M,
						   M2_arrayint cols)
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

M2_bool rawLLL(MutableMatrix *M, 
		MutableMatrixOrNull *U,
		M2_Rational threshold, 
		int strategy)
{
  if (strategy == 0)
    {
      return LLLoperations::LLL(M,U,threshold);
    }

  long a = mpz_get_si(mpq_numref(threshold));
  long b = mpz_get_si(mpq_denref(threshold));
  return ntl_LLL(M,U,a,b,strategy);
}

M2_bool IM2_SmithNormalForm(MutableMatrix *M)
{
#ifdef DEVELOPMENT
#warning "implement smith"
#endif
  ERROR("not implemented yet");
  return 0;
}

M2_bool IM2_HermiteNormalForm(MutableMatrix *M)
{
#ifdef DEVELOPMENT
#warning "implement hermite"
#endif
  ERROR("not implemented yet");
  return 0;
}

  /***************************************************
   ***** Lapack routines for dense mutable matrices **
   ***************************************************/

  /* Each of the following routines accepts honest MutableMatrix arguments,
     and returns false if there is an error.  The return values are placed into
     some of the (already existing) parameters of the routine */
//typedef DMat<CoefficientRingRRR> LMatrixRR;
//typedef DMat<CoefficientRingCCC> LMatrixCC;

M2_arrayint_OrNull rawLU(const MutableMatrix *A,
			 MutableMatrix *L,
			 MutableMatrix *U)
{
  return A->LU(L,U);
}

M2_bool rawNullspaceU(MutableMatrix *U,
		      MutableMatrix *x)
  /* U should be a matrix in LU 'U' format.
     x is set to the matrix whose columns form a basis of Ux=0,
     which is the same as Ax=0. if A = PLU is the LU-decomp of A.
  */
{
  const Ring *R = U->get_ring();
  if (R != x->get_ring())
    {
      ERROR("expected matrices with same base ring");
      return false;
    }
  return U->nullspaceU(x);
}

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
  if (R != b->get_ring() || R != x->get_ring())
    {
      ERROR("expected matrices with same base ring");
      return false;
    }
  if (A->n_rows() != b->n_rows())
    {
      ERROR("expected matrices with the same number of rows");
      return false;
    }
  return A->solve(b,x);
}

M2_bool rawEigenvalues(MutableMatrix *A,
		       MutableMatrix *eigenvalues,
		       M2_bool is_symm_or_hermitian)
{
  return A->eigenvalues(eigenvalues,is_symm_or_hermitian);
}

M2_bool rawEigenvectors(MutableMatrix *A,
			MutableMatrix *eigenvalues,
			MutableMatrix *eigenvectors,
			M2_bool is_symm_or_hermitian)
{
  return A->eigenvectors(eigenvalues, eigenvectors, is_symm_or_hermitian);
}

M2_bool rawSVD(MutableMatrix *A,
	       MutableMatrix *Sigma,
	       MutableMatrix *U,
	       MutableMatrix *VT,
	       M2_bool use_divide_and_conquer)
{
  return A->SVD(Sigma,U,VT,use_divide_and_conquer);
}

M2_bool rawLeastSquares(MutableMatrix *A, 
			MutableMatrix *b, 
			MutableMatrix *x, /* return value: argument modified */
			M2_bool assume_full_rank)
/* Case 1: A is a dense matrix over RR.  Then so are b,x.
   Case 2: A is a dense matrix over CC.  Then so are b,x. */
{
  const Ring *R = A->get_ring();
  if (R != b->get_ring() || R != x->get_ring())
    {
      ERROR("expected matrices with same base ring");
      return false;
    }
  if (A->n_rows() != b->n_rows())
    {
      ERROR("expected matrices with the same number of rows");
      return false;
    }
  return A->least_squares(b,x,assume_full_rank);
}

const MatrixOrNull *rawMatrixClean(M2_RRR epsilon, const Matrix *M)
{
  ERROR("not implemented yet");
  return 0;
}

const RingElementOrNull *rawRingElementClean(M2_RRR epsilon, const RingElement *f)
{
  ERROR("not implemented yet");
  return 0;
}
MutableMatrixOrNull *rawMutableMatrixClean(M2_RRR epsilon, MutableMatrix *M)
{
/* modifies M in place */
  ERROR("not implemented yet");
  return 0;
} 

M2_RRRorNull rawMatrixNorm(M2_RRR p, const Matrix *M)
{
  ERROR("not implemented yet");
  return 0;
}

M2_RRRorNull rawRingElementNorm(M2_RRR p, const RingElement *f)
{
  ERROR("not implemented yet");
  return 0;
}

M2_RRRorNull rawMutableMatrixNorm(M2_RRR p, const MutableMatrix *M)
{
  ERROR("not implemented yet");
  return 0;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
