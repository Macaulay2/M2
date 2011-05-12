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

#include "matrix.hpp"

MutableMatrix * IM2_MutableMatrix_identity(const Ring *R,
						 int n,
						 M2_bool is_dense)
{
  return MutableMatrix::identity(R, n, is_dense);
}

MutableMatrix /* or null */ * IM2_MutableMatrix_make(const Ring *R,
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

const RingElement /* or null */ * IM2_MutableMatrix_get_entry(const MutableMatrix *M, int r, int c)
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
				     engine_RawRingElementArray values)
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

MutableMatrix /* or null */ * IM2_MutableMatrix_submatrix(const MutableMatrix *M,
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

MutableMatrix /* or null */ * IM2_MutableMatrix_submatrix1(const MutableMatrix *M,
						   M2_arrayint cols)
{
  return M->submatrix(cols);
}

/*******************************
 ** Cmputations ****************
 *******************************/

M2_arrayintOrNull IM2_FF_LU(MutableMatrix *M)
{
  return FF_LUComputation::DO(M);
}

M2_bool rawLLL(MutableMatrix *M, 
		MutableMatrix /* or null */ *U,
		gmp_QQ threshold, 
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

M2_arrayintOrNull rawLU(const MutableMatrix *A,
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

////////////////////////////////////////
// Support for RRR and CCC operations //
////////////////////////////////////////

const Matrix /* or null */ *rawMatrixClean(gmp_RR epsilon, const Matrix *M)
{
  if (M->get_ring()->get_precision() == 0)
    {
      ERROR("expected ring over an RR or CC");
      return 0;
    }
  return M->clean(epsilon);
}
const RingElement /* or null */ *rawRingElementClean(gmp_RR epsilon, const RingElement *f)
{
  const Ring *R = f->get_ring();
  if (R->get_precision() == 0)
    {
      ERROR("expected ring over an RR or CC");
      return 0;
    }
  return RingElement::make_raw(R, R->zeroize_tiny(epsilon,f->get_value()));
}
MutableMatrix /* or null */ *rawMutableMatrixClean(gmp_RR epsilon, MutableMatrix *M)
{
/* modifies M in place */
  if (M->get_ring()->get_precision() == 0)
    {
      ERROR("expected ring over an RR or CC");
      return 0;
    }
  //return M->clean(epsilon);
  return NULL;
} 

static gmp_RRorNull get_norm_start(gmp_RR p, const Ring *R)
{
  if (R->get_precision() == 0)
    {
      ERROR("expected ring over an RR or CC");
      return 0;
    }
  gmp_RR norm = getmemstructtype(gmp_RR);
  mpfr_init2(norm, mpfr_get_prec(p));
  mpfr_ui_div(norm,1,p,GMP_RNDN);
  if (!mpfr_zero_p(norm))
    {
      ERROR("Lp norm only implemented for p = infinity");
      mpfr_clear(norm);
      return 0;
    }
  return norm;
}

gmp_RRorNull rawMatrixNorm(gmp_RR p, const Matrix *M)
{
  return M->norm(p);
}

gmp_RRorNull rawRingElementNorm(gmp_RR p, const RingElement *f)
{
  gmp_RR norm = get_norm_start(p, f->get_ring());
  if (!norm) return 0; // error already given.
  f->get_ring()->increase_maxnorm(norm, f->get_value());
  return norm;
}

gmp_RRorNull rawMutableMatrixNorm(gmp_RR p, const MutableMatrix *M)
{
#if 0
  gmp_RR nm = get_norm_start(p, M->get_ring());
  iterator *i = M->begin();
  for (int c=0; c<n_cols(); c++)
    {
      for (i->set(c); i->valid(); i->next())
	{
	  ring_elem a;
	  i->copy_ring_elem(a);
	  R->increase_maxnorm(nm, a);
	}
    }
  delete i;
  return nm;
#else
  return NULL;
#endif
}

#if defined(HAVE_FFLAS_FFPACK)
#include "fflas-ffpack/modular-positive.h"
#include "fflas-ffpack/modular-balanced.h"
#include "fflas-ffpack/ffpack.h"

template < typename FieldType >
typename FieldType::Element *toFFPackMatrix(const Z_mod *kk, const FieldType &F, MutableMatrix *M)
{
  typedef typename FieldType::Element ElementType;
  
  ElementType * N = newarray(ElementType, M->n_rows() * M->n_cols());
  ElementType *inN = N;
  for (size_t i = 0; i<M->n_rows(); i++)
    for (size_t j = 0; j<M->n_cols(); j++)
      {
	ring_elem a;
	M->get_entry(i,j,a);
	int b = kk->to_int(a);
	double d = b;
	F.init(*inN++, d);
      }
  return N;
}

template < typename FieldType >
MutableMatrix *fromFFPackMatrix(const Z_mod *kk, 
				const FieldType &F,
				typename FieldType::Element *N, 
				size_t nrows, 
				size_t ncols)
{
  typedef typename FieldType::Element ElementType;
  
  MutableMatrix * M = MutableMatrix::zero_matrix(kk, nrows, ncols, true);
  ElementType *inN = N;
  for (size_t i = 0; i<nrows; i++)
    for (size_t j = 0; j<ncols; j++)
      {
	unsigned long a;
	F.convert(a, *inN);
	inN++;
	ring_elem b = kk->from_int(a);
	M->set_entry(i,j,b);
      }
  return M;
}

RingElement *rawFFPackDeterminant(MutableMatrix *M)
// M should be a mutable matrix over a finite prime field, 
// of square size.
{
  // declare the field DONE
  // copy the matrix to an ffpack matrix
  // call det
  // translate the answer to a RingElement
  // free the ffpack matrix

  const Ring *R = M->get_ring();
  const Z_mod *kk = R->cast_to_Z_mod();
  if (kk == 0)
    {
      ERROR("expected finite prime field");
      return 0;
    }
  typedef ModularBalanced<double> FieldType;
  typedef FieldType::Element ElementType;
  FieldType F(R->charac());

  ElementType *N = toFFPackMatrix(kk, F, M);

  size_t n = M->n_rows();
  ElementType result = FFPACK::Det(F, n, n, N, n);
  unsigned long res;
  F.convert(res,result);
  deletearray(N);
  return RingElement::make_raw(kk, kk->from_int(res));
}

size_t rawFFPackRank(MutableMatrix *M)
{
  const Ring *R = M->get_ring();
  const Z_mod *kk = R->cast_to_Z_mod();
  if (kk == 0)
    {
      ERROR("expected finite prime field");
      return 0;
    }
  typedef ModularBalanced<double> FieldType;
  typedef FieldType::Element ElementType;
  FieldType F(R->charac());

  ElementType *N = toFFPackMatrix(kk, F, M);

  size_t nr = M->n_rows();
  size_t nc = M->n_cols();
  size_t result = FFPACK::Rank(F, nr, nc, N, nc);
  deletearray(N);
  return result;
}

MutableMatrix /* or null */ * rawFFPackNullSpace(MutableMatrix *M, M2_bool right_side)
{
  const Ring *R = M->get_ring();
  const Z_mod *kk = R->cast_to_Z_mod();
  if (kk == 0)
    {
      ERROR("expected finite prime field");
      return 0;
    }
  typedef ModularBalanced<double> FieldType;
  typedef FieldType::Element ElementType;
  FieldType F(R->charac());

  ElementType *N = toFFPackMatrix(kk, F, M);

  size_t nr = M->n_rows();
  size_t nc = M->n_cols();

  ElementType *nullspace = 0;
  size_t nullspace_dim;
  size_t nullspace_leading_dim;
  FFPACK::NullSpaceBasis(F,
			 (right_side ? FFLAS::FflasRight : FFLAS::FflasLeft),
			 nr, nc, N, nc, nullspace, nullspace_leading_dim, nullspace_dim);
  cerr << "leading dim = " << nullspace_leading_dim << " and dim = " << nullspace_dim << endl;
  size_t nullspace_nrows = (right_side ? nc : nullspace_dim);
  if (right_side && nullspace_dim != nullspace_leading_dim)
    {
      cerr << "error: this should not happen!" << endl;
    }
  else if (!right_side && nullspace_leading_dim != nr)
    {
      cerr << "error: this should not happen either!" << endl;
    }

  MutableMatrix *result_nullspace = fromFFPackMatrix(kk, F, nullspace, nullspace_nrows, nullspace_leading_dim);
  delete [] nullspace;
  return result_nullspace;
}

MutableMatrix /* or null */ * rawFFPackSolve(MutableMatrix *A, MutableMatrix *B, M2_bool right_side)
{
  const Ring *R = A->get_ring();
  const Z_mod *kk = R->cast_to_Z_mod();
  if (kk == 0)
    {
      ERROR("expected finite prime field");
      return 0;
    }
  typedef ModularBalanced<double> FieldType;
  typedef FieldType::Element ElementType;
  FieldType F(R->charac());

  size_t a_rows = A->n_rows();
  size_t a_cols = A->n_cols();

  size_t b_rows = B->n_rows();
  size_t b_cols = B->n_cols();

  ElementType *ffpackA = toFFPackMatrix(kk, F, A);
  ElementType *ffpackB = toFFPackMatrix(kk, F, B);

  // preallocate the space for the solutions:
  size_t x_rows = (right_side ? a_cols : b_rows);
  size_t x_cols = (right_side ? b_cols : a_rows);
  size_t n_eqns = (right_side ? b_cols : b_rows);

  ElementType *ffpackX = newarray(ElementType, x_rows * x_cols);

  int info; // >0 if the system is inconsistent, ==0 means success

  if (n_eqns == 1 && right_side)
    {
      FFPACK::Solve(F, 
		    a_rows, 
		    ffpackA, 
		    a_cols, 
		    ffpackX,
		    1,
		    ffpackB,
		    1);
    }
  else 
    {
      FFPACK::fgesv(F, 
		(right_side ? FFLAS::FflasRight : FFLAS::FflasLeft),
		a_rows, a_cols, 
		(right_side ? x_cols : x_rows), 
		ffpackA, 
		a_cols, // leading dim of A
		ffpackX, x_cols, 
		ffpackB, b_cols,
		&info);

      if (info > 0)
	{
	  // the system is inconsistent
	  ERROR("the system is inconsistent");
	  return 0;
	}
    }

  MutableMatrix *X = fromFFPackMatrix(kk, F, ffpackX, x_rows, x_cols);
  delete [] ffpackX;
  return X;
}
#else
RingElement *rawFFPackDeterminant(MutableMatrix *M)
{
  ERROR("FFPack not present");
  return 0;
}
RingElement *rawFFPackRank(MutableMatrix *M)
{
  ERROR("FFPack not present");
  return 0;
}
MutableMatrix /* or null */ * rawFFPackNullSpace(MutableMatrix *M, M2_bool right_side)
{
  ERROR("FFPack not present");
  return 0;
}
MutableMatrix /* or null */ * rawFFPackSolve(MutableMatrix *A, MutableMatrix *B, M2_bool right_side)
{
  ERROR("FFPack not present");
  return 0;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
