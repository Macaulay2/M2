// Copyright 1995 Michael E. Stillman

#include "relem.hpp"
#include "matrix.hpp"
#include "matrixcon.hpp"
#include "LLL.hpp"
#include "fractionfreeLU.hpp"
#include "text-io.hpp"
#include "exceptions.hpp"

extern Matrix_int_pair global_Matrix_int_pair;

const FreeModule * IM2_Matrix_get_target(const Matrix *M)
{
  return M->rows();
}

const FreeModule * IM2_Matrix_get_source(const Matrix *M)
{
  return M->cols();
}

int IM2_Matrix_n_rows(const Matrix *M)
{
  return M->n_rows();
}

int IM2_Matrix_n_cols(const Matrix *M)
{
  return M->n_cols();
}

M2_arrayint IM2_Matrix_get_degree(const Matrix *M)
{
  return M->degree_monoid()->to_arrayint(M->degree_shift());
}

M2_string IM2_Matrix_to_string(const Matrix *M)
{
     buffer o;
     try {
	  M->text_out(o);
	  return o.to_string();
     }
     catch (exc::engine_error e) {
	  o << "[unprintable matrix]";
	  return o.to_string();
     }
}

unsigned long IM2_Matrix_hash(const Matrix *M)
{
  return M->get_hash_value();
}

const RingElementOrNull * IM2_Matrix_get_entry(const Matrix *M, int r, int c)
{
     try {
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
	  result = M->elem(r,c);
	  return RingElement::make_raw(M->get_ring(), result);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const Matrix * IM2_Matrix_identity(const FreeModule *F,
				   int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::identity(F);
}

const MatrixOrNull * IM2_Matrix_zero(const FreeModule *F,
				     const FreeModule *G,
				     int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::zero(F,G);
}


const MatrixOrNull * IM2_Matrix_make1(const FreeModule *target,
				      int ncols,
				      const RingElement_array *M,
				      int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make(target, ncols, M);
}

const MatrixOrNull * IM2_Matrix_make2(const FreeModule *target,
				      const FreeModule *source,
				      M2_arrayint deg,
				      const RingElement_array *M,
				      int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make(target, source, deg, M);
}

const MatrixOrNull * IM2_Matrix_make_sparse1(const FreeModule *target,
					     int ncols,
					     M2_arrayint rows,
					     M2_arrayint cols,
					     const RingElement_array *entries,
					     int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make_sparse(target, ncols, rows, cols, entries);
}
  
const MatrixOrNull * IM2_Matrix_make_sparse2(const FreeModule *target,
					     const FreeModule *source,
					     M2_arrayint deg,
					     M2_arrayint rows,
					     M2_arrayint cols,
					     const RingElement_array *entries,
					     int preference)
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return Matrix::make_sparse(target, source, deg, rows, cols, entries);
}

M2_bool IM2_Matrix_is_implemented_as_dense(const Matrix *M)
  /* Is the matrix M implemented as dense? */
{
#ifdef DEVELOPMENT
#warning not implemented yet
#endif
  return 0;
}

const MatrixOrNull * IM2_Matrix_remake2(const FreeModule *target,
					const FreeModule *source,
					M2_arrayint deg,
					const Matrix *M,
					int preference)
  /* Create a new matrix (mutable or immutable), from M, with new target,
     source, deg and/or mutable-ness. The new free modules must have 
     the expected rank. 
  */
{
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
  return M->remake(target, source, deg);
}

const MatrixOrNull * IM2_Matrix_remake1(const FreeModule *target,
					const Matrix *M,
					int preference)
  /* Create a new matrix, from M, with new target,
     The target free module must have the expected rank.
     The source free module is computed heuristically from the the target and the
     columns of the matrix.
  */
{
     try {
#ifdef DEVELOPMENT
#warning prefer_dense not yet used
#endif
	  return M->remake(target);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

MatrixOrNull *IM2_Matrix_random(const Ring *R, 
				int r, int c, 
				double fraction_non_zero, 
				int special_type, // 0: general, 1:upper triangular, others?
				int preference)
{
#ifdef DEVELOPMENT
#warning preference not yet used
#endif
  return Matrix::random(R,r,c,fraction_non_zero,special_type);
}

/////////////////////////////////////////////////////////////////////
M2_bool IM2_Matrix_is_zero(const Matrix *M)
{
  return M->is_zero();
}

int				// 1 = true, 0 = false, -1 = error
IM2_Matrix_is_equal(const Matrix *M, 
				  const Matrix *N)
{
     try {
	  /* This checks that the entries of M,N are the same, as well as
	     that the source and target are the same (as graded free modules).
	     Therefore, it can happen that M-N == 0, but M != N.
	  */
	  return M->is_equal(*N);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return -1;
     }
}

M2_bool IM2_Matrix_is_graded(const Matrix *M)
{
  return M->is_homogeneous();
}

const MatrixOrNull * IM2_Matrix_add(const Matrix *M, const Matrix *N)
  /* If the sizes do not match, then NULL is returned.  If they do match,
     the addition is performed, and the source,target,degree are taken from the
     first matrix. */
{
     try {
	  return *M + *N;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_subtract(const Matrix *M, const Matrix *N)
{
     try {
	  return *M - *N;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const Matrix * IM2_Matrix_negate(const Matrix *M)
{
     try {
	  return - *M;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_mult(const Matrix *M, 
				     const Matrix *N, 
				     M2_bool opposite_mult)
{
     try {
	  return M->mult(N, opposite_mult);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_scalar_mult(const RingElement *f,
					    const Matrix *M,
					    M2_bool opposite_mult)
{
     try {
	  if (f->get_ring() != M->get_ring())
	    {
	      ERROR("ring element and matrix should have the same base ring");
	      return 0;
	    }
	  return M->scalar_mult(f->get_value(), opposite_mult);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_concat(const Matrix_array *Ms)
{
     try {
	  unsigned int n = Ms->len;
	  if (n == 0)
	    {
	      ERROR("matrix concat: expects at least one matrix");
	      return 0;
	    }
	  const FreeModule *F = Ms->array[0]->rows();
	  const Ring *R = F->get_ring();
	  MatrixConstructor mat(Ms->array[0]->rows(), 0);
	  int next=0;
	  for (unsigned int i=0; i<n; i++)
	    {
	      const Matrix *M = Ms->array[i];
	      if (F->get_ring() != M->get_ring())
		{
		  ERROR("matrix concat: different base rings");
		  return 0;
		}
	      if (F->rank() != M->n_rows())
		{
		  ERROR("matrix concat: row sizes are not equal");
		  return 0;
		}
	      for (int j=0; j<M->n_cols(); j++)
		{
		  mat.append(R->copy_vec(M->elem(j)));
		  mat.set_column_degree(next++, M->cols()->degree(j));
		}
	    }
	  return mat.to_matrix();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_direct_sum(const Matrix_array *Ms)
{
     try {
	  // Check that the matrices all have the same ring, and that there is
	  // at least one matrix.
	  unsigned int n = Ms->len;
	  if (n == 0)
	    {
	      ERROR("matrix direct sum: expects at least one matrix");
	      return 0;
	    }
	  const Matrix *result = Ms->array[0];
	  const Ring *R = result->get_ring();
	  for (unsigned int i=1; i<n; i++)
	    if (R != Ms->array[i]->get_ring())
	      {
		ERROR("matrix direct sum: different base rings");
		return 0;
	      }
	  for (unsigned int i=1; i<n; i++)
	    result = result->direct_sum(Ms->array[i]);

	  return result;
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_tensor(const Matrix *M,
				       const Matrix *N)
{
     try {
	  return M->tensor(N);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}


const MatrixOrNull * rawModuleTensor(const Matrix *M,
				     const Matrix *N)
{
     try {
	  return M->module_tensor(N);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_transpose(const Matrix *M)
{
     try {
	  return M->transpose();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_reshape(const Matrix *M,
					const FreeModule *F,
					const FreeModule *G)
{
     try {
	  return M->reshape(F,G);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_flip(const FreeModule *F,
				     const FreeModule *G)
{
     try {
	  return Matrix::flip(F,G);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * rawWedgeProduct(int p,
				     int q,
				     const FreeModule *F)
  /* Constructs the map
     exterior(p,F) ** exterior(q,F) --> exterior(p+q,F)
  */
{
     try {
	  return Matrix::wedge_product(p,q,F);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_submatrix(const Matrix *M,
					  M2_arrayint rows,
					  M2_arrayint cols)
{
     try {
	  return M->sub_matrix(rows,cols);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_submatrix1(const Matrix *M,
					   M2_arrayint cols)
{
     try {
	  return M->sub_matrix(cols);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_koszul(int p, const Matrix *M)
{
     try {
	  return M->koszul(p);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * 
rawKoszulMonomials(int nskew,
		     const Matrix *M,
		     const Matrix *N)
{
     try {
#ifdef DEVELOPMENT
#warning "check with 0.9.2 about what this should even do"
#endif
	  if (M->get_ring() != N->get_ring())
	    {
	      ERROR("expected same ring");
	      return 0;
	    }
	  return Matrix::koszul_monomials(nskew,M,N);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull * IM2_Matrix_symm(int p, const Matrix *M)
{
     try {
	  return M->symm(p);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const Matrix * IM2_Matrix_exterior(int p, const Matrix *M, int strategy)
{
  return M->exterior(p,strategy); 
}

M2_arrayint_OrNull IM2_Matrix_sort_columns(const Matrix *M, 
						 int deg_order, 
						 int mon_order)
{
     try {
	  return M->sort(deg_order, mon_order);
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}


const Matrix * IM2_Matrix_minors(int p, const Matrix *M, int strategy)
{
  return M->minors(p,strategy);
}

const Matrix * rawMinors(int p, 
			 const Matrix *M, 
			 int strategy,
			 int n_minors_to_compute, /* -1 means all */
			 M2_arrayint_OrNull first_row_set,
			 M2_arrayint_OrNull first_col_set
			 ) /* Dan: please connect rawMinors to this version */
/* If first_row_set or first_col_set is not NULL, they should both be non-NULL,
   and both have length p.  If not, NULL is returned.
   Compute n_minors_to_compute minors, starting at (first_row_set,first_col_set) if given,
   otherwise starting at the first (0..p-1,0..p-1).
*/
{
  return M->minors(p,strategy,n_minors_to_compute,first_row_set,first_col_set);
}

const MatrixOrNull * IM2_Matrix_pfaffians(int p, const Matrix *M)
{
  return M->pfaffians(p);
}

const MatrixOrNull * IM2_Matrix_diff(const Matrix *M,
				     const Matrix *N)
{
  return M->diff(N,1);
}

const MatrixOrNull * IM2_Matrix_contract(const Matrix *M,
					 const Matrix *N)
{
  return M->diff(N,0);
}

const MatrixOrNull * IM2_Matrix_homogenize(const Matrix *M,
					   int var,
					   M2_arrayint wts)
{
  return M->homogenize(var, wts);
}

const Matrix_pair_OrNull * IM2_Matrix_coeffs(const Matrix *M, M2_arrayint vars)
{
#ifdef DEVELOPMENT
#warning "implement IM2_Matrix_coeffs"
#endif
  return 0;
}

const MatrixOrNull * rawCoefficients(M2_arrayint vars,
				     const Matrix *monoms,
				     const Matrix *M)
{
  return M->coeffs(vars,monoms);
}

const MatrixOrNull * rawBasis(const Matrix *M,
			      M2_arrayint lo_degree, /* possibly length 0 */
			      M2_arrayint hi_degree,
			      M2_arrayint wt,
			      M2_arrayint vars,
			      M2_bool do_truncation,
			      int limit)
{
  return M->Matrix::basis(lo_degree,hi_degree,wt,vars,do_truncation,limit);
}

const MatrixOrNull * IM2_Matrix_monomials(M2_arrayint vars, 
					  const Matrix *M)
{
  return M->monomials(vars);
}

const Matrix * IM2_Matrix_initial(int nparts, const Matrix *M)
{
  return M->lead_term(nparts);
}

M2_arrayint IM2_Matrix_elim_vars(int nparts, const Matrix *M)
{
  return M->elim_vars(nparts);
}

M2_arrayint IM2_Matrix_keep_vars(int nparts, const Matrix *M)
{
  return M->elim_keep(nparts);
}

Matrix_pair_OrNull * rawTopCoefficients(const Matrix *M)
{
  Matrix *coeffs;
  Matrix *monoms;
  coeffs = M->top_coefficients(monoms);
  if (coeffs == NULL)
    return NULL;
  Matrix_pair_OrNull *result = new Matrix_pair;
  result->a = monoms;
  result->b = coeffs;
  return result;
}

Matrix_int_pair * IM2_Matrix_divide_by_var(const Matrix *M, int var, int maxdegree)
  /* If M = [v1, ..., vn], and x = 'var'th variable in the ring, 
     return the matrix [w1,...,wn], where wi * x^(ai) = vi,
     and wi is not divisible by x, or ai = maxdegree, 
     and the integer which is the maximum of the ai's.
     QUESTION: what rings should this work over?
  */
{
  int actualdegree;
  Matrix *N = M->divide_by_var(var, maxdegree, actualdegree);
  Matrix_int_pair *result = &global_Matrix_int_pair;
  result->a = N;
  result->b = actualdegree;
  return result;
}


const Matrix * rawMatrixCompress(const Matrix *M)
{
  return M->compress();
}

#include "Eschreyer.hpp"

Matrix * IM2_kernel_of_GB(const Matrix *m)
  /* Assuming that the columns of G form a GB, this computes
     a Groebner basis of the kernel of these elements, using an appropriate Schreyer order on the
     source of G. */
{
  GBMatrix *n = new GBMatrix(m);
  GBKernelComputation G(n);
  G.calc();
  GBMatrix *syz = G.get_syzygies();
  return syz->to_matrix();
}

const Matrix * rawRemoveMonomialFactors(const Matrix *m, M2_bool make_squarefree_only)
{
  return m->remove_monomial_factors(make_squarefree_only);
}

const Matrix * rawRemoveScalarMultiples(const Matrix *m)
{
  return m->remove_scalar_multiples();
}

const MatrixOrNull *IM2_Matrix_remove_content(const Matrix *M) {
#ifdef DEVELOPMENT
#warning "const MatrixOrNull * IM2_Matrix_remove_content(const Matrix *M) -- not implemented yet"
#endif
     return NULL;
}

const MatrixOrNull *IM2_Matrix_promote(const FreeModule *newTarget,
				 const Matrix *f)
{
     try {
	  ring_elem a;
	  const Ring *R = f->get_ring();
	  const Ring *S = newTarget->get_ring();
	  MatrixConstructor mat(newTarget,f->n_cols());
	  Matrix::iterator i(f);
	  for (int c=0; c<f->n_cols(); c++)
	    for (i.set(c); i.valid(); i.next())
	      if (S->promote(R,i.entry(),a))
		mat.set_entry(i.row(), c, a);
	      else
		{
		  ERROR("cannot promote given matrix");
		  return 0;
		}
	  mat.compute_column_degrees();
	  return mat.to_matrix();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

const MatrixOrNull *IM2_Matrix_lift(const FreeModule *newTarget, 
			      const Matrix *f)
{
     try {
	  ring_elem a;
	  const Ring *R = f->get_ring();
	  const Ring *S = newTarget->get_ring();
	  MatrixConstructor mat(newTarget,f->n_cols());
	  Matrix::iterator i(f);
	  for (int c=0; c<f->n_cols(); c++)
	    for (i.set(c); i.valid(); i.next())
	      if (R->lift(S,i.entry(),a))
		mat.set_entry(i.row(), c, a);
	      else
		{
		  ERROR("cannot lift given matrix");
		  return 0;
		}
	  mat.compute_column_degrees();
	  return mat.to_matrix();
     }
     catch (exc::engine_error e) {
	  ERROR(e.what());
	  return NULL;
     }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
