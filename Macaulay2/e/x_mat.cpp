// Copyright 1995 Michael E. Stillman

#include "relem.hpp"
#include "matrix.hpp"
#include "matrixcon.hpp"
#include "LLL.hpp"
#include "fractionfreeLU.hpp"
#include "text_io.hpp"

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

const M2_arrayint IM2_Matrix_get_degree(const Matrix *M)
{
  return M->degree_monoid()->to_arrayint(M->degree_shift());
}

const M2_string IM2_Matrix_to_string(const Matrix *M)
{
  buffer o;
  M->text_out(o);
  return o.to_string();
}

int IM2_Matrix_hash(const Matrix *M)
{
  return M->get_hash_value();
}

const RingElementOrNull * IM2_Matrix_get_entry(const Matrix *M, int r, int c)
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
  result = M->elem(r,c);
  return RingElement::make_raw(M->get_ring(), result);
}

const Matrix * IM2_Matrix_identity(const FreeModule *F,
				   int preference)
{
#warning prefer_dense not yet used
  return Matrix::identity(F);
}

const MatrixOrNull * IM2_Matrix_zero(const FreeModule *F,
				     const FreeModule *G,
				     int preference)
{
#warning prefer_dense not yet used
  return Matrix::zero(F,G);
}


const MatrixOrNull * IM2_Matrix_make1(const FreeModule *target,
				      int ncols,
				      const RingElement_array *M,
				      int preference)
{
#warning prefer_dense not yet used
  return Matrix::make(target, ncols, M);
}

const MatrixOrNull * IM2_Matrix_make2(const FreeModule *target,
				      const FreeModule *source,
				      const M2_arrayint deg,
				      const RingElement_array *M,
				      int preference)
{
#warning prefer_dense not yet used
  return Matrix::make(target, source, deg, M);
}

const MatrixOrNull * IM2_Matrix_make_sparse1(const FreeModule *target,
					     int ncols,
					     const M2_arrayint rows,
					     const M2_arrayint cols,
					     const RingElement_array *entries,
					     int preference)
{
#warning prefer_dense not yet used
  return Matrix::make_sparse(target, ncols, rows, cols, entries);
}
  
const MatrixOrNull * IM2_Matrix_make_sparse2(const FreeModule *target,
					     const FreeModule *source,
					     const M2_arrayint deg,
					     const M2_arrayint rows,
					     const M2_arrayint cols,
					     const RingElement_array *entries,
					     int preference)
{
#warning prefer_dense not yet used
  return Matrix::make_sparse(target, source, deg, rows, cols, entries);
}

M2_bool IM2_Matrix_is_implemented_as_dense(const Matrix *M)
  /* Is the matrix M implemented as dense? */
{
#warning not implemented yet
  return 0;
}

const MatrixOrNull * IM2_Matrix_remake2(const FreeModule *target,
					const FreeModule *source,
					const M2_arrayint deg,
					const Matrix *M,
					int preference)
  /* Create a new matrix (mutable or immutable), from M, with new target,
     source, deg and/or mutable-ness. The new free modules must have 
     the expected rank. 
  */
{
#warning prefer_dense not yet used
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
#warning prefer_dense not yet used
  return M->remake(target);
}

MatrixOrNull *IM2_Matrix_random(const Ring *R, 
				int r, int c, 
				double fraction_non_zero, 
				int special_type, // 0: general, 1:upper triangular, others?
				int preference)
{
#warning preference not yet used
  return Matrix::random(R,r,c,fraction_non_zero,special_type);
}

/////////////////////////////////////////////////////////////////////
const M2_bool IM2_Matrix_is_zero(const Matrix *M)
{
  return M->is_zero();
}

const M2_bool IM2_Matrix_is_equal(const Matrix *M, 
				  const Matrix *N)
{
  /* This checks that the entries of M,N are the same, as well as
     that the source and target are the same (as graded free modules).
     Therefore, it can happen that M-N == 0, but M != N.
  */
  return M->is_equal(*N);
}

const M2_bool IM2_Matrix_is_graded(const Matrix *M)
{
  return M->is_homogeneous();
}

const MatrixOrNull * IM2_Matrix_add(const Matrix *M, const Matrix *N)
  /* If the sizes do not match, then NULL is returned.  If they do match,
     the addition is performed, and the source,target,degree are taken from the
     first matrix. */
{
  return (*M) + (*N);
}

const MatrixOrNull * IM2_Matrix_subtract(const Matrix *M, const Matrix *N)
{
  return (*M) - (*N);
}

const Matrix * IM2_Matrix_negate(const Matrix *M)
{
  return - (*M);
}

const MatrixOrNull * IM2_Matrix_mult(const Matrix *M, 
				     const Matrix *N, 
				     M2_bool opposite_mult)
{
  return M->mult(N, opposite_mult);
}

const MatrixOrNull * IM2_Matrix_scalar_mult(const RingElement *f,
					    const Matrix *M,
					    M2_bool opposite_mult)
{
  if (f->get_ring() != M->get_ring())
    {
      ERROR("ring element and matrix should have the same base ring");
      return 0;
    }
  return M->scalar_mult(f->get_value(), opposite_mult);
}

const MatrixOrNull * IM2_Matrix_concat(const Matrix_array *Ms)
{
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

const MatrixOrNull * IM2_Matrix_direct_sum(const Matrix_array *Ms)
{
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

const MatrixOrNull * IM2_Matrix_tensor(const Matrix *M,
				       const Matrix *N)
{
  return M->tensor(N);
}


const MatrixOrNull * rawModuleTensor(const Matrix *M,
				     const Matrix *N)
{
  return M->module_tensor(N);
}

const Matrix * IM2_Matrix_transpose(const Matrix *M)
{
  return M->transpose();
}

const MatrixOrNull * IM2_Matrix_reshape(const Matrix *M,
					const FreeModule *F,
					const FreeModule *G)
{
  return M->reshape(F,G);
}

const MatrixOrNull * IM2_Matrix_flip(const FreeModule *F,
				     const FreeModule *G)
{
  return Matrix::flip(F,G);
}

const MatrixOrNull * rawWedgeProduct(int p,
				     int q,
				     const FreeModule *F)
  /* Constructs the map
     exterior(p,F) ** exterior(q,F) --> exterior(p+q,F)
  */
{
  return Matrix::wedge_product(p,q,F);
}

const MatrixOrNull * IM2_Matrix_submatrix(const Matrix *M,
					  const M2_arrayint rows,
					  const M2_arrayint cols)
{
  return M->sub_matrix(rows,cols);
}

const MatrixOrNull * IM2_Matrix_submatrix1(const Matrix *M,
					   const M2_arrayint cols)
{
  return M->sub_matrix(cols);
}

const MatrixOrNull * IM2_Matrix_koszul(int p, const Matrix *M)
{
  return M->koszul(p);
}

const MatrixOrNull * IM2_Matrix_koszul_monoms(const Matrix *M,
					      const Matrix *N)
{
#warning "check with 0.9.2 about what this should even do"
  return Matrix::koszul(0,M,N);
}

const MatrixOrNull * IM2_Matrix_symm(int p, const Matrix *M)
{
  return M->symm(p);
}

const Matrix * IM2_Matrix_exterior(int p, const Matrix *M, int strategy)
{
  return M->exterior(p,strategy); 
}

const M2_arrayint_OrNull IM2_Matrix_sort_columns(const Matrix *M, 
						 int deg_order, 
						 int mon_order)
{
  return M->sort(deg_order, mon_order);
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
#warning "implement IM2_Matrix_coeffs"
  return 0;
}

const MatrixOrNull * rawCoefficients(const M2_arrayint vars,
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

const MatrixOrNull * IM2_Matrix_monomials(const M2_arrayint vars, 
					  const Matrix *M)
{
  return M->monomials(vars);
}

const Matrix * IM2_Matrix_initial(int nparts, const Matrix *M)
{
  return M->lead_term(nparts);
}

const M2_arrayint IM2_Matrix_elim_vars(int nparts, const Matrix *M)
{
  return M->elim_vars(nparts);
}

const M2_arrayint IM2_Matrix_keep_vars(int nparts, const Matrix *M)
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

#include "linalgGB/MonomialSet.hpp"
#if 0
#include "linalgGB/MonomialHeap.hpp"
#endif
#include "linalgGB/MonomialTable.hpp"
#include "linalgGB/interface.hpp"
MonomialLookupTable *make_monideal(const Matrix *M, MonomialSet &H)
{
  const PolynomialRing *P = M->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  const Monoid *MF = P->getMonoid();
  queue <tagged_monomial *> new_elems;
  intarray vp;

  for (int i=0; i<M->n_cols(); i++)
    {
      ring_elem f = M->elem(0,i);
      Nterm *t = f; // numerator of f
      for ( ; t != 0; t=t->next)
	{
	  monomial m;
	  vp.shrink(0);
	  MF->to_varpower(t->monom, vp);
	  vp[0] = (vp[0]-1)/2;
	  H.find_or_insert(vp.raw(), m);
	  new_elems.insert(new tagged_monomial(m,0));
	}
    }

  MonomialLookupTable *result = new MonomialLookupTable(new_elems);
  return result;
}

const Matrix * rawMatrixCompress(const Matrix *M)
{
  return M->compress();

#if 0
  const PolynomialRing *R = M->get_ring()->cast_to_PolynomialRing();
  const Monoid *MF = R->getMonoid();
  MonomialSet H;

  gb_array pols;

  from_M2_matrix(M,&H,NULL,pols);
  H.dump();
  spair_testing(&H,pols);
  return to_M2_matrix(pols,M->rows());
#endif

#if 0
  intarray vp;
  for (int i=0; i<M->n_cols(); i++)
    {
      ring_elem f = M->elem(0,i);
      Nterm *t = f; // numerator of f
      for ( ; t != 0; t=t->next)
	{
	  monomial m;
	  vp.shrink(0);
	  MF->to_varpower(t->monom, vp);
	  vp[0] = (vp[0]-1)/2;
	  H.find_or_insert(vp.raw(), m);
	}
    }
  H.dump();

  // Now make a MonomialTable
  MonomialLookupTable *T = make_monideal(M,H);

  buffer o;
  o << "Number of elements in MonomialTable = " << T->length() << newline;
  emit(o.str());
#endif
  
#if 0
  // Now make a MonomialHeap
  MonomialHeap H1;
  for (int i=0; i<M->n_cols(); i++)
    {
      ring_elem f = M->elem(0,i);
      Nterm *t = f; // numerator of f
      for ( ; t != 0; t=t->next)
	{
	  monomial m;
	  vp.shrink(0);
	  MF->to_varpower(t->monom, vp);
	  vp[0] = (vp[0]-1)/2;
	  H.find_or_insert(vp.raw(), m);
	  H1.insert(vp.raw(), m);
	}
    }
  H1.dump(stderr);
#endif

  //  return M;
  //return M->compress();
}

#if 0

// Code for doing ESchreyer
  GBMatrix *n = new GBMatrix(m);
  GBKernelComputation G(n);
  G.calc();
  GBMatrix *syz = G.get_syzygies();
  return syz->to_matrix();

#endif

const Matrix * rawRemoveMonomialFactors(const Matrix *m, M2_bool make_squarefree_only)
{
  return m->remove_monomial_factors(make_squarefree_only);
}

#include "Eschreyer.hpp"

const Matrix * rawRemoveScalarMultiples(const Matrix *m)
{
  return m->remove_scalar_multiples();
}

const MatrixOrNull *IM2_Matrix_remove_content(const Matrix *M) {
#warning "const MatrixOrNull * IM2_Matrix_remove_content(const Matrix *M) -- not implemented yet"
     return NULL;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
