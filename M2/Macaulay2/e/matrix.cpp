// Copyright 1995-2004 Michael E. Stillman

// setEntry, dotProduct, getEntry
// addColumnMultiple, interchangeColumns

#include "style.hpp"
#include "text_io.hpp"
#include "ring.hpp"
#include "matrix.hpp"
#include "comb.hpp"
#include "det.hpp"
#include "polyring.hpp"
#include "termideal.hpp"
#include "assprime.hpp"
#include "monideal.hpp"

#include "vector.hpp"
#include "exptable.h"

#include <vector>

#include "matrixcon.hpp"
#include "random.hpp"

#if 0
void Matrix::initialize(const FreeModule *r, 
		     const FreeModule *c,
		     const int *deg)
{
  _rows = const_cast<FreeModule *>(r);
  _cols = const_cast<FreeModule *>(c);
  _degree_shift = const_cast<int *>(deg);

  vec Zero = NULL;
  for (int i=0; i<c->rank(); i++) 
    _entries.append(Zero); 

  rowOps = NULL;
  colOps = NULL;
}

void Matrix::freeze(bool is_mutable_flag)
{
  if (is_mutable_flag)
    make_mutable();
  else 
    make_immutable(234123 + get_ring()->get_hash_value() * (7 * n_rows() + 157 * n_cols()));
}

Matrix::Matrix(const FreeModule *r, 
	       const FreeModule *c,
	       const int *deg)
{ 
  initialize(r,
	     c,
	     r->get_ring()->degree_monoid()->make_new(deg));
}

Matrix::Matrix(const FreeModule *r, 
	       const FreeModule *c)
{ 
  initialize(r,
	     c,
	     r->get_ring()->degree_monoid()->make_one());
}

Matrix::Matrix(const FreeModule *r)
{ 
  initialize(r,
	     r->new_free(),
	     r->get_ring()->degree_monoid()->make_one());
}


Matrix::Matrix(const MonomialIdeal *mi) 
{ 
  const FreeModule *r = mi->get_ring()->make_FreeModule(1);
  int *one = r->get_ring()->degree_monoid()->make_one();
  initialize(r, r->new_free(), one);
  append_monideal(mi,0);
}
#endif

Matrix::Matrix(const FreeModule *rows0, 
	       const FreeModule *cols0,
	       const int *degree_shift0,
	       vector<vec> & entries0,
	       bool is_mutable_flag)
{
  _rows = const_cast<FreeModule *>(rows0);
  _cols = const_cast<FreeModule *>(cols0);
  _degree_shift = const_cast<int *>(degree_shift0);
  for (int i=0; i<cols0->rank(); i++)
    _entries.append(entries0[i]);

  rowOps = NULL;
  colOps = NULL;

  if (is_mutable_flag)
    make_mutable();
  else 
    make_immutable(234123 + get_ring()->get_hash_value() * (7 * n_rows() + 157 * n_cols()));
}


const MatrixOrNull * Matrix::make(const FreeModule *target,
				   int ncols,
				   const RingElement_array *M,
				   M2_bool is_mutable_flag)
{
  // Checks to make:
  // each vector in V is over same ring.

  const Ring *R = target->get_ring();
  for (unsigned int i=0; i<M->len; i++)
    {
      if (R != M->array[i]->get_ring())
	{
	  ERROR("expected vectors in the same ring");
	  return 0;
	}
    }

  MatrixConstructor mat(target, ncols, is_mutable_flag);
  unsigned int next = 0;
  for (int r=0; r <target->rank(); r++)
    for (int c=0; c < ncols && next < M->len; c++)
      {
	mat.set_entry(r,c,M->array[next]->get_value());
	next++;
      }
  mat.compute_column_degrees();
  return mat.to_matrix();
}
const MatrixOrNull * Matrix::make(const FreeModule *target,
				   const FreeModule *source,
				   const M2_arrayint deg,
				   const RingElement_array *M,
				   M2_bool is_mutable_flag)
{
  const Ring *R = target->get_ring();
  if (source->get_ring() != R)
    {
      ERROR("expected free modules over the same ring");
      return 0;
    }
  if (R->degree_monoid()->n_vars() != (int)deg->len)
    {
      ERROR("expected degree of matrix to have %d entries", 
	    R->degree_monoid()->n_vars());
      return 0;
    }
  for (unsigned int i=0; i<M->len; i++)
    {
      if (R != M->array[i]->get_ring())
	{
	  ERROR("expected vectors in the same ring");
	  return 0;
	}
    }

  MatrixConstructor mat(target, source, deg->array, is_mutable_flag);

  unsigned int next = 0;
  for (int r=0; r <target->rank(); r++)
    {
      for (int c=0; c < source->rank(); c++)
	{
	  mat.set_entry(r,c,M->array[next]->get_value());
	  next++;
	  if (next >= M->len) break;
	}
    }
  return mat.to_matrix();
}

bool Matrix::make_sparse_vecs(MatrixConstructor &mat,
			       const FreeModule *target,
			       int ncols,
			       const M2_arrayint rows,
			       const M2_arrayint cols,
			       const RingElement_array *entries)
  // returns false if an error, true otherwise.
  // Places the elements into 'mat'.
{
  const Ring *R = target->get_ring();
  for (unsigned int i=0; i<entries->len; i++)
    {
      if (R != entries->array[i]->get_ring())
	{
	  ERROR("expected vectors in the same ring");
	  return false;
	}
    }
  if (rows->len != cols->len || rows->len != entries->len)
    {
      ERROR("sparse matrix creation: encountered different length arrays");
      return false;
    }
  for (int x=0; x < entries->len; x++)
    {
      int r = rows->array[x];
      int c = cols->array[x];
      if (r < 0 || r >= target->rank())
	{
	  ERROR("sparse matrix creation: row index out of range");
	  return false;
	}
      if (c < 0 || c >=ncols)
	{
	  ERROR("sparse matrix creation: column index out of range");
	  return false;
	}
    }

  for (int x=0; x<entries->len; x++)
    {
      int r = rows->array[x];
      int c = cols->array[x];
      mat.set_entry(r,c, entries->array[x]->get_value());
    }
  return true;
}

const MatrixOrNull * Matrix::make_sparse(const FreeModule *target,
					  int ncols,
					  const M2_arrayint rows,
					  const M2_arrayint cols,
					  const RingElement_array *entries,
					  M2_bool is_mutable_flag)
{
  MatrixConstructor mat(target, ncols, is_mutable_flag);
  if (!Matrix::make_sparse_vecs(mat, target, ncols, rows, cols, entries))
    return 0; // error message has already been sent
  mat.compute_column_degrees();
  return mat.to_matrix();
}

const MatrixOrNull * Matrix::make_sparse(const FreeModule *target,
					  const FreeModule *source,
					  const M2_arrayint deg,
					  const M2_arrayint rows,
					  const M2_arrayint cols,
					  const RingElement_array *entries,
					  M2_bool is_mutable_flag)
{
  MatrixConstructor mat(target, source, deg->array, is_mutable_flag);
  if (!Matrix::make_sparse_vecs(mat, target, source->rank(), rows, cols, entries))
    return 0; // error message has already been sent
  return mat.to_matrix();
}

const MatrixOrNull * Matrix::remake(const FreeModule *target,
				 const FreeModule *source,
				 const M2_arrayint deg,
				 M2_bool is_mutable_flag) const
{ 
  if (n_rows() != target->rank() || n_cols() != source->rank())
    {
      ERROR("wrong number of rows or columns");
      return 0;
    }
  if (deg->len != degree_monoid()->n_vars())
    {
      ERROR("degree for matrix has the wrong length");
      return 0;
    }
  const Ring *R = get_ring();
  const Ring *Rtarget = target->get_ring();
  const Ring *Rsource = source->get_ring();
  if (R != Rtarget || Rtarget != Rsource)
    {
      ERROR("expected same ring");
      return 0;
    }

  MatrixConstructor mat(target, source, deg->array, is_mutable_flag);
  for (int i=0; i<source->rank(); i++)
    mat.set_column(i, R->copy_vec(_entries[i]));
  return mat.to_matrix();
}

const MatrixOrNull * Matrix::remake(const FreeModule *target,
				    M2_bool is_mutable_flag) const
{
  if (n_rows() != target->rank())
    {
      ERROR("wrong number of rows");
      return 0;
    }
  const Ring *R = get_ring();
  if (R != target->get_ring())
    {
      ERROR("expected same ring");
      return 0;
    }

  MatrixConstructor mat(target, n_cols(), is_mutable_flag);
  for (int i=0; i<n_cols(); i++)
    mat.set_column(i, R->copy_vec(_entries[i]));
  mat.compute_column_degrees();
  return mat.to_matrix();
}

const MatrixOrNull * Matrix::make(const MonomialIdeal * mi)
{
  const PolynomialRing *P = mi->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a matrix over a polynomial ring");
      return 0;
    }
  const Monoid *M = P->Nmonoms();
  int *mon = M->make_one();

  MatrixConstructor mat(P->make_FreeModule(1), 0, false);
  int next = 0;
  for (Index<MonomialIdeal> i = mi->last(); i.valid(); i--)
    {
      M->from_varpower((*mi)[i]->monom().raw(), mon);
      ring_elem f = P->term(P->Ncoeffs()->from_int(1), mon);
      mat.set_entry(0,next++,f);
    }
  M->remove(mon);

  return mat.to_matrix();
}

bool Matrix::error_column_bound(int c) const
{
  if (c < 0 || c >= n_cols())
    {
      ERROR("column out of range");
      return true;
    }
  return false;
}

bool Matrix::error_row_bound(int r) const
{
  if (r < 0 || r >= n_rows())
    {
      ERROR("row out of range");
      return true;
    }
  return false;
}

bool Matrix::setRowChangeMatrix(Matrix *rops)
{
  if (is_immutable())
    {
      ERROR("expected mutable matrix");
      return false;
    }
  if (rops->get_ring() != get_ring())
    {
      ERROR("matrices over different rings");
      return false;
    }
  if (rops->n_cols() != n_rows())
    {
      ERROR("row change matrix must have %d columns", n_rows());
      return false;
    }

  rowOps = rops;
  return true;
}

bool Matrix::setColumnChangeMatrix(Matrix *cops)
{
  if (is_immutable())
    {
      ERROR("expected mutable matrix");
      return false;
    }
  if (cops->get_ring() != get_ring())
    {
      ERROR("matrices over different rings");
      return false;
    }
  if (cops->n_cols() != n_cols())
    {
      ERROR("column change matrix must have %d columns", n_cols());
      return false;
    }

  colOps = cops;
  return true;
}

MatrixOrNull *Matrix::getRowChangeMatrix()
{
  return rowOps;
}

MatrixOrNull *Matrix::getColumnChangeMatrix()
{
  return colOps;
}

bool Matrix::get_entry(int r, int c, ring_elem &a) const
  // This one returns false if (r,c) is out of range.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  if (!get_ring()->get_entry(_entries[c], r, a))
    a = get_ring()->zero();
  return true;
}

bool Matrix::get_nonzero_entry(int r, int c, ring_elem &result) const
{
  if (c < 0 || c >= n_cols()) return false;
  return get_ring()->get_entry(_entries[c],r,result);
}

bool Matrix::set_entry(int r, int c, const ring_elem a)
  // Returns false if (r,c) is out of range.  It is assumed that the ring of
  // 'a' is the same as the ring of this.
{
  if (error_row_bound(r)) return false;
  if (error_column_bound(c)) return false;
  _rows->get_ring()->set_entry(_entries[c], r, a);
  return true;
}

bool Matrix::interchange_rows(int i, int j, bool do_recording)
  /* swap rows: row(i) <--> row(j) */
{
  if (is_immutable()) return false;
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;
  const Ring *R = get_ring();
  for (int c=0; c<n_cols(); c++)
    R->interchange_rows(_entries[c], i, j);
  if (do_recording && rowOps != 0)
    rowOps->interchange_columns(i,j,false);
  return true;
}


bool Matrix::interchange_columns(int i, int j, bool do_recording)
  /* swap columns: column(i) <--> column(j) */
{
  if (is_immutable()) return false;
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  vec tmp = _entries[i];
  _entries[i] = _entries[j];
  _entries[j] = tmp;
  if (do_recording && colOps != 0)
    colOps->interchange_columns(i,j,false);
  return true;
}

bool Matrix::scale_row(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* row(i) <- r * row(i) */
{
  if (is_immutable()) return false;
  if (error_row_bound(i)) return false;
  const Ring *R = get_ring();
  for (int c=0; c<n_cols(); c++)
    R->mult_row(_entries[c], r, i, opposite_mult);
  if (do_recording && rowOps != 0)
    rowOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool Matrix::scale_column(ring_elem r, int i, bool opposite_mult, bool do_recording)
  /* column(i) <- r * column(i) */
{
  if (is_immutable()) return false;
  if (error_column_bound(i)) return false;
  const Ring *R = get_ring();
  R->mult_vec_to(_entries[i], r, opposite_mult);
  if (do_recording && colOps != 0)
    colOps->scale_column(r,i,opposite_mult,false);
  return true;
}

bool Matrix::row_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* row(i) <- row(i) + r * row(j) */
{
  if (is_immutable()) return false;
  if (error_row_bound(i)) return false;
  if (error_row_bound(j)) return false;
  const Ring *R = get_ring();

  for (int c=0; c<n_cols(); c++)
    R->vec_row_op(_entries[c], i, r, j, opposite_mult);

  if (do_recording && rowOps != 0)
    rowOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

bool Matrix::column_op(int i, ring_elem r, int j, bool opposite_mult, bool do_recording)
  /* column(i) <- column(i) + r * column(j) */
{
  if (is_immutable()) return false;
  if (error_column_bound(i)) return false;
  if (error_column_bound(j)) return false;
  const Ring *R = get_ring();

  vec tmp = R->copy_vec(_entries[j]);
  R->mult_vec_to(tmp, r, opposite_mult); // replaces tmp by r*tmp or tmp*r
  R->add_vec_to(_entries[i], tmp);

  if (do_recording && colOps != 0)
    colOps->column_op(i,r,j,opposite_mult,false);

  return true;
}

#if 0
void Matrix::column_reduce(int c1, int c2, bool do_recording)
{
  if (is_immutable()) return false;
  if (error_column_bound(c1)) return false;
  if (error_column_bound(c2)) return false;
  const Ring *R = get_ring();

  ring_elem a1 = _entries[c1]->coeff; // Assumed to be non-zero...
  ring_elem a2, b;
  if (!get_entry(_entries[c1]->comp,c2,a2))
    return;
  if (!K->is_equal(a1,one))
    {
      ring_elem rem;
      b = K->divide(a2,a1,rem); // division algorithm...
      K->negate_to(b);
    }
  else
    b = K->negate(a2);
  column_op(c1,b,c2,do_recording);
}
void Matrix::row2by2(int r1, int r2, 
		     ring_elem a1, ring_elem a2,
		     ring_elem b1, ring_elem b2,
		     bool do_recording)
{
  for (int i=0; i<n_cols(); i++)
    row2by2(matrix[i],r1,r2,a1,a2,b1,b2);

  if (doRecording && rowOps != 0)
    rowOps->column2by2(r1,r2,a1,a2,b1,b2,false);
}

void Matrix::column2by2(int c1, int c2, 
				     ring_elem a1, ring_elem a2,
				     ring_elem b1, ring_elem b2,
				     bool doRecording)
{
  // do the replacements:
  // new column c1 = a1 * column[c1] + a2 * column[c2]
  // new column c2 = b1 * column[c1] + b2 * column[c2]
  // Make first column: v1 = a1*c1+a2*c2
  sparse_vector *v1 = V->clone(matrix[c1]);
  sparse_vector *v2 = V->clone(matrix[c2]);
  V->scale(v1,a1);
  V->scale(v2,a2);
  V->add(v1,v2);
  // Second column: w1 = b1*c1 + b2*c2
  sparse_vector *w1 = V->clone(matrix[c1]);
  sparse_vector *w2 = V->clone(matrix[c2]);
  V->scale(w1,b1);
  V->scale(w2,b2);
  V->add(w1,w2);
  // Set the matrices:
  V->remove(matrix[c1]);
  V->remove(matrix[c2]);
  matrix[c1] = v1;
  matrix[c2] = w1;
  // Do the recording, if needed:
  if (doRecording && colOps != 0)
    colOps->column2by2(c1,c2,a1,a2,b1,b2,false);
}
#endif
















bool Matrix::is_equal(const Matrix &m) const
{
  if (this == &m) return true;
  if (get_hash_value() != m.get_hash_value())
    return false; // Note that if one is immutable, 
                  // and the other is mutable, false will be returned.
  if (get_ring() != m.get_ring())
    return false;
  if (n_rows() != m.n_rows())
    return false;
  if (n_cols() != m.n_cols())
    return false;
  for (int i=0; i<n_cols(); i++)
    if (! get_ring()->is_equal(elem(i), m.elem(i))) 
      return false;
  return true;
}

bool Matrix::is_zero() const
{
  for (int i=0; i<n_cols(); i++)
    if (elem(i) != 0) return false;
  return true;
}

int Matrix::is_homogeneous() const
{
#warning "check the ring too!"
  int *d = degree_monoid()->make_one();
  for (int i=0; i<n_cols(); i++)
    {
      if (rows()->is_zero(elem(i))) continue;
      if (! rows()->is_homogeneous(elem(i)))
	{
	  degree_monoid()->remove(d);
	  return 0;
	}
 
      rows()->degree(elem(i), d);
      degree_monoid()->divide(d, degree_shift(), d);
      if (0 != degree_monoid()->compare(d, cols()->degree(i)))
	{
	  degree_monoid()->remove(d);
	  return 0;
	}
    }
  degree_monoid()->remove(d);
  return 1;
}

Matrix *Matrix::homogenize(int v, const M2_arrayint wts) const
{
  MatrixConstructor mat(rows(), n_cols(), is_mutable());
  for (int i=0; i<n_cols(); i++)
    mat.set_column(i, rows()->homogenize(elem(i), v, wts));
  mat.compute_column_degrees();
  return mat.to_matrix();
}

Matrix *Matrix::zero(const FreeModule *F, const FreeModule *G, bool is_mutable_flag)
{
  if (F->get_ring() != G->get_ring())
    {
      ERROR("free modules have different base rings");
      return 0;
    }
  MatrixConstructor mat(F,G->rank(),is_mutable_flag);
  mat.set_column_degrees(G);
  return mat.to_matrix();
}

Matrix *Matrix::identity(const FreeModule *F, bool is_mutable_flag)
{
  const ring_elem one = F->get_ring()->one();
  MatrixConstructor mat(F,F->rank(),is_mutable_flag);
  mat.set_column_degrees(F);
  for (int i=0; i<F->rank(); i++)
    mat.set_entry(i,i,one);
  return mat.to_matrix();
}

Matrix *Matrix::operator+(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("matrices have different base rings");
      return 0;
    }
  if (rows()->rank() != m.rows()->rank()
      || cols()->rank() != m.cols()->rank())
    {
      ERROR("matrices have different shapes");
      return 0;
    }
  
  const Ring *R = get_ring();
  const FreeModule *F = rows();
  const FreeModule *G = cols();
  const int *deg;

  if (!rows()->is_equal(m.rows()))
    F = R->make_FreeModule(n_rows());
  
  if (!cols()->is_equal(m.cols()))
    G = R->make_FreeModule(n_cols());
  
  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    deg = degree_shift();
  else
    deg = degree_monoid()->make_one();

  bool is_mutable_flag = is_mutable() && m.is_mutable();
  MatrixConstructor mat(F,G,deg,is_mutable_flag);
  for (int i=0; i<n_cols(); i++)
    {
      vec v = R->copy_vec(elem(i));
      vec w = R->copy_vec(m[i]);
      R->add_vec_to(v,w);
      mat.set_column(i, v);
    }
  return mat.to_matrix();
}

Matrix *Matrix::operator-(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("matrices have different base rings");
      return 0;
    }
  if (rows()->rank() != m.rows()->rank()
      || cols()->rank() != m.cols()->rank())
    {
      ERROR("matrices have different shapes");
      return 0;
    }

  const Ring *R = get_ring();
  const FreeModule *F = rows();
  const FreeModule *G = cols();
  const int *deg;

  if (!rows()->is_equal(m.rows()))
    F = R->make_FreeModule(n_rows());
  
  if (!cols()->is_equal(m.cols()))
    G = R->make_FreeModule(n_cols());
  
  if (EQ == degree_monoid()->compare(degree_shift(), m.degree_shift()))
    deg = degree_shift();
  else
    deg = degree_monoid()->make_one();

  bool is_mutable_flag = is_mutable() && m.is_mutable();
  MatrixConstructor mat(F,G,deg,is_mutable_flag);
  for (int i=0; i<n_cols(); i++)
    mat.set_column(i, F->subtract(elem(i),m[i]));
  return mat.to_matrix();
}

Matrix *Matrix::operator-() const
{
  MatrixConstructor mat(rows(), cols(), degree_shift(), is_mutable());
  for (int i=0; i<n_cols(); i++)
    mat.set_column(i, rows()->negate(elem(i)));
  return mat.to_matrix();
}

MatrixOrNull *Matrix::sub_matrix(const M2_arrayint r, const M2_arrayint c) const
{
  const FreeModule *F = rows()->sub_space(r);
  const FreeModule *G = cols()->sub_space(c);
  if (F == NULL || G == NULL)
    return 0;

  int *trans = newarray(int,n_rows());
  for (int i=0; i<n_rows(); i++)
    trans[i] = -1;

  for (unsigned j=0; j<r->len; j++)
    if (r->array[j] >= 0 && r->array[j] < n_rows())
      trans[r->array[j]] = j;

  MatrixConstructor mat(F,G,degree_shift(),is_mutable());
  for (unsigned int i=0; i<c->len; i++)
    {
      vec v = elem(c->array[i]);
      for ( ; v != NULL; v = v->next)
	if (trans[v->comp] != -1)
	  mat.set_entry(trans[v->comp], i, v->coeff);
    }
  deletearray(trans);
  return mat.to_matrix();
#if 0
  MatrixConstructor mat(F,G,degree_shift(),is_mutable());
  for (unsigned int i=0; i<c->len; i++)
    mat.set_column(i, F->sub_vector(rows(), elem(c->array[i]), r));
  return mat.to_matrix();
#endif
}

MatrixOrNull *Matrix::sub_matrix(const M2_arrayint c) const
{
  const FreeModule *G = cols()->sub_space(c);
  if (G == NULL)
    return 0;

  MatrixConstructor mat(rows(),G,degree_shift(),is_mutable());
  for (unsigned int i=0; i<c->len; i++)
    mat.set_column(i, get_ring()->copy_vec(elem(c->array[i])));
  return mat.to_matrix();
}

MatrixOrNull *Matrix::reshape(const FreeModule *F, const FreeModule *G) const
  // Reshape 'this' : F <--- G, where 
  // (rank F)(rank G) = (nrows this)(ncols this)
{
  if (F->get_ring() != get_ring() || G->get_ring() != get_ring())
    {
      ERROR("reshape: expected same ring");
      return 0;
    }
  if (n_rows() * n_cols() != F->rank() * G->rank())
    {
      ERROR("reshape: ranks of freemodules incorrect");
      return 0;
    }

  // EFFICIENCY: might be better to sort columns at end?
  MatrixConstructor mat(F,G,degree_monoid()->make_one(),false);
  for (int c=0; c<n_cols(); c++)
    for (vecterm *p = elem(c); p != NULL; p = p->next)
      {
	// Determine new component
	int loc = c * n_rows() + p->comp;
	int result_col = loc / F->rank();
	int result_row = loc % F->rank();

	mat.set_entry(result_row,result_col,p->coeff);
      }
  return mat.to_matrix();
}

MatrixOrNull *Matrix::flip(const FreeModule *F, const FreeModule *G)
{
  if (F->get_ring() != G->get_ring())
    {
      ERROR("flip: expected same ring");
      return 0;
    }
  const FreeModule *H = F->tensor(G);
  const FreeModule *K = G->tensor(F);

  MatrixConstructor mat(K,H,F->get_ring()->degree_monoid()->make_one(),false);
  int next = 0;
  for (int f=0; f<F->rank(); f++)
    for (int g=0; g<G->rank(); g++)
      mat.set_column(next++, H->e_sub_i(f + g * F->rank()));
  return mat.to_matrix();
}

Matrix *Matrix::transpose() const
{
  const FreeModule *F = cols()->transpose();
  const FreeModule *G = rows()->transpose();
  int *deg = degree_monoid()->make_one();
  degree_monoid()->divide(deg, degree_shift(), deg);

  MatrixConstructor mat(F,G,deg,is_mutable());
  degree_monoid()->remove(deg);

  // The efficiency of this code relies on the way of ordering
  // the sparse vectors (lead term has largest component)
  for (int c=0; c<n_cols(); c++)
    {
      for (vec t = elem(c); t != 0; t=t->next)
	mat.set_entry(c,t->comp,t->coeff);
    }
  return mat.to_matrix();
#if 0
  Matrix *result = new Matrix(F, G, deg);
  degree_monoid()->remove(deg);
  F->transpose_matrix(*this, *result);
  return result;
#endif
}

Matrix *Matrix::scalar_mult(const ring_elem r, bool opposite_mult) const
{
  const Ring *R = get_ring();
  int *deg = degree_monoid()->make_one();
  if (!R->is_zero(r))
    R->degree(r, deg);
  degree_monoid()->mult(deg, degree_shift(), deg);
  MatrixConstructor mat(rows(), cols(), deg, is_mutable());
  for (int i=0; i<n_cols(); i++)
    {
      vec w = R->copy_vec(elem(i));
      R->mult_vec_to(w,r,opposite_mult);
      mat.set_column(i, w);
    }
  return mat.to_matrix();

#if 0
  Matrix *result = new Matrix(rows(), cols(), deg);
  for (int i=0; i<n_cols(); i++)
    {
      vec w = R->copy_vec(elem(i));
      R->mult_vec_to(w,r,opposite_mult);
      (*result)[i] = w;
    }
  return result;
#endif
}

Matrix *Matrix::concat(const Matrix &m) const
{
  if (get_ring() != m.get_ring())
    {
      ERROR("concat: different base rings");
      return 0;
    }
  if (n_rows() != m.n_rows())
    {
      ERROR("concat: matrices have different numbers of rows");
      return 0;
    }

  const FreeModule *G = cols()->direct_sum(m.cols());
  MatrixConstructor mat(rows(), G, degree_monoid()->make_one(), is_mutable() && m.is_mutable());
  int i;
  int nc = n_cols();
  for (i=0; i<nc; i++)
    mat.set_column(i, get_ring()->copy_vec(elem(i)));
  for (i=0; i<m.n_cols(); i++)
    mat.set_column(nc+i, get_ring()->copy_vec(m.elem(i)));
  return mat.to_matrix();

#if 0
  Matrix *result = new Matrix(rows(), G);
  int i;
  int nc = n_cols();
  for (i=0; i<nc; i++)
    (*result)[i] = rows()->copy(elem(i));
  for (i=0; i<m.n_cols(); i++)
    (*result)[nc+i] = rows()->copy(m.elem(i));

  return result;
#endif

}

Matrix *Matrix::direct_sum(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("concat: different base rings");
      return 0;
    }
  int *deg;
  if (EQ == degree_monoid()->compare(degree_shift(), m->degree_shift()))
    deg = degree_monoid()->make_new(degree_shift());
  else
    deg = degree_monoid()->make_one();

  const FreeModule *F = rows()->direct_sum(m->rows());
  const FreeModule *G = cols()->direct_sum(m->cols());

  MatrixConstructor mat(F,G,deg,is_mutable() && m->is_mutable());
  degree_monoid()->remove(deg);

  int i;
  int nr = n_rows();
  int nc = n_cols();
  for (i=0; i<nc; i++) 
    mat.set_column(i, get_ring()->copy_vec(elem(i)));
  for (i=0; i<m->n_cols(); i++)
    mat.set_column(nc+i, F->component_shift(nr, m->rows(), (*m)[i]));
  return mat.to_matrix();
#if 0  
  Matrix *result = new Matrix(F, G, deg);

  degree_monoid()->remove(deg);

  int i;
  int nr = n_rows();
  int nc = n_cols();
  for (i=0; i<nc; i++) (*result)[i] = F->copy(elem(i));
  for (i=0; i<m->n_cols(); i++)
    (*result)[nc+i] = F->component_shift(nr, m->rows(), (*m)[i]);
  return result;
#endif
}

Matrix *Matrix::mult(const Matrix *m, bool opposite_mult) const
{
  const Ring *R = get_ring();
  if (R != m->get_ring())
    {
      ERROR("matrix mult: different base rings");
      return 0;
    }
  if (n_cols() != m->n_rows())
    {
      ERROR("matrix mult: matrix sizes don't match");
      return 0;
    }

  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m->degree_shift(), deg);

  MatrixConstructor mat(rows(), m->cols(), deg, is_mutable() && m->is_mutable());

  degree_monoid()->remove(deg);

  for (int i=0; i<m->n_cols(); i++)
    mat.set_column(i, R->mult_vec_matrix(this, m->elem(i), opposite_mult));
  return mat.to_matrix();

#if 0
  Matrix *result = new Matrix(rows(), m->cols(), deg);
  degree_monoid()->remove(deg);

  for (int i=0; i<m->n_cols(); i++)
    (*result)[i] = R->mult_vec_matrix(this, m->elem(i), opposite_mult);
  return result;
#endif
}

Matrix *Matrix::module_tensor(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("module tensor: different base rings");
      return 0;
    }
  FreeModule *F = rows()->tensor(m->rows());
  FreeModule *G = rows()->tensor(m->cols());
  FreeModule *G1 = m->rows()->tensor(cols());
  G->direct_sum_to(G1);
  deleteitem(G1);

  MatrixConstructor mat(F,G,
			get_ring()->degree_monoid()->make_one(),
			is_mutable() && m->is_mutable());

  int i, j, next=0;

  for (i=0; i<n_rows(); i++)
    for (j=0; j<m->n_cols(); j++)
      mat.set_column(next++, F->component_shift(i * m->n_rows(), m->rows(), (*m)[j]));

  for (i=0; i<m->n_rows(); i++)
    for (j=0; j<n_cols(); j++)
      mat.set_column(next++, F->tensor_shift(m->n_rows(), i, rows(), elem(j)));
  return mat.to_matrix();

#if 0
  Matrix *result = new Matrix(F, G);

  int i, j, next=0;

  for (i=0; i<n_rows(); i++)
    for (j=0; j<m->n_cols(); j++)
      (*result)[next++] = F->component_shift(i * m->n_rows(), m->rows(), (*m)[j]);

  for (i=0; i<m->n_rows(); i++)
    for (j=0; j<n_cols(); j++)
      (*result)[next++] = F->tensor_shift(m->n_rows(), i, rows(), elem(j));

  return result;
#endif
}
#if 0
// REMOVE THIS ONE??
Matrix *Matrix::random(const Ring *R, int r, int c)
{
  FreeModule *F = R->make_FreeModule(r);
  FreeModule *G = R->make_FreeModule(c);
  Matrix *result = new Matrix(F,G);
  for (int i=0; i<c; i++)
    (*result)[i] = F->random();
  return result;
}
#endif

Matrix *Matrix::random(const Ring *R, 
		       int r, int c, 
		       double fraction_non_zero, 
		       int special_type, // 0: general, 1:upper triangular, others?
		       M2_bool is_mutable_flag)
{
  FreeModule *F = R->make_FreeModule(r);
  FreeModule *G = R->make_FreeModule(c);
  MatrixConstructor mat(F,G,R->degree_monoid()->make_one(),is_mutable_flag);

  // Loop through all selected elements, flip a 'fraction_non_zero' coin, and if non-zero
  // set that element.
  
  mpz_t a;
  mpz_init(a);

  if (special_type == 0)
    {
      for (int i=0; i<c; i++)
	for (int j=0; j<r; j++)
	  {
	    Random::random_integer(a);
	    mat.set_entry(j,i,R->from_int(a));
	  }
    }
  else if (special_type == 1)
    {
      for (int i=0; i<c; i++)
	{
	  int top = (i>=r ? r : i);
	  for (int j=0; j<top; j++)
	    {
	      Random::random_integer(a);
	      mat.set_entry(j,i,R->from_int(a));
	    }
	}

    }
  mpz_clear(a);
  return mat.to_matrix();
}


Matrix *Matrix::tensor(const Matrix *m) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("matrix tensor: different base rings");
      return 0;
    }

  const FreeModule *F = rows()->tensor(m->rows());
  const FreeModule *G = cols()->tensor(m->cols());
  int *deg = degree_monoid()->make_new(degree_shift());
  degree_monoid()->mult(deg, m->degree_shift(), deg);

  MatrixConstructor mat(F,G,deg,is_mutable() && m->is_mutable());
  degree_monoid()->remove(deg);
  int i, j, next = 0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m->n_cols(); j++)
      mat.set_column(next++, F->tensor(rows(), elem(i), 
				       m->rows(), (*m)[j]));
  return mat.to_matrix();
#if 0
  Matrix *result = new Matrix(F, G, deg);

  degree_monoid()->remove(deg);

  int i, j, next = 0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m->n_cols(); j++)
      (*result)[next++] = F->tensor(rows(), elem(i), 
				 m->rows(), (*m)[j]);
				 
  return result;
#endif
}

Matrix *Matrix::diff(const Matrix *m, int use_coef) const
{
  if (get_ring() != m->get_ring())
    {
      ERROR("matrix diff: different base rings");
      return 0;
    }
  FreeModule *F1 = rows()->transpose();
  const FreeModule *F = F1->tensor(m->rows());
  FreeModule *G1 = cols()->transpose();
  const FreeModule *G = G1->tensor(m->cols());
  int *deg = degree_monoid()->make_one();
  degree_monoid()->divide(m->degree_shift(), degree_shift(), deg);
  deleteitem(F1);
  deleteitem(G1);

  MatrixConstructor mat(F,G,deg,is_mutable() && m->is_mutable());
  degree_monoid()->remove(deg);
  int i, j, next=0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m->n_cols(); j++)
      mat.set_column(next++, F->diff(rows(), elem(i), 
				     m->rows(), (*m)[j],
				     use_coef));
  return mat.to_matrix();
#if 0
  Matrix *result = new Matrix(F, G, deg);
  degree_monoid()->remove(deg);
  int i, j, next=0;
  for (i=0; i<n_cols(); i++)
    for (j=0; j<m->n_cols(); j++)
      (*result)[next++] = F->diff(rows(), elem(i), 
				  m->rows(), (*m)[j],
				  use_coef);

  return result;
#endif
}

Matrix *Matrix::lead_term(int n) const
    // Select those monomials in each column
    // which are maximal in the order under
    // the first n weight vectors, where the
    // component slot is considered as the nvars+1 st weight
    // vector.
{
#warning "implement Matrix::lead_term(int n)"
#if 0
  // MES aug 2002
  Matrix *result = new Matrix(rows(), cols(), degree_shift());
  for (int i=0; i<n_cols(); i++)
    (*result)[i] = rows()->lead_term(n, elem(i));
  return result;
#endif
  return 0;
}

#if 0
void Matrix::minimal_lead_terms_ZZ(intarray &result) const
{
  int x;
  M2_arrayint indices;
  array<TermIdeal *> mis;
  const array<vec> vecs = _entries;
  indices = rows()->sort(vecs, NULL, 0, 1);
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  const FreeModule *Rsyz = P->get_Rsyz(); // NULL if not a quotient ring.
  FreeModule *Gsyz = P->make_FreeModule(vecs.length());
  for (x=0; x<n_cols(); x++)
    mis.append(new TermIdeal(P,Gsyz));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices->array[i]];
      vec gsyz, rsyz;
      if (v == NULL) continue;
      if (TI_TERM != mis[v->comp]->search(v->coeff, v->monom, gsyz, rsyz))
	{
	  mis[v->comp]->insert_minimal(
				       new tagged_term(P->Ncoeffs()->copy(v->coeff),
						       P->Nmonoms()->make_new(v->monom),
						       NULL,
						       NULL));
	  result.append(indices->array[i]);
	}
      Gsyz->remove(gsyz);
      if (rsyz != NULL) Rsyz->remove(rsyz);
    }
  for (x=0; x<n_cols(); x++)
    deleteitem(mis[x]);
}
#endif
#if 0
// OLDER THAN THE PREVIOUS VERSION!!
Matrix Matrix::minimal_lead_terms_ZZ() const
{
  int x;
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  FreeModule *Gsyz = P->make_FreeModule(n_cols());
  bump_up(Gsyz);
  array< queue<tagged_term *> > allterms;
  for (int i=0; i<n_cols(); i++)
    {
      vec v = elem(i);
      if (v == NULL) continue;
      allterms[v->comp].insert(
			       new tagged_term(P->Ncoeffs()->copy(v->coeff),
					       P->Nmonoms()->make_new(v->monom),
					       Gsyz->e_sub_i(i),
					       NULL));
    }
  Matrix result(rows());
  for (x=0; x<n_cols(); x++)
    {
      if (allterms[x].length() > 0)
	{
	  TermIdeal *ti = TermIdeal::make_termideal(P,Gsyz,allterms[x]);
	  // Loop through and add the corresponding elements in...
	  for (cursor_TermIdeal k(ti); k.valid(); ++k)
	    {
	      tagged_term *t = *k;
	      vec gsyz = t->_gsyz;
	      vec v = NULL;
	      rows()->apply_map(v, gsyz, entries);
	      rows()->negate_to(v);
	      result.append(v);
	    }
	  deleteitem(ti);
	}
    }
  bump_down(Gsyz);
  return result;
}
#endif

#if 0
void Matrix::minimal_lead_terms(intarray &result) const
{
  if (get_ring()->Ncoeffs()->is_ZZ())
    {
      minimal_lead_terms_ZZ(result);
      return;
    }
  M2_arrayint indices;
  intarray vp;
  array<MonomialIdeal *> mis;
  const array<vec> vecs = _entries;
  indices = rows()->sort(vecs, NULL, 0, 1);
  for (int x=0; x<n_rows(); x++)
    mis.append(new MonomialIdeal(get_ring()));
  for (int i=0; i<vecs.length(); i++)
    {
      vec v = vecs[indices->array[i]];
      if (v == NULL) continue;
      // Reduce each one in turn, and replace.
      Bag *junk_bag;
      vp.shrink(0);
      rows()->lead_varpower(v, vp);
      if (!mis[v->comp]->search(vp.raw(),junk_bag))
	{
	  Bag *b = new Bag(indices->array[i], vp);
	  mis[v->comp]->insert(b);
	  result.append(indices->array[i]);
	}
    }
}

Matrix *Matrix::lead_var_coefficient(Matrix *&monoms) const
{
  Matrix *result = new Matrix(rows());
  monoms = new Matrix(get_ring()->make_FreeModule(1));
  int var, exp;
  for (int i=0; i<n_cols(); i++)
    {
      vec u = elem(i);
      vec v = rows()->lead_var_coefficient(u, var, exp);
      result->append(v);
      ring_elem a = get_ring()->var(var,exp);
      vec w = monoms->rows()->term(0,a);
      get_ring()->remove(a);
      monoms->append(w);
    }
  return result;
}
#endif

M2_arrayint Matrix::elim_vars(int nparts) const
{
  intarray keep;
  for (int i=0; i<n_cols(); i++)
    if (rows()->in_subring(nparts, elem(i)))
      keep.append(i);
  M2_arrayint result = makearrayint(keep.length());
  for (unsigned int i=0; i<result->len; i++)
    result->array[i] = keep[i];
  return result;
}

M2_arrayint Matrix::elim_keep(int nparts) const
{
  intarray keep;
  for (int i=0; i<n_cols(); i++)
    if (!rows()->in_subring(nparts, elem(i)))
      keep.append(i);
  M2_arrayint result = makearrayint(keep.length());
  for (unsigned int i=0; i<result->len; i++)
    result->array[i] = keep[i];
  return result;
}

Matrix *Matrix::divide_by_var(int n, int maxd, int &maxdivided) const
{
  MatrixConstructor mat(rows(), 0, false);
  maxdivided = 0;
  for (int i=0; i<n_cols(); i++)
    {
      if (elem(i) != NULL)
	{
	  int lo,hi;
	  rows()->degree_of_var(n, elem(i), lo,hi);
	  if (maxd >= 0 && lo > maxd)
	    lo = maxd;
	  if (lo > maxdivided)
	    maxdivided = lo;
	  mat.append(rows()->divide_by_var(n, lo, elem(i)));
	}
    }
  return mat.to_matrix();
}

// ideal operations
MatrixOrNull *Matrix::koszul(int p) const
{
  if (n_rows() != 1)
    {
      ERROR("expected a matrix with one row");
      return 0;
    }
  
  FreeModule *F = cols()->exterior(p-1);
  FreeModule *G = cols()->exterior(p);
  const Ring *R = get_ring();
  MatrixConstructor mat(F,G,degree_shift(),false);
  if (p <= 0 || p > n_cols()) return mat.to_matrix();
  int *a = newarray(int,p);
  for (int c=0; c < G->rank(); c++)
    {
      comb::decode(c, a,p);
      int negate = ((p % 2) != 0);
      for (int r=p-1; r>=0; r--)
	{
	  negate = !negate;
	  swap(a[p-1], a[r]);
	  int x = comb::encode(a, p-1);
	  ring_elem f = elem(0, a[p-1]);
	  if (negate)
	    R->negate_to(f);

	  mat.set_entry(x,c,f);
	}
    }
  deletearray(a);
  return mat.to_matrix();
}

static int signdivide(int n, const int *a, const int *b, int *exp)
{
  int sign = 0;
  int sum = 0;
  for (int i=0; i<n; i++)
    {
      int e = a[i] - b[i];
      if (e < 0) return 0;
      exp[i] = e;
      sign += sum*e;
      sum += b[i];
    }
  sign %= 2;
  if (sign == 0) return 1;
  return -1;
}
MatrixOrNull *Matrix::koszul(const Matrix *r, const Matrix *c)
{
  // First check rings: r,c,'this' should be row vectors.
  // and the ring should be a polynomial ring
  const FreeModule *F = r->cols();


  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  if (P == NULL) return 0;
  const Monoid *M = P->Nmonoms();

  MatrixConstructor mat(F, c->cols(), P->degree_monoid()->make_one(), false);

  int nvars = F->get_ring()->n_vars();
  int nrows = r->n_cols();
  int ncols = c->n_cols();
  int *aexp = newarray(int,nvars);
  int *bexp = newarray(int,nvars);
  int *result_exp = newarray(int,nvars);
  for (int i=0; i<ncols; i++)
    {
      if (c->elem(i) == 0) continue;
      const int *a = P->lead_monomial(c->elem(i)->coeff);
      M->to_expvector(a, aexp);
      for (int j=0; j<nrows; j++)
	{
	  if (r->elem(j) == 0) continue;
	  const int *b = P->lead_monomial(r->elem(j)->coeff);
	  M->to_expvector(b, bexp);
	  int sign = signdivide(nvars, aexp, bexp, result_exp);
	  if (sign != 0)
	    {
	      const int *m;
#warning "does m need to be initialized?"
	      M->from_expvector(m, result_exp);
	      ring_elem s = (sign > 0 ? P->Ncoeffs()->one() : P->Ncoeffs()->minus_one());
	      ring_elem f = P->term(s,m);
	      mat.set_entry(j,i,f);
	    }
	}
    }
  deletearray(aexp);
  deletearray(bexp);
  deletearray(result_exp);
  return mat.to_matrix();
}

Matrix *Matrix::wedge_product(int p, int q, const FreeModule *F)
{
  const FreeModule *Fp = F->exterior(p);
  const FreeModule *Fq = F->exterior(q);
  const FreeModule *Fn = F->exterior(p+q);
  const FreeModule *G = Fp->tensor(Fq);
  const Ring *R = F->get_ring();

  MatrixConstructor mat(Fn,G,Fn->get_ring()->degree_monoid()->make_one(),false);

  if (p < 0 || q < 0 || p+q >F->rank())
    return mat.to_matrix();

  if (p == 0 || q == 0)
    {
      for (int i=0; i<G->rank(); i++)
	mat.set_entry(i,i,R->one());
      return mat.to_matrix();
    }

  int *a = newarray(int,p);
  int *b = newarray(int,q);
  int *c = newarray(int,p+q);
  int col = 0;

  for (int i=0; i<Fp->rank(); i++)
    {
      comb::decode(i, a, p);
      for (int j=0; j<Fq->rank(); j++)
	{
	  comb::decode(j, b, q);
	  int sgn = comb::mult_subsets(p,a,q,b,c);
	  if (sgn == 0)
	    {
	      col++;
	      continue;
	    }
	  ring_elem r = F->get_ring()->from_int(sgn);
	  int row = comb::encode(c,p+q);
	  mat.set_entry(row,col++,r);
	}
    }

  deletearray(a);
  deletearray(b);
  deletearray(c);
  return mat.to_matrix();
}

void Matrix::text_out(buffer &o) const
{
  int nrows = n_rows();
  int ncols = n_cols();
//  o << "#rows = " << nrows << " and #cols = " << ncols << endl;
//  o << "rows = ";
//  rows().text_out(o);
//  o << endl << "cols = ";
//  cols().text_out(o);
//  o << endl;

  buffer *p = newarray(buffer,nrows);
  //  buffer *p = new buffer[nrows];
  int r;
  for (int c=0; c<ncols; c++)
    {
      int maxcount = 0;
      for (r=0; r<nrows; r++)
	{
	  ring_elem f = elem(r,c);
	  get_ring()->elem_text_out(p[r], f);
	  get_ring()->remove(f);
	  if (p[r].size() > maxcount)
	    maxcount = p[r].size();
	}
      for (r=0; r<nrows; r++)
	for (int k=maxcount+1-p[r].size(); k > 0; k--)
	  p[r] << ' ';
    }
  for (r=0; r<nrows; r++)
    {
      p[r] << '\0';
      char *s = p[r].str();
      o << s << newline;
    }
  deletearray(p);
}

#if 0
int Matrix::moneq(const int *exp, int *m, const int *vars, int *exp2) const
    // Internal private routine for 'coeffs'.
    // exp2 is a scratch value.  It is a paramter so we only have to allocate 
    // it once...
{
  get_ring()->Nmonoms()->to_expvector(m, exp2);
  int nvars = get_ring()->n_vars();
  for (int i=0; i<nvars; i++)
    {
      if (vars[i] == 0) continue;
      if (exp[i] != exp2[i]) 
	return 0;
      else 
	exp2[i] = 0;
    }
  get_ring()->Nmonoms()->from_expvector(exp2, m);
  return 1;
}
vec Matrix::strip_vector(vec &f, const int *vars, 
			      const FreeModule *F, vec &vmonom) const
    // private routine for 'coeffs'
{
  if (f == NULL) 
    {
      vmonom = NULL;
      return NULL;
    }
  if (get_ring()->Nmonoms() == NULL)
    {
      vmonom = F->e_sub_i(0);
      vec result = f;
      f = NULL;
      return result;
    }
  // At this point, we know that we have a polynomial ring
  int nvars = get_ring()->n_vars();
  int *exp = newarray(int,nvars);
  int *scratch_exp = newarray(int,nvars);
  const Monoid *M = get_ring()->Nmonoms();

  M->to_expvector(f->monom, exp);
  for (int i=0; i<nvars; i++)
    if (vars[i] == 0) exp[i] = 0;

  // the following two lines do NOT work if 'F' is a Schreyer free module,
  // but this routine is private to 'coeffs', where this is not the case.
  vmonom = F->e_sub_i(0);
  M->from_expvector(exp, vmonom->monom);

  vecterm head;
  vecterm *newf = &head;
  vec result = NULL;

  // Loop through f: if monomial matches 'exp', strip and add to result,
  // otherwise leave alone, and place on head list.
  while (f != NULL)
    {
      if (moneq(exp, f->monom, vars, scratch_exp))
	{
	  vec temp = f;
	  f = f->next;
	  temp->next = NULL;
	  rows()->add_to(result, temp);
	}
      else
	{
	  newf->next = f;
	  f = f->next;
	  newf = newf->next;
	  newf->next = NULL;
	}
    }
  newf->next = NULL;
  f = head.next;

  deletearray(exp);
  deletearray(scratch_exp);
  return result;
}
#endif
#if 0
// MES Aug 2002
Matrix *Matrix::simplify(int n) const
{
  int i,j, keep;
  Matrix *result = new Matrix(rows());

  switch (n) {
  case 1:
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result->append(rows()->copy(f));
      }
    break;
    //  case SIMP_SCALAR_MULTIPLES:
  case 2:
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	keep = 1;
	for (j=i+1; j<n_cols(); j++)
	  {
	    vec g = elem(j);
	    if (g == NULL) continue;
	    if (rows()->is_scalar_multiple(f, g))
	      {
		keep = 0;
		break;
	      }
	  }
	if (keep) result->append(rows()->copy(f));
      }
    break;
  case 3:
    // Remove multiple monomial divisors (i.e. x^2*f --> x*f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result->append(rows()->monomial_squarefree(f));
      }
    break;
  case 4:
    // Remove monomial divisors (i.e. x*f --> f)
    for (i=0; i<n_cols(); i++)
      {
	vec f = elem(i);
	if (f == NULL) continue;
	result->append(rows()->remove_monomial_divisors(f));
      }
    break;
#if 0
  case SIMP_ZEROS:
    break;
  case SIMP_MULTIPLES:
    break;
  case SIMP_AUTO_REDUCE:
    break;
  case SIMP_SQUAREFREE:
    break;
  case SIMP_MONOMIAL_DIVISORS:
    break;
#endif
  default:
    ERROR("bad simplification type");
    return 0;
  }

  return result;
}
#endif
#if 0
// MES Aug 2002
Matrix *Matrix::auto_reduce() const
{
  array<vec> vecs;
  int i;
  for (i=0; i<n_cols(); i++)
    vecs.append(rows()->copy(elem(i)));
  rows()->auto_reduce(vecs);
  Matrix *result = new Matrix(rows());
  for (i=0; i<vecs.length(); i++)
    result->append(vecs[i]);
  return result;
}
#endif

M2_arrayint Matrix::sort(int degorder, int monorder) const
  // Sort the columns of 'this': Place the column indices into 'result'.
  // If degorder < 0, sort in descending degree order, if >0 ascending degree
  // If ==0, or in the event that two columns have the same (simple) degree,
  // use the monomial order: monorder > 0 means ascending, <0 means descending.
{
  M2_arrayint_OrNull degs = 0;

  if (degorder != 0)
    {
      degs = makearrayint(n_cols());
      for (int i=0; i<n_cols(); i++)
	degs->array[i] = cols()->primary_degree(i);
    }

  return rows()->sort(_entries, degs, degorder, monorder);
}

#if 0
Matrix *Matrix::coeffs(const int *vars, Matrix * &result_monoms) const
{
  Matrix *result_coeffs = new Matrix(rows());
  result_monoms = new Matrix(get_ring()->make_FreeModule(1));	// One row matrix
  for (int j=0; j<n_cols(); j++)
    {
      vec f = rows()->copy(elem(j));
      vec vmonom;
      while (f != NULL)
	{
	  vec g = strip_vector(f, vars, result_monoms->rows(), vmonom);
	  result_coeffs->append(g);
	  result_monoms->append(vmonom);
	}
    }
  // MES: now sort both matrices...
  return result_coeffs;
}
#endif

MatrixOrNull *Matrix::monomials(M2_arrayint vars) const
  // Returns a one row matrix of all of the monomials in the variable subset 'vars'
  // which occur in 'this'.  These monomials are sorted into increasing degree order.
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected a matrix over a polynomial ring");
      return 0;
    }
  const Monoid *M = P->Nmonoms();
  int nvars = M->n_vars();
  // Check that 'vars' is valid
  for (unsigned int i=0; i<vars->len; i++)
    if (vars->array[i] < 0 || vars->array[i] >= nvars)
      {
	ERROR("expected a list of indices of indeterminates");
	return 0;
      }

  // Now collect all of the monomials
  int *mon = M->make_one();
  int *exp = newarray(int,M->n_vars());
  ring_elem one = P->Ncoeffs()->from_int(1);
  exponent_table *E = exponent_table_new(50000, vars->len);

  for (int c=0; c<n_cols(); c++)
    {
      vec v = elem(c);
      for ( ; v != 0; v = v->next)
	{
	  for (Nterm *t = v->coeff; t != 0; t = t->next)
	    {
	      int *exp1 = newarray(int,vars->len);
	      M->to_expvector(t->monom, exp);
	      for (unsigned int i=0; i<vars->len; i++)
		exp1[i] = exp[vars->array[i]];
	      exponent_table_put(E, exp1, 1);
	    }
	}
    }

  // Take all of these monomials and make an array out of them
  MatrixConstructor mat(get_ring()->make_FreeModule(1),0,false);
  const void ** monoms = exponent_table_to_array(E);
  for (int i=0; i<nvars; i++) exp[i] = 0;
  for (int i=0; monoms[i] != 0; i += 2)
    {
      const int * exp1 = (const int *) monoms[i];
      for (unsigned int j=0; j<vars->len; j++)
	exp[vars->array[j]] = exp1[j];
      M->from_expvector(exp, mon);
      ring_elem a = P->term(one, mon);
      mat.append(rows()->raw_term(a,0));
    }
  
  // Remove the garbage memory
  deletearray(exp);
  M->remove(mon);
  exponent_table_free(&E);

  // Finally, we sort them
  Matrix *result = mat.to_matrix();
  M2_arrayint perm = result->sort(1,1);
  return result->sub_matrix(perm);
}

static vec coeffs_of_vec(exponent_table *E, M2_arrayint vars,
			 const FreeModule *F, vec f)
    // private routine for 'coeffs'.
{
  if (f == NULL) return 0;
  const PolynomialRing *P = F->get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    return 0;
  const Monoid *M = P->Nmonoms();
  int *mon = M->make_one();

  // At this point, we know that we have a polynomial ring
  int nvars = M->n_vars();
  int *exp = newarray(int,nvars);
  int *scratch_exp = newarray(int,nvars);

  vec result = 0;
  for (vec g = f ; g != 0; g = g->next)
    {
      for (Nterm *h = g->coeff; h != 0; h = h->next)
	{
	  M->to_expvector(h->monom, exp);
	  for (unsigned int i=0; i<vars->len; i++)
	    {
	      int v = vars->array[i];
	      scratch_exp[i] = exp[v];
	      exp[v] = 0;
	    }
	  int val = exponent_table_get(E, scratch_exp);
	  if (val > 0)
	    {
	      M->from_expvector(exp, mon);
	      ring_elem t = P->term(h->coeff, mon);
	      vec v = F->raw_term(t,val-1);
	      v->next = result;
	      result = v;
	    }
	}
    }
  deletearray(exp);
  deletearray(scratch_exp);
  M->remove(mon);
  F->sort(result);
  return result;
}

MatrixOrNull *Matrix::coeffs(M2_arrayint vars, const M2_arrayint monoms) const
{
  // Given an array of variable indices, 'vars', and given
  // that 'monoms' and 'this' both have one row, makes a matrix
  // having number of rows = ncols(monoms), 
  //        number of cols = ncols(this),
  // whose (r,c) entry is the coefficient (in the other variables)
  // of this[0,c] in the monomial monoms[0,r].


  // Step 0: Do some error checking
  int nvars = get_ring()->n_vars();
  int nelements = monoms->len / vars->len;
  if (nelements * vars->len != monoms->len)
    {
      ERROR("coeffs: expected an array of exponents");
      return 0;
    }
  for (unsigned int i=0; i<vars->len; i++)
    if (vars->array[i] < 0 || vars->array[i] >= nvars)
      {
	ERROR("coeffs: expected a set of variable indices");
	return 0;
      }

  // Step 1: Make an exponent_table of all of the monoms.
  // We set the value of the i th monomial to be 'i+1', since 0
  // indicates a non-existent entry.

  exponent_table *E = exponent_table_new(nelements, vars->len);
  for (int i=0; i<nelements; i++)
    exponent_table_put(E, monoms->array + i*(vars->len), i+1);

  // Step 2: for each vector column of 'this'
  //     create a column, and put this vector into result.

  MatrixConstructor mat(get_ring()->make_FreeModule(nelements), 0 , false);
  for (int i=0; i<n_cols(); i++)
    mat.append(coeffs_of_vec(E, vars, rows(), elem(i)));

  return mat.to_matrix();
}

MonomialIdeal *Matrix::make_monideal(int n) const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P == 0)
    {
      ERROR("expected polynomial ring");
      return 0;
    }
  const Monoid *M = P->Nmonoms();
  queue <Bag *> new_elems;
  for (int i=0; i<n_cols(); i++)
    {
      vec v = elem(i);
      if (rows()->is_zero(v)) continue;
      if (v->comp != n) continue;
      Bag *b = new Bag(i);
      M->to_varpower(P->lead_monomial(v->coeff), b->monom());
      new_elems.insert(b);      
    }

#warning "make_monideal doesn't handle quotient rings"
#if 0
  // If the base ring is a quotient ring, include these lead monomials.
  if (P->is_quotient_ring())
    {
      const MonomialIdeal *Rideal = P->get_quotient_monomials();
      for (Index<MonomialIdeal> j = Rideal->first(); j.valid(); j++)
	{
	  Bag *b = new Bag(-1, (*Rideal)[j]->monom());
	  new_elems.insert(b);
	}
    }
#endif

  MonomialIdeal *result = new MonomialIdeal(get_ring(), new_elems);
  return result;
}

MonomialIdeal *Matrix::make_skew_monideal(int n) const
{
  MonomialIdeal *result = make_monideal(n);
  const Monoid *M = get_ring()->Nmonoms();
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  if (P != 0 && P->is_skew_commutative())
    {
      intarray vp;
      for (int i=0; i<M->n_vars(); i++)
	if (P->is_skew_var(i))
	  {
	    vp.shrink(0);
	    varpower::var(i,2,vp);
	    Bag *b = new Bag(-1, vp);
	    result->insert_minimal(b);
	  }
    }
  return result;
}



int Matrix::dimension() const
{
  const PolynomialRing *P = get_ring()->cast_to_PolynomialRing();
  const Ring *K = get_ring()->Ncoeffs();
  bool is_ZZ = K->is_ZZ();
  int base = (is_ZZ ? 1 : 0);
  int result = -1;
  if (P != 0)
    {
      int n = get_ring()->n_vars();
      for (int i=0; i<n_rows(); i++)
	{
	  MonomialIdeal *mi = make_skew_monideal(i);
	  AssociatedPrimes ap(mi);
	  int d = n - ap.codimension();
	  if (d > result) result = d;
	}
      if (result != -1) result += base;
      return result;
    }
  else
    {
      // This handles the case when the coefficients are a field, or ZZ
      int i,j;
      int *dims = newarray(int,n_rows());
      for (i=0; i<n_rows(); i++)
	dims[i] = base;
      for (j=0; j<n_cols(); j++)
	{
	  vec f = elem(j);
	  if (f == 0) continue;
	  if (dims[f->comp] == -1) continue;
	  if (K->is_unit(f->coeff))
	    dims[f->comp] = -1;
	  else
	    dims[f->comp] = 0;
	}
      for (i=0; i<n_rows(); i++)
	if (dims[i] > result) result = dims[i];
      deletearray(dims);
      return result;
    }
}

#if 0

static int            kb_do_trunc;
static Matrix       * kb_result;
static MonomialIdeal* kb_monideal;
static int          * kb_deg;
static vec            kb_vec;
static int            kb_n_vars;
static int          * kb_exp;
static int          * kb_mon;
static int          * kb_vec_monom;
static int          * kb_exp_degree;
static const Monoid * kb_D;
static const PolynomialRing * kb_P;

void Matrix::k_basis_insert() const
{
  get_ring()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  get_ring()->Nmonoms()->divide(kb_mon, kb_vec_monom, kb_mon);
  ring_elem tmp = get_ring()->term(get_ring()->Ncoeffs()->from_int(1), kb_mon);
  kb_result->append(rows()->mult(tmp, kb_vec));
  get_ring()->remove(tmp);
}
void Matrix::k_basis0(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (kb_P->is_skew_commutative() &&
	    kb_P->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      kb_D->mult(kb_exp_degree, get_ring()->Nmonoms()->degree_of_var(i),
		     kb_exp_degree);

      int cmp = kb_D->primary_value(kb_exp_degree) - kb_D->primary_value(kb_deg);
      Bag *b;
      if (cmp > 0 
	  && kb_do_trunc 
	  && !kb_monideal->search_expvector(kb_exp,b))
	{
	  k_basis_insert();
	}

      if (cmp <= 0 && !kb_monideal->search_expvector(kb_exp,b))
	{
	  if (cmp == 0)
	    {
	      if (kb_D->compare(kb_exp_degree, kb_deg) == EQ)
		{
		  k_basis_insert();
		}
	    }
	  else
	    k_basis0(i);
	}

      kb_exp[i]--;
      kb_D->divide(kb_exp_degree, get_ring()->Nmonoms()->degree_of_var(i),
		   kb_exp_degree);
    }
}

Matrix *Matrix::k_basis(Matrix &bot, const int *d, int do_trunc) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // Find a basis for (image this)/(image bottom) in degree d.
    // If 'd' is NULL, first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
    // If 'd' is not NULL, it is an element of the degree monoid.
{
  kb_do_trunc = do_trunc;
  kb_result = new Matrix(rows());
  kb_n_vars = get_ring()->n_vars();
  kb_D = get_ring()->degree_monoid();

  kb_mon = get_ring()->Nmonoms()->make_one();
  kb_vec_monom = get_ring()->Nmonoms()->make_one();
  kb_deg = kb_D->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);
  kb_exp_degree = kb_D->make_one();

  int *e = kb_D->make_one();
  kb_D->from_expvector(d, e);

  kb_P = get_ring()->cast_to_PolynomialRing();
  if (kb_P != 0)
    {
      for (int i=0; i<n_rows(); i++)
	{
	  degree_monoid()->divide(e, rows()->degree(i), kb_deg);
	  
	  // get the two monomial ideals
	  MonomialIdeal *top = make_monideal(i);
	  MonomialIdeal *bottom = bot.make_monideal(i);
	  top = *top - *bottom;
	  
	  Bag *b;
	  while (top->remove(b))
	    {
	      kb_vec = elem(b->basis_elem());
	      get_ring()->Nmonoms()->from_varpower(b->monom().raw(),kb_vec_monom);
	      
	      MonomialIdeal *miq = top->intersect(b->monom().raw());
	      
	      kb_monideal = *miq + *bottom;
	      
	      kb_exp_a.shrink(0);
	      varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	      get_ring()->Nmonoms()->degree_of_varpower(b->monom().raw(), 
							kb_exp_degree);
	      
	      int cmp = kb_D->primary_value(kb_exp_degree) 
		- kb_D->primary_value(kb_deg);
	      if ((cmp > 0 && do_trunc)
		  || (0 == kb_D->compare(kb_deg, kb_exp_degree)))
		kb_result->append(rows()->copy(kb_vec));
	      else if (cmp < 0)
		k_basis0(0);
	      
	      deleteitem(b);
	    }
	  //	}
	  //      else if (do_trunc)
	  //	{
	  //	  kb_result.append(rows()->copy(elem(i)));
	  //	}
	}
    }
  
  Matrix *result = kb_result;

  kb_D->remove(kb_deg);
  kb_D->remove(kb_exp_degree);
  kb_D->remove(e);
  get_ring()->Nmonoms()->remove(kb_mon);
  get_ring()->Nmonoms()->remove(kb_vec_monom);  
  kb_result = 0;
  kb_P = 0;

  return result;
}


void Matrix::k_basis1(int firstvar) const
    // Recursively add to the result matrix all monomials in the
    // variables 0..topvar having degree 'deg' which are not in 'mi'.
{
  get_ring()->Nmonoms()->from_expvector(kb_exp, kb_mon);
  get_ring()->Nmonoms()->divide(kb_mon, kb_vec_monom, kb_mon);
  ring_elem tmp = get_ring()->term(get_ring()->Ncoeffs()->from_int(1), kb_mon);
  kb_result->append(rows()->mult(tmp, kb_vec));
  get_ring()->remove(tmp);

  for (int i=firstvar; i<kb_n_vars; i++)
    {
      if (kb_P->is_skew_commutative() &&
	    kb_P->is_skew_var(i) &&
	    kb_exp[i] >= 1)
	{
	  continue;
	}

      kb_exp[i]++;
      Bag *b;
      if (!kb_monideal->search_expvector(kb_exp,b))
	k_basis1(i);

      kb_exp[i]--;
    }
}

Matrix *Matrix::k_basis(Matrix &bot) const
    // Only the lead monomials of the two matrices 'this' and 'bottom' are
    // considered.  Thus, you must perform the required GB's elsewhere.
    // first check that (image this)/(image bottom) has
    // finite dimension, and if so, return a basis.
{
  kb_result = new Matrix(rows());
  kb_n_vars = get_ring()->n_vars();

  kb_mon = get_ring()->Nmonoms()->make_one();
  kb_vec_monom = get_ring()->Nmonoms()->make_one();
  intarray kb_exp_a;
  kb_exp = kb_exp_a.alloc(kb_n_vars);

  kb_P = get_ring()->cast_to_PolynomialRing();
  if (kb_P != 0)
    {
      for (int i=0; i<n_rows(); i++)
	{
	  // get the two monomial ideals
	  MonomialIdeal *top = make_monideal(i);
	  MonomialIdeal *bottom = bot.make_monideal(i);
	  
	  Bag *b, *c;
	  while (top->remove(b))
	    {
	      kb_vec = elem(b->basis_elem());
	      get_ring()->Nmonoms()->from_varpower(b->monom().raw(),kb_vec_monom);
	      
	      MonomialIdeal *miq = top->intersect(b->monom().raw());
	      kb_monideal = *miq + *bottom;
	      
	      kb_exp_a.shrink(0);
	      varpower::to_ntuple(kb_n_vars, b->monom().raw(), kb_exp_a);
	      if (!kb_monideal->search(b->monom().raw(), c))
		k_basis1(0);
	      
	      deleteitem(b);
	    }
	}
    }
  get_ring()->Nmonoms()->remove(kb_mon);  
  get_ring()->Nmonoms()->remove(kb_vec_monom);  
  Matrix *result = kb_result;
  kb_result = 0;
  kb_P = 0;
  return result;
}
#endif

////////////////////////////////////////////////////////////////////////////////
#if 0
#if 0
To do here:
  +k-basis routines
  +monideal routines (done)
  elem(r,c) makes new element, so be sure to remove it if needed

  check error conditions
  lead term routines are a bit messed up

  new routines to add: 
    exterior, symm (how best to do this?)
    random_matrix
    coeffs, 
    inpart, stdpart, mininimalpart (but call them what?? )
#endif

#if 0
void Matrix::schreyer_append(vec v)
{
  if (! rows()->is_zero(v)) 
    {
      int *d = degree_monoid()->make_one();
      rows()->degree(v, d);
      cols()->append(d, v->monom, cols()->rank());
      degree_monoid()->remove(d);
      _entries.append(v);
    }
  else
    append(v);
}
#endif

#endif


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
