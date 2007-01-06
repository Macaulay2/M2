// Copyright 2005  Michael E. Stillman

#include "coeffrings.hpp"
#include "z_mod_p.hpp"
#include "smat.hpp"
#include "mat.hpp"

//////////////////////////
// sparsevec operations //
//////////////////////////

template<typename CoeffRing>
typename SMat<CoeffRing>::sparsevec *SMat<CoeffRing>::vec_new() const
{
  return new sparsevec;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_remove_node(sparsevec *&v) const
{
  deleteitem(v);
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_remove(sparsevec *&v) const
{
  while (v != 0)
    {
      sparsevec *tmp = v;
      v = v->next;
      vec_remove_node(tmp);
    }
}

template<typename CoeffRing>
typename SMat<CoeffRing>::sparsevec *SMat<CoeffRing>::vec_copy(const sparsevec *v) const
{
  sparsevec head;
  sparsevec *result = &head;
  for (const sparsevec *p = v; p != 0; p=p->next)
    {
      sparsevec *w = vec_new();
      result->next = w;
      result = w;
      w->row = p->row;
      coeffR->init_set(w->coeff, p->coeff);
    }
  result->next = 0;
  return head.next;
}

template<typename CoeffRing>
bool SMat<CoeffRing>::vec_get_entry(const sparsevec *v, long r, elem &result) const
{
  for (const sparsevec *p = v; p != 0; p = p->next)
    if (p->row < r)
      break;
    else if (p->row == r)
      {
	coeffR->init_set(result,p->coeff);
	return true;
      }
  return false;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_set_entry(sparsevec *&v, long r, const elem &a) const
{
  sparsevec * p;
  bool iszero = coeffR->is_zero(a);
  sparsevec head;
  head.next = v;
  for (p = &head; p->next != 0; p = p->next)
    if (p->next->row <= r)
      break;

  if (p->next == 0 || p->next->row < r)
    {
      if (iszero) return;
      sparsevec * w = vec_new();
      w->next = p->next;
      w->row = r;
      coeffR->init_set(w->coeff, a);
      p->next = w;
    }
  else if (p->next->row == r)
    {
      if (iszero)
	{
	  // delete node
	  sparsevec * tmp = p->next;
	  p->next = tmp->next;
	  vec_remove_node(tmp);
	}
      else
	p->next->coeff = a;
    }
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_interchange_rows(sparsevec *&v, long i, long j) const
{
  sparsevec *p;
  if (i == j) return;
  if (v == 0) return;
  if (i < j) 
    {
      long tmp = i;
      i = j;
      j = tmp;
    }
  // So now i > j.
  sparsevec head;
  head.next = v;
  sparsevec *vec1;
  sparsevec *vec2;
  for (p = &head; p->next != 0; p=p->next)
    if (p->next->row <= i)
      break;
  vec1 = p;
  for ( ; p->next != 0; p=p->next)
    if (p->next->row <= j)
      break;
  vec2 = p;
  if (vec1->next != 0 && vec1->next->row == i)
    {
      if (vec2->next != 0 && vec2->next->row == j)
	{
	  std::swap(vec1->next->coeff, vec2->next->coeff);
	  return;
	}
    }
  else if (vec2->next != 0 && vec2->next->row == j)
    {
      sparsevec *tmp = vec1;
      vec1 = vec2;
      vec2 = tmp;
      j = i;			// Used below.
    }
  else
    return;

  sparsevec *tmp = vec1->next;
  if (vec2 != tmp)
    {
      vec1->next = tmp->next;
      tmp->next = vec2->next;
      vec2->next = tmp;
    }
  tmp->row = j;
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_scale_row(sparsevec *&v, long r, const elem &a) const
{
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p = p->next)
    if (p->next->row < r) 
      break;
    else if (p->next->row == r)
      {
	coeffR->mult(p->next->coeff, a, p->next->coeff);
	if (coeffR->is_zero(p->next->coeff))
	  {
	    sparsevec *tmp = p->next;
	    p->next = tmp->next;
	    vec_remove_node(tmp);
	  }
	break;
      }
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_scale(sparsevec *&v, const elem &a) const
{
  if (coeffR->is_zero(a))
    {
      vec_remove(v);
      v = 0;
      return;
    }
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p=p->next)
    {
      coeffR->mult(p->next->coeff, a, p->next->coeff);
      if (coeffR->is_zero(p->next->coeff))
	{
	  sparsevec *tmp = p->next;
	  p->next = tmp->next;
	  vec_remove_node(tmp);
	  if (p->next == 0) break;
	}
    }
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_divide_row(sparsevec *&v, long r, const elem &a) const
{
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p = p->next)
    if (p->next->row < r) 
      break;
    else if (p->next->row == r)
      {
	coeffR->divide(p->next->coeff, p->next->coeff, a);
	if (coeffR->is_zero(p->next->coeff))
	  {
	    sparsevec *tmp = p->next;
	    p->next = tmp->next;
	    vec_remove_node(tmp);
	  }
	break;
      }
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_divide(sparsevec *&v, const elem &a) const
{
  if (coeffR->is_zero(a))
    {
      vec_remove(v);
      v = 0;
      return;
    }
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p=p->next)
    {
      coeffR->divide(p->next->coeff, p->next->coeff,a);
      if (coeffR->is_zero(p->next->coeff))
	{
	  sparsevec *tmp = p->next;
	  p->next = tmp->next;
	  vec_remove_node(tmp);
	  if (p->next == 0) break;
	}
    }
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_add_to(sparsevec *&v, sparsevec *&w) const
    // v := v+w, w := 0
{
  if (w == 0) return;
  if (v == 0) { v = w; w = 0; return; }
  sparsevec head;
  sparsevec *result = &head;
  while (true)
    if (v->row < w->row)
      {
	result->next = w;
	result = result->next;
	w = w->next;
	if (w == 0) 
	  {
	    result->next = v;
	    v = head.next;
	    return;
	  }
      }
    else if (v->row > w->row)
      {
	result->next = v;
	result = result->next;
	v = v->next;
	if (v == 0) 
	  {
	    result->next = w;
	    v = head.next;
	    w = 0;
	    return;
	  }
      }
    else
      {
	sparsevec *tmv = v;
	sparsevec *tmw = w;
	v = v->next;
	w = w->next;
	coeffR->add(tmv->coeff, tmv->coeff, tmw->coeff);
	if (coeffR->is_zero(tmv->coeff))
	  {
	    vec_remove_node(tmv);
	  }
	else
	  {
	    result->next = tmv;
	    result = result->next;
	  }
	vec_remove_node(tmw);
	if (w == 0) 
	  {
	    result->next = v;
	    v = head.next;
	    return;
	  }
	if (v == 0) 
	  {
	    result->next = w;
	    v = head.next;
	    w = 0;
	    return;
	  }
      }
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_row_op(sparsevec *&v, long r1, const elem &a, long r2) const
    // row(r1 in v) := row(r1 in v) + a * row(r2 in v)
{
  sparsevec *p;
  sparsevec *vec2 = 0;
  for (p = v; p != 0; p=p->next)
    if (p->row == r2)
      {
	vec2 = p;
	break;
      }
  if (vec2 == 0) return;
  elem c;
  coeffR->mult(c, vec2->coeff, a);
  if (coeffR->is_zero(c)) return; // nothing to change

  // Now add c to the r1'th row of v
  sparsevec head;
  head.next = v;
  for (p = &head; p->next != 0; p=p->next)
    if (p->next->row <= r1)
      break;
  if (p->next == 0 || p->next->row < r1)
    {
      // Make a new node
      sparsevec *w = vec_new();
      w->next = p->next;
      w->row = r1;
      coeffR->init_set(w->coeff, c);
      p->next = w;
    }
  else
    {
      coeffR->add(p->next->coeff, p->next->coeff, c);
      if (coeffR->is_zero(p->next->coeff))
	{
	  sparsevec *tmp = p->next;
	  p->next = tmp->next;
	  vec_remove_node(tmp);
	}
    }
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_row_op2(sparsevec *&v, 
		   long r1, long r2, 
		   const elem &a1, const elem &a2,
		   const elem &b1, const elem &b2) const
    // row(r1 in v) := a1 * row(r1 in v) + a2 * row(r2 in v)
    // row(r2 in v) := b1 * row(r1 in v) + b2 * row(r2 in v) (RHS refers to previous values)
{
  // v[row r1] = a1 * v[r1] + a2 * v[r2]
  // v[row r2] = b1 * v[r1] + b2 * v[r2]
  elem e1,e2, c1,c2,c3,c4;
  bool r1_nonzero = vec_get_entry(v,r1,e1);
  bool r2_nonzero = vec_get_entry(v,r2,e2);
  if (!r1_nonzero && !r2_nonzero) return;

  if (r1_nonzero)
    {
      coeffR->mult(c1, a1, e1);
      coeffR->mult(c3, b1, e1);
    }
  else
    {
      coeffR->set_zero(c1);
      coeffR->set_zero(c3);
    }
  if (r2_nonzero)
    {
      coeffR->mult(c2,a2,e2);
      coeffR->mult(c4,b2,e2);
    }
  else
    {
      coeffR->set_zero(c2);
      coeffR->set_zero(c4);
    }

  coeffR->add(c1,c1,c2);
  coeffR->add(c3,c3,c4);
  vec_set_entry(v,r1,c1);
  vec_set_entry(v,r2,c3);
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_column_op(sparsevec *&v, const elem &a, sparsevec *w) const
    // v := v + a*w
{
  sparsevec *w1 = vec_copy(w);
  vec_scale(w1, a);
  vec_add_to(v, w1);
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_dot_product(sparsevec *v, sparsevec *w, elem &result) const
{
  elem a;
  coeffR->set_zero(result);
  while (true)
    {
      if (v == 0) return;
      if (w == 0) return;
      if (v->row > w->row)
	v = v->next;
      else if (v->row < w->row)
	w = w->next;
      else
	{
	  coeffR->mult(a, v->coeff, w->coeff);
	  coeffR->add(result, result, a);
	  v = v->next;
	  w = w->next;
	}
    }
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_sort(sparsevec *&f) const
{
  if (f == 0 || f->next == 0) return;
  sparsevec *f1 = 0;
  sparsevec *f2 = 0;
  while (f != 0)
    {
      sparsevec *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == 0) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }
  
  vec_sort(f1);
  vec_sort(f2);
  vec_add_to(f1, f2);
  f = f1;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_permute(sparsevec *&v, long start_row, M2_arrayint perm) const
{
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_insert_rows(sparsevec *&v, long i, long n_to_add) const
{
  for (sparsevec *w = v; w != 0 && w->row >= i; w = w->next)
    w->row += n_to_add;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_delete_rows(sparsevec *&v, long i, long j) const
{
  long n_to_delete = j-i+1;
  sparsevec head;
  sparsevec *w = &head;
  w->next = v;
  while (w->next != 0 && w->next->row >= i)
    if (w->next->row <= j)
      {
	// this row is up for the chopping block
	sparsevec *tmp = w->next;
	w->next = tmp->next;
	vec_remove_node(tmp);
      }
    else
      {
	w = w->next;
	w->row -= n_to_delete;
      }
  v = head.next;
}

//////////////////////////
// SMat //////////////////
//////////////////////////
template<typename CoeffRing>
SMat<CoeffRing>::SMat(const RingType *R0, long nrows, long ncols)
  : R(R0),
    coeffR(R0->get_CoeffRing()),
    nrows_(nrows),
    ncols_(ncols)
{
  initialize(nrows,ncols,0);
}

template <> SMat<CoefficientRingR>::SMat(const Ring *R0, long nrows, long ncols)
  : R(R0),
    coeffR(0),
    nrows_(nrows),
    ncols_(ncols)
{
  coeffR = new CoefficientRingR(R0);
  initialize(nrows,ncols,0);
}

template<typename CoeffRing>
void SMat<CoeffRing>::initialize(long nrows, long ncols, sparsevec **cols)
{
  nrows_ = nrows;
  ncols_ = ncols;
  columns_ = newarray(sparsevec *,ncols);
  if (cols == 0)
    {
      for (long i=0; i<ncols; i++)
	columns_[i] = 0;
    }
  else
    {
      for (long i=0; i<ncols; i++)
	columns_[i] = vec_copy(*cols++);
    }
}

template<typename CoeffRing>
void SMat<CoeffRing>::grab(SMat<CoeffRing> *M)
{
  std::swap(R,M->R);
  std::swap(coeffR,M->coeffR);
  std::swap(nrows_,M->nrows_);
  std::swap(ncols_,M->ncols_);
  std::swap(columns_, M->columns_);
}

template<typename CoeffRing>
SMat<CoeffRing> *SMat<CoeffRing>::copy() const
{
  SMat<CoeffRing> *result = new SMat<CoeffRing>(get_ring(), 0, 0);
  result->initialize(nrows_, ncols_, columns_);
  return result;
}

template<typename CoeffRing>
long SMat<CoeffRing>::lead_row(long col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
{
  sparsevec *v = columns_[col];
  if (v == 0) return -1;
  return v->row;
}

template<typename CoeffRing>
long SMat<CoeffRing>::lead_row(long col, elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
{
  sparsevec *v = columns_[col];
  if (v == 0) return -1;
  coeffR->init_set(result, v->coeff);
  return v->row;
}

///////////////////////////////
// Row and column operations //
///////////////////////////////

template<typename CoeffRing>
bool SMat<CoeffRing>::get_entry(long r, long c, elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  return vec_get_entry(columns_[c], r, result);
}

template<typename CoeffRing>
void SMat<CoeffRing>::set_entry(long r, long c, const elem a)
{
  vec_set_entry(columns_[c], r, a);
}

template<typename CoeffRing>
void SMat<CoeffRing>::interchange_rows(long i, long j)
  /* swap rows: row(i) <--> row(j) */
{
  for (long c=0; c<ncols_; c++)
    vec_interchange_rows(columns_[c], i, j);
}

template<typename CoeffRing>
void SMat<CoeffRing>::interchange_columns(long i, long j)
  /* swap columns: column(i) <--> column(j) */
{
  sparsevec *tmp = columns_[i];
  columns_[i] = columns_[j];
  columns_[j] = tmp;
}

template<typename CoeffRing>
void SMat<CoeffRing>::scale_row(long i, elem r)
  /* row(i) <- r * row(i) */
{
  for (long c=0; c<ncols_; c++)
    vec_scale_row(columns_[c], i, r);
}  

template<typename CoeffRing>
void SMat<CoeffRing>::scale_column(long i, elem r)
  /* column(i) <- r * column(i) */
{
  vec_scale(columns_[i], r);
}

template<typename CoeffRing>
void SMat<CoeffRing>::divide_row(long i, elem r)
  /* row(i) <- row(i) / r */
{
  for (long c=0; c<ncols_; c++)
    vec_divide_row(columns_[c], i, r);
}

template<typename CoeffRing>
void SMat<CoeffRing>::divide_column(long i, elem r)
  /* column(i) <- column(i) / r */
{
  vec_divide(columns_[i], r);
}

template<typename CoeffRing>
void SMat<CoeffRing>::row_op(long i, elem r, long j)
  /* row(i) <- row(i) + r * row(j) */
{
  for (long c=0; c<ncols_; c++)
    vec_row_op(columns_[c], i, r, j);
}

template<typename CoeffRing>
void SMat<CoeffRing>::column_op(long i, elem r, long j)
  /* column(i) <- column(i) + r * column(j) */
{
  vec_column_op(columns_[i], r, columns_[j]);
}

template<typename CoeffRing>
void SMat<CoeffRing>::row2by2(long r1, long r2, 
	       elem a1, elem a2,
	       elem b1, elem b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  for (long c=0; c<ncols_; c++)
    vec_row_op2(columns_[c], r1,r2,a1,a2,b1,b2);
}

template<typename CoeffRing>
void SMat<CoeffRing>::column2by2(long c1, long c2,
		  elem a1, elem a2,
		  elem b1, elem b2)
  /* column(c1) <- a1 * column(c1) + a2 * column(c2),
     column(c2) <- b1 * column(c1) + b2 * column(c2)
  */
{
  // Make first column: v1 = a1*c1+a2*c2
  sparsevec *v1 = vec_copy(columns_[c1]);
  sparsevec *v2 = vec_copy(columns_[c2]);
  vec_scale(v1, a1);
  vec_scale(v2, a2);
  vec_add_to(v1,v2);

  // Second column: w1 = b1*c1 + b2*c2
  sparsevec *w1 = columns_[c1];
  sparsevec *w2 = columns_[c2];
  vec_scale(w1, b1);
  vec_scale(w2, b2);
  vec_add_to(w1,w2);

  // Set the matrices:
  columns_[c1] = v1;
  columns_[c2] = w1;
}

template<typename CoeffRing>
void SMat<CoeffRing>::dot_product(long i, long j, elem &result) const
{
  vec_dot_product(columns_[i], columns_[j], result);
}

template<typename CoeffRing>
bool SMat<CoeffRing>::row_permute(long start_row, M2_arrayint perm)
{
  // We copy one row to another location for each cycle in 'perm' of length > 1.
  long nrows_to_permute = perm->len;
  bool *done = newarray_atomic(bool,nrows_to_permute);
  for (long i=0; i<nrows_to_permute; i++)
    done[i] = true;
  for (long i=0; i<nrows_to_permute; i++)
    {
      long j = perm->array[i];
      if (!done[j])
	{
	  ERROR("expected permutation");
	  deletearray(done);
	  return false;
	}
      done[j] = false;
    }
  for (long c=0; c<ncols_; c++)
    vec_permute(columns_[c], start_row, perm);
  return true;
}

template<typename CoeffRing>
bool SMat<CoeffRing>::column_permute(long start_col, M2_arrayint perm)
{
  // We copy one column to another location for each cycle in 'perm' of length > 1.
  long ncols_to_permute = perm->len;
  bool *done = newarray_atomic(bool,ncols_to_permute);
  for (long i=0; i<ncols_to_permute; i++)
    done[i] = true;
  for (long i=0; i<ncols_to_permute; i++)
    {
      long j = perm->array[i];
      if (!done[j])
	{
	  ERROR("expected permutation");
	  deletearray(done);
	  return false;
	}
      done[j] = false;
    }
  sparsevec *tmp = 0;
  long next = 0;
  while (next < ncols_to_permute)
    {
      if (done[next] || perm->array[next] == next)
	{
	  next++;
	}
      else
	{
	  // store col 'next' into tmp
	  tmp = columns_[start_col + next];
	  long r = next;
	  for (;;)
	    {
	      // copy col perm[r] to col r
	      columns_[start_col + r] = columns_[start_col + perm->array[r]];
	      done[r] = true;
	      long next_r = perm->array[r];
	      if (next_r == next) break; // and so r is the previous one
	      r = perm->array[r];
	    }
	  // Now copy tmp back
	  columns_[r] = tmp;
	  done[r] = true;
	}
    }
  deletearray(tmp);
  deletearray(done);
  return true;
}

template<typename CoeffRing>
void SMat<CoeffRing>::insert_columns(long i, long n_to_add) 
/* Insert n_to_add columns directly BEFORE column i. */
{
  sparsevec **tmp = columns_;
  columns_ = 0;
  long orig_ncols = ncols_;

  initialize(nrows_, ncols_ + n_to_add, 0);
  for (long c=0; c<i; c++)
    columns_[c] = tmp[c];
  for (long c=i; c<orig_ncols; c++)
    columns_[c+n_to_add] = tmp[c];

  deletearray(tmp);
}

template<typename CoeffRing>
void SMat<CoeffRing>::insert_rows(long i, long n_to_add)
/* Insert n_to_add rows directly BEFORE row i. */
{
  for (long c=0; c<ncols_; c++)
    vec_insert_rows(columns_[c], i, n_to_add);
  nrows_ += n_to_add;
}

template<typename CoeffRing>
void SMat<CoeffRing>::delete_columns(long i, long j)
/* Delete columns i .. j from M */
{
  for (long c=i; c<=j; c++)
    vec_remove(columns_[c]);
  sparsevec **tmp = columns_;
  columns_ = 0;
  long ndeleted = j-i+1;
  long orig_ncols = ncols_;

  initialize(nrows_, ncols_ - (j-i+1), 0);
  for (long c=0; c<i; c++)
    columns_[c] = tmp[c];
  for (long c=j+1; c<orig_ncols; c++)
    columns_[c-ndeleted] = tmp[c];

  deletearray(tmp);
}

template<typename CoeffRing>
void SMat<CoeffRing>::delete_rows(long i, long j)
/* Delete rows i .. j from M */
{
  for (long c=0; c<ncols_; c++)
    vec_delete_rows(columns_[c], i, j);
  nrows_ -= (j-i+1);
}

template<typename CoeffRing>
bool SMat<CoeffRing>::set_submatrix(M2_arrayint rows,
				    M2_arrayint cols,
				    const MutableMatrix *M)
{
#ifdef DEVELOPMENT
#warning "write set_submatrix"
#endif
  return false;
  MutableMatrix::iterator *i = M->begin();
#if 0
//   long ncols = M->n_cols();
//   for (long c=0; c<ncols; c++)
//     {
//       for (i->set(c); i->valid(); i->next())
// 	{
// 	  ring_elem a;
// 	  i->copy_ring_elem(a);
// 	  coeffR->from_ring_elem(*(first + i->row()), a);
// 	}
//       first += nrows_;
//     }
#endif
  delete i;
  return true;
}

template<typename CoeffRing>
bool SMat<CoeffRing>::is_zero() const
{
  for (long c=0; c<ncols_; c++)
    if (columns_[c] != 0) 
      return false;
  return true;
}

template<typename CoeffRing>
SMat<CoeffRing> * SMat<CoeffRing>::submatrix(M2_arrayint rows, 
					     M2_arrayint cols) const
{
  SMat<CoeffRing> *result = new SMat<CoeffRing>(R,rows->len,cols->len);
  for (long r=0; r<rows->len; r++)
    for (long c=0; c<cols->len; c++)
      {
	elem f;
	get_entry(rows->array[r],cols->array[c],f);
	result->set_entry(r,c,f);
      }
  return result;

#if 0
//   // 6/1/2005: the following is faulty, if rows has duplicate entries.
//   int *trans = newarray_atomic(int,nrows_);
//   for (int i=0; i<nrows_; i++)
//     trans[i] = -1;
// 
//   for (unsigned j=0; j<r->len; j++)
//     if (r->array[j] >= 0 && r->array[j] < nrows_)
//       trans[r->array[j]] = j;
// 
//   for (unsigned int i=0; i<c->len; i++)
//     {
//       result->columns_[i] = subvector(trans, columns_[i]);
//     }
//   deletearray(trans);
//   return result;
// 
//       vec v = elem(c->array[i]);
//       for ( ; v != NULL; v = v->next)
// 	if (trans[v->comp] != -1)
// 	  mat.set_entry(trans[v->comp], i, v->coeff);
//     }
#endif
}

template<typename CoeffRing>
SMat<CoeffRing> * SMat<CoeffRing>::submatrix(M2_arrayint cols) const
{
  SMat<CoeffRing> *result = new SMat<CoeffRing>(R,nrows_,cols->len);
  for (long r=0; r<nrows_; r++)
    for (long c=0; c<cols->len; c++)
      {
	elem f;
	get_entry(r,cols->array[c],f);
	result->set_entry(r,c,f);
      }
  return result;
}

template <typename CoeffRing>
bool SMat<CoeffRing>::is_equal(const MutableMatrix *B) const
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
SMat<CoeffRing> * SMat<CoeffRing>::add(const MutableMatrix *B) const
  // return this + B.  return NULL of sizes or types do not match.
  // note: can add a sparse + dense
  //       can add a matrix over RR and one over CC and/or one over ZZ.
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
SMat<CoeffRing> * SMat<CoeffRing>::subtract(const MutableMatrix *B) const
  // return this - B.  return NULL of sizes or types do not match.
  // note: can subtract a sparse + dense
  //       can subtract a matrix over RR and one over CC and/or one over ZZ.
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
SMat<CoeffRing> * SMat<CoeffRing>::mult(const MutableMatrix *B) const
  // return this * B.  return NULL of sizes or types do not match.
  // note: can mult a sparse + dense
  //       can mult a matrix over RR and one over CC and/or one over ZZ.
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
SMat<CoeffRing> * SMat<CoeffRing>::mult(const elem &f) const
// return f*this.  return NULL of sizes or types do not match.
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
SMat<CoeffRing> * SMat<CoeffRing>::negate() const
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template <typename CoeffRing>
M2_arrayint columnEchelonForm(SMat<CoeffRing> *A)
{
#ifdef DEVELOPMENT
#warning "to be written"
#endif
  return 0;
}

template class SMat<CoefficientRingZZ_NTL>;
template class SMat<CoefficientRingZZp>;
template class SMat<CoefficientRingRR>;
template class SMat<CoefficientRingCC>;
template class SMat<CoefficientRingR>;

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
