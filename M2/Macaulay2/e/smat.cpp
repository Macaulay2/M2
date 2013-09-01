// Copyright 2005  Michael E. Stillman

#include "coeffrings.hpp"
#include "aring-zz-gmp.hpp"
#include "ZZp.hpp"
#include "smat.hpp"
#include "mat.hpp"

#include "aring-zzp.hpp"
#include "aring-RRR.hpp"
#include "aring-RR.hpp"
#include "aring-CCC.hpp"
#include "aring-gf-givaro.hpp"
#include "aring-m2-gf.hpp"
#include "aring-tower.hpp"

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
      ring().init_set(w->coeff, p->coeff);
    }
  result->next = 0;
  return head.next;
}

template<typename CoeffRing>
bool SMat<CoeffRing>::vec_equals(const sparsevec* v, const sparsevec* w) const
{
  for ( ; v != NULL && w != NULL; v=v->next, w=w->next)
    if (!ring().is_equal(v->coeff, w->coeff)) return false;
  return true;
}

template<typename CoeffRing>
bool SMat<CoeffRing>::vec_get_entry(const sparsevec *v, size_t r, elem &result) const
{
  for (const sparsevec *p = v; p != 0; p = p->next)
    if (p->row < r)
      break;
    else if (p->row == r)
      {
        ring().init_set(result,p->coeff);
        return true;
      }
  return false;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_set_entry(sparsevec *&v, size_t r, const elem &a) const
{
  sparsevec * p;
  bool iszero = ring().is_zero(a);
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
      ring().init_set(w->coeff, a);
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
        ring().set(p->next->coeff, a);
    }
  v = head.next;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_interchange_rows(sparsevec *&v, size_t i, size_t j) const
{
  sparsevec *p;
  if (i == j) return;
  if (v == 0) return;
  if (i < j)
    {
      size_t tmp = i;
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
      j = i;                    // Used below.
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
void SMat<CoeffRing>::vec_scale_row(sparsevec *&v, size_t r, const elem &a) const
{
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p = p->next)
    if (p->next->row < r)
      break;
    else if (p->next->row == r)
      {
        ring().mult(p->next->coeff, a, p->next->coeff);
        if (ring().is_zero(p->next->coeff))
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
  if (ring().is_zero(a))
    {
      vec_remove(v);
      v = 0;
      return;
    }
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p=p->next)
    {
      ring().mult(p->next->coeff, a, p->next->coeff);
      if (ring().is_zero(p->next->coeff))
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
void SMat<CoeffRing>::vec_negate(sparsevec *&v) const
{
  for (sparsevec *p = v; p->next != NULL; p=p->next)
    ring().negate(p->coeff, p->coeff);
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_divide_row(sparsevec *&v, size_t r, const elem &a) const
{
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p = p->next)
    if (p->next->row < r)
      break;
    else if (p->next->row == r)
      {
        ring().divide(p->next->coeff, p->next->coeff, a);
        if (ring().is_zero(p->next->coeff))
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
  if (ring().is_zero(a))
    {
      vec_remove(v);
      v = 0;
      return;
    }
  sparsevec head;
  head.next = v;
  for (sparsevec *p = &head; p->next != 0; p=p->next)
    {
      ring().divide(p->next->coeff, p->next->coeff,a);
      if (ring().is_zero(p->next->coeff))
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
        ring().add(tmv->coeff, tmv->coeff, tmw->coeff);
        if (ring().is_zero(tmv->coeff))
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
void SMat<CoeffRing>::vec_row_op(sparsevec *&v, size_t r1, const elem &a, size_t r2) const
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
  ring().init(c);
  ring().set_zero(c);
  ring().mult(c, vec2->coeff, a);
  if (ring().is_zero(c)) return; // nothing to change

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
      ring().init_set(w->coeff, c);
      p->next = w;
    }
  else
    {
      ring().add(p->next->coeff, p->next->coeff, c);
      if (ring().is_zero(p->next->coeff))
        {
          sparsevec *tmp = p->next;
          p->next = tmp->next;
          vec_remove_node(tmp);
        }
    }
  v = head.next;
  ring().clear(c);
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_row_op2(sparsevec *&v,
                   size_t r1, size_t r2,
                   const elem &a1, const elem &a2,
                   const elem &b1, const elem &b2) const
    // row(r1 in v) := a1 * row(r1 in v) + a2 * row(r2 in v)
    // row(r2 in v) := b1 * row(r1 in v) + b2 * row(r2 in v) (RHS refers to previous values)
{
  // v[row r1] = a1 * v[r1] + a2 * v[r2]
  // v[row r2] = b1 * v[r1] + b2 * v[r2]
  elem e1,e2, c1,c2,c3,c4;

  ring().init(c1);
  ring().init(c2);
  ring().init(c3);
  ring().init(c4);
  ring().init(e1);
  ring().init(e2);
  ring().set_zero(c1);
  ring().set_zero(c2);
  ring().set_zero(c3);
  ring().set_zero(c4);
  ring().set_zero(e1);
  ring().set_zero(e2);
  bool r1_nonzero = vec_get_entry(v,r1,e1);
  bool r2_nonzero = vec_get_entry(v,r2,e2);
  if (!r1_nonzero && !r2_nonzero) return;

  if (r1_nonzero)
    {
      ring().mult(c1, a1, e1);
      ring().mult(c3, b1, e1);
    }
  if (r2_nonzero)
    {
      ring().mult(c2,a2,e2);
      ring().mult(c4,b2,e2);
    }

  ring().add(c1,c1,c2);
  ring().add(c3,c3,c4);
  vec_set_entry(v,r1,c1);
  vec_set_entry(v,r2,c3);
  ring().clear(c1);
  ring().clear(c2);
  ring().clear(c3);
  ring().clear(c4);
  ring().clear(e1);
  ring().clear(e2);
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
  ring().init(a);
  ring().set_zero(a);
  ring().set_zero(result);
  while (true)
    {
      if (v == 0 || w == 0) return;
      if (v->row > w->row)
        v = v->next;
      else if (v->row < w->row)
        w = w->next;
      else
        {
          ring().mult(a, v->coeff, w->coeff);
          ring().add(result, result, a);
          v = v->next;
          w = w->next;
        }
    }
  ring().clear(a);
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
void SMat<CoeffRing>::vec_permute(sparsevec *&v, size_t start_row, M2_arrayint perm) const
{
  size_t *perminv = newarray_atomic(size_t,perm->len);
  for (size_t i=0; i<perm->len; i++)
    perminv[perm->array[i]] = i;

  size_t end_row = start_row + perm->len;

  for (sparsevec *w = v; w != 0; w = w->next)
    if (w->row >= start_row && w->row < end_row)
      w->row = start_row + perminv[w->row - start_row];
  vec_sort(v);
  deletearray(perminv);
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_insert_rows(sparsevec *&v, size_t i, size_t n_to_add) const
{
  for (sparsevec *w = v; w != 0 && w->row >= i; w = w->next)
    w->row += n_to_add;
}

template<typename CoeffRing>
void SMat<CoeffRing>::vec_delete_rows(sparsevec *&v, size_t i, size_t j) const
{
  size_t n_to_delete = j-i+1;
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
SMat<CoeffRing>::SMat(const CoeffRing& coeffR0, size_t nrows, size_t ncols)
  : coeffR(&coeffR0),
    nrows_(nrows),
    ncols_(ncols)
{
  initialize(nrows,ncols,0);
}

template<typename CoeffRing>
SMat<CoeffRing>::SMat(const SMat &M)
  : coeffR(M.coeffR),
    nrows_(M.nrows_),
    ncols_(M.ncols_)
{
  initialize(nrows_,ncols_,M.columns_);
}

template<typename CoeffRing>
void SMat<CoeffRing>::initialize(size_t nrows, size_t ncols, sparsevec **cols)
{
  nrows_ = nrows;
  ncols_ = ncols;
  columns_ = newarray(sparsevec *,ncols);
  if (cols == 0)
    {
      for (size_t i=0; i<ncols; i++)
        columns_[i] = 0;
    }
  else
    {
      for (size_t i=0; i<ncols; i++)
        columns_[i] = vec_copy(*cols++);
    }
}

template<typename CoeffRing>
void SMat<CoeffRing>::grab(SMat<CoeffRing> *M)
{
  std::swap(coeffR,M->coeffR);
  std::swap(nrows_,M->nrows_);
  std::swap(ncols_,M->ncols_);
  std::swap(columns_, M->columns_);
}

template<typename CoeffRing>
SMat<CoeffRing> *SMat<CoeffRing>::copy() const
{
  return new SMat(*this);
}

template<typename CoeffRing>
size_t SMat<CoeffRing>::lead_row(size_t col) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     returns -1 if the column is 0 */
{
  sparsevec *v = columns_[col];
  if (v == 0) return -1;
  return v->row;
}

template<typename CoeffRing>
size_t SMat<CoeffRing>::lead_row(size_t col, elem &result) const
  /* returns the largest index row which has a non-zero value in column 'col'.
     Also sets result to be the entry at this index.
     returns -1 if the column is 0, or if col is out of range
     No error is flagged. */
{
  sparsevec *v = columns_[col];
  if (v == 0) return -1;
  ring().init_set(result, v->coeff);
  return v->row;
}

///////////////////////////////
// Row and column operations //
///////////////////////////////

template<typename CoeffRing>
bool SMat<CoeffRing>::get_entry(size_t r, size_t c, elem &result) const
  // Returns false if (r,c) is out of range or if result is 0.  No error
  // is returned. result <-- this(r,c), and is set to zero if false is returned.
{
  return vec_get_entry(columns_[c], r, result);
}

template<typename CoeffRing>
void SMat<CoeffRing>::set_entry(size_t r, size_t c, const elem a)
{
  vec_set_entry(columns_[c], r, a);
}

template<typename CoeffRing>
void SMat<CoeffRing>::interchange_rows(size_t i, size_t j)
  /* swap rows: row(i) <--> row(j) */
{
  for (size_t c=0; c<ncols_; c++)
    vec_interchange_rows(columns_[c], i, j);
}

template<typename CoeffRing>
void SMat<CoeffRing>::interchange_columns(size_t i, size_t j)
  /* swap columns: column(i) <--> column(j) */
{
  sparsevec *tmp = columns_[i];
  columns_[i] = columns_[j];
  columns_[j] = tmp;
}

template<typename CoeffRing>
void SMat<CoeffRing>::scale_row(size_t i, elem r)
  /* row(i) <- r * row(i) */
{
  for (size_t c=0; c<ncols_; c++)
    vec_scale_row(columns_[c], i, r);
}

template<typename CoeffRing>
void SMat<CoeffRing>::scale_column(size_t i, elem r)
  /* column(i) <- r * column(i) */
{
  vec_scale(columns_[i], r);
}

template<typename CoeffRing>
void SMat<CoeffRing>::divide_row(size_t i, elem r)
  /* row(i) <- row(i) / r */
{
  for (size_t c=0; c<ncols_; c++)
    vec_divide_row(columns_[c], i, r);
}

template<typename CoeffRing>
void SMat<CoeffRing>::divide_column(size_t i, elem r)
  /* column(i) <- column(i) / r */
{
  vec_divide(columns_[i], r);
}

template<typename CoeffRing>
void SMat<CoeffRing>::row_op(size_t i, elem r, size_t j)
  /* row(i) <- row(i) + r * row(j) */
{
  for (size_t c=0; c<ncols_; c++)
    vec_row_op(columns_[c], i, r, j);
}

template<typename CoeffRing>
void SMat<CoeffRing>::column_op(size_t i, elem r, size_t j)
  /* column(i) <- column(i) + r * column(j) */
{
  vec_column_op(columns_[i], r, columns_[j]);
}

template<typename CoeffRing>
void SMat<CoeffRing>::row2by2(size_t r1, size_t r2,
               elem a1, elem a2,
               elem b1, elem b2)
  /* row(r1) <- a1 * row(r1) + a2 * row(r2),
     row(r2) <- b1 * row(r1) + b2 * row(r2)
  */
{
  for (size_t c=0; c<ncols_; c++)
    vec_row_op2(columns_[c], r1,r2,a1,a2,b1,b2);
}

template<typename CoeffRing>
void SMat<CoeffRing>::column2by2(size_t c1, size_t c2,
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
void SMat<CoeffRing>::dot_product(size_t i, size_t j, elem &result) const
{
  vec_dot_product(columns_[i], columns_[j], result);
}

template<typename CoeffRing>
bool SMat<CoeffRing>::row_permute(size_t start_row, M2_arrayint perm)
{
  // We copy one row to another location for each cycle in 'perm' of length > 1.
  size_t nrows_to_permute = perm->len;
  bool *done = newarray_atomic(bool,nrows_to_permute);
  for (size_t i=0; i<nrows_to_permute; i++)
    done[i] = true;
  for (size_t i=0; i<nrows_to_permute; i++)
    {
      size_t j = perm->array[i];
      if (!done[j])
        {
          ERROR("expected permutation");
          deletearray(done);
          return false;
        }
      done[j] = false;
    }
  for (size_t c=0; c<ncols_; c++)
    vec_permute(columns_[c], start_row, perm);
  return true;
}

template<typename CoeffRing>
bool SMat<CoeffRing>::column_permute(size_t start_col, M2_arrayint perm)
{
  size_t ncols_to_permute = perm->len;
  bool *done = newarray_atomic(bool,ncols_to_permute);
  sparsevec **tmpvecs = newarray(sparsevec *, ncols_to_permute);
  for (size_t i=0; i<ncols_to_permute; i++)
    done[i] = true;
  for (size_t i=0; i<ncols_to_permute; i++)
    {
      size_t j = perm->array[i];
      if (!done[j])
        {
          ERROR("expected permutation");
          deletearray(done);
          return false;
        }
      done[j] = false;
    }
  for (size_t i=0; i<ncols_to_permute; i++)
    tmpvecs[i] = columns_[start_col + perm->array[i]];
  for (size_t i=0; i<ncols_to_permute; i++)
    columns_[start_col + i] = tmpvecs[i];
  deletearray(tmpvecs);
  deletearray(done);
  return true;
}

template<typename CoeffRing>
void SMat<CoeffRing>::insert_columns(size_t i, size_t n_to_add)
/* Insert n_to_add columns directly BEFORE column i. */
{
  sparsevec **tmp = columns_;
  columns_ = 0;
  size_t orig_ncols = ncols_;

  initialize(nrows_, ncols_ + n_to_add, 0);
  for (size_t c=0; c<i; c++)
    columns_[c] = tmp[c];
  for (size_t c=i; c<orig_ncols; c++)
    columns_[c+n_to_add] = tmp[c];

  deletearray(tmp);
}

template<typename CoeffRing>
void SMat<CoeffRing>::insert_rows(size_t i, size_t n_to_add)
/* Insert n_to_add rows directly BEFORE row i. */
{
  for (size_t c=0; c<ncols_; c++)
    vec_insert_rows(columns_[c], i, n_to_add);
  nrows_ += n_to_add;
}

template<typename CoeffRing>
void SMat<CoeffRing>::delete_columns(size_t i, size_t j)
/* Delete columns i .. j from M */
{
  for (size_t c=i; c<=j; c++)
    vec_remove(columns_[c]);
  sparsevec **tmp = columns_;
  columns_ = 0;
  size_t ndeleted = j-i+1;
  size_t orig_ncols = ncols_;

  initialize(nrows_, ncols_ - (j-i+1), 0);
  for (size_t c=0; c<i; c++)
    columns_[c] = tmp[c];
  for (size_t c=j+1; c<orig_ncols; c++)
    columns_[c-ndeleted] = tmp[c];

  deletearray(tmp);
}

template<typename CoeffRing>
void SMat<CoeffRing>::delete_rows(size_t i, size_t j)
/* Delete rows i .. j from M */
{
  for (size_t c=0; c<ncols_; c++)
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
#if 0
//   size_t ncols = M->n_cols();
//   for (size_t c=0; c<ncols; c++)
//     {
//       for (i->set(c); i->valid(); i->next())
//      {
//        ring_elem a;
//        i->copy_ring_elem(a);
//        ring().from_ring_elem(*(first + i->row()), a);
//      }
//       first += nrows_;
//     }
#endif
}

template<typename CoeffRing>
bool SMat<CoeffRing>::is_zero() const
{
  for (size_t c=0; c<ncols_; c++)
    if (columns_[c] != 0)
      return false;
  return true;
}

template <typename CoeffRing>
bool SMat<CoeffRing>::is_equal(const SMat& B) const
{
  ASSERT(&ring() == &B.ring())
  if (B.numRows() != numRows()) return false;
  if (B.numColumns() != numColumns()) return false;
  for (size_t c = 0; c < numColumns(); c++)
    {
      sparsevec *v = columns_[c];
      sparsevec *w = B.columns_[c];
      if (!vec_equals(v,w))
        return false;
    }
  return true;
}

template <typename CoeffRing>
void SMat<CoeffRing>::setFromSubmatrix(const SMat &A, M2_arrayint rows, M2_arrayint cols)
{
  coeffR = A.coeffR;
  initialize(rows->len, cols->len, NULL);

  for (size_t r=0; r<rows->len; r++)
    for (size_t c=0; c<cols->len; c++)
      {
        elem f;
        coeffR->init(f);
        A.get_entry(rows->array[r],cols->array[c],f);
        set_entry(r,c,f);
      }
}

template <typename CoeffRing>
void SMat<CoeffRing>::setFromSubmatrix(const SMat &A, M2_arrayint cols)
{
  coeffR = A.coeffR;
  initialize(A.numRows(), cols->len, NULL);
  for (size_t r=0; r<nrows_; r++)
    for (size_t c=0; c<cols->len; c++)
      {
        elem f;
        coeffR->init(f);
        //        coeffR->init(f);
        A.get_entry(r,cols->array[c],f);
        set_entry(r,c,f);
      }
}

template <typename CoeffRing>
void SMat<CoeffRing>::addInPlace(const SMat<CoeffRing>& B)
  // return this + B.  return NULL of sizes or types do not match.
{
  ASSERT(&B.ring() == &ring());
  ASSERT(B.numRows() == numRows());
  ASSERT(B.numColumns() == numColumns());

  for (size_t c=0; c<numColumns(); c++)
    {
      sparsevec *v = vec_copy(B.columns_[c]);
      vec_add_to(columns_[c], v);
    }
}

template <typename CoeffRing>
void SMat<CoeffRing>::subtractInPlace(const SMat<CoeffRing>& B)
  // this -= B.
  // assumption:the assert statements below:
{
  ASSERT(&B.ring() == &ring());
  ASSERT(B.numRows() == numRows());
  ASSERT(B.numColumns() == numColumns());

  for (size_t c=0; c<numColumns(); c++)
    {
      sparsevec *v = vec_copy(B.columns_[c]);
      vec_negate(v);
      vec_add_to(columns_[c], v);
    }
}

template <typename CoeffRing>
void SMat<CoeffRing>::negateInPlace()
  // this = -this
{
  for (size_t c=0; c<numColumns(); c++)
    for (sparsevec *p = columns_[c]; p != NULL; p=p->next)
      ring().negate(p->coeff, p->coeff);
}

template <typename CoeffRing>
void SMat<CoeffRing>::scalarMultInPlace(const elem &f)
  // this = f * this
{
  for (size_t c=0; c<numColumns(); c++)
    vec_scale(columns_[c], f);
}

///////////////////////////////////
/// Fast linear algebra routines //
///////////////////////////////////

template<typename CoeffRing>
size_t SMat<CoeffRing>::rank() const
{
  ERROR("not implemented for this ring yet");
  return static_cast<size_t>(-1);
}

template<typename CoeffRing>
size_t SMat<CoeffRing>::new_rank() const
{
  ERROR("not implemented for this ring yet");
  return static_cast<size_t>(-1);
}

template<typename CoeffRing>
void SMat<CoeffRing>::determinant(elem &result) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
void SMat<CoeffRing>::new_determinant(elem &result) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
bool SMat<CoeffRing>::invert(SMat<CoeffRing> &inverse) const
{
  ERROR("not implemented for this ring yet");
  return false;
}

template<typename CoeffRing>
M2_arrayintOrNull SMat<CoeffRing>::rankProfile(bool row_profile) const
{
  ERROR("not implemented for this ring yet");
  return 0;
}

template<typename CoeffRing>
void SMat<CoeffRing>::nullSpace(SMat<CoeffRing> &nullspace, bool right_side) const
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
bool SMat<CoeffRing>::solveLinear(SMat<CoeffRing> &X, const SMat<CoeffRing> &B, bool right_side) const
{
  ERROR("not implemented for this ring yet");
  return false;
}

template<typename CoeffRing>
void SMat<CoeffRing>::addMultipleTo(const SMat<CoeffRing> &A,
                                    const SMat<CoeffRing> &B,
                                    bool transposeA,
                                    bool transposeB,
                                    ElementType& a,
                                    ElementType& b)
{
  ERROR("not implemented for this ring yet");
}

template<typename CoeffRing>
void SMat<CoeffRing>::mult(const SMat<CoeffRing>& B,
                           SMat<CoeffRing>& result) const
{
  ERROR("not implemented for this ring yet");
}

#include "aring-zzp-ffpack.hpp"
#include "aring-zz-flint.hpp"
#include "aring-zzp-flint.hpp"
#include "aring-qq.hpp"
template class SMat<M2::ARingZZGMP>;
template class SMat<M2::ARingZZp>;

template class SMat<M2::ARingQQ>;

#ifdef HAVE_FLINT
template class SMat<M2::ARingZZpFlint>;
template class SMat<M2::ARingQQFlint>;
template class SMat<M2::ARingZZ>;
#endif // HAVE_FLINT

template class SMat<M2::ARingTower>;
template class SMat<M2::ARingZZpFFPACK>;
template class SMat<M2::ARingGFGivaro>;
template class SMat<M2::ARingGFM2>;

template class SMat<CoefficientRingRRR>;
template class SMat<CoefficientRingCCC>;
template class SMat<CoefficientRingR>;
template class SMat<M2::ARingRRR>;
template class SMat<M2::ARingRR>;
template class SMat<M2::ARingCCC>;


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
