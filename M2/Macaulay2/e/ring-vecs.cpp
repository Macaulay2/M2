// Copyright 2003  Michael E. Stillman

#include "ring.hpp"
#include "text_io.hpp"
#include <vector>
#include "matrix.hpp"

//  Notes: ring_elem's are treated as immutable objects: they are not changed, and 
// the fact that one cannot change is used throughout.

vec Ring::new_vec() const
{
  vec result = (vec) getmem(sizeof(vecterm));
  return result;
}

void Ring::remove_vec_node(vec n) const
{
  // Should we just let them go, or free them?
  GC_FREE(n);
}

vec Ring::make_vec(int r, ring_elem a) const
{
  vec result = new_vec();
  result->next = 0;
  result->comp = r;
  result->coeff = a;
  return result;
}

vec Ring::copy(const vecterm * v) const
{
  vecterm head;
  vec result = &head;
  for (const vecterm *p = v; p != 0; p=p->next)
    {
      vec w = new_vec();
      result->next = w;
      result = w;
      w->comp = p->comp;
      w->coeff = p->coeff; // copy is not done
    }
  result->next = 0;
  return head.next;
}

void Ring::remove(vec v) const
{
  while (v != 0)
    {
      vec tmp = v;
      v = v->next;
      remove_vec_node(tmp);
    }
}


///////////////////////////////////////
// Routines which do not modify vecs //
///////////////////////////////////////

bool Ring::is_equal(const vecterm * a, const vecterm * b) const
{
  for ( ;; a = a->next, b = b->next)
    {
      if (a == NULL)
	{
	  if (b == NULL) return true;
	  return false;
	}
      if (b == NULL) return false;
      if (a->comp != b->comp) return false;
      if (!this->is_equal(a->coeff, b->coeff)) return false;
    }
}

bool Ring::get_entry(const vecterm * v, int r, ring_elem &result) const
{
  for (const vecterm *p = v; p != 0; p = p->next)
    if (p->comp < r)
      break;
    else if (p->comp == r)
      {
	result = p->coeff;
	return true;
      }
  return false;
}

int Ring::n_nonzero_terms(const vecterm * v) const
{
  int result = 0;
  for ( ; v != NULL; v = v->next)
    result++;
  return result;
}

vec Ring::sub_vector(const vecterm * v, const M2_arrayint r) const
{
  if (v == 0) return 0;
  // Largest component which occurs in v occurs first.
  vector<int> trans(v->comp+1);
  for (int i=0; i<v->comp; i++)
    trans.push_back(-1);

  for (unsigned j=0; j<r->len; j++)
    if (r->array[j] >= 0 && r->array[j] <= v->comp)
      trans[r->array[j]] = j;

  vecterm head;
  vecterm *result = &head;
  for ( ; v != NULL; v = v->next)
    if (trans[v->comp] != -1)
      {
	result->next = new_vec();
	result = result->next;
	result->next = 0;
	result->coeff = v->coeff;
	result->comp = trans[v->comp];
      }
  result->next = NULL;
  result = head.next;

  sort(result);
  return result;
}

void Ring::elem_text_out(buffer &o, const vecterm * v) const
{
  if (v == NULL)
    {
      o << "0";
      return;
    }

  int old_one = p_one;
  int old_parens = p_parens;
  int old_plus = p_plus;

  p_one = 0;
  for (const vecterm *t = v; t != NULL; t = t->next)
    {
      this->elem_text_out(o,t->coeff);
      o << "<" << t->comp << ">";
      p_plus = 1;
    }

  p_one = old_one;
  p_parens = old_parens;
  p_plus = old_plus;
}

// int FreeModule::lead_component(vec v) const
// ring_elem FreeModule::lead_coefficient(vec v) const

///////////////////////////////////////
// Routines which modify a vec ////////
///////////////////////////////////////

void Ring::mult(vec &v, const ring_elem r, bool opposite_mult) const
{
  if (this->is_zero(r))
    {
      remove(v);
      v = 0;
    }
  vecterm head;
  head.next = v;
  for (vec p = &head; p->next != 0; p=p->next)
    {
      //old version: this->mult_to(p->next->coeff, a);
      ring_elem c;
      if (opposite_mult)
	c = this->mult(p->next->coeff, r);
      else 
	c = this->mult(r,p->next->coeff);
      p->next->coeff = c;
      if (this->is_zero(p->next->coeff))
	{
	  vec tmp = p->next;
	  p->next = tmp->next;
	  remove_vec_node(tmp);
	}
    }
  v = head.next;
}

void Ring::mult_row(vec &v, int i, const ring_elem r, bool opposite_mult) const
{
  vecterm head;
  head.next = v;
  for (vec p = &head; p->next != 0; p = p->next)
    if (p->next->comp < i) 
      break;
    else if (p->next->comp == i)
      {
	ring_elem c;
	if (opposite_mult)
	  c = mult(p->next->coeff, r);
	else
	  c = mult(r, p->next->coeff);
	p->next->coeff = c;
	if (this->is_zero(p->next->coeff))
	  {
	    vec tmp = p->next;
	    p->next = tmp->next;
	    remove_vec_node(tmp);
	  }
	break;
      }
}

vec Ring::mult_vec_matrix(const Matrix *m,
			  vec v,
			  bool opposite_mult) const
{
  // Multiply m * v, using left or right mult for each scalar mult.

  // Each loop below should read
  // result = 0
  // for each non-zero term f e[component] of the vector v
  //    result += f M[v]
  vec result = NULL;
  for ( ; v != NULL; v = v->next)
    {
      vec w = this->copy(m->elem(v->comp));
      mult(w, v->coeff, !opposite_mult);
      this->add(result, w);
    }
  return result;
}

void Ring::divide(vec &v, const ring_elem a) const
{
  if (this->is_zero(a))
    {
      remove(v);
      v = 0;
    }
  vecterm head;
  head.next = v;
  for (vec p = &head; p->next != 0; p=p->next)
    {
      //old version: this->mult_to(p->next->coeff, a);
      ring_elem c = this->divide(p->next->coeff,a); // exact or quotient?? MES MES
      p->next->coeff = c;
      if (this->is_zero(p->next->coeff))
	{
	  vec tmp = p->next;
	  p->next = tmp->next;
	  remove_vec_node(tmp);
	}
    }
  v = head.next;
}

void Ring::divide_row(vec &v, int r, const ring_elem a) const
{
  vecterm head;
  head.next = v;
  for (vec p = &head; p->next != 0; p = p->next)
    if (p->next->comp < r) 
      break;
    else if (p->next->comp == r)
      {
	ring_elem c = this->divide(p->next->coeff, a); // exact or quotient?? MES MES
	p->next->coeff = c;
	if (this->is_zero(p->next->coeff))
	  {
	    vec tmp = p->next;
	    p->next = tmp->next;
	    remove_vec_node(tmp);
	  }
	break;
      }
}

void Ring::interchange_rows(vec &v, int i, int j) const
{
  vec p;
  if (i == j) return;
  if (v == 0) return;
  if (i < j) 
    {
      int tmp = i;
      i = j;
      j = tmp;
    }
  // So now i > j.
  vecterm head;
  head.next = v;
  vec vec1;
  vec vec2;
  for (p = &head; p->next != 0; p=p->next)
    if (p->next->comp <= i)
      break;
  vec1 = p;
  for ( ; p->next != 0; p=p->next)
    if (p->next->comp <= j)
      break;
  vec2 = p;
  if (vec1->next != 0 && vec1->next->comp == i)
    {
      if (vec2->next != 0 && vec2->next->comp == j)
	{
	  ring_elem tmp = vec1->next->coeff;
	  vec1->next->coeff = vec2->next->coeff;
	  vec2->next->coeff = tmp;
	  return;
	}
    }
  else if (vec2->next != 0 && vec2->next->comp == j)
    {
      vec tmp = vec1;
      vec1 = vec2;
      vec2 = tmp;
      j = i;			// Used below.
    }
  else
    return;

  vec tmp = vec1->next;
  if (vec2 != tmp)
    {
      vec1->next = tmp->next;
      tmp->next = vec2->next;
      vec2->next = tmp;
    }
  tmp->comp = j;
  v = head.next;
}

void Ring::add(vec &v, vec &w) const
{
  if (w == NULL) return;
  if (v == NULL) { v = w; w = NULL; return; }
  vecterm head;
  vec result = &head;
  while (true)
    if (v->comp < w->comp)
      {
	result->next = w;
	result = result->next;
	w = w->next;
	if (w == NULL) 
	  {
	    result->next = v;
	    v = head.next;
	    return;
	  }
      }
    else if (v->comp > w->comp)
      {
	result->next = v;
	result = result->next;
	v = v->next;
	if (v == NULL) 
	  {
	    result->next = w;
	    v = head.next;
	    w = NULL;
	    return;
	  }
      }
    else
      {
	vec tmv = v;
	vec tmw = w;
	v = v->next;
	w = w->next;
	this->add_to(tmv->coeff, tmw->coeff);
	if (this->is_zero(tmv->coeff))
	  {
	    remove_vec_node(tmv);
	  }
	else
	  {
	    result->next = tmv;
	    result = result->next;
	  }
	remove_vec_node(tmw);
	if (w == NULL) 
	  {
	    result->next = v;
	    v = head.next;
	    return;
	  }
	if (v == NULL) 
	  {
	    result->next = w;
	    v = head.next;
	    w = NULL;
	    return;
	  }
      }
}

void Ring::vec_row_op(vec &v, int i, ring_elem r, int j, bool opposite_mult) const
{
  vec p;
  vec vec2 = 0;
  for (p = v; p != 0; p=p->next)
    if (p->comp == i)
      {
	vec2 = p;
	break;
      }
  if (vec2 == 0) return;
  ring_elem r1;
  if (opposite_mult)
    r1 = this->mult(vec2->coeff, r);
  else 
    r1 = this->mult(r, vec2->coeff);
  vecterm head;
#warning "BUG??? check that r1 is zero!!"
  head.next = v;
  for (p = &head; p->next != 0; p=p->next)
    if (p->next->comp <= j)
      break;
  if (p->next == 0 || p->next->comp < j)
    {
      // Make a new node
      vec w = new_vec();
      w->next = p->next;
      w->comp = j;
      w->coeff = r1;
      p->next = w;
    }
  else
    {
      this->add_to(p->next->coeff, r1);
      if (this->is_zero(p->next->coeff))
	{
	  vec tmp = p->next;
	  p->next = tmp->next;
	  remove_vec_node(tmp);
	}
    }

  v = head.next;
}


void Ring::row2by2(vec &v, 
		      int r1, int r2,
		      ring_elem a1, ring_elem a2,
		      ring_elem b1, ring_elem b2) const
{
  // v[row r1] = a1 * v[r1] + a2 * v[r2]
  // v[row r2] = b1 * v[r1] + b2 * v[r2]
  ring_elem e1,e2, c1,c2,c3,c4;
  bool r1_nonzero = get_entry(v,r1,e1);
  bool r2_nonzero = get_entry(v,r2,e2);
  if (!r1_nonzero && !r2_nonzero) return;

  if (r1_nonzero)
    {
      c1 = this->mult(a1,e1);
      c3 = this->mult(b1,e1);
    }
  else
    {
      c1 = this->from_int(0);
      c3 = this->from_int(0);
    }
  if (r2_nonzero)
    {
      c2 = this->mult(a2,e2);
      c4 = this->mult(b2,e2);
    }
  else
    {
      c2 = this->from_int(0);
      c4 = this->from_int(0);
    }

  this->add_to(c1,c2);
  this->add_to(c3,c4);
  set_entry(v,r1,c1);
  set_entry(v,r2,c3);
}

ring_elem Ring::dot_product(const vecterm *v, const vecterm *w) const
{
  ring_elem result = this->from_int(0);
  while (true)
    {
      if (v == 0) return result;
      if (w == 0) return result;
      if (v->comp > w->comp)
	v = v->next;
      else if (v->comp < w->comp)
	w = w->next;
      else
	{
	  ring_elem a = this->mult(v->coeff, w->coeff);
	  this->add_to(result,a);
	  v = v->next;
	  w = w->next;
	}
    }
}


void Ring::set_entry(vec &v, int r, ring_elem a) const
{
  vec p;
  bool iszero = this->is_zero(a);
  vecterm head;
  head.next = v;
  for (p = &head; p->next != 0; p = p->next)
    if (p->next->comp <= r)
      break;

  if (p->next == 0 || p->next->comp < r)
    {
      if (iszero) return;
      vec w = new_vec();
      w->next = p->next;
      w->comp = r;
      w->coeff = a;
      p->next = w;
    }
  else if (p->next->comp == r)
    {
      if (iszero)
	{
	  // delete node
	  vec tmp = p->next;
	  p->next = tmp->next;
	  remove_vec_node(tmp);
	}
      else
	p->next->coeff = a;
    }
  v = head.next;
}

void Ring::sort(vecterm *&f) const
{
  // Internal routine to place the elements back in order after 
  // an operation such as subvector.
  // Divide f into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.
  
  if (f == NULL || f->next == NULL) return;
  vecterm *f1 = NULL;
  vecterm *f2 = NULL;
  while (f != NULL)
    {
      vecterm *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == NULL) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }
  
  sort(f1);
  sort(f2);
  add(f1, f2);
  f = f1;
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
