#if defined(__MWERKS__)
#pragma optimization_level 2
#endif
// Copyright 1995  Michael E. Stillman

#include "freemod.hpp"
#include "comb.hpp"
#include "text_io.hpp"
#include "matrix.hpp"
#include "polyring.hpp"
#include "ringmap.hpp"
#include "ntuple.hpp"
#include "termideal.hpp"

#include "geovec.hpp"


#if 0

vec FreeModule::get_terms(vec v, int lo, int hi) const
{
  vecterm head;
  vecterm *result = &head;
  int nmons = n_terms(v);
  if (lo < 0) lo = nmons + lo;
  if (hi < 0) hi = nmons + hi;
  int n = 0;
  while (v != NULL)
    {
      if (n > hi) break;
      if (n >= lo)
	{
#warning "8-8-04 commented out"
#if 0
	  result->next = copy_term(v);
#endif
	  result = result->next;
	}
      v = v->next;
      n++;
    }
  result->next = NULL;
  return head.next;
}
#endif

//////////////////////////////////////////////
//  Addition, subtraction ////////////////////
//////////////////////////////////////////////

#if 0
// 8-8-04 MES

#endif

///////////////////////////////////////////////////
// Sorting a list of vectors (and maybe degrees) //
///////////////////////////////////////////////////

static int deg_ascending;
static int monorder_ascending;
static int * sort_vals;
static const array<vec> * sort_vecs;
static const int * sort_degs;

int FreeModule::sort_compare(int i, int j) const
{
  if (i == j) return 0;
  if (deg_ascending != 0)
    {
      int d1 = sort_degs[i];
      int d2 = sort_degs[j];
      if (d1 > d2) return -deg_ascending;
      if (d1 < d2) return deg_ascending;
    }
  vec v1 = (*sort_vecs)[i];
  vec v2 = (*sort_vecs)[j];
  if (v1 == NULL) return -monorder_ascending;
  if (v2 == NULL) return monorder_ascending;
  int cmp = compare(v1, v2);
  if (cmp > 0) return -monorder_ascending;
  if (cmp < 0) return monorder_ascending;  
#if 0
  if (K->is_ZZ())
    {
      // Compare coeficients as well.
      cmp = K->cast_to_Z()->compare(v1->coeff, v2->coeff);
      buffer o;
      o << "comparing ";
      K->elem_text_out(o, v1->coeff);
      o << " and ";
      K->elem_text_out(o, v2->coeff);
      o << " result = " << cmp << newline;
      emit(o.str());
      if (cmp < 0) return 1;
      if (cmp > 0) return -1;
    }
#endif
  return 0;
}

int FreeModule::sort_partition(int lo, int hi) const
{
  int pivot = sort_vals[lo];
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
      do { j--; }
      while (sort_compare(sort_vals[j], pivot) < 0);
      do { i++; }
      while (sort_compare(sort_vals[i], pivot) > 0);

      if (i < j)
	{
	  int tmp = sort_vals[j];
	  sort_vals[j] = sort_vals[i];
	  sort_vals[i] = tmp;
	}
      else
	return j;
    }
}

void FreeModule::sort_range(int lo, int hi) const
{
  if (lo < hi)
    {
      int q = sort_partition(lo, hi);
      sort_range(lo, q);
      sort_range(q+1, hi);
    }
}

M2_arrayint_OrNull 
FreeModule::sort(const array<vec> &vecs, 
		 const M2_arrayint degrees, 
		 int degorder, // -1=descending, 0=don't use, 1=ascending
		 int monorder // -1=descending, 1=ascending.
		 ) const
{
  M2_arrayint result = makearrayint(vecs.length());
  if (vecs.length() == 0) return result;

  monorder_ascending = monorder;
  deg_ascending = degorder;

  sort_vals = result->array;
  for (int i=0; i<vecs.length(); i++)
    sort_vals[i] = i;

  sort_vecs = &vecs;
  sort_degs = NULL;
  if (deg_ascending) {
    if (degrees->len != static_cast<unsigned int>(vecs.length())) {
	ERROR("sort: specified degree order, without giving degrees");
	return 0;
      }
    sort_degs = degrees->array;
  }

  sort_range(0,vecs.length()-1);
  sort_vals = NULL;
  sort_degs = NULL;
  return result;
}

//////////////////////////////////////////////
//  Divisibility checks               ////////
//                                    ////////
//////////////////////////////////////////////
#if 0
int FreeModule::is_scalar_multiple(vec f, vec g) const
  // is df = cg, some scalars c,d?
{
  if (f == NULL) return 1;
  if (g == NULL) return 1;
  ring_elem c = f->coeff;
  ring_elem d = g->coeff;
  vec p,q;
  for (p=f, q=g; p != NULL && q != NULL; p=p->next, q=q->next)
    {
      if (p->comp != q->comp) return 0;
      if (M->compare(p->monom, q->monom) != 0) return 0;
    }
  for (p=f, q=g; p != NULL && q != NULL; p=p->next, q=q->next)
    {
      ring_elem c1 = K->mult(c, q->coeff);
      ring_elem d1 = K->mult(d, p->coeff);
      int isequal = K->is_equal(c1, d1);
      K->remove(c1);
      K->remove(d1);
      if (!isequal) return 0;
    }
  if (q == NULL && p == NULL) return 1;
  return 0;
}

void FreeModule::monomial_divisor(vec f, int *exp) const
{
  if (f == NULL || M == NULL) return;
  int *m = M->make_one();
  int *exp1 = newarray(int,M->n_vars());
  M->divide(f->monom, base_monom(f->comp), m);
  M->to_expvector(m, exp);

  for (vec a = f->next; a != NULL; a = a->next)
    {
      M->divide(a->monom, base_monom(a->comp), m);
      M->to_expvector(m, exp1);
      ntuple::gcd(M->n_vars(), exp, exp1, exp);
    }

  deletearray(exp1);
  M->remove(m);
}

vec FreeModule::monomial_squarefree(vec f) const
{
  if (M == NULL) return copy(f);
  int *exp = newarray(int,R->n_vars());
  monomial_divisor(f, exp);
  // Now divide each term by exp[i]-1, if exp[i] >= 2
  for (int i=0; i<M->n_vars(); i++)
    if (exp[i] >= 1) exp[i]--;

  // Divide f by exp:
  vec result = divide_by_expvector(exp, f);

  deletearray(exp);
  return result;
}

vec FreeModule::remove_monomial_divisors(vec f) const
{
  if (M == NULL) return copy(f);
  int *exp = newarray(int,R->n_vars());
  monomial_divisor(f, exp);

  // Divide f by exp:
  vec result = divide_by_expvector(exp, f);

  deletearray(exp);
  return result;
}
#endif





void FreeModule::change_degree(int i, const int *deg)
{ 
  // WARNING: this modifies the degree, and should only be used during
  // the construction of a free module (or matrix).
  assert(i >= 0);
  assert(i < rank());
  degree_monoid()->copy(deg, components[i]);
}


vec FreeModule::random() const
{
  vec result = NULL;
  for (int i=0; i<rank(); i++)
    {
      vec v = R->make_vec(i,R->random());
      if (v != NULL)
	{
	  v->next = result;
	  result = v;
	}
    }
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
