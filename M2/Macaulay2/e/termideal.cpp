// Copyright 1996  Michael E. Stillman

#include "termideal.hpp"
#include "polyring.hpp"
#include "matrix.hpp"

stash *TermIdeal::mystash;
stash *mon_term::mystash;

TermIdeal::TermIdeal(const FreeModule *FF, const FreeModule *FFsyz)
{
  R = FF->Ring_of()->cast_to_poly_ring();
  assert(R != NULL);
  M = R->Nmonoms();
  K = R->Ncoeffs();
  F = FF;
  Fsyz = FFsyz;
  nvars = R->n_vars();
  one = K->from_int(1);
  bump_up(R);			// Don't bother bumping the others
  bump_up(F);
  bump_up(Fsyz);

  terms = new_mon_term_head();
  count = 0;
}

TermIdeal::~TermIdeal()
{
  while (terms->next != terms)
    delete_mon_term(terms->next);
  delete_mon_term(terms);
  K->remove(one);
  bump_down(Fsyz);
  bump_down(F);
  bump_down(R);
}

void TermIdeal::from_list(queue<mon_term *> &elems)
{
  // Place these elements into buckets by degree
  // Sort them by monomial
  // For each monom m on this list:
  // 	L := elements with this monom
  //    find all divisors of m 
  //    if any have coeff 1, remove all elements of L, continue
  //    find gcd g of set of divisors.
  //    remove any elements of L whose lead coeffs are div by g
  //    if L is not empty, then compute new gcd h:
  //      insert an element h*m + ...; and its rep.
  
  typedef mon_term *mon_term_star;
  mon_term *p;
  array<mon_term *> divs;

  // Place these elements into an array
  int n = elems.length();
  if (n == 0) return;
  int i = 0;
  int this_n = 0;
  mon_term **a;
  a = new mon_term_star[n];
  while (elems.remove(p))
    a[i++] = p;
  sort(a,0,n-1);		// Sort in ascending degree, then monomial order,
				// then size in K.
  
  // Now loop through each monomial in 'a', inserting into
  // the termideal the result

  while (this_n < n)
    {
      divs.shrink(0);
      find_all_divisors(a[this_n]->lead_exp(), divs);
      int last_elem = this_n;
      while (last_elem < n 
	     && M->compare(a[this_n]->monom(), a[last_elem]->monom()) == EQ)
	last_elem++;

      // If any of these has lead term 1, then ignore the current monomial
      int any_is_one = 0;
      for (i=0; i<divs.length(); i++)
	if (divs[i]->coeff_is_one)
	  {
	    any_is_one = 1;
	    break;
	  }
      if (!any_is_one) 
	{
	  // Compute the gcd of all elements in 'divs'.
	  mon_term *g = gcd(divs, a[this_n]->monom());
	  divs.shrink(0);
	  select_non_divisors(a + this_n, last_elem - this_n, g, divs);
	  mon_term *h = gcd(divs, a[this_n]->monom()); // This includes the element 'g'.
	  insert_minimal(h);			       // We may insert at end here... MESX
	  
	}
      for (int j=this_n; j<last_elem; j++)
	delete_mon_term(a[j]);
      this_n = last_elem;
      continue;
    }
  delete [] a;
}

void TermIdeal::select_non_divisors(mon_term **a, int nelems, mon_term *g,
				    array<mon_term *> &result_divs) const
{
  int i;
  if (g == NULL)
    {
      for (i=0; i<nelems; i++)
	result_divs.append(a[i]);
      return;
    }
  result_divs.append(g);
  for (i=0; i<nelems; i++)
    {
      ring_elem r;
      ring_elem f = K->divide(a[i]->coeff(), g->coeff(), r);
      if (!K->is_zero(r))
	result_divs.append(a[i]);
      K->remove(f);
      K->remove(r);
    }
}

void TermIdeal::text_out(ostream &o) const
{
  for (mon_term *p = terms->next; p != terms; p = p->next)
    {
      K->elem_text_out(o, p->coeff());
      M->elem_text_out(o, p->monom());
      o << " ";
    }
}

void TermIdeal::bin_out(ostream &/*o*/) const
{
  cerr << "bin_out not implemented for term ideals" << endl;
}

mon_term *TermIdeal::new_mon_term_head() const
{
  mon_term *result = new mon_term;
  result->next = result->prev = result;
  result->coeff_is_one = 0;
  result->expmask = 0;
  result->degree = 0;
  result->elem = NULL;
  return result;
}

mon_term *TermIdeal::mult_mon_term(const int *m, mon_term *p) const
    // Multiply p by m
{
  vec f = F->mult_by_monomial(m, p->elem->f);
  vec fsyz = Fsyz->mult_by_monomial(m, p->elem->fsyz);
  GB_elem *g = new_gb_elem(f, fsyz, 0);

  return new_mon_term(g);
}

void TermIdeal::delete_mon_term(mon_term *&p) const
{
  if (p == NULL) return;
  if (p->next != NULL) p->next->prev = p->prev;
  if (p->prev != NULL) p->prev->next = p->next;
  // MESX: what about the GB_elem?
  p = NULL;
}

GB_elem *TermIdeal::new_gb_elem(const vec f, const vec fsyz, int me) const
{
  GB_elem *result = new GB_elem(F->copy(f), Fsyz->copy(fsyz), 0);
  result->lead_exp = new int[M->n_vars()];
  M->to_expvector(f->monom, result->lead_exp);
  result->me = me;
  return result;
}
mon_term *TermIdeal::new_mon_term(GB_elem *bag) const
{
  mon_term *result = new mon_term;
  result->next = result->prev = NULL;
  result->elem = bag;

  result->coeff_is_one = (K->is_equal(result->coeff(), one));
  result->expmask = monomial_mask(result->lead_exp());
  result->degree = F->primary_degree(bag->f);
  return result;
}

void TermIdeal::link(mon_term *s, mon_term *t)
{
  // Place 't' in the list containing 's' BEFORE 's'.
  t->next = s;
  t->prev = s->prev;
  s->prev = t;
  t->prev->next = t;
}

void TermIdeal::unlink(mon_term *s)
{
  s->next->prev = s->prev;
  s->prev->next = s->next;
  s->next = NULL;
  s->prev = NULL;
}

int TermIdeal::monomial_mask(const int *exp) const
{
  int result = 0;
  int i,j;
  for (i=0, j=0; i<nvars; i++, j++)
    {
      if (j == 8*sizeof(int)) j=0;
      if (exp[i] > 0)
	result |= (1 << j);
    }
  return result;
}

int TermIdeal::exp_divides(const int *e, const int *f) const
{
  for (int i=0; i<nvars; i++)
    if (e[i] > f[i]) return 0;
  return 1;
}

int TermIdeal::compare(mon_term *p, mon_term *q) const
{
  // Compare monomials. If equal, compare absolute values of coeffs.
  int cmp = M->compare(p->monom(), q->monom());
  return cmp;
}



TermIdeal *TermIdeal::make_termideal(const Matrix &m, int n)
{
  TermIdeal *result = new TermIdeal(m.rows(), m.cols());
  queue <mon_term *> new_elems;
  for (int i=0; i<m.n_cols(); i++)
    {
      vec v = m[i];
      if (m.rows()->is_zero(v)) continue;
      if (m.rows()->lead_component(v) != n) continue;
      v = m.rows()->copy(v);
      vec vsyz = m.cols()->e_sub_i(i);
      GB_elem *g = result->new_gb_elem(v, vsyz, i);
      mon_term *p = result->new_mon_term(g);
      new_elems.insert(p);
    }

  // If the base ring is a quotient ring, include these lead monomials.
  if (m.Ring_of()->is_quotient_poly_ring())
    {
      MonomialIdeal Rideal = m.Ring_of()->get_quotient_monomials();
      for (Index<MonomialIdeal> j = Rideal.first(); j.valid(); j++)
	{
	  Nterm *f = (Nterm *) Rideal[j]->basis_ptr();
	  vec v = m.rows()->term(n, f);
	  GB_elem *g = result->new_gb_elem(v, NULL, 0);
	  mon_term *m = result->new_mon_term(g);
	  new_elems.insert(m);
	}
    }

  result->from_list(new_elems);
  return result;
}

void TermIdeal::append_to_matrix(Matrix m, int i) const
{
  // Should check: i is in range, m has same ring as this.
  if (i < 0 || i >= m.n_rows())
    {
      *gError << "index out of range";
      return;
    }
  if (m.Ring_of() != R)
    {
      *gError << "incorrect base ring";
      return;
    }
  for (mon_term *p = terms->next; p != terms; p = p->next)
    {
      ring_elem r = R->term(p->coeff(), p->monom());
      vec v = m.rows()->term(i, r);
      if (v != NULL) m.append(v);
    }
}
Matrix TermIdeal::change_matrix() const
{
  Matrix result(Fsyz);
  for (mon_term *p = terms->next; p != terms; p = p->next)
    {
      if (p->elem->f == NULL) continue;	// This is a ring element
      vec v = Fsyz->copy(p->elem->f);
      result.append(v);
    }
  return result;
}


mon_term *TermIdeal::insert_minimal(mon_term *t)
{
  // Inserts 't' into the TermIdeal.  If the monomial is already a member,
  // then the old one is replaced with this new one.  This is one way to replace
  // terms with smaller coefficients.
  // Terms are sorted in increasing order.

  mon_term *result = NULL;
  mon_term *s;
  for (s = terms->next; s != terms; s=s->next)
    {
      int cmp = compare(s,t);
      if (cmp == LT) continue;

      if (cmp == EQ)
	{
	  result = s;
	  s = s->next;
	  unlink(s->prev);
	  count--;
	}
      break;
    }
  link(s,t);
  count++;
  return result;
}


void TermIdeal::insert_w_deletions(mon_term *t, queue<mon_term *> &deletions)
{
  // Since the elements are stored in increasing degree order, we may 
  // insert 't', and then look for deletions from this point on
  mon_term *s = insert_minimal(t);
  if (s != NULL) deletions.insert(s);
  for (s = t->next; s != terms; s=s->next)
    {
      if (exp_divides(t->lead_exp(), s->lead_exp()))
	{
	  unlink(s);
	  deletions.insert(s);
	}
    }
}

int TermIdeal::sort_compare(const mon_term *p, const mon_term *q) const
{
  if (p->degree > q->degree) return GT;
  if (p->degree < q->degree) return LT;
  
  int cmp = M->compare(p->monom(), q->monom());
  return cmp;
  if (cmp != EQ) return cmp;
//  return K->compare(p->coeff(), q->coeff());
}

int TermIdeal::sort_partition(mon_term *a[], int lo, int hi)
{
  mon_term *pivot = a[lo];
  int i = lo-1;
  int j = hi+1;
  for (;;)
    {
      do { j--; }
      while (sort_compare(a[j], pivot) > 0);

      do { i++; }
      while (sort_compare(a[i], pivot) < 0);

      if (i < j)
	{
	  mon_term *tmp = a[j];
	  a[j] = a[i];
	  a[i] = tmp;
	}
      else
	return j;
    }
}

void TermIdeal::sort(mon_term *a[], int lo, int hi)
{
  if (lo < hi)
    {
      int q = sort_partition(a, lo, hi);
      sort(a, lo, q);
      sort(a, q+1, hi);
    }
}

int TermIdeal::find_first(const int *exp, mon_term *&result) const
{
  int expmask = ~(monomial_mask(exp));

  for (mon_term *t = terms->next; t != terms; t=t->next)
    if ((expmask & t->expmask) == 0)
      {
	int is_div = 1;
	for (int i=0; i<nvars; i++)
	  if (exp[i] < t->lead_exp()[i])
	    {
	      is_div = 0;
	      break;
	    }
	if (is_div)
	  {
	    result = t;
	    return 1;
	  }
      }
  return 0;
}

void TermIdeal::find_all_divisors(const int *exp, array<mon_term *> &result) const
{
  int expmask = ~(monomial_mask(exp));

  for (mon_term *t = terms->next; t != terms; t=t->next)
    if ((expmask & t->expmask) == 0)
      {
	int is_div = 1;
	for (int i=0; i<nvars; i++)
	  if (exp[i] < t->lead_exp()[i])
	    {
	      is_div = 0;
	      break;
	    }
	if (is_div)
	  result.append(t);
      }
}

mon_term *TermIdeal::gcd(array<mon_term *> &elems, const int *m) const
{
  //sort_by_field(elems);		// Sorted in increasing abs value

  mon_term *smallest = NULL;
  for (int i=0; i<elems.length(); i++)
    {
      // First, multiply this element 'up'
      int *factor = M->make_one();
      M->divide(m, elems[i]->elem->f->monom, factor);
      mon_term *this_one = mult_mon_term(factor, elems[i]);

      if (smallest == NULL)
	{
	  smallest = this_one;
	  continue;
	}

      // Combine 'smallest', 'this_one'
      ring_elem u,v;
      ring_elem g = K->gcd_extended(smallest->coeff(), this_one->coeff(), u, v);
      vec tmp = F->mult_by_coeff(u, smallest->elem->f);
      vec tmp2 = F->mult_by_coeff(v, this_one->elem->f);
      F->add_to(tmp, tmp2);
      smallest->elem->f = tmp;
      tmp = Fsyz->mult_by_coeff(u, smallest->elem->fsyz);
      tmp2 = Fsyz->mult_by_coeff(v, this_one->elem->fsyz);
      Fsyz->add_to(tmp, tmp2);
      smallest->elem->fsyz = tmp;
      delete_mon_term(this_one);
      K->remove(u);
      K->remove(v);
      K->remove(g);
    }
  return smallest;
}

