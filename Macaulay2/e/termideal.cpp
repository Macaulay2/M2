// Copyright 1996  Michael E. Stillman

#include "termideal.hpp"
#include "polyring.hpp"
#include "matrix.hpp"
#include "text_io.hpp"

// MES Aug 2002: still to fix up
TermIdeal::TermIdeal(GBRing *GR, const FreeModule *GGsyz)
{
  M = GR->get_flattened_monoid();
  K = GR->get_flattened_coefficients(); // Should be ZZ ?
  Gsyz = GGsyz;
  nvars = M->n_vars();
  one = K->from_int(1);

  // MESXX: the following should all be taken from the ring R.
  if (GR->is_quotient_ring())
    ring_terms = GR->get_quotient_monomials_ZZ()->ring_terms;
  else
    ring_terms = NULL;

  terms = new_mon_term_head();
  count = 0;
}

TermIdeal::~TermIdeal()
{
}

void TermIdeal::from_list(queue<tagged_term *> &elems)
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
  
  typedef tagged_term *tagged_term_star;
  tagged_term *p;
  array<tagged_term *> divs;

  int *exp = newarray_atomic(int,M->n_vars());
  // Place these elements into an array
  int n = elems.length();
  if (n == 0) return;
  int i = 0;
  int this_n = 0;
  tagged_term **a;
  a = newarray(tagged_term_star,n);
  while (elems.remove(p))
    a[i++] = p;
  sort(a,0,n-1);		// Sort in ascending degree, then monomial order,
				// then size in K.
  
  // Now loop through each monomial in 'a', inserting into
  // the termideal the result

  while (this_n < n)
    {
      divs.shrink(0);
      M->to_expvector(a[this_n]->monom(), exp);
      bool coeff_is_one = find_all_divisors(exp, divs);
      int last_elem = this_n;
      while (last_elem < n 
	     && M->compare(a[this_n]->monom(), a[last_elem]->monom()) == EQ)
	last_elem++;

      // If any of these has lead term 1, then ignore the current monomial
      if (!coeff_is_one)
	{
	  // Compute the gcd of all elements in 'divs'.
	  tagged_term *g = gcd(divs, a[this_n]->monom());
	  divs.shrink(0);
	  select_non_divisors(a + this_n, last_elem - this_n, g, divs);
	  if (divs.length() > 1 || g == NULL)  // First case: g has been added.
				// Second case: there will be a minimal element added.
	    {
	      tagged_term *h = gcd(divs, a[this_n]->monom()); // This includes the element 'g'.
	      insert_minimal(h);			       // We may insert at end here... MESX
	    }
	}
      for (int j=this_n; j<last_elem; j++)
	delete_tagged_term(a[j]);
      this_n = last_elem;
      continue;
    }
  deletearray(a);
  deletearray(exp);
}

void TermIdeal::select_non_divisors(tagged_term **a, 
				    int nelems, 
				    tagged_term *g,
				    array<tagged_term *> &result_divs) const
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
      ring_elem f = K->divide(a[i]->coeff(), g->coeff(), r); // MES MES
      if (!K->is_zero(r))
	result_divs.append(a[i]);
      K->remove(f);
      K->remove(r);
    }
}

void TermIdeal::text_out(buffer &o) const
{
  for (mon_term *p = terms->next; p != terms; p = p->next)
    {
      K->elem_text_out(o, p->coeff());
      M->elem_text_out(o, p->monom());
      o << " ";
    }
}

mon_term *TermIdeal::new_mon_term_head() const
{
  mon_term *result = new mon_term;
  result->next = result->prev = result;
  result->coeff_is_one = false;
  result->expmask = 0;
  result->_lead_exp = NULL;
  result->t = NULL;

  return result;
}

void TermIdeal::delete_tagged_term(tagged_term *&t) const
{
  if (t == NULL) return;
  K->remove(t->_coeff);
  M->remove(t->_monom);
  GR->remove(t->_gsyz);
  deleteitem(t);
  t = NULL;
}

void TermIdeal::delete_mon_term(mon_term *&p) const
{
  if (p == NULL) return;
  if (p->next != NULL) p->next->prev = p->prev;
  if (p->prev != NULL) p->prev->next = p->next;

  delete_tagged_term(p->t);
  deletearray(p->_lead_exp);
  deleteitem(p);
  p = NULL;
}

mon_term *TermIdeal::new_mon_term(tagged_term *t) const
  // The element 't' and its entries are considered consumed.
{
  mon_term *result = new mon_term;
  result->t = t;
  result->next = result->prev = NULL;
  result->_lead_exp = newarray_atomic(int,M->n_vars());
  M->to_expvector(t->monom(), result->_lead_exp);

  result->coeff_is_one = (K->is_equal(result->coeff(), one));
  result->expmask = monomial_mask(result->lead_exp());
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



TermIdeal *TermIdeal::make_termideal(const Matrix *m, int n)
{
  int i;

  // MES Aug 2002: what should these lines be? 
  const PolynomialRing *R = m->get_ring()->cast_to_PolynomialRing();
  const PolynomialRing *A = R->get_base_poly_ring();
  while (A->is_quotient_ring())
    A = A->get_base_poly_ring();

  const FreeModule *Gsyz = R->make_FreeModule(m->n_cols()); 
  GBRing *GR = R->get_gb_ring();
  const Monoid *M = GR->get_flattened_monoid();
  const Ring *K = GR->get_flattened_coefficients(); // Should be ZZ ?

  TermIdeal *result = new TermIdeal(GR,Gsyz);
  queue <tagged_term *> new_elems;
  for (i=0; i<m->n_cols(); i++)
    {
      gbvector *v = GR->gbvector_from_vec(m->rows(), (*m)[i]);
      if (GR->gbvector_is_zero(v)) continue;
      if (GR->gbvector_lead_component(v) != n) continue;
      gbvector *vsyz = GR->gbvector_e_sub_i(Gsyz,i);
      tagged_term *p = new tagged_term(
			     K->copy(v->coeff),
			     M->make_new(v->monom),
			     vsyz);

      new_elems.insert(p);
    }

  result->from_list(new_elems);
  return result;
}

TermIdeal *TermIdeal::make_termideal(GBRing *GR,
				     const FreeModule *Gsyz, 
				     queue<tagged_term *> &elems)
{
  TermIdeal *result = new TermIdeal(GR,Gsyz);
  result->from_list(elems);
  return result;
}

// MES Aug 2002: this needs to be done too
#if 0
TermIdeal *TermIdeal::make_ring_termideal(const PolynomialRing *R,
					  const array<ring_elem> &elems1,
					  const array<ring_elem> &elems2,
					  array<ring_elem> &result)
{
  // R should be the polynomial ring, NOT a quotient.
  int i;
  array<ring_elem> all;
  queue<tagged_term *> guys;
  int n = elems1.length();
  for (i=0; i<n; i++)
    all.append(R->copy(elems1[i]));
  for (i=0; i<elems2.length(); i++)
    all.append(R->copy(elems2[i]));
  FreeModule *G = R->make_FreeModule(all.length());

  for (i=0; i<all.length(); i++)
    {
      Nterm *f = all[i];
      tagged_term *t = new 
	tagged_term(R->Ncoeffs()->copy(f->coeff),
		    R->Nmonoms()->make_new(f->monom),
		    G->e_sub_i(i));
      guys.insert(t);
    }
  TermIdeal *ti = make_termideal(R,G,guys);

  // Now we loop through ti, adding to 'result', and resetting the 'gsyz'
  // field of 'ti'.  We then place the entire list in 'ring_terms'.
  mon_term *p;
  for (p = ti->terms->next; p != ti->terms; p = p->next)
    {
      tagged_term *t = p->t;
      Nterm *f = R->from_int(0);
      R->apply_ring_elements(f, t->gsyz(), all);
      result.append(f);
    }
  const FreeModule *RRsyz = R->make_FreeModule(result.length());
  ti->Rsyz = RRsyz;
  i = 0;
  for (p = ti->terms->next; p != ti->terms; p = p->next)
    {
      tagged_term *t = p->t;
      G->remove(t->_gsyz);
      t->_gsyz = NULL;
      t->_rsyz = ti->Rsyz->e_sub_i(i++);
      ti->Rsyz->negate_to(t->_rsyz);
    }

  ti->ring_terms = ti->terms;
  ti->terms = ti->new_mon_term_head();
  ti->count = 0;
  return ti;
}
#endif

#if 0
// MES Aug 2002: this needs to be done too
void TermIdeal::append_to_matrix(Matrix *m, int i) const
{
  // Should check: i is in range, m has same ring as this.
  if (i < 0 || i >= m->n_rows())
    {
      ERROR("index out of range");
      return;
    }
  if (m->get_ring() != R)
    {
      ERROR("incorrect base ring");
      return;
    }
  for (mon_term *p = terms->next; p != terms; p = p->next)
    {
      ring_elem r = R->term(p->coeff(), p->monom());
      vec v = m->rows()->raw_term(r,i);
      if (v != NULL) m->append(v);
    }
}
#endif

Matrix *TermIdeal::change_matrix() const
{
  Matrix *result = new Matrix(Gsyz);
  for (mon_term *p = terms->next; p != terms; p = p->next)
    {
      vec vsyz = GR->gbvector_to_vec(Gsyz, p->t->_gsyz);
      result->append(vsyz);
    }
  return result;
}

#if 0
// MES Aug 2002: this needs to be done too
Matrix *TermIdeal::ring_change_matrix() const
{
  Matrix *result = new Matrix(Rsyz);
  for (mon_term *p = terms->next; p != terms; p = p->next)
    {
      vec vsyz = Rsyz->copy(p->t->_rsyz);
      result->append(vsyz);
    }
  return result;
}
#endif

tagged_term *TermIdeal::insert_minimal(tagged_term *tt)
{
  // Inserts 't' into the TermIdeal.  If the monomial is already a member,
  // then the old one is replaced with this new one.  This is one way to replace
  // terms with smaller coefficients.
  // Terms are sorted in increasing order.

  mon_term *junk;
  tagged_term *result = insert_minimal(tt,junk);
  return result;
}

tagged_term *TermIdeal::insert_minimal(tagged_term *tt, mon_term *&new_t)
{
  // Inserts 't' into the TermIdeal.  If the monomial is already a member,
  // then the old one is replaced with this new one.  This is one way to replace
  // terms with smaller coefficients.
  // Terms are sorted in increasing order.

  new_t = new_mon_term(tt);

  tagged_term *result = NULL;
  mon_term *s;
  for (s = terms->next; s != terms; s=s->next)
    {
      int cmp = compare(s,new_t);
      if (cmp == LT) continue;

      if (cmp == EQ)
	{
	  result = s->t;
	  s->t = NULL;
	  s = s->next;
	  unlink(s->prev);
	  count--;
	}
      break;
    }
  link(s,new_t);
  count++;
  return result;
}


void TermIdeal::insert_w_deletions(tagged_term *t, queue<tagged_term *> &deletions)
{
  // Since the elements are stored in increasing degree order, we may 
  // insert 't', and then look for deletions from this point on
  mon_term *monterm_t, *s;
  int *exp = newarray_atomic(int,nvars);
  M->to_expvector(t->monom(), exp);
  tagged_term *old_t = insert_minimal(t, monterm_t);
  if (old_t != NULL) deletions.insert(old_t);
  for (s = monterm_t->next; s != terms; s=s->next)
    {
      if (exp_divides(exp, s->lead_exp()))
	{
	  unlink(s);
	  deletions.insert(s->t);
	  s->t = NULL;
	  delete_mon_term(s);
	}
    }
  deletearray(exp);
}

// MES Aug 2002: this needs to be done too
int TermIdeal::replace_minimal(const ring_elem new_coeff, const int *mon)
{
  // First see if 'mon' occurs.  If not, return -1.  If it does appear,
  // and if rsyz=0, gsyz=single term, then replace the coefficient,
  // and return gsyz->comp
  for (mon_term *s = terms->next; s != terms; s=s->next)
    {
      int cmp = M->compare(s->monom(),mon);
      if (cmp == GT) break;
      if (cmp == EQ)
	{
	  tagged_term *t = s->t;
	  if (t->gsyz() == NULL || t->gsyz()->next != NULL)
	    break;
	  K->remove(t->_coeff);
	  t->_coeff = K->copy(new_coeff);
	  s->coeff_is_one = K->is_equal(new_coeff, one);
	  return t->gsyz()->comp;
	}
    }
  return -1;
}

int TermIdeal::sort_compare(const tagged_term *p, const tagged_term *q) const
{
  //  if (p->degree > q->degree) return GT;
  //  if (p->degree < q->degree) return LT;
  
  int cmp = M->compare(p->monom(), q->monom());
  return cmp;
  if (cmp != EQ) return cmp;
//  return K->compare(p->coeff(), q->coeff());
}

int TermIdeal::sort_partition(tagged_term *a[], int lo, int hi)
{
  tagged_term *pivot = a[lo];
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
	  tagged_term *tmp = a[j];
	  a[j] = a[i];
	  a[i] = tmp;
	}
      else
	return j;
    }
}

void TermIdeal::sort(tagged_term *a[], int lo, int hi)
{
  if (lo < hi)
    {
      int q = sort_partition(a, lo, hi);
      sort(a, lo, q);
      sort(a, q+1, hi);
    }
}

bool TermIdeal::find_all_divisors(const int *exp, array<tagged_term *> &result) const
  // Return value: true means that a monomial divisor with lead coeff '1' was found.
{
  // MESXX: if a 'one' is found, use it immediately!
  int expmask = ~(monomial_mask(exp));

  if (ring_terms != NULL)
    for (mon_term *t = ring_terms->next; t != ring_terms; t=t->next)
      if ((expmask & t->expmask) == 0)
	{
	  bool is_div = true;
	  for (int i=0; i<nvars; i++)
	    if (exp[i] < t->lead_exp()[i])
	      {
		is_div = false;
		break;
	      }
	  if (is_div)
	    {
	      if (t->coeff_is_one)
		{
		  result.shrink(0);
		  result.append(t->t);
		  return true;
		}
	      result.append(t->t);
	    }
	}
  for (mon_term *t = terms->next; t != terms; t=t->next)
    if ((expmask & t->expmask) == 0)
      {
	bool is_div = true;
	for (int i=0; i<nvars; i++)
	  if (exp[i] < t->lead_exp()[i])
	    {
	      is_div = false;
	      break;
	    }
	if (is_div)
	  {
	    if (t->coeff_is_one)
	      {
		result.shrink(0);
		result.append(t->t);
		return true;
	      }
	    result.append(t->t);
	  }
      }
  return false; // A '1' lead coefficient was not found.
}

// MES Aug 2002: this needs to be done too
tagged_term *TermIdeal::gcd(array<tagged_term *> &elems, const int *m) const
{
  //sort_by_field(elems);		// Sorted in increasing abs value

  // Need: smallest->coeff, monom, gsyz, rsyz.  The other elements
  // are not needed until the mon term is created at the end.
  if (elems.length() == 0) return NULL;
  ring_elem c = K->copy(elems[0]->coeff());

  int *factor = M->make_one();
  M->divide(m, elems[0]->monom(), factor);

  gbvector *gsyz = GR->mult_by_monomial(Gsyz, factor, elems[0]->gsyz());
  gbvector *tmp, *tmp2;

  for (int i=1; i<elems.length(); i++)
    {
      // First, multiply this element 'up'
      M->divide(m, elems[i]->monom(), factor);

      // First find the multipliers
      ring_elem u,v;
      ring_elem g = K->gcd_extended(c, elems[i]->coeff(), u, v);
      K->remove(c);
      c = g;
      // Take combination of elems[i]->gsyz, gsyz.
      if (!K->is_zero(u))
	tmp = GR->mult_by_coeff(Gsyz, u, gsyz);
      else
	tmp = NULL;
      if (!K->is_zero(v))
	tmp2 = GR->mult_by_term(Gsyz, v, factor, elems[i]->gsyz());
      else
	tmp2 = NULL;
      GR->add_to(Gsyz, tmp, tmp2);
      GR->remove(gsyz);
      gsyz = tmp;

      K->remove(u);
      K->remove(v);
    }
  M->remove(factor);
  return new tagged_term(c,M->make_new(m),gsyz);
}

int TermIdeal::search(const int *m, ring_elem &result_gcd, 
		      gbvector * &result_gsyz) const
  // Given a monomial 'm', find the gcd 'result_gcd' of all of the coefficients of
  // the monomials in 'this' which divide 'm'.  result_gsyz is used to 
  // generate this element, as usual.
  // The result integer: the number of divisors found.  If this is 0, then result_gcd is 
  // undefined, and result_gsyz is a null pointer.  Caveat: there may be more divisors
  // than the ones actually found, if the gcd becomes 1 earlier.
{
  int *exp = newarray_atomic(int,nvars);
  array<tagged_term *> divs;
  M->to_expvector(m,exp);
  find_all_divisors(exp, divs);
  if (divs.length() == 0)
    {
      result_gsyz = NULL;
      return 0;
    }
  tagged_term *p = gcd(divs, m);
  result_gcd = p->_coeff;
  result_gsyz = p->_gsyz;
  p->_coeff = (Nterm*)0;
  p->_gsyz = 0;
  delete_tagged_term(p);
  return divs.length();
}

int TermIdeal::search(const ring_elem coeff, const int *m, 
		      ring_elem &result_gcd, gbvector * &result_gsyz) const
  // Returns one of TI_TERM, TI_MONOMIAL, TI_NONE.
  // Returns TI_TERM if coeff*m is in the term ideal.
  // Returns TI_MONOMIAL if m is in the ideal generated by the monomials
  // Returns TI_NONE if m is not in this monomial ideal.
  // The result_gsyz is the 'multiplier' that may be used to 
  // perform the reduction.  These have the effect of reducing the coefficient
  // mod the gcd of the elements in the termideal of monomial 'm'.
{
  gbvector *gsyz;
  result_gsyz = 0;

  int nfound = search(m, result_gcd, gsyz);
  if (nfound == 0) return TI_NONE;

  ring_elem rem;
  ring_elem d = K->divide(coeff, result_gcd, rem); // MES MES
  if (!K->is_zero(d))
    {
      result_gsyz = GR->mult_by_coeff(Gsyz,d,gsyz);
    }

  GR->remove(gsyz);
  bool iszero = K->is_zero(rem);
  K->remove(rem);
  if (iszero) return TI_TERM;
  return TI_MONOMIAL;
}

int TermIdeal::search(const ring_elem coeff, const int *m, 
		      gbvector * &result_gsyz) const
  // Returns one of TI_TERM, TI_MONOMIAL, TI_NONE.
  // Returns TI_TERM if coeff*m is in the term ideal.
  // Returns TI_MONOMIAL if m is in the ideal generated by the monomials
  // Returns TI_NONE if m is not in this monomial ideal.
  // The result_gsyz is the 'multiplier' that may be used to 
  // perform the reduction.  These have the effect of reducing the coefficient
  // mod the gcd of the elements in the termideal of monomial 'm'.
{
  ring_elem termgcd;
  int result = search(coeff, m, termgcd, result_gsyz);
  if (result != TI_NONE)
    K->remove(termgcd);
  return result;
}

// MES Aug 2002: what to do with this one?
Matrix *TermIdeal::search(const Matrix *m) const
{
  Matrix *result = new Matrix(Gsyz);
  for (int i=0; i<m->n_cols(); i++)
    {
      gbvector *v = GR->gbvector_from_vec(m->rows(), (*m)[i]);
      gbvector *gsyz;
      if (v != NULL)
	search(v->coeff,v->monom,gsyz);
      else
	gsyz = NULL;
      vec vsyz = GR->gbvector_to_vec(m->rows(), gsyz);
      result->append(vsyz);
    }
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
