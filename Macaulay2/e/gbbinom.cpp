// Copyright 1997 Michael E. Stillman

#include "gbbinom.hpp"
#include "ntuple.hpp"

/////////////////////////////
// Monomials and binomials //
/////////////////////////////

binomial_ring::binomial_ring(const Ring *RR, 
			     int *wts,
			     bool revlex)
  : R(RR),
    F(new FreeModule(RR,1)),
    nvars(RR->n_vars()),
    have_weights(wts != NULL),
    weights(NULL),
    revlex(revlex)
{
  int i;

  bump_up(R);
  bump_up(F);

  nslots = nvars + 1;
  degrees = new int[nvars];
  for (i=0; i<nvars; i++) 
    degrees[i] = - R->Nmonoms()->primary_degree_of_var(i);

  if (have_weights)
    {
      nslots++;
      weights = new int[nvars];
      for (i=0; i<nvars; i++) weights[i] = -wts[i];
    }

  monstash = new stash("monomials", sizeof(int)*nslots);
}

binomial_ring::binomial_ring(const Ring *RR)
{
  *gError << "MES: not implemented yet";
}

binomial_ring::~binomial_ring()
{
  bump_down(F);
  bump_down(R);
  delete [] degrees;
  delete [] weights;
  delete monstash;
}

void binomial_ring::remove_monomial(monomial &m) const
{
  if (m == NULL) return;
  monstash->delete_elem(m);
  m = NULL;
}

monomial binomial_ring::new_monomial() const
{
  return (monomial)((binomial_ring *) this)->monstash->new_elem();
}

void binomial_ring::set_weights(monomial m) const
{
  int i;
  int deg = 0;
  for (i=0; i<nvars; i++)
      deg += degrees[i] * m[i];
  m[nvars] = deg;
  if (have_weights)
    {
      int wt = 0;
      for (i=0; i<nvars; i++)
	wt += weights[i] * m[i];
      m[nvars+1] = wt;
    }
}

monomial binomial_ring::make_monomial(int *exp) const
  // Make a monomial from an exponent vector
{
  monomial result = new_monomial();
  for (int i=0; i<nvars; i++)
    result[i] = exp[i];
  set_weights(result);
  return result;
}
void binomial_ring::remove_binomial(binomial &f) const
{
  remove_monomial(f.lead);
  remove_monomial(f.tail);
}
binomial binomial_ring::make_binomial() const
  // allocates the monomials
{
  return binomial(new_monomial(), new_monomial());
}

int binomial_ring::weight(monomial m) const
{
  if (have_weights) return -m[nvars+1];
  return 0;
}

int binomial_ring::degree(monomial m) const
{
  return -m[nvars];
}

unsigned int binomial_ring::mask(monomial m) const
{
  return ntuple::mask(nvars, m);
}

bool binomial_ring::divides(monomial m, monomial n) const
{
  for (int i=0; i<nvars; i++)
    if (m[i] > n[i]) return false;
  return true;
}

monomial binomial_ring::quotient(monomial m, monomial n) const
  // return m:n
{
  monomial result = new_monomial();
  for (int i=0; i<nslots; i++) 
    {
      int x = m[i] - n[i];
      result[i] = (x > 0 ? x : 0);
    }
  set_weights(result);
  return result;
}

monomial binomial_ring::lcm(monomial m, monomial n) const
  // return lcm(m,n)
{
  monomial result = new_monomial();
  for (int i=0; i<nvars; i++) 
    result[i] = (m[i] > n[i] ? m[i] : n[i]);
  set_weights(result);
  return result;
}

monomial binomial_ring::divide(monomial m, monomial n) const
{
  monomial result = new_monomial();
  for (int i=0; i<nslots; i++)
    result[i] = m[i] - n[i];
  return result;
}

monomial binomial_ring::mult(monomial m, monomial n) const
{
  monomial result = new_monomial();
  for (int i=0; i<nslots; i++)
    result[i] = m[i] + n[i];
  return result;
}

monomial binomial_ring::spair(monomial lcm, monomial a, monomial b) const
  // computes lcm - a + b
{
  monomial result = new_monomial();
  for (int i=0; i<nslots; i++)
    result[i] = lcm[i] - a[i] + b[i];
  return result;
}

void binomial_ring::spair_to(monomial a, monomial b, monomial c) const
{
  for (int i=0; i<nslots; i++)
    a[i] += -b[i] + c[i];
}

bool binomial_ring::gcd_is_one(monomial m, monomial n) const
{
  // Return true if supp(m) and supp(n) are disjoint
  for (int i=0; i<nvars; i++)
    if (m[i] > 0 && n[i] > 0) return false;
  return true;
}

bool binomial_ring::remove_content(monomial &m, monomial &n) const
{
  // If m and n have a common monomial factor, remove it from each, and 
  // return true.  Otehrwise return false.
  bool result = false;
  for (int i=0; i<nvars; i++)
    {
      if (m[i] > 0 && n[i] > 0)
	{
	  if (m[i] > n[i])
	    {
	      m[i] -= n[i];
	      n[i] = 0;
	    }
	  else
	    {
	      n[i] -= m[i];
	      m[i] = 0;
	    }
	  result = true;
	}
    }
  if (result)
    {
      set_weights(m);  // This is an inefficient way to do this...
      set_weights(n);
    }
  return result;
}

int binomial_ring::compare(monomial m, monomial n) const
{
  int i;
  if (have_weights)
    {
      i = nvars+1;
      if (m[i] > n[i]) return GT;
      if (m[i] < n[i]) return LT;
      i--;
    }
  else
    i = nvars;
  // check degree? For now...
  if (m[i] > n[i]) return GT;
  if (m[i] < n[i]) return LT;
  if (revlex)
    for ( ;i>=0; i--)
      {
	if (m[i] > n[i]) return LT;
	if (m[i] < n[i]) return GT;
      }
  else
    for ( ; i>=0; i--)
      {
	if (m[i] > n[i]) return GT;
	if (m[i] < n[i]) return LT;
      }
  return EQ;
}

void binomial_ring::translate_monomial(const binomial_ring *old_ring, monomial &m) const
{
  int i;
  if (m == NULL) return;
  monomial result = new_monomial();
  for (i=0; i<old_ring->nvars; i++)
    result[i] = m[i];
  for (i=old_ring->nvars; i<nvars; i++)
    result[i] = 0;
  old_ring->remove_monomial(m);
  set_weights(m);
  m = result;
}

void binomial_ring::translate_binomial(const binomial_ring *old_ring, binomial &f) const
{
  translate_monomial(old_ring, f.lead);
  translate_monomial(old_ring, f.tail);
}

vec binomial_ring::monomial_to_vector(monomial m) const
{
  if (m == NULL) return NULL;
  intarray vp;
  varpower::from_ntuple(nvars, m, vp);
  return F->from_varpower(vp.raw(),0);
}

vec binomial_ring::binomial_to_vector(binomial f) const
{
  vec v1 = monomial_to_vector(f.lead);
  vec v2 = monomial_to_vector(f.tail);
  F->subtract_to(v1,v2);
  return v1;
}
vec binomial_ring::binomial_to_vector(binomial f, int n) const
{
  vec v1 = monomial_to_vector(f.lead);
  bool include_tail = false;
  if (n == 0) 
    include_tail = true;
  else if (n == 1 && degree(f.tail) == degree(f.lead))
    include_tail = true;
  else if (n == 2 && degree(f.tail) == degree(f.lead)
	   && weight(f.tail) == weight(f.lead))
    include_tail = true;

  if (include_tail)
    {
      vec v2 = monomial_to_vector(f.tail);
      F->subtract_to(v1,v2);
    }
  return v1;
}

bool binomial_ring::vector_to_binomial(vec f, binomial &result) const
  // result should already have both monomials allocated
  // returns false if f is not a binomial, otherwise result is set.
{
  if (f == NULL || f->next == NULL || f->next->next != NULL) 
    return false;

  R->Nmonoms()->to_expvector(f->monom, result.lead);
  set_weights(result.lead);

  R->Nmonoms()->to_expvector(f->next->monom, result.tail);
  set_weights(result.tail);

  return true;
}

void binomial_ring::intvector_to_binomial(vec f, binomial &result) const
  // result should be a preallocated binomial
{
  for (int i=0; i<nslots; i++)
    {
      result.lead[i] = 0;
      result.tail[i] = 0;
    }

  for ( ; f != NULL; f = f->next)
    {
      int e = ZZ->coerce_to_int(f->coeff);
      if (e > 0)
	result.lead[f->comp] = e;
      else if (e < 0)
	result.tail[f->comp] = -e;
    }

  set_weights(result.lead);
  set_weights(result.tail);
}

bool binomial_ring::normalize(binomial &f) const
  // Return false if 'f' is zero.  Otherwise return true,
  // and possibly swap the terms of f so that f.lead is the lead term.
{
  int cmp = compare(f.lead, f.tail);
  if (cmp == EQ) return false;
  if (cmp == LT)
    {
      monomial a = f.lead;
      f.lead = f.tail;
      f.tail = a;
    }
  return true;
}

bool binomial_ring::one_reduction_step(binomial &f, binomial g) const
  // returns false if the reduction is zero, otherwise modifies f.
  // (f might be modified in either case).
{
  // MES: need to consider the cases: divide by content, homog_prime.
  for (int i=0; i<nslots; i++)
    f.lead[i] += - g.lead[i] + g.tail[i];
  return normalize(f);
}

bool binomial_ring::calc_s_pair(binomial_s_pair &s, binomial &result) const
{
  binomial f = s.f1->f;
  binomial g = s.f2->f;
  for (int i=0; i<nslots; i++)
    {
      result.lead[i] = s.lcm[i] - f.lead[i] + f.tail[i];
      result.tail[i] = s.lcm[i] - g.lead[i] + g.tail[i];
    }
  return normalize(result);
}

///////////////////////
// S pair management //
///////////////////////

binomial_s_pair_set::binomial_s_pair_set(const binomial_ring *RR)
  : R(RR),
    _prev_lcm(NULL)
{
  _pairs = new s_pair_degree_list; // list header
}

void binomial_s_pair_set::enlarge(const binomial_ring *newR)
{
  const binomial_ring *old_ring = R;
  R = newR;

  R->remove_monomial(_prev_lcm);
  _prev_lcm = NULL;
  for (s_pair_degree_list *thisdeg = _pairs->next; thisdeg != NULL; thisdeg = thisdeg->next)
    for (s_pair_lcm_list *thislcm = thisdeg->pairs->next; thislcm != NULL; thislcm = thislcm->next)
      R->translate_monomial(old_ring, thislcm->lcm);
}

void binomial_s_pair_set::remove_lcm_list(s_pair_lcm_list *p)
{
  while (p->pairs != NULL)
    {
      s_pair_elem *thispair = p->pairs;
      p->pairs = thispair->next;
      delete thispair;
    }
  delete [] p->lcm;
  delete p;
}
void binomial_s_pair_set::remove_pair_list(s_pair_degree_list *p)
{
  while (p->pairs != NULL)
    {
      s_pair_lcm_list *thislcm = p->pairs;
      p->pairs->next = thislcm->next;
      remove_lcm_list(thislcm);
    }
  delete p;
}
binomial_s_pair_set::~binomial_s_pair_set()
{
  while (_pairs != NULL)
    {
      s_pair_degree_list *thisdeg = _pairs;
      _pairs = thisdeg->next;
      remove_pair_list(thisdeg);
    }
  delete _pairs;
  R->remove_monomial(_prev_lcm);
}

void binomial_s_pair_set::insert_pair(s_pair_degree_list *q, binomial_s_pair &s)
{
  int cmp;
  s_pair_lcm_list *r = q->pairs;
  while (true)
    {
      if (r->next == NULL || 
	  ((cmp = R->compare(s.lcm, r->next->lcm)) == GT))
	{
	  // Insert new lcm node
	  s_pair_lcm_list *r1 = new s_pair_lcm_list;
	  r1->next = r->next;
	  r1->lcm = s.lcm;
	  r1->pairs = NULL;
	  r->next = r1;
	  break;
	}
      if (cmp == EQ)
	{
	  R->remove_monomial(s.lcm);
	  break;
	}
      r = r->next;
    }
  r = r->next;
  s_pair_elem *s1 = new s_pair_elem(s.f1, s.f2);
  s1->next = r->pairs;
  r->pairs = s1;
}

void binomial_s_pair_set::insert(binomial_gb_elem *p)
{
  monomial lcm = R->make_monomial(p->f.lead);
  binomial_s_pair s(p, NULL, lcm);
  insert(s);
}

void binomial_s_pair_set::insert(binomial_s_pair s)
{
  int deg = R->degree(s.lcm);
  s_pair_degree_list *q = _pairs;
  while (true)
    {
      if (q->next == NULL || q->next->deg > deg)
	{
	  // Insert new degree node
	  s_pair_degree_list *q1 = new s_pair_degree_list;
	  q1->next = q->next;
	  q1->deg = deg;
	  q1->pairs = new s_pair_lcm_list;
	  q1->pairs->next = NULL;
	  q->next = q1;
	  break;
	}
      if (q->next->deg == deg) break;
      q = q->next;
    }
  q = q->next;
  insert_pair(q, s);
  _n_elems++;
  q->n_elems++;
}

bool binomial_s_pair_set::next(const int *d, binomial_s_pair &result)
  // returns next pair in degrees <= *d, if any.
  // the caller should not free any of the three fields of the
  // s_pair!!
{
  if (_pairs->next == NULL) return false;
  if (d != NULL && _pairs->next->deg > *d) return false;
  s_pair_degree_list *thisdeg = _pairs->next;
  s_pair_lcm_list *thislcm = thisdeg->pairs->next;
  s_pair_elem *s = thislcm->pairs;

  thisdeg->n_elems--;
  _n_elems--;

  result = binomial_s_pair(s->f1, s->f2, thislcm->lcm);
  
  thislcm->pairs = s->next;
  if (thislcm->pairs == NULL)
    {
      // Now we must remove this set
      thisdeg->pairs = thislcm->next;
      R->remove_monomial(_prev_lcm);
      _prev_lcm = thislcm->lcm;
      thislcm->lcm = NULL;
      delete thislcm;
      
      if (thisdeg->pairs == NULL)
	{
	  // Now we must remove this larger degree list
	  _pairs->next = thisdeg->next;
	  delete thisdeg;
	}
    }

  delete s;
  return true;
}

int binomial_s_pair_set::lowest_degree() const
{
  if (_pairs->next == NULL) return -1;
  return _pairs->next->deg;
}

int binomial_s_pair_set::n_elems(int d) const
{
  s_pair_degree_list *p = _pairs;
  while (p->next != NULL && p->next->deg < d) p = p->next;
  if (p->next == NULL) return 0;
  return p->next->n_elems;
}

int binomial_s_pair_set::n_elems() const
{
  return _n_elems;
}

///////////////////////
// Binomial GB table //
///////////////////////
binomialGB::binomialGB(const binomial_ring *R)
  : R(R), first(NULL), _max_degree(0)
{
}

binomialGB::~binomialGB()
{
  // Do nothing much, except maybe clear out stuff
  // so no stray pointers are around
  
  R = NULL;
  first = NULL;
}

void binomialGB::enlarge(const binomial_ring *newR)
{
  R = newR;
}

void binomialGB::minimalize_and_insert(binomial_gb_elem *f)
  // remove elements which have lead term divisible by in(f).
  // optionally auto-reduces the other elements as well.
{
  monomial m = f->f.lead;
  gbmin_elem *fm = new gbmin_elem(f, R->degree(m));
  gbmin_elem *p;
  int deg = R->degree(m);
  if (deg > _max_degree) _max_degree = deg;
  if (first == NULL)
    {
      first = fm;
      first->next = NULL;
      return;
    }
  p = first;
  for ( ; p->next != NULL; p = p->next)
    {
      if (R->compare(m, p->next->elem->f.lead) == LT)
	break;
    }
  fm->next = p->next;
  p->next = fm;
  
  for (p = fm; p->next != NULL; p = p->next)
    {
      if (R->degree(p->next->elem->f.lead) > deg 
	  && R->divides(m, p->next->elem->f.lead))
	{
	  // remove this element
	  gbmin_elem *q = p->next;
	  binomial_gb_elem *qe = q->elem;
	  p->next = q->next;
	  q->next = NULL;
	  qe->smaller = f;
	}
      else
	reduce_monomial(p->next->elem->f.tail);
    }
}

binomialGB::monomial_list *binomialGB::find_divisor(binomialGB::monomial_list *I, monomial m) const
{
  unsigned int mask = ~(R->mask(m));
  int d = R->degree(m);
  for (monomial_list *p = I; p != NULL; p = p->next)
    {
      if (R->degree(p->m) > d) return NULL;
      if (mask & p->mask) continue;
      if (R->divides(p->m, m))
	return p;
    }
  return NULL;
}

binomialGB::monomial_list *binomialGB::ideal_quotient(monomial m) const
{
  monomial_list *r;
  monomial_list **deglist = new monomial_list *[_max_degree+1];
  for (int i=0; i<=_max_degree; i++)
    deglist[i] = NULL;

  for (iterator p = begin(); p != end(); p++)
    {
      binomial_gb_elem *g = *p;
      gbmin_elem *gm = new gbmin_elem(g, R->mask(g->f.lead));
      monomial n = R->quotient(g->f.lead, m);
      monomial_list *nl = new monomial_list(n,R->mask(n),gm);
      int d = R->degree(n);
      nl->next = deglist[d];
      deglist[d] = nl;
    }
  monomial_list *result = NULL;

  for (int d=0; d<=_max_degree; d++)
    if (deglist[d] != NULL)
      {
	monomial_list *currentresult = NULL;
	while (deglist[d] != NULL)
	  {
	    monomial_list *p = deglist[d];
	    deglist[d] = p->next;
	    if (find_divisor(result, m))
	      {
		R->remove_monomial(p->m);
		delete p;
	      }
	    else if ((r = find_divisor(currentresult, m)))
	      {
		gbmin_elem *p1 = new gbmin_elem(p->tag->elem, p->mask);
		R->remove_monomial(p->m);
		delete p;
		p1->next = r->tag;
		r->tag = p1;
	      }
	    else
	      {
		p->next = currentresult;
		currentresult = p;
	      }
	  }
	if (result == NULL)
	  result = currentresult;
	else if (currentresult != NULL)
	  {
	    monomial_list *q;
	    for (q = result; q->next != NULL; q = q->next);
	    q->next = currentresult;
	  }
	currentresult = NULL;
      }
  delete [] deglist;
  return result;
}

void binomialGB::make_new_pairs(binomial_s_pair_set *Pairs, binomial_gb_elem *f) const
{
  // Compute (a minimal generating set of)
  // the ideal quotient in(Gmin) : in(f).
  // Remove any that satisfy certain criteria:
  // 1. gcd(lead terms) is 1: remove.
  // 2. if homog_prime, gcd(tail terms) is not 1: remove.
  // Any that pass these tests, insert into Pairs.

  monomial m = f->f.lead;
  monomial_list *I = ideal_quotient(m);

  for (monomial_list *q = I; q != NULL; q = q->next)
    {
      gbmin_elem *ge = q->tag;  // a list of possibles
      
      binomial_gb_elem *g = ge->elem;
      // Criterion 1: gcd of lead terms must be not 1.
      // This only needs to be checked once for this list.
      // MES: write this.

      // Criterion 2: if a homogeneous prime, 
      // gcd of tails must be 1.
      // Check this for each possibility, I guess.
      // MES: write this.

      // Finally do the insert
      monomial lcm = R->mult(m, q->m);
      Pairs->insert(binomial_s_pair(f, g, lcm));
    }
}

binomial_gb_elem *binomialGB::find_divisor(monomial m) const
{
  // The pairs are assumed to be sorted in creasing degree order.
  // Is this valid?  MES
  unsigned int mask = ~(R->mask(m));
  int d = R->degree(m);
  for (iterator p = begin(); p != end(); ++p)
    {
      binomial_gb_elem *g = *p;
      if (R->degree(g->f.lead) > d) return NULL;
      if (mask & p.this_elem()->mask) continue;
      if (R->divides(g->f.lead, m))
	return g;
    }
  return NULL;
}

void binomialGB::reduce_monomial(monomial m) const
  // replace m with its normal form.
{
  binomial_gb_elem *p;
  while ((p = find_divisor(m)))
    R->spair_to(m, p->f.lead, p->f.tail);
}

bool binomialGB::reduce(binomial &f) const
{
  while (true)
    {
      binomial_gb_elem *p = find_divisor(f.lead);
      if (p == NULL)
	{
	  reduce_monomial(f.tail);
	  return R->normalize(f);
	}
      else 
	{
	  // Do the division:
	  if (!R->one_reduction_step(f,p->f))  // Modifies 'f'.
	    return false;
	}
    }
}

/////////////////////////////
// Binomial GB computation //
/////////////////////////////

stash *binomialGB_comp::mystash;

binomialGB_comp::binomialGB_comp(const Ring *RR, int *wts, bool revlex)
  : gb_comp(2),
    is_homogeneous(false),
    is_homogeneous_prime(false),
    flag_auto_reduce(true),
    flag_divide_by_variables(false),
    flag_use_monideal(false)
{
  R = new binomial_ring(RR, wts, revlex);
  Pairs = new binomial_s_pair_set(R);
  Gmin = new binomialGB(R);
}

binomialGB_comp::~binomialGB_comp()
{
  int i;
  delete Gmin;
  delete Pairs;
  // remove each element of Gens
  for (i=0; i<Gens.length(); i++)
    delete Gens[i];
  // remove each element of G
  for (i=0; i<G.length(); i++)
    delete G[i];
  // The following is just to ease garbage collection
  for (i=0; i<mingens.length(); i++)
    mingens[i] = NULL;
  for (i=0; i<mingens_subring.length(); i++)
    mingens_subring[i] = NULL;
  delete R;
}

//////////////////////////
// Incremental routines //
//////////////////////////

void binomialGB_comp::enlarge(const Ring *newR, int *wts)
{
  const binomial_ring *old_ring = R;
  R = new binomial_ring(newR, wts, old_ring->revlex);

  // We need to change all of the monomials in sight.
  Gmin->enlarge(R);
  Pairs->enlarge(R);

  for (int i=0; i<Gens.length(); i++)
    R->translate_binomial(old_ring, Gens[i]->f);
  for (int i=0; i<G.length(); i++)
    R->translate_binomial(old_ring, G[i]->f);

  delete old_ring;
}

void binomialGB_comp::add_generators(const Matrix &m)
{
  int i;
  binomial f;
  binomial_gb_elem *p;
  if (m.Ring_of()->is_Z())
    {
      for (i=0; i<m.n_cols(); i++)
	{
	  f = R->make_binomial();
	  R->intvector_to_binomial(m[i],f);
	  p = new binomial_gb_elem(f);
	  Gens.append(p);
	  Pairs->insert(p);
	}
    }
  else
    {
      for (i=0; i<m.n_cols(); i++)
	{
	  f = R->make_binomial();
	  if (R->vector_to_binomial(m[i], f))
	    {
	      p = new binomial_gb_elem(f);
	      Gens.append(p);
	      Pairs->insert(p);
	    }
	  else
	    {
	      *gError << "expected binomials";
	      return;
	    }
	}
    }
}

////////////////////////
// Computation proper //
////////////////////////

void binomialGB_comp::process_pair(binomial_s_pair s)
{
  bool ismin, subringmin;
  binomial f;

  if (s.f2 == NULL)
    {
      // A generator
      ismin = true;
      subringmin = true;
      f = s.f1->f;
    }
  else
    {
      if (s.f1->smaller != NULL || s.f2->smaller != NULL)
	{
	  if (s.f1->smaller != s.f2 && s.f2->smaller != s.f1)
	    {
	      return;
	    }
	}
      
      if (!R->calc_s_pair(s, f))
	{
	  // The pair already reduces to zero.
	  return;
	}
      
      ismin = false;
      subringmin = (R->weight(s.lcm) > 0);
    }

  if (Gmin->reduce(f))
    {
      subringmin = (subringmin && R->weight(f.lead) == 0);
      binomial_gb_elem *p = new binomial_gb_elem(f);      
      Gmin->make_new_pairs(Pairs, p);
      Gmin->minimalize_and_insert(p);
      if (ismin) mingens.append(p);
      if (subringmin) mingens_subring.append(p);
      G.append(p);
    }
}

int binomialGB_comp::gb_done(const intarray &/*stop_condtions*/) const
{
  return COMP_COMPUTING;
}

int binomialGB_comp::calc(const int *deg, const intarray &stop_conditions)
{
  binomial_s_pair s;
  while (Pairs->next(deg, s))
    {
      int ret = gb_done(stop_conditions);
      if (ret != COMP_COMPUTING) return ret;
      process_pair(s);		// consumes 's'.
      if (system_interrupted) return COMP_INTERRUPTED;
    }
  if (Pairs->n_elems() == 0)
    return COMP_DONE;
  return COMP_DONE_DEGREE_LIMIT;
}

///////////////////////
// Obtaining results //
///////////////////////

Matrix binomialGB_comp::subring()
{
  // Subsequent calls will not receive duplicate elements
  Matrix result = Matrix(R->F);
  for (int i=0; i<mingens_subring.length(); i++)
    {
      result.append(R->binomial_to_vector(mingens_subring[i]->f));
      mingens_subring[i] = NULL;
    }
  mingens_subring.shrink(0);
  return result;
}

Matrix binomialGB_comp::subringGB()
{
  Matrix result = Matrix(R->F);
  for (binomialGB::iterator p = Gmin->begin(); p != Gmin->end(); p++)
    if (R->weight((*p)->f.lead) == 0)
      result.append(R->binomial_to_vector((*p)->f));
  return result;
}

Matrix binomialGB_comp::reduce(const Matrix &m, Matrix &/*lift*/)
{
  *gError << "MES: not implemented yet";
  return m;
}

Vector binomialGB_comp::reduce(const Vector &v, Vector &/*lift*/)
{
  *gError << "MES: not implemented yet";
  return v;
}

int binomialGB_comp::contains(const Matrix &/*m*/)
{
  *gError << "MES: not implemented yet";
  return 0;
}

bool binomialGB_comp::is_equal(const gb_comp */*q*/)
{
  *gError << "MES: not implemented yet";
  return false;
}
  
Matrix binomialGB_comp::min_gens_matrix()
{
  Matrix result = Matrix(R->F);
  for (int i=0; i<mingens.length(); i++)
    result.append(R->binomial_to_vector(mingens[i]->f));
  return result;
}

Matrix binomialGB_comp::initial_matrix(int n)
{
  Matrix result = Matrix(R->F);
  for (binomialGB::iterator p = Gmin->begin(); p != Gmin->end(); p++)
      result.append(R->binomial_to_vector((*p)->f, n));
  return result;
}

Matrix binomialGB_comp::gb_matrix()
{
  Matrix result = Matrix(R->F);
  for (binomialGB::iterator p = Gmin->begin(); p != Gmin->end(); p++)
      result.append(R->binomial_to_vector((*p)->f));
  return result;
}

Matrix binomialGB_comp::change_matrix()
{
  return Matrix(R->R);
}

Matrix binomialGB_comp::syz_matrix()
{
  return Matrix(R->R);
}

void binomialGB_comp::stats() const
{
}
