// Copyright 1997 Michael E. Stillman

#include "gbbinom.hpp"

/////////////////////////////
// Monomials and binomials //
/////////////////////////////

binomial_ring::binomial_ring(Ring *RR, int nvars, int nwts, int *wts)
  : R(RR),
    F(new FreeModule(RR,1)),
    nvars(nvars),
    nweights(nwts)
{
  int i;

  bump_up(R);
  bump_up(F);

  nslots = nvars + nwts;

  weights = new int[nvars*nweights];
  for (i=0; i<nvars*nweights; i++) weights[i] = wts[i];

  monstash = new stash("monomials", sizeof(int)*nslots);
}

binomial_ring::~binomial_ring()
{
  bump_down(F);
  bump_down(R);
  delete [] degrees;
  delete [] weights;
  delete monstash;
}

void binomial_ring:::remove_monomial(monomial &m) const
{
  monstash->delete_elem(m);
  m = NULL;
}

monomial binomial_ring::new_monomial() const
{
  return (monomial)((binomial_ring *) this)->monstash->new_elem();
}

void binomial_ring::set_weights(monomial m) const
{
  int i,j;
  for (i=0; i<nweights; i++) result[i] = 0;
  for (i=0; i<nvars; i++)
    if (m[i] != 0)
      for (int j=0; j<nweights; j++)
	result[j] += m[i] * weights[i*nweights+j];
}

monomial binomial_ring::make_monomial(int *exp) const
  // Make a monomial from an exponent vector
{
  monomial result = new_monomial();
  for (int i=0; i<nvars; i++)
    result[nweights+i] = exp[i];
  set_weights(result);
  return result;
}

monomial binomial_ring::quotient(monomial m, monomial n) const
  // return m:n
{
  monomial result = new_monomial();
  for (int i=0; i<nslots; i++) 
    {
      int x = m[i] - n[i];
      result[nweights+i] = (x > 0 ? x : 0);
    }
  set_weights(result);
  return result;
}

monomial binomial_ring::lcm(monomial m, monomial n) const
  // return lcm(m,n)
{
  monomial result = new_monomial();
  for (int i=0; i<vars; i++) 
    result[nweights+i] = (m[i] > n[i] ? m[i] : n[i]);
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

bool binomial_ring::gcd_is_one(monomial m, monomial n) const
{
  // Return true if supp(m) and supp(n) are disjoint
  m += nweights;
  n += nweights;
  for (int i=0; i<nvars; i++)
    if (m[i] > 0 && n[i] > 0) return false;
  return true;
}

bool binomial_ring::remove_content(monomial &m, monomial &n) const
{
  // If m and n have a common monomial factor, remove it from each, and 
  // return true.  Otehrwise return false.
  int *m1 = m + nweights;
  int *m2 = n + nweights;
  bool result = false;
  for (int i=0; i<nvars; i++)
    {
      if (m[i] > 0 && n[i] > 0)
	{
	  if (m[i] > n[i])
	    {
	      m1[i] -= m2[i];
	      m2[i] = 0;
	    }
	  else
	    {
	      m2[i] -= m1[i];
	      m1[i] = 0;
	    }
	  result = true;
	}
    }
  return result;
}

bool binomial_ring::in_subring(monomial m) const
{
  return (m[1] == 0);
}

int binomial_ring::mask(monomial *m) const
{
  return ntuple::mask(nvars, m+nweights);
}

int binomial_ring::compare(monomial m, monomial n) const
{
  for (int i=0; i<nweights; i++)
    {
      if (m[i] > n[i]) return GT;
      if (m[i] < n[i]) return LT;
    }
  for (int i=nslots; i>nweights; i--)
    {
      if (m[i] > n[i]) return LT;
      if (m[i] < n[i]) return GT;
    }
  return EQ;
}

void binomial_ring::translate_monomial(const binomial_ring *old_ring, monomial &m) const
{
  int i;
  if (m == NULL) return;
  monomial result = new_monomial();
  for (i=0; i<old_ring->n_slots; i++)
    result[i] = m[i];
  for (i=old_ring->n_slots; i<nslots; i++)
    result[i] = 0;
  old_ring->remove_monomial(m);
  m = result;
}

void translate_binomial(const binomial_ring *old_ring, binomial &m) const
{
  translate_monomial(old_ring, m.f);
  translate_monomial(old_ring, m.g);
}

vec binomial_ring::monomial_to_vector(monomial m) const
{
  if (m == NULL) return NULL;
  intarray vp;
  varpower::from_ntuple(nvars, m+nweights, vp);
  return F->from_varpower(vp.raw(),0);
}

vec binomial_ring::binomial_to_vector(binomial f) const
{
  vec v1 = monomial_to_vector(f.f);
  vec v2 = monomial_to_vector(f.g);
  F->subtract_to(v1,v2);
  return v1;
}

binomial binomial_ring::vector_to_binomial(vec f) const
{
  binomial result;
  result.f = NULL;
  result.g = NULL;
  if (f == NULL) return result;

  result.f = new_monomial();
  R->Nmonoms()->to_expvector(f->monom, result.f+nweights);
  set_weights(result.f);

  f = f->next;

  if (f == NULL) return result;

  result.g = new_monomial();
  R->Nmonoms()->to_expvector(f->monom, result.g+nweights);
  set_weights(result.g);

  return result;
}

binomial binomial_ring::intvector_to_binomial(vec f) const
{
  binomial result;
  result.f = new_monomial();
  result.g = new_monomial();
  for (int i=0; i<nslots; i++)
    {
      result.f[i] = 0;
      result.g[i] = 0;
    }

  for ( ; f != NULL; f = f->next)
    {
      int e = ZZ->coerce_to_int(f->coeff);
      if (e > 0)
	result.f[nweights + f->comp] = e;
      else if (e < 0)
	result.g[nweights + f->comp] = -e;
    }

  set_weights(result.f);
  set_weights(result.g);
  return result;
}

void binomial_ring::one_reduction_step(binomial *&f, binomial *g) const
{
  if (f.g == NULL)
    {
      if (g.g != NULL)
	{
	  monomial m = spair(f.f,g.f,g.g);
	  remove_monomial(f.f);
	  f.f = m;
	}
    }
  else if (g.g == NULL)
    {
      remove_monomial(f.f);
      f.f = f.g;
      f.g = NULL;
    }
  else
    {
      monomial m1 = spair(lcm, f.f, f.g);
      monomial m2 = spair(lcm, g.f, g.g);
      int cmp = compare(m1, m2);
      if (cmp == EQ)
	{
	  remove_monomial(f.f);
	  remove_monomial(f.g);  // These set results to NULL.
	  remove_monomial(m1);
	  remove_monomial(m2);
	}
      else if (cmp == LT)
	{
	  // swap the two monomials
	  monomial a = result.f;
	  result.f = result.g;
	  result.g = a;
	}
    }
  return result;
}

binomial binomial_ring::calc_s_pair(binomial f, binomial g, monomial lcm) const
{
  // lcm should be the lead term of in(f), in(g) (which must be non-NULL!)

  binomial result;
  result.f = NULL;
  result.g = NULL;
  if (f.g == NULL)
    {
      if (g.g != NULL)
	result.f = spair(lcm,g.f,f.g);
    }
  else if (g.g == NULL)
    {
      result.f = spair(lcm, f.f, f.g)
    }
  else
    {
      result.f = spair(lcm, f.f, f.g);
      result.g = spair(lcm, g.f, g.g);
      int cmp = compare(result.f,result.g);
      if (cmp == EQ)
	{
	  remove_monomial(result.f);
	  remove_monomial(result.g);  // These set results to NULL.
	}
      else if (cmp == LT)
	{
	  // swap the two monomials
	  monomial a = result.f;
	  result.f = result.g;
	  result.g = a;
	}
    }
  return result;
}

///////////////////////
// S pair management //
///////////////////////

s_pair_set::s_pair_set()
{
}

s_pair_set::~s_pair_set()
{
}

void s_pair_set::insert(s_pair *p, monomial *lcm)
{
}

s_pair *s_pair_set::next()
  // returns NULL if none left
{
}

s_pair *s_pair_set::next(int d)
  // returns next pair in degrees <= d, if any.
{
}

int s_pair_set::lowest_degree() const
{
}

int s_pair_set::n_elems(int d) const
{
}

int s_pair_set::n_elems() const
{
}

///////////////////////
// Binomial GB table //
///////////////////////

void binomialGB::minimalize(gb_elem *f)
  // remove elements which have lead term divisible by in(f).
{
}

void binomialGB::insert(gb_elem *f)
  // optionally auto-reduces the other elements as well.
{
}

  
void binomialGB::make_new_pairs(s_pair_set &Pairs, gb_elem *f)
{
}

gb_elem *binomialGB::find_divisor(monomial *m)
{
  // Maybe return just the polynomial itself?

  // Two cases: (1) The divisors are in a monomial ideal
  // (2) The possible divisors are on a linked list.
}

void binomialGB::reduce(binomial *&f)
{
  // Some special flags: flag_divide_by_variables, flag_auto_reduce

  binomial head;
  binomial *result = &head;
  while (f != NULL)
    {
      gb_elem *p = find_divisor(f->monom, Gmin);
      if (p == NULL)
	{
	  result->next = f;
	  f = f->next;
	}
      else 
	{
	  // Do the division:
	  one_step_reduction(f,p);  // Modifies 'f'.
	  if (flag_divide_by_variables)
	    {
	      if (divide_by_variables(f)) // Modifies 'f'
		{
		  if (is_homogeneous) 
		    {
		      remove_binomial(f);
		      return NULL;
		    }
		}
	    }
	}
    }
  result->next = NULL;
  f = head.next;
}

/////////////////////////////
// Binomial GB computation //
/////////////////////////////

stash *binomialGB_comp::mystash;

binomialGB_comp::binomialGB_comp(const Ring *RR)
  : gb_comp(2),
    R(RR)
{
  F = new FreeModule(R,1);
  bump_up(F);
  nvars = R->n_vars();
  nweights = 0;
  nslots = 2*nvars;
  pstash = new stash("binomials", sizeof(int) * nslots);
  bump_up(R);
}

binomialGB_comp::~binomialGB_comp()
{
  delete pstash;
}

//////////////////////////
// Incremental routines //
//////////////////////////

void binomialGB_comp::enlarge(const Ring *R)
{
  // Make a new stash
  // Loop through and copy the old polynomials to the new stash
  // Delete the old stash
}

void binomialGB_comp::add_generators(const Matrix &m)
{
  // MES: not quite!!
  int i;
  if (m.Ring_of()->is_Z())
    {
      for (i=0; i<m.n_cols(); i++)
	{
	  binomial *f = intvector_to_binomial(m[i]);
	  Pairs.insert(new s_pair(f));
	}
    }
  else
    {
      for (i=0; i<m.n_cols(); i++)
	{
	  binomial *f = vector_to_binomial(m[i]);
	  Pairs.insert(new s_pair(f));
	}
    }
}

////////////////////////
// Computation proper //
////////////////////////

void binomialGB_comp::process_pair(s_pair *s)
{
  binomial *f = R->calc_s_pair(s->f, s->g);
  bool ismin = (s->g == NULL);  // will this be a minimal generator if it reduces to non-zero?
  bool subringmin = s->XXX;	// will this be a minimal generator of the subring, if it reduces
				// to a non-zero element in the subring?
  delete s;
  Gmin.reduce(f);
  if (f != NULL)
    {
      subringmin = (subringmin && R->in_subring(f));
      Gmin.make_new_pairs(f, Pairs);
      Gmin.minimalize(f);
      gb_elem *p = new gb_elem(f);
      Gmin.insert(p);  // does auto-reduction, depending on flags.
      if (ismin)
	mingens.append(p->f);
      if (subringmin)
	mingens_subring.append(p->f);
      G.append(p);
    }
}

int binomialGB_comp::gb_done(const intarray &stop_condtions) const
{
}

int binomialGB_comp::calc(const int *deg, const intarray &stop_conditions)
{
  s_pair *s;
  while (s = Pairs.next(deg)) do
    {
      int ret = gb_done(stop_conditions);
      if (ret != COMP_COMPUTING) return ret;
      process_pair(s);		// consumes 's'.
      if (system_interrupted) return COMP_INTERRUPTED;
    }

  if (deg == NULL) 
    return COMP_DONE;
  return COMP_DONE_DEGREE_LIMIT;
}

///////////////////////
// Obtaining results //
///////////////////////

Matrix binomialGB_comp::subring(int n)
{
}

Matrix binomialGB_comp::subringGB(int n)
{
}

Matrix binomialGB_comp::reduce(const Matrix &m, Matrix &lift)
{
}

Vector binomialGB_comp::reduce(const Vector &v, Vector &lift)
{
}

int binomialGB_comp::contains(const Matrix &m)
{
}

bool binomialGB_comp::is_equal(const gb_comp *q)
{
}
  
Matrix binomialGB_comp::min_gens_matrix()
{
  Matrix result = Matrix(F);
  for (int i=0; i<gens.length(); i++)
    result.append(binomial_to_vector(gens[i]));
  return result;
}

Matrix binomialGB_comp::initial_matrix(int n)
{
}

Matrix binomialGB_comp::gb_matrix()
{
}

Matrix binomialGB_comp::change_matrix()
{
  return Matrix(R);
}

Matrix binomialGB_comp::syz_matrix()
{
  return Matrix(R);
}

void binomialGB_comp::stats() const
{
}

