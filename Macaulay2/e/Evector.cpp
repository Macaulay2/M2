// Copyright 1998 by Michael Stillman

#include "Evector.hpp"
#include "text_io.hpp"

EVector::EVector() : F(0), len(0), elems(0) {}

EVector::EVector(const EFreeModule *FF, int len, evec *v)
  : F(FF),
    len(len),
    elems(v)
{
}

EVector::EVector(const EFreeModule *FF, evec *v)
  : F(FF),
    len(0),
    elems(v)
{
  for (evec *a = v; a != 0; a=a->next)
    len++;
}

EVector &EVector::operator=(const EVector &source)
{
  F = source.F;
  len = source.len;
  elems = source.elems;
  (const_cast<EVector &>(source)).len = 0;
  (const_cast<EVector &>(source)).elems = 0;
  //((EVector)source).len = 0;
  //((EVector)source).elems = 0;
  return *this;
}
void EVector::prepend_term(evec *t)
{
  len++;
  t->next = elems;
  elems = t;
}

void EVector::sort()
{
  if (len <= 1) return;
  EVectorHeap result(F);
  while (elems != 0)
    {
      evec *tm = elems;
      elems = elems->next;
      tm->next = 0;
      result.add(tm);
    }
  EVector tmp = result.value();
  *this = tmp;
}

ERingElement EVector::getComponent(int x) const
{
  return getRing()->vec_component(*this,x);
}

void EVector::addTo(EVector &g)
{
  getRing()->vec_add_to(*this, g);
}

void EVector::subtractTo(EVector &g)
{
  g.negateTo();
  addTo(g);
}

EVector EVector::multiply_by_ZZ(int m) const
{
  const ERingElement a = getRing()->from_int(m);
  return leftMultiply(a);
}

EVector EVector::leftMultiply(const ERingElement a) const
{
  return getRing()->vec_left_mult(a,*this);
}

EVector EVector::rightMultiply(const ERingElement a) const
{
  return getRing()->vec_right_mult(*this,a);
}

bool EVector::isGraded(monomial *&result_degree) const
{
  monomial *lo;
  degreeLoHi(lo,result_degree);
  return F->getDegreeMonoid()->is_equal(lo,result_degree);
}

void EVector::degreeLoHi(const monomial *&lo, const monomial *&hi) const
{
  // loop through each degree, computing degreeWeights ( + value
  // from free module).  Get the monomial from this exponent vector.
  int ndegrees = F->getDegreeMonoid()->n_vars();
  int *a = new int[ndegrees];
  int *b = new int[ndegrees];
  for (int i=0; i<ndegrees; i++)
    degreeWeightsLoHi(i, a[i], b[i]);
  lo = F->getDegreeMonoid()->monomial_from_exponents(a);
  hi = F->getDegreeMonoid()->monomial_from_exponents(b);
  delete [] a;
  delete [] b;
}

const monomial *EVector::degree() const
{
  monomial *lo, *hi;
  degreeLoHi(lo,hi);
  return hi;
}

void EVector::degreeWeightsLoHi(int i, int &lo, int &hi) const
{
  if (len == 0) return;
  const ERing *R = getRing();
  const EMonoid *D = R->getDegreeMonoid();
  const int *wts = R->getDegreeVector(i);
  int nwts = R->n_vars();
  EVector::iterator p = *this;
  R->vec_degree_lohi(*p, nwts,wts,lo,hi);
  int d = D->to_exponents(F->getDegree(p->component))[i];
  lo += d;
  hi += d;
  for (++p; p.valid(); ++p)
    {
      int d1,d2;
      getRing()->vec_degree_lohi(*p, nwts,wts,d1,d2);
      d = D->to_exponents(F->getDegree(p->component))[i];
      d1 += d;
      d2 += d;
      if (d1 < lo) lo = d1;
      if (d2 > hi) hi = d2;
    }
}

void EVector::degreeWeightsLoHi(int nwts, const int *wts, 
                            const int *componentdegs, 
                            int &lo, int &hi) const
{
  if (len == 0) 
    {
      lo = hi = 0;
      return;
    }
  const ERing *R = getRing();
  EVector::iterator p = *this;
  R->vec_degree_lohi(*p,nwts,wts,lo,hi);
  int d = componentdegs[p->component];
  lo += d;
  hi += d;
  for (++p; p.valid(); ++p)
    {
      int d1,d2;
      R->vec_degree_lohi(*p,nwts,wts,d1,d2);
      d = componentdegs[p->component];
      d1 += d;
      d2 += d;
      if (d1 < lo) lo = d1;
      if (d2 > hi) hi = d2;
    }
}
void EVector::degreeWeightsLoHi(int nwts, const int *wts, 
                            int &lo, int &hi) const
{
  if (len == 0) 
    {
      lo = hi = 0;
      return;
    }
  const ERing *R = getRing();
  EVector::iterator p = *this;
  R->vec_degree_lohi(*p,nwts,wts,lo,hi);
  for (++p; p.valid(); ++p)
    {
      int d1, d2;
      R->vec_degree_lohi(*p,nwts,wts,d1,d2);
      if (d1 < lo) lo = d1;
      if (d2 > hi) hi = d2;
    }
}

bool EVector::homogenize(int v, int d, int nwts, const int *wts, EVector &result) const
{
  evec *tm;
  if (wts[v] == 0) return false;

  const EPolynomialRing *R = F->getRing()->toPolynomialRing();
  if (R == 0)
    {
      gError << "";
      return false;  // MES: What about fraction fields?
    }
  const EMonoid *M = R->getMonoid();
  int nvars = M->n_vars();
  
  const int *exp1;
  int *exp = new int[nvars];

  EVectorHeap H(F);
  for (EVector::iterator a = *this; a.valid(); ++a)
    {
      int i;
      exp1 = M->to_exponents(a->monom);
      for (i=0; i<nvars; i++)
        exp[i] = exp1[i];
      int e = 0;
      for (i=0; i<nvars; i++) e += wts[i] * exp[i];
      //e += F->getDegree(a->comp)->exponents[0];
      if (((d-e) % wts[v]) != 0)
	{
	  // We cannot homogenize, so clean up and exit.
	  EVector bad = H.value();
	  gError << "homogenization impossible";
	  delete [] exp;
	  return false;
	}
      exp[v] += (d - e) / wts[v];

      tm = R->vec_new_term();
      tm->coeff = a->coeff;
      tm->component = a->component;
      tm->monom = M->monomial_from_exponents(exp);
      H.add(tm);
    }
  delete [] exp;
  result = H.value();
  return true;
}

bool EVector::homogenize(int v, int nwts, const int *wts, EVector &result) const
{
  int lo, hi;
  degreeWeightsLoHi(nwts,wts,lo,hi);
  int d = (wts[v] > 0 ? hi : lo);
  return homogenize(v,d,nwts,wts,result);
}

EVector EVector::componentShift(const EFreeModule *newF, int r) const
{
  EVector::collector h(newF);
  for (EVector::iterator a = *this; a.valid(); ++a)
    {
      evec *tm = getRing()->vec_copy_term(*a);
      tm->component += r;
      h.append(tm);
    }
  EVector result = h.value();
  if (F->hasInducedOrder() || newF->hasInducedOrder())
    result.sort();
  return result;
}

EVector EVector::tensorShift(const EFreeModule *newF, int n, int m) const
{
  EVector::collector h(newF);
  for (EVector::iterator a = *this; a.valid(); ++a)
    {
      evec *tm = getRing()->vec_copy_term(*a);
      tm->component = n * tm->component + m;
      h.append(tm);
    }
  EVector result = h.value();
  if (F->hasInducedOrder() || newF->hasInducedOrder())
    result.sort();
  return result;
}
EVector EVector::tensor(const EFreeModule *newF, const EVector &w) const
{
  int n = w.getFreeModule()->rank();
  EVectorHeap result(newF);
  for (EVector::iterator a = *this; a.valid(); ++a)
    {
      ERingElement tm = getRing()->vec_term_to_ring(*a);
      EVector w1 = componentShift(newF, a->component * n);
      EVector w2 = getRing()->vec_left_mult(tm,w1);
      getRing()->remove(tm);
      result.add(w2);
    }
  return result.value();
}
EVector EVector::translate(const EFreeModule *newF) const
{
  EVector result = clone();
  if (F == newF) return result;
  result.F = newF;
  if (F->hasInducedOrder() || newF->hasInducedOrder())
    result.sort();
  return result;
}
EVector EVector::translate(const EFreeModule *newF, int newcomp) const
{
  EVector::collector H(newF);
  for (iterator t = *this; t.valid(); ++t)
    {
      evec *p = getRing()->vec_copy_term(*t);
      p->component = newcomp;
      H.append(p);
    }
  EVector result = H.value();
  result.sort();
  return result;
}
EVector EVector::subvector(const EFreeModule *newF, const intarray &r) const
{
  int *trans = new int[F->rank()];
  int i;
  for (i=0; i<F->rank(); i++)
    trans[i] = -1;
  for (i=0; i<r.length(); i++)
    if (r[i] >= 0 && r[i] < F->rank())
      trans[r[i]] = i;

  EVectorHeap result(newF);
  for (EVector::iterator a = *this; a.valid(); ++a)
    if (trans[a->component] != -1)
      {
        evec *tm = getRing()->vec_copy_term(*a);
        tm->component = trans[a->component];
        result.add(tm);
      }
  return result.value();
}

EVector EVector::leadTerm(int n, bool only_same_component) const
{
  if (len == 0) return F->zero();
  const EPolynomialRing *R = getRing()->toPolynomialRing();
  if (R == 0)
    {
      // Return a copy of the lead term of v.
      evec *p = getRing()->vec_copy_term(elems);
      return EVector(F,1,p);
    }
  const EMonoid *M = R->getMonoid();
  if (n == -1)
    n = M->getMonomialOrder()->n_blocks();
  evec *first = elems;
  EVector::collector w(F);
  for (EVector::iterator p = *this; p.valid(); ++p)
    if (M->compare(p->monom,first->monom,n) == EQ)
      {
        if (only_same_component && (p->component != first->component))
          continue;
        w.append(R->vec_copy_term(*p));
      }
    else 
      break;
  return w.value();
}

ERingElement EVector::diff_term(const monomial *d, 
				const monomial *m, 
				const monomial *& result_monom, 
				bool use_coeff) const
{
  const EPolynomialRing *R = getRing()->toPolynomialRing();
  const ERing *K = R->getCoefficientRing();
  const EMonoid *M = R->getMonoid();
  if (!M->divides(d,m)) return K->zero();
  result_monom = M->divide(m,d);
  if (!use_coeff) return K->one();
  // Must find the coefficient.
  const int *exp1 = M->to_exponents(d);
  const int *exp2 = M->to_exponents(m);
  ERingElement result = K->one();
  for (int i=0; i<R->n_vars(); i++)
    for (int j=exp1[i]-1; j>=0; --j)
      {
	ERingElement g = K->from_int(exp2[i]-j);
	ERingElement g1 = K->mult(g,result);
	K->remove(g);
	K->remove(result);
	result = g1;
	if (K->is_zero(result))
	  return result;
      }
  return result;
}

EVector EVector::diff_by_term(const EFreeModule *resultF,
			      const evec *p,
			      bool use_coeffs) const
{
  collector result(resultF);
  const ERing *K = getRing()->getVectorCoefficientRing();
  for (iterator t = *this; t.valid(); ++t)
    {
      ERingElement a = K->mult(p->coeff, t->coeff);
      if (K->is_zero(a))
	{
	  K->remove(a);
	  continue;
	}
      evec *tm = getRing()->vec_new_term();
      tm->coeff = a;
      tm->component = getFreeModule()->rank() * (p->component) + t->component;
      ERingElement g = diff_term(p->monom, t->monom, tm->monom, use_coeffs);
      tm->next = 0;
      if (K->is_zero(g))
	{
	  K->remove(g);
	  getRing()->vec_remove_term(tm);
	  continue;
	}
      ERingElement g1 = K->mult(g,a);
      K->remove(a);
      K->remove(g);
      tm->coeff = g1;
      result.append(tm);
    }
  return result.value();
}

EVector EVector::diff(const EFreeModule *resultF, const EVector &w, bool use_coeffs) const
{
  EVectorHeap result(resultF);
  for (iterator t = *this; t.valid(); ++t)
    {
      EVector h = w.diff_by_term(F, *t, use_coeffs); 
      result.add(h);
    }
  return result.value();
}


//////////////////
// Coefficients //
//////////////////
// These two routines are used in EMatrix::coefficients.

static bool moneq(const int *exp1, int *exp2, int nvars, const bool *vars)
  // Compares exp1 and exp2 in the variables for which vars[i] is true.
  // Returns true if exp1 and exp2 are equal in these components.
  // As a by-product, the entries of exp2 in these variables are set to zero.
{
  for (int i=0; i<nvars; i++)
    {
      if (!vars[i]) continue;
      if (exp1[i] != exp2[i]) 
	return false;
      else 
	exp2[i] = 0;
    }
  return true;
}
EVector EVector::strip_vector(const bool *vars, 
			      const EFreeModule *Fmonom, EVector &vmonom)
  // WARNING: modifies 'this'.
{
  if (len == 0)
    {
      vmonom = Fmonom->zero();
      return F->zero();
    }
  const EPolynomialRing *R = getRing()->toPolynomialRing();
  if (R == 0)
    {
      vmonom = Fmonom->basisElement(0);
      return *this;  // This sets this to the zero vector.
    }
  // At this point, we know that we have a polynomial ring
  int nvars = R->n_vars();
  int *exp = new int[nvars];
  int *scratch_exp = new int[nvars];
  const EMonoid *M = R->getMonoid();

  M->copy_exponents(elems->monom, exp);
  for (int i=0; i<nvars; i++)
    if (!vars[i]) exp[i] = 0;

  vmonom = R->vec_term(Fmonom,
		       R->getCoefficientRing()->one(), 
		       M->monomial_from_exponents(exp),
		       0);

  EVectorHeap result(F);
  collector newthis(F);

  // Loop through f: if monomial matches 'exp', strip and add to result,
  // otherwise leave alone, and place on head list.
  while (elems != 0)
    {
      evec *tm = elems;
      elems = elems->next;
      len--;
      tm->next = 0;
      M->copy_exponents(tm->monom, scratch_exp);
      if (moneq(exp, scratch_exp, nvars, vars))
	{
	  tm->monom = M->monomial_from_exponents(scratch_exp);
	  result.add(tm);
	}
      else
	newthis.append(tm);
    }
  delete [] exp;
  delete [] scratch_exp;
  *this = newthis.value();
  return result.value();
}

////////////////////
// Change of ring //
////////////////////

bool EVector::promote(const EFreeModule *newF, EVector &result) const
  // Rf --> A, ring(this) = Rf, ring(newF) = A
{
  if (getRing() == newF->getRing())
    {
      result = translate(newF);
      return true;
    }
  return newF->getRing()->vec_promote(*this, newF, result);
}

bool EVector::lift(const EFreeModule *newF, EVector &result) const
  // Rg --> A, ring(this)=A, ring(newF) = Rg.
{
  if (getRing() == newF->getRing())
    {
      result = translate(newF);
      return true;
    }
  return getRing()->vec_lift(*this, newF, result);
}



/////////////////
// Vector heap //
/////////////////

static int heap_size[GEOHEAP_SIZE] = {4, 16, 64, 256, 1024, 4096, 
				    16384, 65536, 262144, 1048576, 4194304,
				    16777216, 67108864, 268435456,
				    1073741824};


EVectorHeap::EVectorHeap(const EFreeModule *FF)
  : F(FF),
  top_of_heap(-1)
{
  int i;
  for (i=0; i<GEOHEAP_SIZE; i++)
    heap[i] = EVector(F,0,0);
}

EVectorHeap::~EVectorHeap()
{
  // The user of this class must insure that all 'evec's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

void EVectorHeap::add(EVector &G)
{
  int len = G.len;
  int i = 0;
  while (len >= heap_size[i]) i++;
  heap[i].addTo(G);
  len = heap[i].len;
  while (len >= heap_size[i])
    {
      i++;
      heap[i].addTo(heap[i-1]);
      len = heap[i].len;
    }
  if (i > top_of_heap)
    top_of_heap = i;
}

void EVectorHeap::add(evec *tm)
{
  EVector v(F,tm);
  add(v);
  v.elems = 0;
}

EVector EVectorHeap::value()
{
  EVector result = F->zero();
  for (int i=0; i<=top_of_heap; i++)
    {
      if (heap[i].len == 0) continue;
      result.addTo(heap[i]);
    }
  top_of_heap = -1;
  return result;
}

#if 0
bool EVectorHeap::remove_lead_term(int &coeff, monomial *&monom)
{
  int lead_so_far = -1;
  for (int i=0; i <= top_of_heap; i++)
    {
      if (heap[i].len == 0) continue;
      if (lead_so_far < 0) 
	{
	  lead_so_far = i;
	  continue;
	}
      int cmp = M->compare(heap[lead_so_far].elems->monom, heap[i].elems->monom);
      if (cmp == GT) continue;
      if (cmp == LT)
	{
	  lead_so_far = i;
	  continue;
	}
      // At this point we have equality
      heap[lead_so_far].elems->coeff = K->add(heap[lead_so_far].elems->coeff, heap[i].elems->coeff);
      heap[i].len--;
      evec * tmp = heap[i].elems;
      heap[i].elems = tmp->next;
      tmp->next = 0;
      R->delete_term(tmp);

      if (K->is_zero(heap[lead_so_far].elems->coeff))
	{
	  // Remove, and start over
	  heap[lead_so_far].len--;
	  tmp = heap[lead_so_far].elems;
	  heap[lead_so_far].elems = tmp->next;
	  tmp->next = 0;
	  R->delete_term(tmp);
	  lead_so_far = -1;
	  i = -1;
	}
    }
  if (lead_so_far < 0) return false;
  heap[lead_so_far].len--;
  evec * result = heap[lead_so_far].elems;
  heap[lead_so_far].elems = result->next;
  result->next = NULL;
  coeff = result->coeff;
  monom = result->monom;
  R->delete_term(result);
  return true;
}
#endif

/////////////////////
// Polynomial heap //
/////////////////////
EPolynomialRing::heap::heap(const EPolynomialRing *R)
  : R(R),
  top_of_heap(-1)
{
  M = R->getMonoid();
  K = R->getCoefficientRing();

  int i;
  for (i=0; i<GEOHEAP_SIZE; i++)
    theHeap[i] = 0;
}

EPolynomialRing::heap::~heap()
{
  // The user of this class must insure that all 'epoly's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

int EPolynomialRing::heap::n_terms(const epoly *g) const
{
  int len = 0;
  for ( ; g!=0; g=g->next) len++;
  return len;
}
void EPolynomialRing::heap::add(ERingElement G)
{
  epoly *g = POLYVAL(G);
  add(g);
}

void EPolynomialRing::heap::add(epoly *g)
{
  int len = n_terms(g);
  int i = 0;
  while (len >= heap_size[i]) i++;
  R->add_to(theHeap[i], g);
  len = n_terms(theHeap[i]);
  while (len >= heap_size[i])
    {
      i++;
      R->add_to(theHeap[i], theHeap[i-1]);
      len = n_terms(theHeap[i]);
    }
  if (i > top_of_heap)
    top_of_heap = i;
}

epoly * EPolynomialRing::heap::poly_value()
{
  epoly *result = 0;
  for (int i=0; i<=top_of_heap; i++)
    {
      if (theHeap[i] == 0) continue;
      R->add_to(result, theHeap[i]);
    }
  top_of_heap = -1;
  return result;
}
ERingElement EPolynomialRing::heap::value()
{
  epoly *result = poly_value();
  return POLY_TO_ERingElement(result);
}

// This is a test of our interfaces:
#if 0

EVector EFreeModule::random() const
{
  ERing *K = this->R->getCoefficientRing();
  EMonoid *M = this->R->getMonoid();

  EVectorCollect result(this);
  for (int i=0; i<rank; i++)
    result.append(makeTerm(K->random(), M->one(), i));

  return result.value();
}

#endif
