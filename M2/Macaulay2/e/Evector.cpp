// Copyright 1998 by Michael Stillman

#include "Evector.hpp"
#include "text_io.hpp"

EVector::~EVector()
{
  if (F == 0) return;
  const EPolynomialRing *R = F->getRing();
  while (elems != NULL)
    {
      poly *tmp = elems;
      elems = elems->next;
      R->deleteTerm(tmp);
    }
  F = 0;
  len = 0;
}

void EVector::prepend_term(poly *t)
{
  len++;
  t->next = elems;
  elems = t;
}

EVector *EVector::clone() const
{
  const EPolynomialRing *R = F->getRing();
  poly head;
  poly *result = &head;
  for (poly *a = elems; a != 0; a = a->next)
    {
      result->next = R->copy_term(a);
      result = result->next;
    }
  result->next = 0;
  EVector *G = F->buildVector(head.next,len);
  return G;
}

EVector *EVector::zero(const EFreeModule *F)
{
  EVector *result = F->buildVector(0,0);
  return result;
}

bool EVector::isEqual(const EVector *w) const
{
  if (w->len != len) return false;
  //if (w->F != F) return false;	// This is OK?
  if (!F->isEqual(w->F)) return false;
  const EPolynomialRing *R = F->getRing();
  const EMonoid *M = R->getMonoid();
  const ECoefficientRing *K = R->getCoefficientRing();
  poly *a = elems;
  poly *b = w->elems;
  for ( ; a != 0; a=a->next, b=b->next)
    {
      if (a->component != b->component)
	return false;
      if (!K->is_equal(a->coeff, b->coeff))
	return false;
      if (!M->is_equal(a->monom, b->monom))
	return false;
    }
  return true;
}

ERingElement *EVector::getComponent(int x) const
{
  const EPolynomialRing *R = F->getRing();
  poly head;
  poly *result = &head;
  int newlen = 0;
  for (poly *a = elems; a != 0; a = a->next)
    if (a->component == x)
    {
      result->next = R->copy_term(a);
      result = result->next;
      result->component = 0;
      newlen++;
    }
  result->next = 0;
  EVector *G = R->getRingFreeModule()->buildVector(head.next,newlen);
  return G;
}

EVector *EVector::leadTerm(int n, bool only_same_component) const
{
  const EPolynomialRing *R = F->getRing();
  const EMonoid *M = R->getMonoid();
  if (n == -1)
    n = M->getMonomialOrder()->n_blocks();
  int newlen = 0;
  poly head;
  poly *result = &head;
  poly *first = elems;
  for (poly *a = elems; a != 0; a=a->next)
    {
      if (M->compare(a->monom,first->monom,n) == EQ)
        {
          if (only_same_component && (a->component != first->component))
            continue;
          result->next = R->copy_term(a);
          newlen++;
          result = result->next;
        }
      else 
        break;
    }
  result->next = 0;
  return F->buildVector(head.next,newlen);
}

EVector *EVector::getTerms(int lo, int hi) const
{
  const EPolynomialRing *R = F->getRing();
  if (lo < 0) lo = len + lo;
  if (hi < 0) hi = len + hi;
  int n = 0;
  int newlen = 0;
  poly head;
  poly *result = &head;
  for (poly *a = elems; a != 0; a=a->next)
    {
      if (n > hi) break;
      if (n >= lo)
        {
          result->next = R->copy_term(a);
          newlen++;
          result = result->next;
        }
      n++;
    }
  result->next = 0;
  return F->buildVector(head.next,newlen);
}

void EVector::addTo(EVector *&g)
{
  EVector *f = (EVector *)this;
  F->getRing()->addTo(f, g);
}

EVector *EVector::negate() const
{
  const EPolynomialRing *R = F->getRing();
  const ECoefficientRing *K = R->getCoefficientRing();
  poly head;
  poly *result = &head;
  for (poly *a = elems; a != 0; a = a->next)
    {
      result->next = R->newTerm();
      result = result->next;
      result->coeff = K->negate(a->coeff);
      result->monom = a->monom;
      result->component = a->component;
    }
  result->next = 0;
  EVector *G = F->buildVector(head.next,len);
  return G;
}

void EVector::subtractTo(EVector *&g)
{
  EVector *f = (EVector *)this;
  EVector *h = g->negate();
  F->getRing()->addTo(f, h);
  delete g;
  g = h;
}

EVector *EVector::multiply_by_ZZ(int m) const
{
  const EPolynomialRing *R = F->getRing();
  const ECoefficientRing *K = R->getCoefficientRing();
  field m1 = K->from_int(m);
  if (K->is_zero(m1))
    return F->zero();
  poly head;
  poly *result = &head;
  for (poly *a = elems; a != 0; a = a->next)
    {
      result->next = R->newTerm();
      result = result->next;
      result->coeff = K->mult(m1,a->coeff);
      result->monom = a->monom;
      result->component = a->component;
    }
  result->next = 0;
  EVector *G = F->buildVector(head.next,len);

  // NOTE: once we can mod out by ideals, need to call a normalize routine...
  return G;
}

EVector *EVector::add(EVector *g) const
{
  EVector *h1 = clone();
  EVector *h2 = g->clone();
  h1->addTo(h2);
  return h1;
}

EVector *EVector::subtract(EVector *g) const
{
  EVector *h1 = clone();
  EVector *h2 = g->negate();
  h1->addTo(h2);
  return h1;
}

EVector *EVector::multiply(const EVector *a) const
{
  return F->getRing()->multiply(this,a);
}

EVector *EVector::rightMultiply(const EVector *a) const
{
  return F->getRing()->rightMultiply(this,a);
}

bool EVector::isGraded(monomial *&result_degree) const
{
  monomial *lo;
  degreeLoHi(lo,result_degree);
  return F->getDegreeMonoid()->is_equal(lo,result_degree);
}

void EVector::degreeLoHi(monomial *&lo, monomial *&hi) const
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

monomial *EVector::degree() const
{
  monomial *lo, *hi;
  degreeLoHi(lo,hi);
  return hi;
}

void EVector::degreeWeightsLoHi(int i, int &lo, int &hi) const
{
  if (len == 0) return;
  poly *p = elems;
  const EMonoid *M = F->getMonoid();
  const EMonoid *D = F->getDegreeMonoid();
  const int *wts = F->getRing()->getDegreeVector(i);
  buffer o;
  o << "weights: ";
  for (int j=0; j<M->n_vars(); j++)
    o << wts[j] << " ";
  int d = M->degree(p->monom,wts);
  d += D->to_exponents(F->getDegree(p->component))[i];
  o << "degree " << i << ": " << d << " ";
  lo = hi = d;
  for (p = p->next; p != 0; p=p->next)
    {
      d = M->degree(p->monom,wts);
      d += D->to_exponents(F->getDegree(p->component))[i];
      o << d << " ";
      if (d < lo) lo = d;
      if (d > hi) hi = d;
    }
  o << newline;
  emit(o.str());
}

void EVector::degreeWeightsLoHi(const int *wts, 
                            const int *componentdegs, 
                            int &lo, int &hi) const
{
  if (len == 0) return;
  poly *p = elems;
  const EMonoid *M = F->getMonoid();
  int d = M->degree(p->monom,wts);
  d += componentdegs[p->component];
  lo = hi = d;
  for (p = p->next; p != 0; p=p->next)
    {
      d = M->degree(p->monom,wts);
      d += componentdegs[p->component];
      if (d < lo) lo = d;
      if (d > hi) hi = d;
    }
}
void EVector::degreeWeightsLoHi(const int *wts, 
                            int &lo, int &hi) const
{
  if (len == 0) return;
  poly *p = elems;
  const EMonoid *M = F->getMonoid();
  int d = M->degree(p->monom,wts);
  lo = hi = d;
  for (p = p->next; p != 0; p=p->next)
    {
      d = M->degree(p->monom,wts);
      if (d < lo) lo = d;
      if (d > hi) hi = d;
    }
}

EVector *EVector::homogenize(int v, int d, const int *wts) const
{
  if (wts[v] == 0) return 0;

  const EPolynomialRing *R = F->getRing();
  const EMonoid *M = R->getMonoid();
  const ECoefficientRing *K = R->getCoefficientRing();
  int nvars = M->n_vars();
  
  const int *exp1;
  int *exp = new int[nvars];

  poly head;
  poly *result = &head;
  for (poly *a = elems ; a != NULL; a = a->next)
    {
      exp1 = M->to_exponents(a->monom);
      for (int i=0; i<nvars; i++)
        exp[i] = exp1[i];
      int e = 0;
      for (int i=0; i<nvars; i++) e += wts[i] * exp[i];
      //e += F->getDegree(a->comp)->exponents[0];
      if (((d-e) % wts[v]) != 0)
	{
	  // We cannot homogenize, so clean up and exit.
	  result->next = 0;
	  R->delete_terms(head.next);
	  gError << "homogenization impossible";
	  delete [] exp;
	  return 0;
	}
      exp[v] += (d - e) / wts[v];

      result->next = R->newTerm();
      result = result->next;
      result->coeff = a->coeff;
      result->component = a->component;
      result->monom = M->monomial_from_exponents(exp);
    }
  result->next = 0;
  EVector *G = F->buildVector(head.next,len);
  G->sort();
  delete [] exp;
  return G;
}

EVector *EVector::homogenize(int v, const int *wts) const
{
  int lo, hi;
  degreeWeightsLoHi(wts,lo,hi);
  int d = (wts[v] > 0 ? hi : lo);
  return homogenize(v,d,wts);
}

#if 0
EVector *EVector::diff(const int *exponents, bool use_coeffs) const
{
  
}
#endif

void EVector::sort()
{
  // Divide elems into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.
  
  if (len <= 1) return;
  F->sort(elems);
  len = F->getRing()->n_terms(elems);
}





static int heap_size[GEOHEAP_SIZE] = {4, 16, 64, 256, 1024, 4096, 
				    16384, 65536, 262144, 1048576, 4194304,
				    16777216, 67108864, 268435456,
				    1073741824};


EVectorHeap::EVectorHeap(const EFreeModule *FF)
  : F(FF),
  top_of_heap(-1)
{
  R = F->getRing();
  M = R->getMonoid();
  K = R->getCoefficientRing();

  int i;
  for (i=0; i<GEOHEAP_SIZE; i++)
    heap[i] = F->zero();
}

EVectorHeap::~EVectorHeap()
{
  // The user of this class must insure that all 'poly's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

void EVectorHeap::add(EVector *G)
{
  int len = G->len;
  int i = 0;
  while (len >= heap_size[i]) i++;
  heap[i]->addTo(G);
  len = heap[i]->len;
  while (len >= heap_size[i])
    {
      i++;
      heap[i]->addTo(heap[i-1]);
      len = heap[i]->len;
    }
  if (i > top_of_heap)
    top_of_heap = i;
}

EVector *EVectorHeap::value()
{
  EVector *result = F->zero();
  for (int i=0; i<=top_of_heap; i++)
    {
      if (heap[i]->len == 0) continue;
      result->addTo(heap[i]);
    }
  top_of_heap = -1;
  return result;
}

#if 0
bool EVectorHeap::remove_lead_term(field &coeff, monomial *&monom)
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
      poly * tmp = heap[i].elems;
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
  poly * result = heap[lead_so_far].elems;
  heap[lead_so_far].elems = result->next;
  result->next = NULL;
  coeff = result->coeff;
  monom = result->monom;
  R->delete_term(result);
  return true;
}
#endif

// This is a test of our interfaces:
#if 0
// Result goes into the free module F
EVector EVector::eval(const RingMap *map, const EFreeModule *resultF)
{
  EVectorHeap result(resultF);

  for (EVectorCursor i = this; i.valid(); i++)
    {
      vector f = map->evaluateTerm(F, resultF, i->getCoefficient(), i->getMonomial());
      result.add(f);
    }
  return result.value();
}

EVector EFreeModule::random() const
{
  ECoefficientRing *K = this->R->getCoefficientRing();
  EMonoid *M = this->R->getMonoid();

  EVectorCollect result(this);
  for (int i=0; i<rank; i++)
    result.append(makeTerm(K->random(), M->one(), i));

  return result.value();
}
#endif
