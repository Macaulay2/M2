#include "EZZp.hpp"
#include "Ering.hpp"
#include "Evector.hpp"

stash *ERing::vec_stash = 0;
stash *ERing::vecpoly_stash = 0;

EZZ *EZZ::_ZZ = 0;
void EZZp::gcd_extended(int a, int b, int &u, int &v, int &g) const
{
 int q ;
 int u1, v1, g1;
 int utemp, vtemp, gtemp;
 
 g1 = b;     u1 = 0;         v1 = 1;
 g  = a;     u  = 1;         v  = 0;
 while (g1 != 0)
   {
     q = g / g1 ;
     gtemp = g - q * g1;
     utemp = u - q * u1;
     vtemp = v - q * v1;
     g  = g1;    u  = u1;     v  = v1 ;
     g1 = gtemp; u1 = utemp;  v1 = vtemp;
   }
}

int object_ERingElement::length_of() const 
{ 
  if (R->is_zero(val)) return 0;
  const EPolynomialRing *S = R->toPolynomialRing();
  if (S == 0) return 1;
  return S->n_terms(POLYVAL(val));
}

////////////////////////////////
// Polynomial/vector routines //
////////////////////////////////

////////////////////////
// new low-level term //
////////////////////////
epoly *EPolynomialRing::new_poly_term() const
  // Return a new element, which has NO fields set.
  // WARNING: you MUST set these fields immediately.
{
  epoly *result = (epoly *)(epoly_stash->new_elem());
  return result;
}
evec *ERing::vec_new_term() const
  // Return a new element, which has NO fields set.
  // WARNING: you MUST set these fields immediately.
{
  evec *result = (evec *)(evec_stash->new_elem());
  return result;
}
///////////////////////////
// low-level remove term //
///////////////////////////
void EPolynomialRing::remove_poly_term(epoly *f) const
{
  if (f == 0) return;
  K->remove(f->coeff);
  epoly_stash->delete_elem(f);
}
void ERing::vec_remove_term(evec *f) const
{
  if (f == 0) return;
  K->remove(f->coeff);
  evec_stash->delete_elem(f);
}
///////////////////////////
// low-level copy term   //
///////////////////////////
// vector version is virtual, since we don't know about the monomial.
epoly *EPolynomialRing::copy_term(const epoly *t) const
{
  epoly *result = new_poly_term();
  result->coeff = K->clone(t->coeff);
  result->monom = t->monom;
  result->next = 0;
  return result;
}
evec *ERing::vec_copy_term(const evec *t) const
{
  evec *result = vec_new_term();
  result->coeff = K->clone(t->coeff);
  result->component = t->component;
  result->next = 0;
  return result;
}
evec *EPolynomialRing::vec_copy_term(const evec *t) const
{
  evec *result = vec_new_term();
  result->coeff = K->clone(t->coeff);
  result->component = t->component;
  result->monom = t->monom;
  result->next = 0;
  return result;
}
//////////////////////
// are terms equal? //
//////////////////////
bool EPolynomialRing::terms_equal(const epoly *s, const epoly *t) const
{
  if (!K->is_equal(s->coeff, t->coeff)) return false;
  if (!getMonoid()->is_equal(s->monom, t->monom)) return false;
  return true;
}
bool ERing::vec_terms_equal(const evec *s, const evec *t) const
{
  if (s->component != t->component) return false;
  if (!K->is_equal(s->coeff, t->coeff)) return false;
  return true;
}
bool EPolynomialRing::vec_terms_equal(const evec *s, const evec *t) const
{
  if (s->component != t->component) return false;
  if (!K->is_equal(s->coeff, t->coeff)) return false;
  if (!getMonoid()->is_equal(s->monom, t->monom)) return false;
  return true;
}
///////////////////////////////
// compare terms: LT,EQ,GT ////
// EQ doesn't nec mean equal //
///////////////////////////////
int ERing::vec_compare_terms(const evec *v, const evec *w) const
{
  int cmp = v->component - w->component;
  if (cmp < 0) return LT;
  if (cmp > 0) return GT;
  return EQ;
}
int EPolynomialRing::vec_compare_terms(const evec *v, const evec *w) const
{
  return getMonoid()->compare(v->monom, v->component, w->monom, w->component);
}
///////////////////////
// vec -> ring elem  //
///////////////////////
ERingElement ERing::vec_term_to_ring(const evec *v) const
{
  return K->clone(v->coeff);
}
ERingElement EPolynomialRing::vec_term_to_ring(const evec *v) const
{
  epoly *tm = new_poly_term();
  tm->coeff = K->clone(v->coeff);
  tm->monom = v->monom;
  tm->next = 0;
  return POLY_TO_ERingElement(tm);
}
ERingElement ERing::vec_component(const EVector &v, int x) const
{
  // We use the fact that elements are stored in descending component.
  for (EVector::iterator t = v; t.valid(); ++t)
    {
      if (t->component < x) break;
      if (t->component == x) return vec_term_to_ring(*t);
    }
  return zero();
}
ERingElement EPolynomialRing::vec_component(const EVector &v, int x) const
{
  // We must scan the entire vector
  collector result(this);
  for (EVector::iterator t = v; t.valid(); ++t)
    {
      if (t->component != x) continue;
      epoly *tm = POLYVAL(vec_term_to_ring(*t));
      result.append(tm);
    }
  return result.value();
}

///////////////////////////
// number of terms       //
///////////////////////////
int EPolynomialRing::n_terms(const epoly *f) const
{
  int result = 0;
  for ( ; f != 0; f=f->next) result++;
  return result;
}
#if 0
int ERing::n_terms(const evec *f) const
{
  int result = 0;
  for ( ; f != 0; f=f->next) result++;
  return result;
}
#endif
///////////
// clone //
///////////
epoly * EPolynomialRing::clone(const epoly *v) const
{
  collector w(this);
  for (iterator p = v; p.valid(); ++p)
    w.append(copy_term(*p));
  return w.poly_value();
}
EVector EVector::clone() const
{
  EVector::collector w(F);
  for (EVector::iterator p = *this; p.valid(); ++p)
    w.append(getRing()->vec_copy_term(*p));
  return w.value();
}
////////////
// remove //
////////////
void EPolynomialRing::remove(epoly *f) const
{
  while (f != 0) {
    epoly *tmp = f;
    f = f->next;
    remove_poly_term(tmp);
  }
}
void EVector::reset()
  // Reset this to 0 vector.
{
  if (elems == 0) return;
  const ERing *R = getRing();
  evec *f = elems;
  elems = 0;
  len = 0;
  while (f != 0)
    {
      evec *tmp = f;
      f = f->next;
      R->vec_remove_term(tmp);
    }
}
EVector::~EVector()
  // Reset this to 0 vector.
{
  reset();
}
/////////////
// isEqual //
/////////////
bool EPolynomialRing::is_equal(const epoly *a, const epoly *b) const
{
  for ( ; a != 0; a=a->next, b=b->next)
    {
      if (b == 0) return false;
      if (!terms_equal(a,b)) return false;
    }
  if (b != 0) return false;
  return true;
}
bool EVector::isEqual(const EVector &w) const
{
  // Note: here we do NOT check the free module!
  if (len != w.len) return false;
  const ERing *R = getRing();
  if (R != w.getRing()) return false;
  EVector::iterator a = *this;
  EVector::iterator b = w;
  for ( ; a.valid(); ++a, ++b)
    if (!R->vec_terms_equal(*a, *b)) return false;
  return true;
}
///////////////////////
// negate_to, negate //
///////////////////////
void EPolynomialRing::negate_to(const epoly *a) const
{  
  for (iterator p = a; p.valid(); ++p)
    K->negate_to(p->coeff);
}
void EVector::negateTo()
  // Polynomial and non-polynomial vector cases
{
  const ERing *K = getRing()->getVectorCoefficientRing();  // !!
  for (iterator p = *this; p.valid(); ++p)
    K->negate_to(p->coeff);
}
epoly *EPolynomialRing::negate(const epoly *a) const
{  
  epoly *tmp;
  collector result(this);
  for (iterator p = a; p.valid(); ++p)
    {
      tmp = copy_term(*p);
      K->negate_to(tmp->coeff);
      result.append(tmp);
    }
  return result.poly_value();
}
EVector EVector::negate() const
  // Polynomial and non-polynomial vector cases
{
  const ERing *R = getRing();
  const ERing *K = R->getVectorCoefficientRing();
  evec *tmp;
  collector result(F);
  for (EVector::iterator p = *this; p.valid(); ++p)
    {
      tmp = R->vec_copy_term(*p);
      K->negate_to(tmp->coeff);
      result.append(tmp);
    }
  return result.value();
}
////////////
// add_to //
////////////
// Versions: polyring, vector, polyvector, polyvector(schreyer)
// 
void EPolynomialRing::add_to(epoly *&f, epoly *&g) const
{
  epoly *tmf, *tmg, *tmp;
  // Addition of ring elements
  const EMonoid *M = getMonoid();
  
  if (g == 0) return;
  if (f == 0) { f = g; g = 0; return; }
  collector result(this);
  while (1)
    switch (M->compare(f->monom, 0, g->monom, 0))
      {
      case LT:
        tmp = g;
        g = g->next;
	tmp->next = 0;
        result.append(tmp);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    f = result.poly_value();
	    return;
	  }
	break;
      case GT:
        tmp = f;
        f = f->next;
	tmp->next = 0;
        result.append(tmp);
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    f = result.poly_value();
	    g = 0;
	    return;
	  }
	break;
      case EQ:
	tmf = f;
	tmg = g;
	f = f->next;
	g = g->next;
	tmf->next = 0;
	tmg->next = 0;
	K->add_to(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  remove_poly_term(tmf);
	else
	  result.append(tmf);
	remove_poly_term(tmg);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    f = result.poly_value();
	    return;
	  }
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    g = 0;
	    f = result.poly_value();
	    return;
	  }
	break;
      }
}
static int sign(int c)
{
  if (c < 0) return LT;
  if (c > 0) return GT;
  return EQ;
}
void ERing::vec_add_to(EVector &v, EVector &w) const
{
  evec *tmf, *tmg, *tmp;
  
  evec *f = v.elems;
  evec *g = w.elems;
  if (g == 0) return;
  if (f == 0) { v = w; w.len = 0; w.elems = 0; return; }
  v.elems = 0;
  w.elems = 0;
  w.len = 0;
  EVector::collector result(v.getFreeModule());
  while (1)
    switch (sign(f->component - g->component))
      {
      case LT:
        tmp = g;
        g = g->next;
	tmp->next = 0;
        result.append(tmp);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      case GT:
        tmp = f;
        f = f->next;
	tmp->next = 0;
        result.append(tmp);
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      case EQ:
	tmf = f;
	tmg = g;
	f = f->next;
	g = g->next;
	tmf->next = 0;
	tmg->next = 0;
	K->add_to(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  vec_remove_term(tmf);
	else
	  result.append(tmf);
	vec_remove_term(tmg);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      }
}
void EPolynomialRing::add_to0(EVector &v, EVector &w) const
{
  evec *tmf, *tmg, *tmp;
  const EMonoid *M = getMonoid();
  
  evec *f = v.elems;
  evec *g = w.elems;
  if (g == 0) return;
  if (f == 0) { v = w; w.len = 0; w.elems = 0; return; }
  v.elems = 0;
  w.elems = 0;
  w.len = 0;
  EVector::collector result(v.getFreeModule());
  while (1)
    switch (M->compare(f->monom, f->component, g->monom, g->component))
      {
      case LT:
        tmp = g;
        g = g->next;
	tmp->next = 0;
        result.append(tmp);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      case GT:
        tmp = f;
        f = f->next;
	tmp->next = 0;
        result.append(tmp);
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      case EQ:
	tmf = f;
	tmg = g;
	f = f->next;
	g = g->next;
	tmf->next = 0;
	tmg->next = 0;
	K->add_to(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  vec_remove_term(tmf);
	else
	  result.append(tmf);
	vec_remove_term(tmg);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      }
}
void EPolynomialRing::add_to_schreyer(EVector &v, EVector &w) const
{
  evec *tmf, *tmg, *tmp;
  const EMonoid *M = getMonoid();
  
  evec *f = v.elems;
  evec *g = w.elems;
  if (g == 0) return;
  if (f == 0) { v = w; w.len = 0; w.elems = 0; return; }
  v.elems = 0;
  w.elems = 0;
  w.len = 0;
  EVector::collector result(v.getFreeModule());
  while (1)
    switch (M->compare(f->monom, 0, g->monom, 0))
      {
      case LT:
        tmp = g;
        g = g->next;
	tmp->next = 0;
        result.append(tmp);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      case GT:
        tmp = f;
        f = f->next;
	tmp->next = 0;
        result.append(tmp);
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      case EQ:
	tmf = f;
	tmg = g;
	f = f->next;
	g = g->next;
	tmf->next = 0;
	tmg->next = 0;
	K->add_to(tmf->coeff, tmg->coeff);
	if (K->is_zero(tmf->coeff))
	  vec_remove_term(tmf);
	else
	  result.append(tmf);
	vec_remove_term(tmg);
	if (g == 0) 
	  {
	    result.append_final_terms(f);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	if (f == 0) 
	  {
	    result.append_final_terms(g);
	    EVector tmpv = result.value();
	    v = tmpv;
	    return;
	  }
	break;
      }
}
void EPolynomialRing::vec_add_to(EVector &v, EVector &w) const
{
  if (v.getFreeModule()->hasInducedOrder())
    add_to_schreyer(v,w);
  else
    add_to0(v,w);
}
///////////////////
// add, subtract //
///////////////////
epoly *EPolynomialRing::add(const epoly *a, const epoly *b) const
{
  epoly *a1 = clone(a);
  epoly *b1 = clone(b);
  add_to(a1,b1);
  return a1;
}
EVector EVector::add(const EVector &b) const
{
  EVector a1 = clone();
  EVector b1 = b.clone();
  a1.addTo(b1);
  return a1;
}
epoly *EPolynomialRing::subtract(const epoly *a, const epoly *b) const
{
  epoly *a1 = clone(a);
  epoly *b1 = negate(b);
  add_to(a1,b1);
  return a1;
}
EVector EVector::subtract(const EVector &b) const
{
  EVector a1 = clone();
  EVector b1 = b.negate();
  a1.addTo(b1);
  return a1;
}
////////////////////////////
// make_term, make_vector //
////////////////////////////
epoly * EPolynomialRing::_make_term(const ERingElement a, 
                                   const monomial *m) const
{
  if (K->is_zero(a)) return 0;
  epoly *result = new_poly_term();
  result->coeff = K->clone(a);
  result->monom = m;
  result->next = 0;
  
  // Now we need to reduce this element, if in a quotient ring.
  return result;

}
EVector ERing::vec_make(const EFreeModule *F, const ERingElement a, int x) const
{
  if (K->is_zero(a)) return F->zero();

  evec *tm = vec_new_term();
  tm->coeff = K->clone(a);
  tm->component = x;
  tm->next = 0;
  return EVector(F,1,tm);
}
// Design flaw: this nest routine MUST have 'a' in what ring?
// Two choices: 'this', 'K'.  Should be 'this'.
EVector EPolynomialRing::vec_make(const EFreeModule *F, const ERingElement a, int x) const
{
  EVector::collector result(F);
  for (iterator p = a; p.valid(); ++p)
    {
      evec *tm = vec_new_term();
      tm->coeff = K->clone(p->coeff);
      tm->component = x;
      tm->monom = p->monom;
      result.append(tm);
    }
  return result.value();
}
EVector EPolynomialRing::vec_term(const EFreeModule *F, 
                                     const ERingElement a, 
                                     const monomial *m,
                                     int x) const
{
  if (K->is_zero(a)) return F->zero();

  evec *tm = vec_new_term();
  tm->coeff = K->clone(a);
  tm->monom = m;
  tm->component = x;
  tm->next = 0;
  return EVector(F,1,tm);  // WARNING: NEED to reduce this once quotient
                           // rings are in place.
}
/////////////////////////////
// from_int, basis_element //
/////////////////////////////
epoly *EPolynomialRing::_from_int(int a) const
{
  ERingElement b = K->from_int(a);
  epoly *result = _make_term(b,getMonoid()->one());
  K->remove(b);
  return result;
}
////////////////
// lead stuff //
////////////////
const monomial *EPolynomialRing::leadMonomial(const ERingElement f) const
{
  epoly *t = POLYVAL(f);
  if (t == 0) return 0;
  return t->monom;
}
ERingElement EPolynomialRing::leadCoefficient(const ERingElement f) const
{
  epoly *t = POLYVAL(f);
  if (t == 0) return K->zero();
  return K->clone(t->coeff);
}
ERingElement EPolynomialRing::leadTerm(const ERingElement f, int n) const
{
  epoly *first = POLYVAL(f);
  if (first == 0) return zero();
  const EMonoid *M = getMonoid();
  if (n == -1)
    n = M->getMonomialOrder()->n_blocks();
  collector w(this);
  for (iterator p = f; p.valid(); ++p)
    if (M->compare(p->monom,first->monom,n) == EQ)
      w.append(copy_term(*p));
    else 
      break;
  return w.value();
}

////////////
// degree //
////////////
void ERing::degreeLoHi(ERingElement f, 
		       const monomial *&lo, 
		       const monomial *&hi) const
{
  int ndegrees = D->n_vars();
  int *a = new int[ndegrees];
  int *b = new int[ndegrees];
  for (int i=0; i<ndegrees; i++)
    degreeWeightsLoHi(f, ndegrees, getDegreeVector(i), a[i], b[i]);
  lo = D->monomial_from_exponents(a);
  hi = D->monomial_from_exponents(b);
  delete [] a;
  delete [] b;
}
const monomial * ERing::degree(ERingElement f) const
{
  int ndegrees = D->n_vars();
  int *a = new int[ndegrees];
  int junk;
  for (int i=0; i<ndegrees; i++)
    degreeWeightsLoHi(f, ndegrees, getDegreeVector(i), junk, a[i]);
  const monomial *result = D->monomial_from_exponents(a);
  delete [] a;
  return result;
}
bool ERing::isGraded(ERingElement f, 
		     const monomial *&result_degree) const
{
  const monomial *lo;
  degreeLoHi(f,lo,result_degree);
  return D->is_equal(lo,result_degree);
}

void ERing::degreeWeightsLoHi(ERingElement f, int nwts, const int *wts,
                       int &lo,
                       int &hi) const
{
  // The default situation is ungraded:
  lo = hi = 0;
}
void EPolynomialRing::degreeWeightsLoHi(ERingElement f, int nwts, const int *wts,
                                int &lo,
                                int &hi) const
{
  const EMonoid *M = getMonoid();
  iterator p = f;
  if (!p.valid()) 
    {
      lo = hi = 0;
      return;
    }
  int d = M->degree(p->monom, wts);
  lo = hi = d;
  for (++p; p.valid(); ++p)
    {
      d = M->degree(p->monom, wts);
      if (nwts > nvars)
        {
          int d1, d2;
          K->degreeWeightsLoHi(p->coeff, nwts-nvars, wts+nvars, d1, d2);
          d1 += d;
          d2 += d;
          if (d1 < lo) lo = d1;
          if (d2 > hi) hi = d2;
        }
      else
	{
	  if (d < lo) lo = d;
	  else if (d > hi) hi = d;
	}
    }
}
void ERing::vec_degree_lohi(const evec *v, int nwts, const int *wts,
                            int &lo, int &hi) const
{
  if (nvars == 0 || v == 0)
    {
      lo = hi = 0;
      return;
    }
  degreeWeightsLoHi(v->coeff, nwts, wts, lo, hi);
}
void EPolynomialRing::vec_degree_lohi(const evec *v, int nwts, const int *wts,
                                     int &lo, int &hi) const
{
  if (nwts < nvars || v == 0) 
    {
      lo = hi = 0;
      return;
    }
  int d = getMonoid()->degree(v->monom, wts);
  wts += nvars;
  nwts -= nvars;
  if (nwts > 0)
    {
      K->degreeWeightsLoHi(v->coeff, nwts, wts,lo,hi);
      lo += d;
      hi += d;
    }
  else
    lo = hi = d;
}

///////////////
// get terms //
///////////////
ERingElement EPolynomialRing::getTerms(const ERingElement f, int lo, int hi) const
{
  int len = n_terms(POLYVAL(f));
  if (lo < 0) lo = len + lo;
  if (hi < 0) hi = len + hi;
  int n = 0;
  collector w(this);
  for (iterator p = f; p.valid(); ++p)
    {
      if (n > hi) break;
      if (n >= lo) w.append(copy_term(*p));
      n++;
    }
  return w.value();
}
EVector EVector::getTerms(int lo, int hi) const
{
  if (lo < 0) lo = len + lo;
  if (hi < 0) hi = len + hi;
  int n = 0;
  EVector::collector w(F);
  for (EVector::iterator p = *this; p.valid(); ++p)
    {
      if (n > hi) break;
      if (n >= lo) w.append(getRing()->vec_copy_term(*p));
      n++;
    }
  return w.value();
}
////////////////////
// multiplication //
////////////////////
epoly *EPolynomialRing::mult_by_ZZ(const epoly *a, int n) const
{
  if (n == 0) return 0;
  collector result(this);
  for (iterator t = a; t.valid(); ++t)
    {
      epoly *tm = new_poly_term();
      tm->coeff = K->mult_by_ZZ(t->coeff, n);
      if (K->is_zero(tm->coeff))
	{
	  remove_poly_term(tm);
	  continue;
	}
      tm->monom = t->monom;
      result.append(tm);
    }
  return result.poly_value();
}

EVector ERing::vec_left_mult(const ERingElement a, const EVector &b) const
{
  if (is_zero(a)) return b.getFreeModule()->zero();
  EVector::collector result(b.getFreeModule());
  for (EVector::iterator t = b; t.valid(); ++t)
    {
      evec *tm = vec_new_term();
      tm->coeff = mult(a,t->coeff);
      if (is_zero(tm->coeff))
	{
	  vec_remove_term(tm);
	  continue;
	}
      tm->component = t->component;
      result.append(tm);
    }
  return result.value();
}
EVector ERing::vec_right_mult(const EVector &a, const ERingElement b) const
{
  if (is_zero(b)) return a.getFreeModule()->zero();
  EVector::collector result(a.getFreeModule());
  for (EVector::iterator t = a; t.valid(); ++t)
    {
      evec *tm = vec_new_term();
      tm->coeff = mult(t->coeff,b);
      if (is_zero(tm->coeff))
	{
	  vec_remove_term(tm);
	  continue;
	}
      tm->component = t->component;
      result.append(tm);
    }
  return result.value();
}
///////////////////////////////
// mult1 for EPolynomialRing //
///////////////////////////////
epoly * EPolynomialRing::mult1(const epoly *a, // single term
                               const epoly *b  // entire polynomial
                               ) const
{
  collector result(this);
  for (iterator t = b; t.valid(); ++t)
    {
      epoly *tm = new_poly_term();
      tm->coeff = K->mult(a->coeff,t->coeff);
      if (K->is_zero(tm->coeff))
	{
	  remove_poly_term(tm);
	  continue;
	}
      tm->monom = getMonoid()->mult(a->monom, t->monom);
      result.append(tm);
    }
  return result.poly_value();
}
EVector EPolynomialRing::mult1(
    const EFreeModule *resultF,
    const epoly *f,    // single term
    const EVector &g   // entire vector
    ) const
{
  evec *tm;
  EVector::collector result(resultF);
  for (EVector::iterator h = g; h.valid(); ++h)
    {
      tm = vec_new_term();
      tm->coeff = K->mult(f->coeff, h->coeff);
      if (K->is_zero(tm->coeff))
	{
	  vec_remove_term(tm);
	  continue;
	}
      tm->monom = getMonoid()->mult(f->monom, h->monom);
      tm->component = h->component;
      result.append(tm);
    }
  return result.value();
}
EVector EPolynomialRing::mult1(
    const EFreeModule *resultF,
    const evec *f,   // single term
    const epoly *g   // entire polynomial
    ) const
{
  evec *tm;
  EVector::collector result(resultF);
  for (iterator h = g; h.valid(); ++h)
    {
      tm = vec_new_term();
      tm->coeff = K->mult(f->coeff, h->coeff);
      if (K->is_zero(tm->coeff))
	{
	  vec_remove_term(tm);
	  continue;
	}
      tm->monom = getMonoid()->mult(f->monom, h->monom);
      tm->component = f->component;
      result.append(tm);
    }
  return result.value();
}
///////////////////////////////////
// mult1 for ESkewPolynomialRing //
///////////////////////////////////
epoly * ESkewCommPolynomialRing::mult1(const epoly *a, // single term
                               const epoly *b  // entire polynomial
                               ) const
{
  collector result(this);
  for (iterator t = b; t.valid(); ++t)
    {
      int sgn = skew_mult_sign(a->monom, t->monom);
      if (sgn == 0) continue;
      epoly *tm = new_poly_term();
      tm->coeff = K->mult(a->coeff,t->coeff);
      if (K->is_zero(tm->coeff))
	{
	  remove_poly_term(tm);
	  continue;
	}
      if (sgn < 0)
	K->negate_to(tm->coeff);
      tm->monom = getMonoid()->mult(a->monom, t->monom);
      result.append(tm);
    }
  return result.poly_value();
}
EVector ESkewCommPolynomialRing::mult1(
    const EFreeModule *resultF,
    const epoly *f,    // single term
    const EVector &g   // entire vector
    ) const
{
  evec *tm;
  EVector::collector result(resultF);
  for (EVector::iterator h = g; h.valid(); ++h)
    {
      int sgn = skew_mult_sign(f->monom, h->monom);
      if (sgn == 0) continue;
      tm = vec_new_term();
      tm->coeff = K->mult(f->coeff, h->coeff);
      if (K->is_zero(tm->coeff))
	{
	  vec_remove_term(tm);
	  continue;
	}
      if (sgn < 0)
	K->negate_to(tm->coeff);
      tm->monom = getMonoid()->mult(f->monom, h->monom);
      tm->component = h->component;
      result.append(tm);
    }
  return result.value();
}
EVector ESkewCommPolynomialRing::mult1(
    const EFreeModule *resultF,
    const evec *f,   // single term
    const epoly *g   // entire polynomial
    ) const
{
  evec *tm;
  EVector::collector result(resultF);
  for (iterator h = g; h.valid(); ++h)
    {
      int sgn = skew_mult_sign(f->monom, h->monom);
      if (sgn == 0) continue;
      tm = vec_new_term();
      tm->coeff = K->mult(f->coeff, h->coeff);
      if (K->is_zero(tm->coeff))
	{
	  vec_remove_term(tm);
	  continue;
	}
      if (sgn < 0)
	K->negate_to(tm->coeff);
      tm->monom = getMonoid()->mult(f->monom, h->monom);
      tm->component = f->component;
      result.append(tm);
    }
  return result.value();
}
////////////////////////////
// mult1 for EWeylAlgebra //
////////////////////////////

epoly * EWeylAlgebra::weyl_diff(
	  const ERingElement c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const epoly *g) const  // An entire polynomial
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the commutative
  // monoid.

  collector result(this);
  int i;
  int *exp = new int[nderivatives];
  int *deriv_exp = new int[nvars];
  int *result_exp = new int[nvars];
  for (i=0; i<nvars; i++)
    deriv_exp[i] = 0;
  if (homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i=0; i<nderivatives; i++)
	{
	  sum += 2*derivatives[i];
	  deriv_exp[derivative[i]] = derivatives[i];
	  deriv_exp[commutative[i]] = derivatives[i];
	}
      deriv_exp[homog_var] = -sum;
    }
  else
    for (i=0; i<nderivatives; i++)
      {
	deriv_exp[derivative[i]] = derivatives[i];
	deriv_exp[commutative[i]] = derivatives[i];
      }

  for (iterator t = g; t.valid(); ++t)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is set.
      M->copy_exponents(t->monom, result_exp);
      extractCommutativePart(result_exp, exp);
      if (divides(derivatives,exp))
	{
	  ERingElement a = diff_coefficients(c,derivatives,exp);
	  if (K->is_zero(a))
	    {
	      K->remove(a);
	      continue;
	    }
	  ERingElement b = K->mult(a, t->coeff);
	  K->remove(a);
	  if (K->is_zero(b))
	    {
	      K->remove(b);
	      continue;
	    }
	  // Now compute the new monomial:
	  epoly *tm = new_poly_term();
	  tm->coeff = b;
	  for (int i=0; i<nvars; i++)
	    result_exp[i] += expf[i] - deriv_exp[i];
	  tm->monom = M->monomial_from_exponents(result_exp);
	  result.append(tm);
	}
    }
  delete [] exp;
  delete [] result_exp;
  return result.poly_value();
}
EVector EWeylAlgebra::weyl_diff(
	  const ERingElement c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const EVector &g) const  // An entire polynomial
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the commutative
  // monoid.

  EVector::collector result(g.getFreeModule());
  int i;
  int *exp = new int[nderivatives];
  int *deriv_exp = new int[nvars];
  int *result_exp = new int[nvars];
  for (i=0; i<nvars; i++)
    deriv_exp[i] = 0;
  if (homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i=0; i<nderivatives; i++)
	{
	  sum += 2*derivatives[i];
	  deriv_exp[derivative[i]] = derivatives[i];
	  deriv_exp[commutative[i]] = derivatives[i];
	}
      deriv_exp[homog_var] = -sum;
    }
  else
    for (i=0; i<nderivatives; i++)
      {
	deriv_exp[derivative[i]] = derivatives[i];
	deriv_exp[commutative[i]] = derivatives[i];
      }

  for (EVector::iterator t = g; t.valid(); ++t)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is set.
      M->copy_exponents(t->monom, result_exp);
      extractCommutativePart(result_exp, exp);
      if (divides(derivatives,exp))
	{
	  ERingElement a = diff_coefficients(c,derivatives,exp);
	  if (K->is_zero(a))
	    {
	      K->remove(a);
	      continue;
	    }
	  ERingElement b = K->mult(a, t->coeff);
	  K->remove(a);
	  if (K->is_zero(b))
	    {
	      K->remove(b);
	      continue;
	    }
	  // Now compute the new monomial:
	  evec *tm = vec_new_term();
	  tm->coeff = b;
	  tm->component = t->component;
	  for (int i=0; i<nvars; i++)
	    result_exp[i] += expf[i] - deriv_exp[i];
	  tm->monom = M->monomial_from_exponents(result_exp);
	  result.append(tm);
	}
    }
  delete [] exp;
  delete [] result_exp;
  return result.value();
}
EVector EWeylAlgebra::weyl_diff(
	  const EFreeModule *resultF,
	  const ERingElement c,
	  const int *expf,  // The exponent vector of f
	  int component,
	  const int *derivatives, 
	  const epoly * g) const  // An entire polynomial
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the commutative
  // monoid.

  EVector::collector result(resultF);
  int i;
  int *exp = new int[nderivatives];
  int *deriv_exp = new int[nvars];
  int *result_exp = new int[nvars];
  for (i=0; i<nvars; i++)
    deriv_exp[i] = 0;
  if (homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i=0; i<nderivatives; i++)
	{
	  sum += 2*derivatives[i];
	  deriv_exp[derivative[i]] = derivatives[i];
	  deriv_exp[commutative[i]] = derivatives[i];
	}
      deriv_exp[homog_var] = -sum;
    }
  else
    for (i=0; i<nderivatives; i++)
      {
	deriv_exp[derivative[i]] = derivatives[i];
	deriv_exp[commutative[i]] = derivatives[i];
      }

  for (iterator t = g; t.valid(); ++t)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is set.
      M->copy_exponents(t->monom, result_exp);
      extractCommutativePart(result_exp, exp);
      if (divides(derivatives,exp))
	{
	  ERingElement a = diff_coefficients(c,derivatives,exp);
	  if (K->is_zero(a))
	    {
	      K->remove(a);
	      continue;
	    }
	  ERingElement b = K->mult(a, t->coeff);
	  K->remove(a);
	  if (K->is_zero(b))
	    {
	      K->remove(b);
	      continue;
	    }
	  // Now compute the new monomial:
	  evec *tm = vec_new_term();
	  tm->coeff = b;
	  tm->component = component;
	  for (int i=0; i<nvars; i++)
	    result_exp[i] += expf[i] - deriv_exp[i];
	  tm->monom = M->monomial_from_exponents(result_exp);
	  result.append(tm);
	}
    }
  delete [] exp;
  delete [] result_exp;
  return result.value();
}

epoly * EWeylAlgebra::mult1(
    const epoly *f, // single term
    const epoly *g  // entire polynomial
    ) const
{
  int *top_derivative = new int[nderivatives];
  int *current_derivative = new int[nderivatives];

  heap result(this);

  const int *expf = M->to_exponents(f->monom);
  extractDerivativePart(expf, top_derivative);
  for (int i=0; i<nderivatives; i++) current_derivative[i] = 0;
  // Loop over each current_derivative <= top_derivative.
  do {
      ERingElement c = multinomial(f->coeff, top_derivative, current_derivative);
      epoly * h = weyl_diff(c,expf,current_derivative,g);
      K->remove(c);
      result.add(h);
  } while (increment(current_derivative, top_derivative));

  delete [] top_derivative;
  delete [] current_derivative;
  return result.poly_value();
}
EVector EWeylAlgebra::mult1(
    const EFreeModule *resultF,
    const epoly *f,    // single term
    const EVector &g   // entire vector
    ) const
{
  int *top_derivative = new int[nderivatives];
  int *current_derivative = new int[nderivatives];

  EVectorHeap result(resultF);

  const int *expf = M->to_exponents(f->monom);
  extractDerivativePart(expf, top_derivative);
  for (int i=0; i<nderivatives; i++) current_derivative[i] = 0;
  // Loop over each current_derivative <= top_derivative.
  do {
      ERingElement c = multinomial(f->coeff, top_derivative, current_derivative);
      EVector h = weyl_diff(c,expf,current_derivative,g);
      K->remove(c);
      result.add(h);
  } while (increment(current_derivative, top_derivative));

  delete [] top_derivative;
  delete [] current_derivative;
  return result.value();
}
EVector EWeylAlgebra::mult1(
    const EFreeModule *resultF,
    const evec *f,   // single term
    const epoly *g   // entire polynomial
    ) const
{
  int *top_derivative = new int[nderivatives];
  int *current_derivative = new int[nderivatives];

  EVectorHeap result(resultF);

  const int *expf = M->to_exponents(f->monom);
  extractDerivativePart(expf, top_derivative);
  for (int i=0; i<nderivatives; i++) current_derivative[i] = 0;
  // Loop over each current_derivative <= top_derivative.
  do {
      ERingElement c = multinomial(f->coeff, top_derivative, current_derivative);
      EVector h = weyl_diff(resultF,c,expf,f->component,current_derivative,g);
      K->remove(c);
      result.add(h);
  } while (increment(current_derivative, top_derivative));

  delete [] top_derivative;
  delete [] current_derivative;
  return result.value();
}


////////////////////////////////////////////////
// Multiply routines for all polynomial rings //
////////////////////////////////////////////////
epoly * EPolynomialRing::mult(const epoly *a, const epoly *b) const
{
  if (a == 0 || b == 0) return 0;
  if (a->next == 0) return mult1(a,b);
  heap result(this);
  for (iterator t = a; t.valid(); ++t)
    {
      epoly *f = mult1(*t,b);
      result.add(f);
    }
  return result.poly_value();
}
EVector EPolynomialRing::vec_left_mult(const ERingElement a, const EVector &b) const
{
  epoly *a1 = POLYVAL(a);
  const EFreeModule *F = b.getFreeModule();
  if (a1 == 0) return F->zero();
  if (a1->next == 0) return mult1(F,a1,b);
  EVectorHeap result(F);
  for (iterator t = a1; t.valid(); ++t)
    {
      EVector v = mult1(F,*t,b);
      result.add(v);
    }
  return result.value();
}
EVector EPolynomialRing::vec_right_mult(const EVector &a, const ERingElement b) const
{
  const EFreeModule *F = a.getFreeModule();
  if (a.len == 0) return F->zero();
  if (a.len == 1) return mult1(F,a.elems,POLYVAL(b));
  EVectorHeap result(F);
  for (EVector::iterator t = a; t.valid(); ++t)
    {
      EVector v = mult1(F,*t,POLYVAL(b));
      result.add(v);
    }
  return result.value();
}

