#include "gbring.hpp"
#include "text_io.hpp"
#include "ZZ.hpp"
#include "QQ.hpp"
#include "freemod.hpp"
#include "geovec.hpp"
#include "frac.hpp"
#include "polyring.hpp"
#include "weylalg.hpp"
#include "skewpoly.hpp"
#include "solvable.hpp"

#define sizeofgbvector(s,len) (sizeof(*s) - sizeof(s->monom) + (len)*sizeof(s->monom[0]))


gbvector * GBRing::new_raw_term()
{
  return (gbvector *) GC_MALLOC(gbvector_size);
}

/*************************
 * Exponent handling *****
 *************************/

exponents GBRing::exponents_make()
{
  int *e = newarray(int,_nvars+2); // length is nvars
  return e;
}

void GBRing::exponents_delete(exponents e)
{
  deletearray(e);
}

////////////////////////////////////////////////////////////////
GBRing::~GBRing()
{
}
GBRingPoly::~GBRingPoly()
{
}
GBRingWeyl::~GBRingWeyl()
{
}
GBRingWeylZZ::~GBRingWeylZZ()
{
}
GBRingSkew::~GBRingSkew()
{
}
GBRingSolvable::~GBRingSolvable()
{
}

GBRing::GBRing(const Ring *K0, const Monoid *M0)
  : _schreyer_encoded(false),
    M(M0),
    K(K0),
    _coeffs_ZZ(false),  // set below
    _nvars(M->n_vars()),
    _degrees(0), // set below
    _is_skew(false),
    _skew(),
    is_weyl(false),
    weyl(0),
    is_solvable(false),
    solvable(0),
    _one(K->from_int(1))
{
  _EXP1 = newarray(int,_nvars+2);
  _EXP2 = newarray(int,_nvars+2);
  _EXP3 = newarray(int,_nvars+2);
  _EXP4 = newarray(int,_nvars+2);
  _SKEW1 = newarray(int,_nvars);
  _SKEW2 = newarray(int,_nvars);
  _MONOM1 = M->make_one();
  _MONOM2 = M->make_one();

  gbvector *used_to_determine_size = 0;
  gbvector_size = sizeofgbvector(used_to_determine_size,M->monomial_size());

  if (K == globalZZ)
    _coeffs_ZZ = true;

  // How to get the degree vector?
  _degrees = newarray(int,_nvars);
  for (int i=0; i<_nvars; i++)
    _degrees[i] = M->primary_degree_of_var(i);
}

/////////////////////////////////////////////////////////////////////////
// Multiplication routines for Poly, SkewPoly, Weyl, Solvable algebras //
/////////////////////////////////////////////////////////////////////////

//////////////////////
// Polynomial rings //
//////////////////////

GBRing * GBRing::create_PolynomialRing(const Ring *K, const Monoid *M)
{
  return new GBRingPoly(K,M);
}

gbvector *GBRingPoly::mult_by_term(const FreeModule *F,
				   const gbvector *f,
				   ring_elem u,
				   const int *monom,
				   int comp)
{
  // Remember: this multiplies w/o regard to any quotient elements
  gbvector head;
  gbvector *inresult = &head;

  for (const gbvector *s = f; s != NULL; s = s->next)
    {
      gbvector *t = new_raw_term();
      t->next = 0;
      t->comp = s->comp + comp;
      t->coeff = K->mult(u, s->coeff);
      M->mult(monom, s->monom, t->monom);
      inresult->next = t;
      inresult = inresult->next;
    }
  inresult->next = 0;
  return head.next;
}

///////////////////////////////////////
// Skew commutative polynomial rings //
///////////////////////////////////////

GBRingSkew::GBRingSkew(const Ring *K0, 
		       const Monoid *M0, 
		       SkewMultiplication skew0)
  : GBRing(K0,M0)
{
  _is_skew = true;
  _skew = skew0;
}

GBRing * GBRing::create_SkewPolynomialRing(const Ring *K0, 
					   const Monoid *M0, 
					   SkewMultiplication skew0)
{
  return new GBRingSkew(K0,M0,skew0);
}

gbvector *GBRingSkew::mult_by_term(const FreeModule *F,
				   const gbvector *f, 
				   const ring_elem c, 
				   const int *m,
				   int comp)
  // Computes c*m*f, BUT NOT doing normal form wrt a quotient ideal..
{
  // BUG BUG BUG BUG!!!! Doesn't handle encoded Schreyer orders....
  // WE NEED TO FIX THIS.
  gbvector head;
  gbvector *inresult = &head;

  M->to_expvector(m, _EXP1);

  for (const gbvector *s = f; s != NULL; s = s->next)
    {
      M->to_expvector(s->monom, _EXP2);
      int sign = _skew.mult_sign(_EXP1, _EXP2);
      if (sign == 0) continue;

      gbvector *t = new_raw_term();
      t->next = 0;
      t->comp = s->comp + comp;
      t->coeff = K->mult(c, s->coeff);
      if (sign < 0)
	K->negate_to(t->coeff);

      M->mult(m, s->monom, t->monom);
      inresult->next = t;
      inresult = inresult->next;
    }
  inresult->next = 0;
  return head.next;
}

///////////////////
// Weyl algebra ///
///////////////////
GBRingWeyl::GBRingWeyl(const Ring *K0, const Monoid *M0, const WeylAlgebra *R0)
  : GBRing(K0,M0)
{
  is_weyl = true;
  weyl = R0;
}

GBRingWeylZZ::GBRingWeylZZ(const Ring *K0, const Monoid *M0, const WeylAlgebra *R0)
  : GBRingWeyl(K0,M0,R0)
{
  is_weyl = true;
  weyl = R0;
}

GBRing * GBRing::create_WeylAlgebra(const Ring *K0, const Monoid *M0, const WeylAlgebra *R0)
{
  if (K0 == globalZZ)
    return new GBRingWeylZZ(K0,M0,R0);
  return new GBRingWeyl(K0,M0,R0); 
}

gbvector *GBRingWeyl::mult_by_term(const FreeModule *F,
				   const gbvector *f,
				   ring_elem u,
				   const int *monom,
				   int comp)
{
  // Remember: this multiplies w/o regard to any quotient elements
  gbvectorHeap result(this,F);
  return weyl->gbvector_mult_by_term(result,f,u,monom,comp);
}

gbvector *GBRingWeylZZ::mult_by_term(const FreeModule *F,
				   const gbvector *f,
				   ring_elem u,
				   const int *monom,
				   int comp)
{
  // Remember: this multiplies w/o regard to any quotient elements
  gbvectorHeap result(this,F);
  return weyl->gbvector_mult_by_term(result,f,u,monom,comp);
}

///////////////////////
// Solvable algebras //
///////////////////////
GBRingSolvable::GBRingSolvable(const Ring *K0, 
			       const Monoid *M0, 
			       const SolvableAlgebra *R0)
  : GBRing(K0,M0)
{
  is_solvable = true;
  solvable = R0;
}

GBRing * GBRing::create_SolvableAlgebra(const Ring *K0, 
					const Monoid *M0, 
					const SolvableAlgebra *R0)
{
  return new GBRingSolvable(K0,M0,R0);
}

gbvector *GBRingSolvable::mult_by_term(const FreeModule *F,
				       const gbvector *f,
				       ring_elem u,
				       const int *monom,
				       int comp)
{
  // Remember: this multiplies w/o regard to any quotient elements
#warning "implement GBRingSolvable::mult_by_term"
  return 0;
}

//////////////////////
// gbvector support //
//////////////////////

void GBRing::gbvector_remove_term(gbvector *f)
{
  // It is not clear whether we should try to free elements of K
  f->next = 0;
  f->coeff = (Nterm*)0;
  GC_FREE((char *)f);
}

void GBRing::gbvector_remove(gbvector *f)
{
  // It is not clear whether we should try to free elements of K
  while (f != 0)
    {
      gbvector *g = f;
      f = f->next;
      gbvector_remove_term(g);
    }
}

gbvector * GBRing::gbvector_term(const FreeModule *F, ring_elem coeff, int comp)
  // Returns coeff*e_sub_i in F, the monomial is set to 1.
{
  gbvector *v = (gbvector *) GC_MALLOC(gbvector_size);
  v->coeff = coeff;
  v->comp = comp;
  v->next = 0;
  if (_schreyer_encoded)
    M->copy(F->get_schreyer_order()->base_monom(comp-1), v->monom);
  else
    M->one(v->monom);
  return v;
}

gbvector * GBRing::gbvector_copy_term(const gbvector *t)
{
  gbvector *v = (gbvector *) GC_MALLOC(gbvector_size);
  v->next = 0;
  v->coeff = t->coeff;
  v->comp = t->comp;
  M->copy(t->monom, v->monom);
  return v;
}

bool GBRing::gbvector_is_equal(const gbvector *a, 
			       const gbvector *b) const
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
      if (!K->is_equal(a->coeff, b->coeff)) return false;
      if (M->compare(a->monom, b->monom) != 0)
	return false;
    }
}

int GBRing::gbvector_n_terms(const gbvector *v) const
{
  int result = 0;
  for ( ; v != NULL; v = v->next)
    result++;
  return result;
}

int GBRing::exponents_weight(const int *e) const
  // e is an exponent vector 0.._nvars-1
{
  int sum = 0;
  for (int i=0; i<_nvars; i++)
    sum += e[i] * _degrees[i];
  return sum;
}

void GBRing::gbvector_weight(const FreeModule *F, const gbvector *f,
		     int &result_lead,
		     int &result_lo,
		     int &result_hi)
{
#warning "gbvector_weight not implemented"
}

int GBRing::gbvector_term_weight(const FreeModule *F, 
			   const gbvector *f)
{
  if (f == 0) return 0;
  gbvector_get_lead_exponents(F,f,_EXP1);
  int fdeg = F->primary_degree(f->comp-1);
  fdeg += exponents_weight(_EXP1);
  return fdeg;
}

int GBRing::gbvector_degree(const FreeModule *F, 
			    const gbvector *f)
{
  /* Return the maximum degree of any term of a */
  bool first_term = true;
  int deg = 0;
  if (f == 0) return 0;
  for (const gbvector *t=f; t != 0; t = t->next)
    {
      gbvector_get_lead_exponents(F,t,_EXP1);
      int tdeg = F->primary_degree(t->comp-1);
      tdeg += exponents_weight(_EXP1);
      if (first_term)
	{
	  deg = tdeg;
	  first_term = false;
	}
      else
	if (tdeg > deg) deg = tdeg;
    }
  return deg;
}

int GBRing::gbvector_compare(const FreeModule *F,
		     const gbvector *f,
		     const gbvector *g) const
  // Return LT, EQ, GT depending on the monomial order of lead(f), lead(g) in F.
{
  if (!_schreyer_encoded && F->get_schreyer_order())
    {
#if 0
      return M->compare_schreyer(f->monom,
				 F->get_schreyer_order()->base_monom(f->comp-1),
				 F->get_schreyer_order()->compare_num(f->comp-1),
				 g->monom,
				 F->get_schreyer_order()->base_monom(g->comp-1),
				 F->get_schreyer_order()->compare_num(g->comp-1))
#endif
    }

  int cmp;
  if (_up_order)
    cmp = M->compare(f->monom, -f->comp, g->monom, -g->comp);
  else
    cmp = M->compare(f->monom, f->comp, g->monom, g->comp);
  return cmp;
}

gbvector * GBRing::gbvector_lead_term(int n, 
				      const FreeModule *F, 
				      const gbvector *v)
  // What should we do with Schreyer orders?  This probably doesn't
  // make much sense, except to get lead monomials.
{
  if (v == NULL) return NULL;
  gbvector head;
  gbvector *result = &head;
  for (const gbvector *t = v; t != NULL; t = t->next)
      {
	if (M->compare(n, t->monom, v->monom) != 0) break;
	result->next = gbvector_copy_term(t);
	result = result->next;
      }
  result->next = NULL;
  return head.next;
}

void GBRing::gbvector_get_lead_monomial(const FreeModule *F,
					const gbvector *f, 
					int *result)
  // This copies the monomial to result.  If a Schreyer order,
  // the result will NOT be the total monomial.
{

  if (_schreyer_encoded && F->get_schreyer_order() != 0)
    M->divide(f->monom, F->get_schreyer_order()->base_monom(f->comp-1), result);
  else 
    M->copy(f->monom, result);
}

void GBRing::gbvector_get_lead_exponents(const FreeModule *F,
					 const gbvector *f, 
					 int *result)
{
  // result[0]..result[nvars-1] is set
  if (_schreyer_encoded && F->get_schreyer_order() != 0)
    {
      M->divide(f->monom, F->get_schreyer_order()->base_monom(f->comp-1), _MONOM1);
      M->to_expvector(_MONOM1, result);
    }
  else 
    M->to_expvector(f->monom, result);
}

void GBRing::gbvector_mult_by_coeff_to(gbvector *f, ring_elem u)
{
  for ( ; f != 0; f=f->next)
    K->mult_to(f->coeff,u);
}

gbvector * GBRing::gbvector_mult_by_coeff(const gbvector *v, ring_elem u)
{
  gbvector head;
  gbvector *result = &head;
  for (const gbvector *t = v; t != NULL; t = t->next)
      {
	result->next = gbvector_copy_term(t);
	result = result->next;
	K->mult_to(result->coeff, u);
      }
  result->next = NULL;
  return head.next;
}

gbvector * GBRing::gbvector_copy(const gbvector *f)
{
  gbvector head;
  gbvector *b = &head;
  for ( ; f != 0; f = f->next, b = b->next)
    b->next = gbvector_copy_term(f);
  b->next = 0;
  return head.next;
}

void GBRing::gbvector_add_to(const FreeModule *F,
			     gbvector * &f, gbvector * &g)
  // If F has an un-encoded Schreyer order, we should call
  // another function here, with compare defined...
{
  if (g == NULL) return;
  if (f == NULL) { f = g; g = NULL; return; }
  gbvector head;
  gbvector *result = &head;
  while (1)
    switch (gbvector_compare(F, f, g))
      {
      case LT:
	result->next = g;
	result = result->next;
	g = g->next;
	if (g == NULL) 
	  {
	    result->next = f; 
	    f = head.next;
	    return;
	  }
	break;
      case GT:
	result->next = f;
	result = result->next;
	f = f->next;
	if (f == NULL) 
	  {
	    result->next = g; 
	    f = head.next;
	    g = NULL;
	    return;
	  }
	break;
      case EQ:
	gbvector *tmf = f;
	gbvector *tmg = g;
	f = f->next;
	g = g->next;
	K->add_to(tmf->coeff, tmg->coeff);
#if 0
	if (R->is_ZZ_quotient)
	  {
	    ring_elem t = K->remainder(tmf->coeff, R->ZZ_quotient_value);
	    K->remove(tmf->coeff);
	    tmf->coeff = t;
	  }
#endif
	if (K->is_zero(tmf->coeff))
	  {
	    gbvector_remove_term(tmf);
	  }
	else
	  {
	    result->next = tmf;
	    result = result->next;
	  }
	gbvector_remove_term(tmg);
	if (g == NULL) 
	  {
	    result->next = f; 
	    f = head.next;
	    return;
	  }
	if (f == NULL) 
	  {
	    result->next = g; 
	    f = head.next;
	    g = NULL;
	    return;
	  }
	break;
      }
}

void GBRing::gbvector_text_out(buffer &o,
			       const FreeModule *F,
			       const gbvector *v) const
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
  for (const gbvector *t = v; t != NULL; t = t->next)
    {
      int isone = (M == NULL || M->is_one(t->monom));
      K->elem_text_out(o,t->coeff);
      if (!isone)
	M->elem_text_out(o, t->monom);
      o << "<" << t->comp << ">";
      p_plus = 1;
    }

  p_one = old_one;
  p_parens = old_parens;
  p_plus = old_plus;
}

void GBRing::divide_exponents(const int *exp1,
			      const int *exp2,
			      int *result) const
{
  for (int i=0; i<_nvars; i++)
    *result++ = *exp1++ - *exp2++;
}

void GBRing::exponent_syzygy(const int *exp1,
			     const int *exp2,
			     int *exp3,
			     int *exp4)
{
  for (int i=0; i<_nvars; i++)
    {
      int a = exp1[i] - exp2[i];
      if (a > 0)
	{
	  exp3[i] = 0;
	  exp4[i] = a;
	}
      else
	{
	  exp3[i] = -a;
	  exp4[i] = 0;
	}
    }
}

/////////////////////////////////////////////////////
// mult_by_term routines ////////////////////////////
/////////////////////////////////////////////////////

void GBRing::gbvector_mult_by_term(const FreeModule *F,
				   const FreeModule *Fsyz,
				   ring_elem a, 
				   const int *m, // element of M, a monomial
				   const gbvector *f,
				   const gbvector *fsyz,
				   gbvector * &result,
				   gbvector * &result_syz)
{
  // TODO
  // The reason this has both result,result_syz together is in case we want to
  //   reduce result_fsyz mod a quotient ideal.  Do we really need to do this as
  //   we go?  This will require experimentation.
  result = mult_by_term(F,f,a,m,0);
  result_syz = mult_by_term(Fsyz,fsyz,a,m,0);
}

void GBRing::gbvector_reduce_lead_term(const FreeModule *F,
				       const FreeModule *Fsyz,
				       gbvector * flead,
				       gbvector * &f,
				       gbvector * &fsyz,
				       const gbvector *g,
				       const gbvector *gsyz)
{
  int comp;
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  ring_elem u,v;
  K->syzygy(a,b,u,v); // If possible, u==1, anyway, u>0
  gbvector_get_lead_exponents(F,f,_EXP1); // Removes the Schreyer part
  gbvector_get_lead_exponents(F,g,_EXP2); // Removes the Schreyer part
  divide_exponents(_EXP1,_EXP2,_EXP3);
  if (g->comp == 0)
    comp = f->comp;
  else
    comp = 0;
  if (!K->is_equal(u,_one))
    {
      gbvector_mult_by_coeff_to(f,u); // modifies f
      gbvector_mult_by_coeff_to(flead,u);
      gbvector_mult_by_coeff_to(fsyz,u);
    }
  // mult f,flead by u (if u != 1)
  // now mult g to cancel
  if (is_skew_commutative())
    {
      if (_skew.mult_sign(_EXP3, _EXP2) < 0)
	K->negate_to(v);
    }
  M->from_expvector(_EXP3, _MONOM1);
  gbvector *result1 = mult_by_term(F,g, v,_MONOM1,comp);
  gbvector_add_to(F,f,result1);
  if (fsyz != 0 || gsyz != 0) 
    {
      gbvector *result_syz1 = mult_by_term(Fsyz,gsyz, v,_MONOM1, comp);
      gbvector_add_to(Fsyz,fsyz,result_syz1);
    }
}

void GBRing::gbvector_reduce_lead_term_coeff(const FreeModule *F,
				       const FreeModule *Fsyz,
				       gbvector * flead,
				       gbvector * &f,
				       gbvector * &fsyz,
				       const gbvector *g,
				       const gbvector *gsyz,
				       ring_elem &denom)
  // This routine varies from  gbvector_reduce_lead_term in just ONE LINE:
  // where denom gets multiplied by the multiplier to f (if not 1).
{
  int comp;
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  ring_elem u,v;
  K->syzygy(a,b,u,v); // If possible, u==1, anyway, u>0
  gbvector_get_lead_exponents(F,f,_EXP1); // Removes the Schreyer part
  gbvector_get_lead_exponents(F,g,_EXP2); // Removes the Schreyer part
  divide_exponents(_EXP1,_EXP2,_EXP3);
  if (g->comp == 0)
    comp = f->comp;
  else
    comp = 0;
  if (!K->is_equal(u,_one))
    {
      gbvector_mult_by_coeff_to(f,u); // modifies f
      gbvector_mult_by_coeff_to(flead,u);
      gbvector_mult_by_coeff_to(fsyz,u);
      K->mult_to(denom, u);
    }
  // mult f,flead by u (if u != 1)
  // now mult g to cancel
  if (is_skew_commutative())
    {
      if (_skew.mult_sign(_EXP3, _EXP2) < 0)
	K->negate_to(v);
    }
  M->from_expvector(_EXP3, _MONOM1);
  gbvector *result1 = mult_by_term(F,g, v,_MONOM1,comp);
  gbvector_add_to(F,f,result1);
  if (fsyz != 0 || gsyz != 0) 
    {
      gbvector *result_syz1 = mult_by_term(Fsyz,gsyz, v,_MONOM1, comp);
      gbvector_add_to(Fsyz,fsyz,result_syz1);
    }
}

void GBRing::gbvector_cancel_lead_terms(
		    const FreeModule *F,
		    const FreeModule *Fsyz,
		    const gbvector *f,
		    const gbvector *fsyz,
		    const gbvector *g,
		    const gbvector *gsyz,
		    gbvector *&result,
		    gbvector *&result_syz)
  // If u*x^A*leadmonom(f) = v*x^B*leadmonom(g) (mod lower terms),
  // set result := u*x^A*f - v*x^B*g
  //     resultsyz := u*x^A*fsyz - v*x^B*gyz
  // To keep in mind:
  //  (a) Schreyer orders
  //  (b) Quotient ideal
  // Currently: this does nothing with the quotient ring
{
  int comp;
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  ring_elem u,v;
  K->syzygy(a,b,u,v); // If possible, u==1, anyway, u>0
  gbvector_get_lead_exponents(F,f,_EXP1); // Removes the Schreyer part
  gbvector_get_lead_exponents(F,g,_EXP2); // Removes the Schreyer part
  exponent_syzygy(_EXP1,_EXP2,_EXP3,_EXP4);
  if (g->comp == 0)
    comp = f->comp;
  else
    comp = 0;
  if (is_skew_commutative())
    {
      // Note that _EXP3 * _EXP1 = _EXP4 * _EXP2 up to sign
      if (_skew.mult_sign(_EXP3, _EXP1) !=
	  _skew.mult_sign(_EXP4, _EXP2))
	K->negate_to(v);
    }
  M->from_expvector(_EXP3, _MONOM1);
  M->from_expvector(_EXP4, _MONOM2);
  result = mult_by_term(F,f, u,_MONOM1,0);
  gbvector *result1 = mult_by_term(F,g, v,_MONOM2,comp);
  gbvector_add_to(F,result,result1);
  if (fsyz == 0 && gsyz == 0) 
    result_syz = 0;
  else
    {
      result_syz = mult_by_term(Fsyz,fsyz, u,_MONOM1,0);
      gbvector *result_syz1 = mult_by_term(Fsyz,gsyz, v,_MONOM2, comp);
      gbvector_add_to(Fsyz,result_syz,result_syz1);
    }
}

void GBRing::gbvector_apply(const FreeModule *F,
			    const FreeModule *Fsyz,
			    gbvector * & f, gbvector * & fsyz,
			    const gbvector * gsyz,
			    const gbvector **elems,
			    const gbvector **elems_syz,
			    const gbvector **quotients)
{
  // modifies (f,fsyz)
  // gsyz is allowed to have negative elements.  These refer to 
  // quotient ring elements.  In this case, the component that
  // is used is the lead component of f. (i.e. this is designed for
  // cancelling lead terms).
  // [combines: freemod::apply_quotient_ring_elements, 
  // GBZZ_comp::apply_gb_elements]
  
  for ( ; gsyz != 0; gsyz = gsyz->next)
    {
      if (gsyz->comp < 0)
	{
	  gbvector *v = mult_by_term(F, quotients[-1-gsyz->comp],
				     gsyz->coeff,
				     gsyz->monom,
				     f->comp);
	  gbvector_add_to(F, f, v);
	}
      else 
	{
	  gbvector *v = mult_by_term(F, elems[gsyz->comp],
				     gsyz->coeff,
				     gsyz->monom,
				     0);
	  gbvector_add_to(F, f, v);
	  gbvector *vsyz = mult_by_term(Fsyz, elems_syz[gsyz->comp],
				     gsyz->coeff,
				     gsyz->monom,
				     0);
	  gbvector_add_to(Fsyz, fsyz, vsyz);
	  
	}
    }
}

/////////////////////
// Content removal //
/////////////////////
void GBRing::divide_coeff_exact_to_ZZ(gbvector * f, M2_Integer u) const
{
  for ( ; f != 0; f=f->next)
    mpz_divexact(MPZ_VAL(f->coeff), MPZ_VAL(f->coeff), u);
}

void GBRing::lower_content_ZZ(gbvector *f, M2_Integer content) const
  // content should be a positive number.  Modify this value
  // so that new value of content = gcd(old-content, content(f)).
{
  if (f == 0) return;
  for ( ; f != 0; f=f->next)
    {
      mpz_gcd(content,content,MPZ_VAL(f->coeff));
      if (mpz_cmp_si(content,1) == 0)
	return;
    }
}  

void GBRing::gbvector_remove_content_ZZ(gbvector *f, 
					gbvector *fsyz,
					mpz_t denom) const
  // let c = gcd(content(f),content(fsyz)).
  // set f := f/c,  fsyz := fsyz/c.
  // denom *= c
{
  // This routine assumes that the coeff ring is ZZ (originally QQ).

  gbvector *g = f;
  gbvector *gsyz = fsyz;
  mpz_t content;
  int leadsign;
  if (g != 0)
    {
      leadsign = mpz_sgn(MPZ_VAL(g->coeff));
      mpz_init_set(content, MPZ_VAL(g->coeff));
      g = g->next;
    }
  else if (gsyz != 0)
    {
      leadsign = mpz_sgn(MPZ_VAL(gsyz->coeff));
      mpz_init_set(content, MPZ_VAL(gsyz->coeff));
      gsyz = gsyz->next;
    }
  else
    return;

  lower_content_ZZ(g, content);
  lower_content_ZZ(gsyz, content);
  
  mpz_abs(content,content);
  if (mpz_cmp_si(content,1) == 0) 
    {
      mpz_clear(content);

      if (leadsign < 0)
	{
	  for (gbvector *h = f ; h != 0; h=h->next)
	    mpz_neg(MPZ_VAL(h->coeff),MPZ_VAL(h->coeff));
	  for (gbvector *hsyz = fsyz; hsyz != 0; hsyz=hsyz->next)
	    mpz_neg(MPZ_VAL(hsyz->coeff),MPZ_VAL(hsyz->coeff));
	}
      return;
    }

  if (leadsign < 0)
    mpz_neg(content,content);
  divide_coeff_exact_to_ZZ(f,content);
  divide_coeff_exact_to_ZZ(fsyz,content);
  mpz_mul(denom, content, denom);
  mpz_clear(content);
}

void GBRing::gbvector_remove_content(gbvector *f, 
				     gbvector *fsyz,
				     ring_elem &denom)
  // let c = gcd(content(f),content(fsyz)).
  // set f := f/c,  fsyz := fsyz/c, denom *= c.
  // If K is not ZZ, then c is set to make f monic.
  // 
{
  if (_coeffs_ZZ) 
    {
      mpz_ptr d = MPZ_VAL(denom);
      gbvector_remove_content_ZZ(f,fsyz,d);
      // denom contains the changes to d...
      return;
    }
  // At this point, our coefficient ring is a field (What about
  // finite extensions of ZZ?)
  ring_elem c,cinv;
  if (f != 0) c = f->coeff;
  else if (fsyz != 0) c = fsyz->coeff;
  else return;
  cinv = K->invert(c);
  gbvector_mult_by_coeff_to(f,cinv);
  gbvector_mult_by_coeff_to(fsyz,cinv);
  K->mult_to(denom,c);
}

void GBRing::gbvector_remove_content(gbvector *f, 
				     gbvector *fsyz)
  // let c = gcd(content(f),content(fsyz)).
  // set f := f/c,  fsyz := fsyz/c.
{
  ring_elem denom = K->from_int(0);
  gbvector_remove_content(f,fsyz,denom);
}

////////////////////
// Auto-reduction //
////////////////////
const gbvector *GBRing::find_coeff(const FreeModule *F,
				   const gbvector *f, const gbvector *g) const
  /* If the monomial (g->monom,g->comp) appears exactly in f, 
     then return a pointer to that term, if not, return 0. */
{
  while (f != 0)
    {
      int cmp = gbvector_compare(F,f,g);
      if (cmp == LT) 
	break;
      else if (cmp == EQ)
	return f;
      else
	f = f->next;
    }
  return 0;
}

void GBRing::gbvector_auto_reduce(const FreeModule *F,
				  const FreeModule *Fsyz,
				  gbvector * &f, 
				  gbvector * &fsyz,
				  const gbvector *g, 
				  const gbvector *gsyz)
  // If g = a*x^A*ei + lower terms
  // and if f = ... + b*x^A*ei + ...
  // and if u*a + v*b = 0
  // then set f := u*f - v*g, fsyz := u*fsyz - v*gsyz
{
  const gbvector *t;
  if ((t = find_coeff(F,f,g)) != 0)
    {
      ring_elem u,v;
      K->syzygy(t->coeff, g->coeff, u, v);
      if (!K->is_equal(u,_one))
	{
	  gbvector_mult_by_coeff_to(f,u);
	  gbvector_mult_by_coeff_to(fsyz,u);
	}
      gbvector *g1 = gbvector_mult_by_coeff(g,v);
      gbvector *gsyz1 = gbvector_mult_by_coeff(gsyz,v);
      gbvector_add_to(F,f,g1);
      gbvector_add_to(Fsyz,fsyz,gsyz1);
      gbvector_remove_content(f,fsyz);
    }
}

//////////////////////////////
// gbvector heap operations //
//////////////////////////////

gbvectorHeap::gbvectorHeap(GBRing *GR0, const FreeModule *FF)
: GR(GR0),
  F(FF),
  K(GR0->get_flattened_coefficients()),
  top_of_heap(-1),
  mLead(-1)
{
  // set K
  int i;
  for (i=0; i<GEOHEAP_SIZE; i++)
    heap[i] = NULL;
}

gbvectorHeap::~gbvectorHeap()
{
  // The user of this class must insure that all 'vecterm's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

void gbvectorHeap::add(gbvector * p)
{
  mLead = -1;
  int len = GR->gbvector_n_terms(p);
  int i= 0;
  while (len >= heap_size[i]) i++;
  GR->gbvector_add_to(F,heap[i], p);
  len = GR->gbvector_n_terms(heap[i]);
  p = NULL;
  while (len >= heap_size[i])
    {
      i++;
      GR->gbvector_add_to(F,heap[i], heap[i-1]);
      len = GR->gbvector_n_terms(heap[i]);
      heap[i-1] = NULL;
    }
  if (i > top_of_heap)
    top_of_heap = i;
}

const gbvector * gbvectorHeap::get_lead_term()
{
  int lead_so_far = -1;
  for (int i=0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      if (lead_so_far < 0) 
	{
	  lead_so_far = i;
	  continue;
	}
      int cmp = GR->gbvector_compare(F,heap[lead_so_far], heap[i]);
      if (cmp == GT) continue;
      if (cmp == LT)
	{
	  lead_so_far = i;
	  continue;
	}
      // At this point we have equality
      K->add_to(heap[lead_so_far]->coeff, heap[i]->coeff);
      gbvector * tmp = heap[i];
      heap[i] = tmp->next;
      tmp->next = NULL;
      GR->gbvector_remove(tmp);

      if (K->is_zero(heap[lead_so_far]->coeff))
	{
	  // Remove, and start over
	  tmp = heap[lead_so_far];
	  heap[lead_so_far] = tmp->next;
	  tmp->next = NULL;
	  GR->gbvector_remove(tmp);
	  lead_so_far = -1;
	  i = -1;
	}
    }
  mLead = lead_so_far;
  if (lead_so_far < 0) return NULL;
  gbvector * result = heap[lead_so_far];
  return result;
}
gbvector * gbvectorHeap::remove_lead_term()
{
  if (mLead < 0) get_lead_term();
  if (mLead < 0) return NULL;
  gbvector * result = heap[mLead];
  heap[mLead] = result->next;
  result->next = NULL;
  mLead = -1;
  return result;
}

gbvector * gbvectorHeap::value()
{
  gbvector * result = NULL;
  for (int i=0; i<=top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      GR->gbvector_add_to(F, result, heap[i]);
      heap[i] = NULL;
    }
  top_of_heap = -1;
  return result;
}

gbvector * gbvectorHeap::current_value() const
{
  gbvector * result = NULL;
  for (int i=0; i<=top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      gbvector * tmp = GR->gbvector_copy(heap[i]);
      GR->gbvector_add_to(F, result, tmp);
    }
  return result;
}






#if 0
gbvector *GBRing::mult_by_exp_term(const FreeModule *F,
				   const gbvector *f,
				   const ring_elem u,
				   const int *exp,
				   int comp)
{
  // There are several versions of this routine:
  // (1) Usual poynomial ring
  // (2) Skew-commutative
  // (3) Weyl algebra
  // (4) Solvable algebra
  // IE, this is a virtual function
}
#endif

#if 0
void GBRing::normal_form(const FreeModule *F, 
			 const FreeModule *Fsyz,
			 gbvector *&f,
			 gbvector *&fsyz)
{
  // TODO
  // What is this really supposed to do?  Reduce fsyz, and possibly multiply f
  //  by some scalar?
}
#endif

void GBRing::reduce_lead_term_heap(const FreeModule *F,
				   const FreeModule *Fsyz,
				   const gbvector *fcurrent_lead,
				   const int *exponents,// exponents of fcurrent_lead
				   gbvector * flead,
				   gbvectorHeap &f,
				   gbvectorHeap &fsyz,
				   const gbvector *g,
				   const gbvector *gsyz)
  //TODO
{
  // These two are taken from gb.cpp gb_geo_reduce
#if 0
	  M->divide(lead->monom, g->monom, reduce_ndiv);
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  // If S!=0, then reduce_ndiv is a TOTAL monomial
	  // THIS FAILS FOR skew commutative case
	  gbvector *h, *hjunk;
	  GR->mult_by_term(F, Fsyz, c, reduce_ndiv, lead->comp, g, 0, h, hjunk);
	  fb.add(h);
#endif
#if 0
	  ring_elem c = R->Ncoeffs()->negate(lead->coeff);
	  M->divide(lead->monom, q->f->monom, reduce_ndiv);
	  // But here, reduce_ndiv is just a monomial in the ring
	  gbvector *h, *hsyz;
	  GR->mult_by_term(F, Fsyz,
			   c, reduce_ndiv, 0, 
			   q->f, q->fsyz,
			   h, hsyz);
	  fb.add(h);		// Eats h
	  fsyzb.add(hsyz);	// Eats hsyz
#endif

}



extern "C" void dvec(const GBRing *R, gbvector *v)
{
  buffer o;
  const FreeModule *F = 0;
  R->gbvector_text_out(o, F, v);
  emit(o.str());
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
