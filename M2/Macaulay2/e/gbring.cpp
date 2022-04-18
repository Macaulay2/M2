#include "gbring.hpp"

#include <assert.h>
#include <stdio.h>

#include "ZZ.hpp"
#include "ZZp.hpp"
#include "aring-glue.hpp"
#include "coeffrings.hpp"
#include "freemod.hpp"
#include "mem.hpp"
#include "ntuple.hpp"
#include "ring.hpp"
#include "schorder.hpp"
#include "text-io.hpp"
#include "weylalg.hpp"

#define sizeofgbvector(s, len) \
  (sizeof(*s) - sizeof(s->monom) + (len) * sizeof(s->monom[0]))

void GBRing::memstats()
{
  buffer o;
  mem->stats(o);
  emit(o.str());
}

gbvector *GBRing::new_raw_term()
{
  void *p = mem->new_elem();
  return (reinterpret_cast<gbvector *>(p));
}

/*************************
 * Exponent handling *****
 *************************/

exponents GBRing::exponents_make()
{
  int *e = newarray_atomic(int, _nvars + 2);  // length is nvars
  return e;
}

void GBRing::exponents_delete(exponents e) { freemem(e); }
////////////////////////////////////////////////////////////////
GBRing::~GBRing()
{
  delete mem;  // all other things will be garbage collected
}
GBRingPoly::~GBRingPoly() {}
GBRingWeyl::~GBRingWeyl() {}
GBRingWeylZZ::~GBRingWeylZZ() {}
GBRingSkew::~GBRingSkew() {}
GBRingSolvable::~GBRingSolvable() {}
GBRing::GBRing(const Ring *K0, const Monoid *M0)
    : _schreyer_encoded(true),
      M(M0),
      K(K0),
      _coeffs_ZZ(false),  // set below
      zzp(0),
      _nvars(M->n_vars()),
      _up_order(false),
      _is_skew(false),
      _skew(),
      _skew_monoms(0),
      is_weyl(false),
      weyl(0),
      is_solvable(false),
      solvable(0),
      _one(K->from_long(1))
{
  exp_size = EXPONENT_BYTE_SIZE(_nvars + 2);
  monom_size = MONOMIAL_BYTE_SIZE(M->monomial_size());

  gbvector_size = sizeofgbvector(((gbvector *)0), M->monomial_size());
  mem = new stash("gbvector", gbvector_size);

  const Z_mod *Kp = K->cast_to_Z_mod();
  if (Kp != 0) zzp = Kp->get_CoeffRing();

  if (K == globalQQ) K = globalZZ;
  if (K == globalZZ) _coeffs_ZZ = true;
}

/////////////////////////////////////////////////////////////////////////
// Multiplication routines for Poly, SkewPoly, Weyl, Solvable algebras //
/////////////////////////////////////////////////////////////////////////

//////////////////////
// Polynomial rings //
//////////////////////

GBRing *GBRing::create_PolynomialRing(const Ring *K, const Monoid *M)
{
  return new GBRingPoly(K, M);
}

gbvector *GBRingPoly::mult_by_term1(const FreeModule *F,
                                    const gbvector *f,
                                    ring_elem u,
                                    const int *monom,
                                    int comp)
{
  // Remember: this multiplies w/o regard to any quotient elements
  gbvector head;
  gbvector *inresult = &head;
  int monlen = M->monomial_size();

  for (const gbvector *s = f; s != NULL; s = s->next)
    {
      gbvector *t = new_raw_term();
      t->next = 0;
      t->comp = s->comp + comp;
      t->coeff = K->mult(u, s->coeff);
      ntuple::mult(monlen, monom, s->monom, t->monom);
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
    : GBRing(K0, M0)
{
  _is_skew = true;
  _skew = skew0;
  int **skew_monoms = newarray(int *, _skew.n_skew_vars());
  int *exp = newarray_atomic_clear(int, M0->n_vars());
  for (int v = 0; v < _skew.n_skew_vars(); v++)
    {
      exp[_skew.skew_variable(v)]++;
      skew_monoms[v] = M0->make_one();
      M0->from_expvector(exp, skew_monoms[v]);
      exp[_skew.skew_variable(v)]--;
    }
  _skew_monoms = skew_monoms;
}

GBRing *GBRing::create_SkewPolynomialRing(const Ring *K0,
                                          const Monoid *M0,
                                          SkewMultiplication skew0)
{
  return new GBRingSkew(K0, M0, skew0);
}

gbvector *GBRingSkew::mult_by_term1(const FreeModule *F,
                                    const gbvector *f,
                                    const ring_elem c,
                                    const int *m,
                                    int comp)
// Computes c*m*f, BUT NOT doing normal form wrt a quotient ideal..
{
  gbvector head;
  gbvector *inresult = &head;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);

  M->to_expvector(m, EXP1);

  for (const gbvector *s = f; s != NULL; s = s->next)
    {
      gbvector_get_lead_exponents(F, s, EXP2);
      int sign = _skew.mult_sign(EXP1, EXP2);
      if (sign == 0) continue;

      gbvector *t = new_raw_term();
      t->next = 0;
      t->comp = s->comp + comp;
      t->coeff = K->mult(c, s->coeff);
      if (sign < 0) K->negate_to(t->coeff);

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
    : GBRing(K0, M0)
{
  is_weyl = true;
  weyl = R0;
}

GBRingWeylZZ::GBRingWeylZZ(const Ring *K0,
                           const Monoid *M0,
                           const WeylAlgebra *R0)
    : GBRingWeyl(K0, M0, R0)
{
  is_weyl = true;
  weyl = R0;
}

GBRing *GBRing::create_WeylAlgebra(const Ring *K0,
                                   const Monoid *M0,
                                   const WeylAlgebra *R0)
{
  if (K0 == globalZZ) return new GBRingWeylZZ(K0, M0, R0);
  return new GBRingWeyl(K0, M0, R0);
}

gbvector *GBRingWeyl::mult_by_term1(const FreeModule *F,
                                    const gbvector *f,
                                    ring_elem u,
                                    const int *monom,
                                    int comp)
{
  // Remember: this multiplies w/o regard to any quotient elements
  gbvectorHeap result(this, F);
  return weyl->gbvector_mult_by_term(result, f, u, monom, comp);
}

gbvector *GBRingWeylZZ::mult_by_term1(const FreeModule *F,
                                      const gbvector *f,
                                      ring_elem u,
                                      const int *monom,
                                      int comp)
{
  // Remember: this multiplies w/o regard to any quotient elements
  gbvectorHeap result(this, F);
  return weyl->gbvector_mult_by_term(result, f, u, monom, comp);
}

///////////////////////
// Solvable algebras //
///////////////////////
GBRingSolvable::GBRingSolvable(const Ring *K0,
                               const Monoid *M0,
                               const SolvableAlgebra *R0)
    : GBRing(K0, M0)
{
  is_solvable = true;
  solvable = R0;
}

GBRing *GBRing::create_SolvableAlgebra(const Ring *K0,
                                       const Monoid *M0,
                                       const SolvableAlgebra *R0)
{
  return new GBRingSolvable(K0, M0, R0);
}

gbvector *GBRingSolvable::mult_by_term1(const FreeModule *F,
                                        const gbvector *f,
                                        ring_elem u,
                                        const int *monom,
                                        int comp)
{
// Remember: this multiplies w/o regard to any quotient elements
#ifdef DEVELOPMENT
#warning "implement GBRingSolvable::mult_by_term"
#endif
  return 0;
}

//////////////////////
// gbvector support //
//////////////////////

void GBRing::gbvector_remove_term(gbvector *f)
{
  // It is not clear whether we should try to free elements of K
  f->next = 0;
  f->coeff = ZERO_RINGELEM;
  mem->delete_elem(f);
  // GC_FREE(reinterpret_cast<char *>(f));
}

void GBRing::gbvector_remove(gbvector *f0)
{
  // It is not clear whether we should try to free elements of K
  gbvector *f = f0;
  while (f != 0)
    {
      gbvector *g = f;
      f = f->next;
      gbvector_remove_term(g);
    }
}

gbvector *GBRing::gbvector_term(const FreeModule *F, ring_elem coeff, int comp)
// Returns coeff*e_sub_i in F, the monomial is set to 1.
{
  gbvector *v = new_raw_term();
  v->coeff = coeff;
  v->comp = comp;
  v->next = 0;
  M->one(v->monom);

  const SchreyerOrder *S;
  if (comp > 0 && _schreyer_encoded && (S = F->get_schreyer_order()) != 0)
    S->schreyer_up(v->monom, comp - 1, v->monom);

  return v;
}

gbvector *GBRing::gbvector_raw_term(ring_elem coeff, const int *monom, int comp)
// Returns coeff*monom*e_sub_i in a free module.  If the order is a Schreyer
// order, the 'monom' should already be encoded.
{
  gbvector *v = new_raw_term();
  v->coeff = coeff;
  v->comp = comp;
  v->next = 0;
  M->copy(monom, v->monom);

  return v;
}

gbvector *GBRing::gbvector_term(const FreeModule *F,
                                ring_elem coeff,
                                const int *monom,
                                int comp)
// Returns coeff*monom*e_sub_i in F.  If comp is 0, then F is never
// considered.
{
  gbvector *v = gbvector_raw_term(coeff, monom, comp);

  const SchreyerOrder *S;
  if (comp > 0 && _schreyer_encoded && (S = F->get_schreyer_order()) != 0)
    S->schreyer_up(v->monom, comp - 1, v->monom);

  return v;
}

gbvector *GBRing::gbvector_term_exponents(const FreeModule *F,
                                          ring_elem coeff,
                                          const int *exp,
                                          int comp)
// Returns coeff*exp*e_sub_i in F.  If comp is 0, then F is never
// considered.
// exp should be an exponent vector of length == n_vars()
{
  gbvector *v = new_raw_term();
  v->coeff = coeff;
  v->comp = comp;
  v->next = 0;
  M->from_expvector(exp, v->monom);

  const SchreyerOrder *S;
  if (comp > 0 && _schreyer_encoded && (S = F->get_schreyer_order()) != 0)
    S->schreyer_up(v->monom, comp - 1, v->monom);

  return v;
}

gbvector *GBRing::gbvector_copy_term(const gbvector *t)
{
  gbvector *v = new_raw_term();
  v->next = 0;
  v->coeff = t->coeff;
  v->comp = t->comp;
  M->copy(t->monom, v->monom);
  return v;
}

bool GBRing::gbvector_is_equal(const gbvector *a, const gbvector *b) const
{
  for (;; a = a->next, b = b->next)
    {
      if (a == NULL)
        {
          if (b == NULL) return true;
          return false;
        }
      if (b == NULL) return false;
      if (a->comp != b->comp) return false;
      if (!K->is_equal(a->coeff, b->coeff)) return false;
      if (M->compare(a->monom, b->monom) != 0) return false;
    }
}

int GBRing::gbvector_n_terms(const gbvector *v) const
{
  int result = 0;
  for (; v != NULL; v = v->next) result++;
  return result;
}

void GBRing::gbvector_multidegree(const FreeModule *F,
                                  const gbvector *f,
                                  int *&result_degree)
{
  /* Return the multi degree of the first term of f */
  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);

  gbvector_get_lead_monomial(F, f, MONOM1);
  result_degree = M->degree_monoid()->make_one();
  M->multi_degree(MONOM1, result_degree);
  M->degree_monoid()->mult(
      result_degree, F->degree(f->comp - 1), result_degree);
}

int GBRing::gbvector_compare(const FreeModule *F,
                             const gbvector *f,
                             const gbvector *g) const
// Return LT, EQ, GT depending on the monomial order of lead(f), lead(g) in F.
{
  int cmp;
  const SchreyerOrder *S = F->get_schreyer_order();

  if (S)
    {
      if (_schreyer_encoded)
        cmp = S->schreyer_compare_encoded(
            f->monom, f->comp - 1, g->monom, g->comp - 1);
      else
        cmp = S->schreyer_compare(f->monom, f->comp - 1, g->monom, g->comp - 1);
    }
  else
    {
      // At this point F doesn't have a Schreyer order
      if (_up_order)
        cmp = M->compare(f->monom, -f->comp, g->monom, -g->comp);
      else
        cmp = M->compare(f->monom, f->comp, g->monom, g->comp);
    }
  return cmp;
}

gbvector *GBRing::gbvector_lead_term(int nparts,
                                     const FreeModule *F,
                                     const gbvector *v)
// What should we do with Schreyer orders?  This probably doesn't
// make much sense, except to get lead monomials.
// If nparts < 0, only take the actual lead term (i.e. one monomial
// in one component only).
{
#ifdef DEVELOPMENT
#warning "Schreyer order question"
#endif
  if (v == NULL) return NULL;
  if (nparts < 0)
    {
      return gbvector_copy_term(v);
    }
  else
    {
      int nslots = M->n_slots(nparts);
      gbvector head;
      gbvector *result = &head;
      for (const gbvector *t = v; t != NULL; t = t->next)
        {
          if (M->compare(nslots, t->monom, v->monom) != 0) break;
          result->next = gbvector_copy_term(t);
          result = result->next;
        }
      result->next = NULL;
      return head.next;
    }
}

static bool isparallel(M2_arrayint w, int *e, int *f)
// We assume: w->len is the same as the number of variables.
{
  // true if e-f is a positive multiple of w

  int i, j, a = 0, b;
  int nvars = w->len;
  for (i = 0; i < nvars; i++)
    {
      a = e[i] - f[i];
      if (a) break;
      if (w->array[i] != 0) return false;
    }
  // i is the first spot where e, f differ
  for (j = i + 1; j < nvars; j++)
    {
      b = e[j] - f[j];
      if (a * w->array[j] != b * w->array[i]) return false;
    }
  return true;
}

gbvector *GBRing::gbvector_parallel_lead_terms(M2_arrayint w,
                                               const FreeModule *F,
                                               const gbvector *leadv,
                                               const gbvector *v)
{
  if (v == NULL) return NULL;

  // loop through every term of v.  Keep those t that satisfy: exp(leadv) -
  // exp(t)
  // is a multiple of w
  //

  exponents lead = ALLOCATE_EXPONENTS(exp_size);
  exponents e = ALLOCATE_EXPONENTS(exp_size);

  M->to_expvector(leadv->monom, lead);

  gbvector head;
  gbvector *result = &head;
  for (const gbvector *t = v; t != NULL; t = t->next)
    {
      M->to_expvector(t->monom, e);
      if (leadv == t || isparallel(w, lead, e))
        {
          result->next = gbvector_copy_term(t);
          result = result->next;
        }
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
  const SchreyerOrder *S;
  if (_schreyer_encoded && (S = F->get_schreyer_order()) != 0 && f->comp > 0)
    S->schreyer_down(f->monom, f->comp - 1, result);
  else
    M->copy(f->monom, result);
}

void GBRing::gbvector_get_lead_exponents(const FreeModule *F,
                                         const gbvector *f,
                                         int *result)
{
  const SchreyerOrder *S;
  if (_schreyer_encoded && (S = F->get_schreyer_order()) != 0 && f->comp > 0)
    {
      monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);

      S->schreyer_down(f->monom, f->comp - 1, MONOM1);
      M->to_expvector(MONOM1, result);
    }
  else
    M->to_expvector(f->monom, result);
}

void GBRing::gbvector_mult_by_coeff_to(gbvector *f, ring_elem u)
{
  for (; f != 0; f = f->next) K->mult_to(f->coeff, u);
}

void GBRing::gbvector_negate_to(gbvector *f) const
{
  for (; f != 0; f = f->next) K->negate_to(f->coeff);
}

gbvector *GBRing::gbvector_mult_by_coeff(const gbvector *v, ring_elem u)
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

gbvector *GBRing::gbvector_copy(const gbvector *f)
{
  gbvector head;
  gbvector *b = &head;
  for (; f != 0; f = f->next, b = b->next) b->next = gbvector_copy_term(f);
  b->next = 0;
  return head.next;
}

void GBRing::gbvector_add_to_zzp(const FreeModule *F,
                                 gbvector *&f,
                                 gbvector *&g)
{
  if (g == NULL) return;
  if (f == NULL)
    {
      f = g;
      g = NULL;
      return;
    }
  gbvector head;
  gbvector *result = &head;
  while (1)
    {
      int compare_result = gbvector_compare(F, f, g);
      if (compare_result == LT)
        {
          result->next = g;
          result = result->next;
          g = g->next;
          if (g == NULL)
            {
              result->next = f;
              f = head.next;
              return;
            }
        }
      else if (compare_result == GT)
        {
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
        }
      else
        {
          gbvector *tmf = f;
          gbvector *tmg = g;
          f = f->next;
          g = g->next;
          CoefficientRingZZp::elem result_coeff;
          zzp->init(result_coeff);
          zzp->add(result_coeff, tmf->coeff.get_int(), tmg->coeff.get_int());
          tmf->coeff = ring_elem(result_coeff);
          if (zzp->is_zero(tmf->coeff.get_int()))
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
        }
    }
}

void GBRing::gbvector_add_to(const FreeModule *F, gbvector *&f, gbvector *&g)
{
  if (g == NULL) return;
  if (f == NULL)
    {
      f = g;
      g = NULL;
      return;
    }
  if (zzp)
    {
      gbvector_add_to_zzp(F, f, g);
      return;
    }
  gbvector head;
  gbvector *result = &head;
  while (1) switch (gbvector_compare(F, f, g))
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
//      if (R->is_ZZ_quotient)
//        {
//          ring_elem t = K->remainder(tmf->coeff, R->ZZ_quotient_value);
//          K->remove(tmf->coeff);
//          tmf->coeff = t;
//        }
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

void GBRing::gbvector_sort(const FreeModule *F, gbvector *&f)
{
  // Divide f into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.

  if (f == NULL || f->next == NULL) return;
  gbvector *f1 = NULL;
  gbvector *f2 = NULL;
  while (f != NULL)
    {
      gbvector *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == NULL) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }

  gbvector_sort(F, f1);
  gbvector_sort(F, f2);
  gbvector_add_to(F, f1, f2);
  f = f1;
}

void GBRing::gbvector_text_out(buffer &o,
                               const FreeModule *F,
                               const gbvector *v,
                               int nterms) const
{
  if (v == NULL)
    {
      o << "0";
      return;
    }

  bool p_one = false;
  bool p_plus = false;
  bool p_parens = false;
  int count = nterms;
  const gbvector *t;
  for (t = v; t != NULL && count != 0; t = t->next, count--)
    {
      int isone = (M == NULL || M->is_one(t->monom));
      K->elem_text_out(o, t->coeff, p_one, p_plus, p_parens);
      if (!isone) M->elem_text_out(o, t->monom, p_one);
      if (t->comp >= 1) o << "<" << t->comp << ">";
      p_plus = true;
    }
  if (t != NULL) o << "+...";
}

void GBRing::divide_exponents(const int *exp1,
                              const int *exp2,
                              int *result) const
{
  for (int i = 0; i < _nvars; i++) *result++ = *exp1++ - *exp2++;
}

void GBRing::exponent_syzygy(const int *exp1,
                             const int *exp2,
                             int *exp3,
                             int *exp4)
{
  for (int i = 0; i < _nvars; i++)
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

gbvector *GBRing::mult_by_term(const FreeModule *F,
                               const gbvector *f,
                               ring_elem u,
                               const int *monom,
                               int comp)
{
  gbvector *result = mult_by_term1(F, f, u, monom, comp);
  const SchreyerOrder *S;
  if (comp > 0 && _schreyer_encoded && (S = F->get_schreyer_order()) != 0)
    {
      for (gbvector *t = result; t != 0; t = t->next)
        S->schreyer_up(t->monom, comp - 1, t->monom);
    }
  return result;
}

void GBRing::gbvector_mult_by_term(const FreeModule *F,
                                   const FreeModule *Fsyz,
                                   ring_elem a,
                                   const int *m,  // element of M, a monomial
                                   const gbvector *f,
                                   const gbvector *fsyz,
                                   gbvector *&result,
                                   gbvector *&result_syz)
{
  // TODO
  // The reason this has both result,result_syz together is in case we want to
  //   reduce result_fsyz mod a quotient ideal.  Do we really need to do this as
  //   we go?  This will require experimentation.
  result = mult_by_term(F, f, a, m, 0);
  result_syz = mult_by_term(Fsyz, fsyz, a, m, 0);
}

void GBRing::find_reduction_coeffs(const FreeModule *F,
                                   const gbvector *f,
                                   const gbvector *g,
                                   ring_elem &u,
                                   ring_elem &v)
{
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  K->syzygy(a, b, u, v);  // If possible, u==1, anyway, u>0

  if (is_skew_commutative())
    {
      exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP3 = ALLOCATE_EXPONENTS(exp_size);
      gbvector_get_lead_exponents(F, f, EXP1);  // Removes the Schreyer part
      gbvector_get_lead_exponents(F, g, EXP2);  // Removes the Schreyer part
      divide_exponents(EXP1, EXP2, EXP3);
      if (_skew.mult_sign(EXP3, EXP2) < 0) K->negate_to(v);
    }
}

bool GBRing::find_reduction_coeffs_ZZ(const FreeModule *F,
                                      const gbvector *f,
                                      const gbvector *g,
                                      ring_elem &v)
{
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  ring_elem rem;

  assert(globalZZ == K);
  rem = globalZZ->remainderAndQuotient(a, b, v);
  if (globalZZ->is_zero(v)) return false;
  v = globalZZ->negate(v);

  if (is_skew_commutative())
    {
      exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP3 = ALLOCATE_EXPONENTS(exp_size);
      gbvector_get_lead_exponents(F, f, EXP1);  // Removes the Schreyer part
      gbvector_get_lead_exponents(F, g, EXP2);  // Removes the Schreyer part
      divide_exponents(EXP1, EXP2, EXP3);
      if (_skew.mult_sign(EXP3, EXP2) < 0) K->negate_to(v);
    }

  return globalZZ->is_zero(rem);
}

void GBRing::find_reduction_monomial(
    const FreeModule *F,
    const gbvector *f,
    const gbvector *g,
    int &comp,
    int *&monom)  // there must be enough space here
{
  M->divide(f->monom, g->monom, monom);

  if (g->comp == 0)
    {
      comp = f->comp;
      const SchreyerOrder *S;
      if (_schreyer_encoded && (S = F->get_schreyer_order()) != 0)
        S->schreyer_down(monom, comp - 1, monom);
    }
  else
    {
      comp = 0;
    }
}

void GBRing::gbvector_reduce_lead_term(const FreeModule *F,
                                       const FreeModule *Fsyz,
                                       gbvector *flead,
                                       gbvector *&f,
                                       gbvector *&fsyz,
                                       const gbvector *g,
                                       const gbvector *gsyz,
                                       bool use_denom,
                                       ring_elem &denom)
{
  int comp;
  ring_elem u, v;

  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);

  find_reduction_coeffs(F, f, g, u, v);
  find_reduction_monomial(F, f, g, comp, MONOM1);

  if (!K->is_equal(u, _one))
    {
      gbvector_mult_by_coeff_to(f, u);  // modifies f
      gbvector_mult_by_coeff_to(flead, u);
      gbvector_mult_by_coeff_to(fsyz, u);
      if (use_denom) K->mult_to(denom, u);
    }

  gbvector *result1 = mult_by_term(F, g, v, MONOM1, comp);
  gbvector_add_to(F, f, result1);
  if (gsyz != 0)
    {
      gbvector *result_syz1 = mult_by_term(Fsyz, gsyz, v, MONOM1, comp);
      gbvector_add_to(Fsyz, fsyz, result_syz1);
    }
}

void GBRing::gbvector_reduce_with_marked_lead_term(const FreeModule *F,
                                                   const FreeModule *Fsyz,
                                                   gbvector *flead,
                                                   gbvector *&f,
                                                   gbvector *&fsyz,
                                                   const gbvector *ginitial,
                                                   const gbvector *g,
                                                   const gbvector *gsyz,
                                                   bool use_denom,
                                                   ring_elem &denom)
{
  int comp;
  ring_elem u, v;

  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);

  find_reduction_coeffs(F, f, ginitial, u, v);
  find_reduction_monomial(F, f, ginitial, comp, MONOM1);

  if (!K->is_equal(u, _one))
    {
      gbvector_mult_by_coeff_to(f, u);  // modifies f
      gbvector_mult_by_coeff_to(flead, u);
      gbvector_mult_by_coeff_to(fsyz, u);
      if (use_denom) K->mult_to(denom, u);
    }

  gbvector *result1 = mult_by_term(F, g, v, MONOM1, comp);
  gbvector_add_to(F, f, result1);
  if (gsyz != 0)
    {
      gbvector *result_syz1 = mult_by_term(Fsyz, gsyz, v, MONOM1, comp);
      gbvector_add_to(Fsyz, fsyz, result_syz1);
    }
}

void GBRing::gbvector_reduce_lead_term(const FreeModule *F,
                                       const FreeModule *Fsyz,
                                       gbvector *flead,
                                       gbvector *&f,
                                       gbvector *&fsyz,
                                       const gbvector *g,
                                       const gbvector *gsyz)
{
  ring_elem junk;
  gbvector_reduce_lead_term(F, Fsyz, flead, f, fsyz, g, gsyz, false, junk);
}

bool GBRing::gbvector_reduce_lead_term_ZZ(const FreeModule *F,
                                          const FreeModule *Fsyz,
                                          gbvector *&f,
                                          gbvector *&fsyz,
                                          const gbvector *g,
                                          const gbvector *gsyz)
// Never multiplies f by anything.  IE before(f), after(f) are equiv. mod g.
// this should ONLY be used if K is globalZZ.
{
  int comp;
  ring_elem v;

  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);

  bool result = find_reduction_coeffs_ZZ(F, f, g, v);
  if (K->is_zero(v)) return false;

  find_reduction_monomial(F, f, g, comp, MONOM1);

  gbvector *result1 = mult_by_term(F, g, v, MONOM1, comp);
  gbvector_add_to(F, f, result1);
  if (gsyz != 0)
    {
      gbvector *result_syz1 = mult_by_term(Fsyz, gsyz, v, MONOM1, comp);
      gbvector_add_to(Fsyz, fsyz, result_syz1);
    }
  return result;
}

void GBRing::gbvector_cancel_lead_terms(const FreeModule *F,
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
  ring_elem u, v;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP3 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP4 = ALLOCATE_EXPONENTS(exp_size);

  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);
  monomial MONOM2 = ALLOCATE_MONOMIAL(monom_size);

  K->syzygy(a, b, u, v);                    // If possible, u==1, anyway, u>0
  gbvector_get_lead_exponents(F, f, EXP1);  // Removes the Schreyer part
  gbvector_get_lead_exponents(F, g, EXP2);  // Removes the Schreyer part
  exponent_syzygy(EXP1, EXP2, EXP3, EXP4);
  if (g->comp == 0)
    comp = f->comp;
  else
    comp = 0;
  if (is_skew_commutative())
    {
      // Note that EXP3 * EXP1 = EXP4 * EXP2 up to sign
      if (_skew.mult_sign(EXP3, EXP1) != _skew.mult_sign(EXP4, EXP2))
        K->negate_to(v);
    }
  M->from_expvector(EXP3, MONOM1);
  M->from_expvector(EXP4, MONOM2);
  result = mult_by_term(F, f, u, MONOM1, 0);
  gbvector *result1 = mult_by_term(F, g, v, MONOM2, comp);
  gbvector_add_to(F, result, result1);
  if (fsyz == 0 && gsyz == 0)
    result_syz = 0;
  else
    {
      result_syz = mult_by_term(Fsyz, fsyz, u, MONOM1, 0);
      gbvector *result_syz1 = mult_by_term(Fsyz, gsyz, v, MONOM2, comp);
      gbvector_add_to(Fsyz, result_syz, result_syz1);
    }
}

void GBRing::gbvector_replace_2by2_ZZ(const FreeModule *F,
                                      const FreeModule *Fsyz,
                                      gbvector *&f,
                                      gbvector *&fsyz,
                                      gbvector *&g,
                                      gbvector *&gsyz)
{
  // ASSUMPTIONS: coefficient ring is ZZ
  //  lead monomial of f and g are the same (with possibly different coeffs)
  // If u * leadcoeff(f) + v * leadcoeff(g) = gd is the gcd,
  // then:
  //  g <-- u * f + v * g
  //  f <-- c * f - d * g, where c = leadcoeff(g)//gd, d = leadcoeff(f)//gd
  mpz_t u, v, gd;
  mpz_init(u);
  mpz_init(v);
  mpz_init(gd);
  mpz_gcdext(gd, u, v, f->coeff.get_mpz(), g->coeff.get_mpz());

  gbvector *new_g = 0;
  gbvector *g2 = 0;
  gbvector *new_gsyz = 0;
  gbvector *gsyz2 = 0;

  if (mpz_sgn(v) != 0)
    {
      g2 = gbvector_mult_by_coeff(g, ring_elem(v));
      gsyz2 = gbvector_mult_by_coeff(gsyz, ring_elem(v));
    }
  if (mpz_sgn(u) != 0)
    {
      new_g = gbvector_mult_by_coeff(f, ring_elem(u));
      new_gsyz = gbvector_mult_by_coeff(fsyz, ring_elem(u));
    }

  gbvector_add_to(F, new_g, g2);
  gbvector_add_to(Fsyz, new_gsyz, gsyz2);

  mpz_fdiv_q(u, g->coeff.get_mpz(), gd);
  mpz_fdiv_q(v, f->coeff.get_mpz(), gd);
  mpz_neg(v, v);

  gbvector *new_f = gbvector_mult_by_coeff(f, ring_elem(u));
  gbvector *new_fsyz = gbvector_mult_by_coeff(fsyz, ring_elem(u));
  gbvector *f2 = gbvector_mult_by_coeff(g, ring_elem(v));
  gbvector *fsyz2 = gbvector_mult_by_coeff(gsyz, ring_elem(v));

  gbvector_add_to(F, new_f, f2);
  gbvector_add_to(Fsyz, new_fsyz, fsyz2);

  f = new_f;
  fsyz = new_fsyz;
  g = new_g;
  gsyz = new_gsyz;

  mpz_clear(u);
  mpz_clear(v);
  mpz_clear(gd);
}

void GBRing::gbvector_combine_lead_terms_ZZ(const FreeModule *F,
                                            const FreeModule *Fsyz,
                                            const gbvector *f,
                                            const gbvector *fsyz,
                                            const gbvector *g,
                                            const gbvector *gsyz,
                                            gbvector *&result,
                                            gbvector *&result_syz)
// If u*x^A*leadmonom(f) + v*x^B*leadmonom(g) = gcd(u,v)*monom (mod lower
// terms),
// set result := u*x^A*f + v*x^B*g
//     resultsyz := u*x^A*fsyz + v*x^B*gyz
// To keep in mind:
//  (a) Schreyer orders
//  (b) Quotient ideal
// Currently: this does nothing with the quotient ring
{
  int comp;
  const ring_elem a = f->coeff;
  const ring_elem b = g->coeff;
  mpz_t gab, u1, v1;

  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP3 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP4 = ALLOCATE_EXPONENTS(exp_size);

  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);
  monomial MONOM2 = ALLOCATE_MONOMIAL(monom_size);

  mpz_init(gab);
  mpz_init(u1);
  mpz_init(v1);
  mpz_gcdext(gab, u1, v1, a.get_mpz(), b.get_mpz());
  mpz_clear(gab);
  //these ring_elem must not escape the function, because they aren't allocated on
  //the gc heap
  ring_elem u = ring_elem(u1);
  ring_elem v = ring_elem(v1);
  if (globalZZ->is_zero(u) || globalZZ->is_zero(v))
    {
      result = 0;
      result_syz = 0;
      return;
    }
  gbvector_get_lead_exponents(F, f, EXP1);  // Removes the Schreyer part
  gbvector_get_lead_exponents(F, g, EXP2);  // Removes the Schreyer part
  exponent_syzygy(EXP1, EXP2, EXP3, EXP4);
  if (g->comp == 0)
    comp = f->comp;
  else
    comp = 0;
  if (is_skew_commutative())
    {
      // Note that EXP3 * EXP1 = EXP4 * EXP2 up to sign
      if (_skew.mult_sign(EXP3, EXP1) != _skew.mult_sign(EXP4, EXP2))
        K->negate_to(v);
    }
  M->from_expvector(EXP3, MONOM1);
  M->from_expvector(EXP4, MONOM2);
  result = mult_by_term(F, f, u, MONOM1, 0);
  gbvector *result1 = mult_by_term(F, g, v, MONOM2, comp);
  gbvector_add_to(F, result, result1);
  if (fsyz == 0 && gsyz == 0)
    result_syz = 0;
  else
    {
      result_syz = mult_by_term(Fsyz, fsyz, u, MONOM1, 0);
      gbvector *result_syz1 = mult_by_term(Fsyz, gsyz, v, MONOM2, comp);
      gbvector_add_to(Fsyz, result_syz, result_syz1);
    }
  mpz_clear(u1);
  mpz_clear(v1);
}

void GBRing::gbvector_apply(const FreeModule *F,
                            const FreeModule *Fsyz,
                            gbvector *&f,
                            gbvector *&fsyz,
                            const gbvector *gsyz,
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

  for (; gsyz != 0; gsyz = gsyz->next)
    {
      if (gsyz->comp < 0)
        {
          gbvector *v = mult_by_term(
              F, quotients[-1 - gsyz->comp], gsyz->coeff, gsyz->monom, f->comp);
          gbvector_add_to(F, f, v);
        }
      else
        {
          gbvector *v =
              mult_by_term(F, elems[gsyz->comp], gsyz->coeff, gsyz->monom, 0);
          gbvector_add_to(F, f, v);
          gbvector *vsyz = mult_by_term(
              Fsyz, elems_syz[gsyz->comp], gsyz->coeff, gsyz->monom, 0);
          gbvector_add_to(Fsyz, fsyz, vsyz);
        }
    }
}

/////////////////////
// Content removal //
/////////////////////
void GBRing::divide_coeff_exact_to_ZZ(gbvector *f, gmp_ZZ u) const
{
  mpz_t a;
  mpz_init(a);
  for (; f != 0; f = f->next)
    {
      mpz_divexact(a, f->coeff.get_mpz(), u);
      f->coeff = globalZZ->RingZZ::from_int(a);
    }
  mpz_clear(a);
}

void GBRing::lower_content_ZZ(gbvector *f, mpz_ptr content) const
// content should be a positive number.  Modify this value
// so that new value of content = gcd(old-content, content(f)).
{
  if (f == 0) return;
  for (; f != 0; f = f->next)
    {
      mpz_gcd(content, content, f->coeff.get_mpz());
      if (mask_mpz_cmp_si(content, 1) == 0) return;
    }
}

void GBRing::gbvector_remove_content_ZZ(gbvector *f,
                                        gbvector *fsyz,
                                        bool use_denom,
                                        ring_elem &denom) const
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
      leadsign = mpz_sgn(g->coeff.get_mpz());
      mpz_init_set(content, g->coeff.get_mpz());
      g = g->next;
    }
  else if (gsyz != 0)
    {
      leadsign = mpz_sgn(gsyz->coeff.get_mpz());
      mpz_init_set(content, gsyz->coeff.get_mpz());
      gsyz = gsyz->next;
    }
  else
    return;

  lower_content_ZZ(g, content);
  lower_content_ZZ(gsyz, content);

  mpz_abs(content, content);
  if (mask_mpz_cmp_si(content, 1) == 0)
    {
      mpz_clear(content);

      if (leadsign < 0)
        {
          gbvector_negate_to(f);
          gbvector_negate_to(fsyz);
        }
      return;
    }

  if (leadsign < 0) mpz_neg(content, content);
  divide_coeff_exact_to_ZZ(f, content);
  divide_coeff_exact_to_ZZ(fsyz, content);
  if (use_denom)
    {
      denom = globalZZ->mult(denom, ring_elem(content));
    }
  mpz_clear(content);
}

void GBRing::gbvector_remove_content(gbvector *f,
                                     gbvector *fsyz,
                                     bool use_denom,
                                     ring_elem &denom)
// let c = gcd(content(f),content(fsyz)).
// set f := f/c,  fsyz := fsyz/c, denom *= c.
// If K is not ZZ, then c is set to make f monic.
//
{
  if (_coeffs_ZZ)
    {
      gbvector_remove_content_ZZ(f, fsyz, use_denom, denom);
      return;
    }
  // At this point, our coefficient ring is a field (What about
  // finite extensions of ZZ?)
  ring_elem c, cinv;
  if (f != 0)
    c = f->coeff;
  else if (fsyz != 0)
    c = fsyz->coeff;
  else
    return;
  cinv = K->invert(c);
  gbvector_mult_by_coeff_to(f, cinv);
  gbvector_mult_by_coeff_to(fsyz, cinv);
  if (use_denom) K->mult_to(denom, c);
}

void GBRing::gbvector_remove_content(gbvector *f, gbvector *fsyz)
// let c = gcd(content(f),content(fsyz)).
// set f := f/c,  fsyz := fsyz/c.
{
  ring_elem junk;
  gbvector_remove_content(f, fsyz, false, junk);
}

////////////////////
// Auto-reduction //
////////////////////
const gbvector *GBRing::find_coeff(const FreeModule *F,
                                   const gbvector *f,
                                   const gbvector *g) const
/* If the monomial (g->monom,g->comp) appears exactly in f,
   then return a pointer to that term, if not, return 0. */
{
  while (f != 0)
    {
      int cmp = gbvector_compare(F, f, g);
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
                                  gbvector *&f,
                                  gbvector *&fsyz,
                                  const gbvector *g,
                                  const gbvector *gsyz)
// If g = a*x^A*ei + lower terms
// and if f = ... + b*x^A*ei + ...
// and if u*a + v*b = 0
// then set f := u*f - v*g, fsyz := u*fsyz - v*gsyz
{
  const gbvector *t;
  if ((t = find_coeff(F, f, g)) != 0)
    {
      ring_elem u, v;
      K->syzygy(t->coeff, g->coeff, u, v);
      if (!K->is_equal(u, _one))
        {
          gbvector_mult_by_coeff_to(f, u);
          gbvector_mult_by_coeff_to(fsyz, u);
        }
      gbvector *g1 = gbvector_mult_by_coeff(g, v);
      gbvector *gsyz1 = gbvector_mult_by_coeff(gsyz, v);
      gbvector_add_to(F, f, g1);
      gbvector_add_to(Fsyz, fsyz, gsyz1);
      gbvector_remove_content(f, fsyz);

      if (M2_gbTrace == 10)
        {
          buffer o;
          o << "auto reducing by ";
          gbvector_text_out(o, F, g);
          o << "\n  --  giving ";
          gbvector_text_out(o, F, f);
          emit_line(o.str());
        }
    }
}

void GBRing::gbvector_auto_reduce_ZZ(const FreeModule *F,
                                     const FreeModule *Fsyz,
                                     gbvector *&f,
                                     gbvector *&fsyz,
                                     const gbvector *g,
                                     const gbvector *gsyz)
// If g = a*x^A*ei + lower terms
// and if f = ... + b*x^A*ei + ...
// and if v*a + b is the balanced remainder of b by a
// then set f := f + v*g, fsyz := fsyz + v*gsyz
// No content is removed.
{
  assert(globalZZ == K);
  const gbvector *t;
  if ((t = find_coeff(F, f, g)) != 0)
    {
      ring_elem v = globalZZ->quotient(t->coeff, g->coeff);
      if (globalZZ->is_zero(v)) return;
      v = globalZZ->negate(v);
      gbvector *g1 = gbvector_mult_by_coeff(g, v);
      gbvector *gsyz1 = gbvector_mult_by_coeff(gsyz, v);
      gbvector_add_to(F, f, g1);
      gbvector_add_to(Fsyz, fsyz, gsyz1);

      if (M2_gbTrace == 10)
        {
          buffer o;
          o << "auto reducing by ";
          gbvector_text_out(o, F, g);
          o << "\n  --  giving ";
          gbvector_text_out(o, F, f);
          emit_line(o.str());
        }
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
  for (i = 0; i < GEOHEAP_SIZE; i++) heap[i] = NULL;
}

gbvectorHeap::~gbvectorHeap()
{
  // The user of this class must insure that all 'vecterm's
  // have been removed first.  Thus, we don't need to
  // do anything here.
}

void gbvectorHeap::mult_by_coeff(ring_elem a)
{
  for (int i = top_of_heap; i >= 0; i--)
    if (heap[i] != 0) GR->gbvector_mult_by_coeff_to(heap[i], a);
}

void gbvectorHeap::add(gbvector *p)
{
  mLead = -1;
  int len = GR->gbvector_n_terms(p);
  int i = 0;
  while (len >= heap_size[i]) i++;
  GR->gbvector_add_to(F, heap[i], p);
  len = GR->gbvector_n_terms(heap[i]);
  p = NULL;
  while (len >= heap_size[i])
    {
      i++;
      GR->gbvector_add_to(F, heap[i], heap[i - 1]);
      len = GR->gbvector_n_terms(heap[i]);
      heap[i - 1] = NULL;
    }
  if (i > top_of_heap) top_of_heap = i;
}

const gbvector *gbvectorHeap::get_lead_term()
{
  int lead_so_far = -1;
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      if (lead_so_far < 0)
        {
          lead_so_far = i;
          continue;
        }
      int cmp = GR->gbvector_compare(F, heap[lead_so_far], heap[i]);
      if (cmp == GT) continue;
      if (cmp == LT)
        {
          lead_so_far = i;
          continue;
        }
      // At this point we have equality
      K->add_to(heap[lead_so_far]->coeff, heap[i]->coeff);
      gbvector *tmp = heap[i];
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
  gbvector *result = heap[lead_so_far];
  return result;
}
gbvector *gbvectorHeap::remove_lead_term()
{
  if (mLead < 0) get_lead_term();
  if (mLead < 0) return NULL;
  gbvector *result = heap[mLead];
  heap[mLead] = result->next;
  result->next = NULL;
  mLead = -1;
  return result;
}

gbvector *gbvectorHeap::value()
{
  gbvector *result = NULL;
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      GR->gbvector_add_to(F, result, heap[i]);
      heap[i] = NULL;
    }
  top_of_heap = -1;
  return result;
}

gbvector *gbvectorHeap::current_value() const
{
  gbvector *result = NULL;
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      gbvector *tmp = GR->gbvector_copy(heap[i]);
      GR->gbvector_add_to(F, result, tmp);
    }
  return result;
}

#include "debug.hpp"
void gbvectorHeap::show() const
{
  for (int i = 0; i <= top_of_heap; i++)
    {
      if (heap[i] == NULL) continue;
      printf("%d ", i);
      dgbvec(GR, heap[i]);
      printf("\n");
    }
}

void GBRing::reduce_marked_lead_term_heap(
    const FreeModule *F,
    const FreeModule *Fsyz,
    const gbvector *fcurrent_lead,
    const int *exponents,  // exponents of fcurrent_lead
    gbvector *flead,
    gbvectorHeap &f,
    gbvectorHeap &fsyz,
    const gbvector
        *marked_in_g,  // lead term of g to use to determine multipliers
    const gbvector *g,
    const gbvector *gsyz)
{
  int comp;
  ring_elem u, v;

  monomial MONOM1 = ALLOCATE_MONOMIAL(monom_size);

  find_reduction_coeffs(F, fcurrent_lead, marked_in_g, u, v);
  find_reduction_monomial(F, fcurrent_lead, marked_in_g, comp, MONOM1);

  if (!K->is_equal(u, _one))
    {
      gbvector_mult_by_coeff_to(flead, u);
      f.mult_by_coeff(u);
      fsyz.mult_by_coeff(u);
    }

  gbvector *result1 = mult_by_term(F, g, v, MONOM1, comp);
  f.add(result1);
  if (gsyz != 0)
    {
      gbvector *result_syz1 = mult_by_term(Fsyz, gsyz, v, MONOM1, comp);
      fsyz.add(result_syz1);
    }
}

void GBRing::reduce_lead_term_heap(
    const FreeModule *F,
    const FreeModule *Fsyz,
    const gbvector *fcurrent_lead,
    const int *exponents,  // exponents of fcurrent_lead
    gbvector *flead,
    gbvectorHeap &f,
    gbvectorHeap &fsyz,
    const gbvector *g,
    const gbvector *gsyz)
{
  reduce_marked_lead_term_heap(
      F, Fsyz, fcurrent_lead, exponents, flead, f, fsyz, g, g, gsyz);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
