// Copyright 1996 Michael E. Stillman

#include <iostream>
#include "poly.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "ringmap.hpp"
#include "matrix.hpp"
#include "ZZ.hpp"
#include "ntuple.hpp"
#include "gbring.hpp"
#include "frac.hpp"
#include "geopoly.hpp"
#include "ZZ.hpp"
#include "monomial.hpp"
#include "relem.hpp"

#include "aring-glue.hpp"  // for globalQQ

#include "debug.hpp"

#define POLY(q) ((q).poly_val)

PolyRing *PolyRing::trivial_poly_ring = 0;  // Will be ZZ[]

void PolyRing::clear()
{
  PolynomialRing::clear();
  delete gb_ring_;
  gb_ring_ = 0;
}

PolyRing::~PolyRing() { clear(); }
const PolyRing *PolyRing::get_trivial_poly_ring()
{
  if (trivial_poly_ring == 0)
    {
      make_trivial_ZZ_poly_ring();
    }

  return trivial_poly_ring;
}

// Order of initialization of trivial monoid M, and trivial ring ZZ[].
//
// (a) make ZZ ring.  Sets degree_ring, D_ to be 0.
// (b) make trivial monoid M, with M->info->degree_ring = 0.
// (c) make trivial poly R = ring(ZZ,M).
// (d) set ZZ's degree ring to be R
// (e) set M's degree ring to be R
// (f) set R's degree ring to be R.

void PolyRing::make_trivial_ZZ_poly_ring()
{
  if (trivial_poly_ring != 0) return;

  //  globalZZ = new RingZZ;
  globalZZ = makeIntegerRing();
  Monoid *M = Monoid::get_trivial_monoid();
  trivial_poly_ring = new PolyRing();

  Monoid::set_trivial_monoid_degree_ring(trivial_poly_ring);
  globalZZ->initialize_ZZ(trivial_poly_ring);
  trivial_poly_ring->initialize_poly_ring(globalZZ, M, trivial_poly_ring);

  const PolyRing *flatR = trivial_poly_ring;
  trivial_poly_ring->gb_ring_ = GBRing::create_PolynomialRing(
      flatR->getCoefficientRing(), flatR->getMonoid());
}

void PolyRing::initialize_poly_ring(const Ring *K,
                                    const Monoid *M,
                                    const PolynomialRing *deg_ring)
// This version is to be called directly ONLY by initialize_poly_ring
// and make_trivial_ZZ_poly_ring.
{
  initialize_ring(K->characteristic(), deg_ring, M->get_heft_vector());

  initialize_PolynomialRing(K, M, this, this, NULL);
  poly_size_ = sizeof(Nterm) + sizeof(int) * (M_->monomial_size() - 1);

  gb_ring_ = 0;

  // A polynomial ring is ALWAYS graded (if the coeff vars, if any,
  //   all have degree 0, which is the case with our flattened poly rings
  this->setIsGraded(true);

  exp_size = EXPONENT_BYTE_SIZE(nvars_);

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);
}

void PolyRing::initialize_poly_ring(const Ring *K, const Monoid *M)
{
  initialize_poly_ring(K, M, M->get_degree_ring());
}

const PolyRing *PolyRing::create(const Ring *K, const Monoid *M)
{
  PolyRing *R = new PolyRing;
  R->initialize_poly_ring(K, M);
  R->gb_ring_ = GBRing::create_PolynomialRing(K, M);
  return R;
}

#if 0
// const PolyRing *PolyRing::create(const Ring *K, const Monoid *M)
//   // Create the ring K[M].
//   // K must be either a basic ring, or an ambient polynomial ring,
//   //  possibly non-commutative of some sort.
// {
//   const PolynomialRing *A = K->cast_to_PolynomialRing();
//   if (A == 0 || K->is_basic_ring())
//     {
//       // K is a basic ring
//       return create_poly_ring(K,M);
//     }
//   const PolyRing *B = A->getAmbientRing();
//   return B->createPolyRing(M);
// }
//
//
// const PolyRing *PolyRing::createPolyRing(const Monoid *M) const
//   // creates this[M], which is commutative in M variables, but possible skew or Weyl
//   // commutative in (some of) the variables of this
// {
//   const Monoid *newM = Monoid::tensor_product(M, getMonoid());
//   if (newM == 0) return 0;
//
//   return create_poly_ring(getCoefficients(),
//                        newM,
//                        this,
//                        M);
// }
#endif

void PolyRing::text_out(buffer &o) const
{
  K_->text_out(o);
  M_->text_out(o);
}

Nterm *PolyRing::new_term() const
{
  Nterm *result = GETMEM(Nterm *, poly_size_);
  result->next = nullptr;
  result->coeff = 0;  // This value is never used, one hopes...
  // In fact, it gets used in the line below:       K->remove(tmp->coeff);
  // which is called from the line below:           remove(idiotic);
  // and it crashes there, because this assignment only sets the integer
  // part of the union, so on a machine with 4 byte ints and 8 byte pointers,
  // the
  // pointer part is not NULL!
  result->coeff.poly_val = nullptr;  // so I added this line
  return result;
}

Nterm *PolyRing::copy_term(const Nterm *t) const
{
  Nterm *result = new_term();
  result->coeff = K_->copy(t->coeff);
  M_->copy(t->monom, result->monom);
  return result;
}

ring_elem PolyRing::from_long(long n) const
{
  ring_elem a = K_->from_long(n);
  if (K_->is_zero(a))
    {
      return ZERO_RINGELEM;
    }
  Nterm *result = new_term();
  result->coeff = a;
  M_->one(result->monom);
  return result;
}
ring_elem PolyRing::from_int(mpz_srcptr n) const
{
  ring_elem a = K_->from_int(n);
  if (K_->is_zero(a))
    {
      return ZERO_RINGELEM;
    }
  Nterm *result = new_term();
  result->coeff = a;
  M_->one(result->monom);
  return result;
}

bool PolyRing::from_rational(mpq_srcptr q, ring_elem &result) const
{
  ring_elem a;
  bool ok = K_->from_rational(q, a);
  if (not ok) return false;
  if (K_->is_zero(a))
    {
      result = ZERO_RINGELEM;
    }
  else
    {
      Nterm *resultpoly = new_term();
      resultpoly->coeff = a;
      M_->one(resultpoly->monom);
      result = resultpoly;
    }
  return true;
}

ring_elem PolyRing::fromCoefficient(ring_elem &coeff) const
{
  Nterm *result;
  if (K_->is_zero(coeff))
    {
      result = ZERO_RINGELEM;
      return result;
    }
  result = new_term();
  result->coeff = coeff;
  M_->one(result->monom);
  return result;
}
bool PolyRing::from_BigComplex(gmp_CC z, ring_elem &result) const
{
  ring_elem a;
  if (!K_->from_BigComplex(z, a))
    {
      result = ZERO_RINGELEM;
      return false;
    }
  result = fromCoefficient(a);
  return true;
}

bool PolyRing::from_BigReal(gmp_RR z, ring_elem &result) const
{
  ring_elem a;
  if (!K_->from_BigReal(z, a))
    {
      result = ZERO_RINGELEM;
      return false;
    }
  result = fromCoefficient(a);
  return true;
}

bool PolyRing::from_Interval(gmp_RRi z, ring_elem &result) const
{
    ring_elem a;
    if (!K_->from_Interval(z, a))
      {
        result = ZERO_RINGELEM;
        return false;
      }
    result = fromCoefficient(a);
    return true;
}

bool PolyRing::from_double(double z, ring_elem &result) const
{
  ring_elem a;
  if (!K_->from_double(z, a))
    {
      result = ZERO_RINGELEM;
      return false;
    }
  result = fromCoefficient(a);
  return true;
}
bool PolyRing::from_complex_double(double re,
                                   double im,
                                   ring_elem &result) const
{
  ring_elem a;
  if (!K_->from_complex_double(re, im, a))
    {
      result = ZERO_RINGELEM;
      return false;
    }
  result = fromCoefficient(a);
  return true;
}

ring_elem PolyRing::var(int v) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  for (int i = 0; i < nvars_; i++) EXP1[i] = 0;
  if (v >= 0 && v < nvars_)
    EXP1[v] = 1;
  else
    {
      std::cerr << "internal error: PolyRing::var(int v) with v = " << v
                << "  nvars = " << nvars_ << std::endl;
      std::cerr << "aborting" << std::endl;
      abort();
      return ZERO_RINGELEM;
    }
  Nterm *result = new_term();
  result->coeff = K_->from_long(1);
  M_->from_expvector(EXP1, result->monom);
  return result;
}

int PolyRing::index_of_var(const ring_elem a) const
{
  Nterm *f = a;
  if (f == 0 || f->next != 0) return -1;
  if (!K_->is_equal(f->coeff, K_->from_long(1))) return -1;
  int result = -1;
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  M_->to_expvector(f->monom, EXP1);
  for (int i = 0; i < n_vars(); i++)
    if (EXP1[i] > 1)
      return -1;
    else if (EXP1[i] == 1)
      {
        if (result >= 0) return -1;
        result = i;
      }
  return result;
}

M2_arrayint PolyRing::support(const ring_elem a) const
{
  exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
  exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
  for (int i = 0; i < n_vars(); i++) EXP1[i] = 0;
  for (const Nterm *f = a; f != 0; f = f->next)
    {
      M_->to_expvector(f->monom, EXP2);
      for (int j = 0; j < n_vars(); j++)
        if (EXP2[j] != 0) EXP1[j] = 1;
    }
  int nelems = 0;
  for (int i = 0; i < n_vars(); i++)
    if (EXP1[i] > 0) nelems++;
  M2_arrayint result = M2_makearrayint(nelems);
  int next = 0;
  for (int i = 0; i < n_vars(); i++)
    if (EXP1[i] > 0) result->array[next++] = i;
  return result;
}

int check_coeff_ring(const Ring *coeffR, const PolyRing *P)
// returns the following:
//  if coeffR is K_, then n_vars()
//  if coeffR is a poly ring, then the difference in #vars
//  otherwise returns -1.
// If a negative number is returned, then an error is set.
{
  int nvars = P->n_vars();
  if (coeffR != P->getCoefficients())
    {
      const PolynomialRing *P0 = coeffR->cast_to_PolynomialRing();
      if (P0 == 0)
        nvars = -1;
      else
        nvars -= P0->n_vars();
    }
  if (nvars < 0) ERROR("expected coefficient ring");
  return nvars;
}

bool PolyRing::promote(const Ring *Rf,
                       const ring_elem f,
                       ring_elem &result) const
{
  // case 1:  Rf = A[x]/J ---> A[x]/I  is one of the 'base_ring's of 'this'.
  // case 2:  Rf = A      ---> A[x]/I  is the ring of scalars

  // Cases:
  //  Rf is a poly ring (or quotient of one), with the same coeff ring as this.
  //  Rf is a poly ring (or quotient) with the same monoid as this (but coeffs
  //  are maybe different).
  //  Rf is the coeff ring of this
  //  Rf is not the coeff ring K of this, but is a base ring of K (e.g. ZZ -->
  //  QQ, A --> frac(A), any others?)
  //  Rf is a poly ring in a smaller number of variables

  int nvars0 = n_vars();
  if (Rf != K_)
    {
      const PolynomialRing *Rf1 = Rf->cast_to_PolynomialRing();
      if (Rf1 != 0)
        nvars0 -= Rf1->n_vars();
      else
        return false;
      if (Rf1 && nvars0 == 0)
        {
          result = copy(f);
          return true;
        }
    }

  int *exp = newarray_atomic_clear(int, nvars0);
  result = make_logical_term(Rf, f, exp);
  return true;
}

bool PolyRing::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // case 1:  Rf = A[x]/J ---> A[x]/I  is one of the 'base_ring's of 'this'.
  // case 2:  Rf = A      ---> A[x]/I  is the ring of scalars

  // We assume that Rg is one of the coefficient rings of 'this'

  Nterm *t = f;
  if (t == 0)
    {
      result = Rg->zero();
      return true;
    }

  int nvars0 = n_vars();
  if (Rg != K_)
    {
      const PolynomialRing *Rg1 = Rg->cast_to_PolynomialRing();
      if (Rg1 != 0)
        nvars0 -= Rg1->n_vars();
      else
        return false;
    }

  int *exp = newarray_atomic(int, nvars0);
  lead_logical_exponents(nvars0, f, exp);
  if (!ntuple::is_one(nvars0, exp)) return false;
  if (n_logical_terms(nvars0, f) > 1) return false;
  result = lead_logical_coeff(Rg, f);
  return true;
}

ring_elem PolyRing::preferred_associate(ring_elem ff) const
{
  Nterm *f = ff;
  if (f == NULL) return from_long(1);
  ring_elem c = K_->preferred_associate(f->coeff);
  Nterm *t = new_term();
  t->coeff = c;
  M_->one(t->monom);
  t->next = 0;
  return t;
}

ring_elem PolyRing::preferred_associate_divisor(ring_elem ff) const
// ff is an element of 'this'.
// result is in the coefficient ring
// If the coefficient ring of this is
//   ZZ -- gcd of all, same sign as lead coeff
//   QQ -- gcd(numerators)/lcm(denominators)
//   basic field -- lead coeff
//   frac(poly ring) -- gcd(numerators)/lcm(denominators)
//   frac(quotient of a poly ring) -- error
{
  ring_elem result = getCoefficients()->zero();
  assert(getCoefficients()->has_associate_divisors());
  for (Nterm *f = ff; f != NULL; f = f->next)
    {
      if (!getCoefficients()->lower_associate_divisor(result, f->coeff))
        // ie it cannot change, no matter what next coeff is
        return result;
    }
  return result;
}

bool PolyRing::is_unit(const ring_elem ff) const
{
  Nterm *f = ff;
  if (f == NULL) return false;
  if (f->next == NULL && M_->is_one(f->monom) && K_->is_unit(f->coeff))
    return true;
  return false;
}

bool PolyRing::is_zero(const ring_elem f) const
{
  Nterm *a = f;
  return a == NULL;
}

bool PolyRing::is_equal(const ring_elem f, const ring_elem g) const
{
  Nterm *a = f;
  Nterm *b = g;
  for (;; a = a->next, b = b->next)
    {
      if (a == NULL)
        {
          if (b == NULL) return true;
          return false;
        }
      if (b == NULL) return false;
      if (!K_->is_equal(a->coeff, b->coeff)) return false;
      if (nvars_ > 0 && (M_->compare(a->monom, b->monom) != 0)) return false;
    }
}

int PolyRing::compare_elems(const ring_elem f, const ring_elem g) const
{
  int cmp;
  Nterm *a = f;
  Nterm *b = g;
  for (;; a = a->next, b = b->next)
    {
      if (a == NULL)
        {
          if (b == NULL) return 0;
          return -1;
        }
      if (b == NULL) return 1;
      if (nvars_ > 0)
        {
          cmp = M_->compare(a->monom, b->monom);
          if (cmp != 0) return cmp;
        }
      cmp = K_->compare_elems(a->coeff, b->coeff);
      if (cmp != 0) return cmp;
    }
}

bool PolyRing::is_homogeneous(const ring_elem f) const
{
  if (!is_graded()) return false;
  Nterm *t = f;
  if (t == 0) return true;
  bool result = true;
  int *e = degree_monoid()->make_one();
  int *degf = degree_monoid()->make_one();
  M_->multi_degree(t->monom, degf);
  for (t = t->next; t != NULL; t = t->next)
    {
      M_->multi_degree(t->monom, e);
      if (0 != degree_monoid()->compare(degf, e))
        {
          result = false;
          break;
        }
    }
  return result;
}

void PolyRing::degree(const ring_elem f, int *degf) const
{
  multi_degree(f, degf);
}

bool PolyRing::multi_degree(const ring_elem f, int *degf) const
{
  Nterm *t = f;
  int *e = degree_monoid()->make_one();
  if (t == 0 || M_->n_vars() == 0)
    {
      degree_monoid()->one(degf);
      return true;
    }
  M_->multi_degree(t->monom, degf);
  bool result = true;
  for (t = t->next; t != NULL; t = t->next)
    {
      M_->multi_degree(t->monom, e);
      if (0 != degree_monoid()->compare(degf, e))
        {
          result = false;
          ;
          degree_monoid()->lcm(degf, e, degf);
        }
    }
  return result;
}

void PolyRing::degree_weights(const ring_elem f,
                              M2_arrayint wts,
                              int &lo,
                              int &hi) const
{
  Nterm *t = f;
  if (t == NULL)
    {
      lo = hi = 0;
      return;
    }
  int e = M_->degree_weights(t->monom, wts);
  lo = hi = e;
  for (t = t->next; t != NULL; t = t->next)
    {
      e = M_->degree_weights(t->monom, wts);
      if (e > hi)
        hi = e;
      else if (e < lo)
        lo = e;
    }
}

ring_elem PolyRing::homogenize(const ring_elem f,
                               int v,
                               int d,
                               M2_arrayint wts) const
{
  // assert(wts[v] != 0);
  // If an error occurs, then return 0, and set gError.

  int *exp = newarray_atomic(int, nvars_);
  int maxlen = (wts->len < nvars_ ? wts->len : nvars_);
  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      M_->to_expvector(a->monom, exp);
      int e = 0;
      for (int i = 0; i < maxlen; i++) e += wts->array[i] * exp[i];
      if (((d - e) % wts->array[v]) != 0)
        {
          // We cannot homogenize, so clean up and exit.
          result->next = NULL;
          ERROR("homogenization impossible");
          result = NULL;
          return result;
        }
      exp[v] += (d - e) / wts->array[v];
      if (is_skew_ && skew_.is_skew_var(v) && exp[v] > 1) continue;
      result->next = new_term();
      result = result->next;
      result->coeff = K_->copy(a->coeff);
      M_->from_expvector(exp, result->monom);
    }
  result->next = NULL;
  sort(head.next);  // The monomial order, etc. might all have changed.
                    // Some terms might even drop out
  freemem(exp);
  return head.next;
}

ring_elem PolyRing::homogenize(const ring_elem f, int v, M2_arrayint wts) const
{
  Nterm *result = NULL;
  if (POLY(f) == NULL) return result;
  int lo, hi;
  degree_weights(f, wts, lo, hi);
  assert(wts->array[v] != 0);
  int d = (wts->array[v] > 0 ? hi : lo);
  return homogenize(f, v, d, wts);
}

ring_elem PolyRing::copy(const ring_elem f) const
{
  Nterm *a = f;
  Nterm head;
  Nterm *result = &head;
  for (; a != NULL; a = a->next, result = result->next)
    result->next = copy_term(a);
  result->next = NULL;
  return head.next;
}

void PolyRing::remove(ring_elem &f) const {}
void PolyRing::internal_negate_to(ring_elem &f) const
{
  Nterm *v = f;
  while (v != NULL)
    {
      K_->negate_to(v->coeff);
      v = v->next;
    }
}

void PolyRing::internal_add_to(ring_elem &ff, ring_elem &gg) const
{
  Nterm *f = ff;
  Nterm *g = gg;
  gg = ZERO_RINGELEM;
  if (g == NULL) return;
  if (f == NULL)
    {
      ff = g;
      return;
    }
  Nterm head;
  Nterm *result = &head;
  while (1) switch (M_->compare(f->monom, g->monom))
      {
        case -1:
          result->next = g;
          result = result->next;
          g = g->next;
          if (g == NULL)
            {
              result->next = f;
              ff = head.next;
              return;
            }
          break;
        case 1:
          result->next = f;
          result = result->next;
          f = f->next;
          if (f == NULL)
            {
              result->next = g;
              ff = head.next;
              return;
            }
          break;
        case 0:
          Nterm *tmf = f;
          Nterm *tmg = g;
          f = f->next;
          g = g->next;
          K_->add_to(tmf->coeff, tmg->coeff);
          if (is_ZZ_quotient_)
            {
              ring_elem t = globalZZ->remainder(tmf->coeff, ZZ_quotient_value_);
              tmf->coeff = t;
            }
          if (!K_->is_zero(tmf->coeff))
            {
              result->next = tmf;
              result = result->next;
            }
          if (g == NULL)
            {
              result->next = f;
              ff = head.next;
              return;
            }
          if (f == NULL)
            {
              result->next = g;
              ff = head.next;
              return;
            }
          break;
      }
}

void PolyRing::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  internal_negate_to(g);
  internal_add_to(f, g);
}

ring_elem PolyRing::negate(const ring_elem f) const
{
  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K_->negate(a->coeff);
      M_->copy(a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

ring_elem PolyRing::add(const ring_elem f, const ring_elem g) const
{
  ring_elem a = copy(f);
  ring_elem b = copy(g);
  internal_add_to(a, b);
  return a;
}

ring_elem PolyRing::subtract(const ring_elem f, const ring_elem g) const
{
  ring_elem a = copy(f);
  ring_elem b = negate(g);
  internal_add_to(a, b);
  return a;
}

ring_elem PolyRing::mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const int *m) const
// return f*c*m
{
  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K_->mult(a->coeff, c);
      M_->mult(m, a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

void PolyRing::mult_coeff_to(ring_elem a, ring_elem &f) const
{
  Nterm *t = f;
  if (t == NULL) return;
  for (; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K_->mult(a, tmp);
    }
}
void PolyRing::divide_coeff_to(ring_elem &f, ring_elem a) const
// f is in this, a is in base coefficient ring
{
  Nterm *t = f;
  if (t == NULL) return;
  for (; t != NULL; t = t->next)
    {
      ring_elem tmp = t->coeff;
      t->coeff = K_->divide(tmp, a);
    }
}

ring_elem PolyRing::mult(const ring_elem f, const ring_elem g) const
{
  polyheap H(this);
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      ring_elem h = mult_by_term(g, a->coeff, a->monom);
      H.add(h);
    }
  return H.value();
}

ring_elem PolyRing::power(const ring_elem f0, mpz_srcptr n) const
{
  ring_elem ff, result;

  if (mpz_sgn(n) == 0) return from_long(1);
  if (is_zero(f0)) return ZERO_RINGELEM;

  mpz_t abs_n;
  mpz_init(abs_n);
  mpz_abs(abs_n, n);
  if (mpz_sgn(n) > 0)
    ff = f0;
  else
    ff = invert(f0);

  Nterm *f = ff;

  // In this case, the computation may only be formed in two
  // cases: (1) f is a constant, or (2) n is small enough
  std::pair<bool, int> n1 = RingZZ::get_si(abs_n);
  if (n1.first)
    {
      result = power(f, n1.second);
    }
  else if (is_unit(f))  // really want a routine 'is_scalar'...
    {
      ring_elem a = K_->power(f->coeff, abs_n);
      result = make_flat_term(a, f->monom);
    }
  else
    {
      ERROR("exponent too large");
      result = ZERO_RINGELEM;
    }

  mpz_clear(abs_n);
  return result;
}

ring_elem PolyRing::power_direct(const ring_elem ff, int n) const
{
    ring_elem result, g, rest, h, tmp;
    ring_elem coef1, coef2, coef3;

    Nterm *lead = ff;
    if (lead == NULL) return ZERO_RINGELEM;

    rest = lead->next;
    g = from_long(1);

    // Start the result with the n th power of the lead term
    Nterm *t = new_term();
    t->coeff = K_->power(lead->coeff, n);
    M_->power(lead->monom, n, t->monom);
    t->next = NULL;
    //  if (_base_ring != NULL) normal_form(t);  NOT NEEDED
    result = t;

    if (POLY(rest) == 0) return result;
    int *m = M_->make_one();

    mpz_t bin_c;

    mpz_init_set_ui(bin_c, 1);

    for (int i = 1; i <= n; i++)
    {
        tmp = mult(g, rest);
        g = tmp;

        mpz_mul_ui(bin_c, bin_c, n - i + 1);
        mpz_fdiv_q_ui(bin_c, bin_c, i);

        coef1 = K_->from_int(bin_c);

        if (!K_->is_zero(coef1))
        {
            coef2 = K_->power(lead->coeff, n - i);
            coef3 = K_->mult(coef1, coef2);
            M_->power(lead->monom, n - i, m);

            h = mult_by_term(g, coef3, m);
            add_to(result, h);
        }
    }
    mpz_clear(bin_c);
    return result;
}

ring_elem PolyRing::power(const ring_elem f0, int n) const
{
  ring_elem ff;

  if (n > 0)
    ff = f0;
  else if (n < 0)
  {
    ff = invert(f0);
    n = -n;
  }
  else
    return from_long(1);

  if(!characteristic())
  {
    return power_direct(ff, n);
  }
  else
  {
    long p=characteristic(), pk=1;
    ring_elem result = from_long(1);
    ring_elem gg = copy(ff), temp; // no need to copy, just the correct number of terms

    while(n)
    {
      for(Nterm *it=ff, *jt=gg; it!=NULL; it=it->next, jt=jt->next)
      {
        jt->coeff = K_->power(it->coeff, pk);
        M_->power(it->monom, pk, jt->monom);
      }

      temp = power_direct(gg, n%p);
      result = mult(result, temp);

      pk *= p;
      n /= p;
    }

    return result;
  }
}

ring_elem PolyRing::invert(const ring_elem f) const
{
  Nterm *ft = f;
  if (is_zero(f))
    {
      ERROR("cannot divide by zero");
      return ZERO_RINGELEM;
    }
  if (ft->next == NULL)
    {
      if (M_->is_one(ft->monom))
        {
          Nterm *t = new_term();
          t->coeff = K_->invert(ft->coeff);
          M_->one(t->monom);
          return t;
        }
      else if (M_->is_invertible(ft->monom))
        {
          Nterm *t = new_term();
          t->coeff = K_->invert(ft->coeff);
          M_->power(ft->monom, -1, t->monom);
          return t;
        }
    }

  ERROR("element is not invertible in this ring");
  return ZERO_RINGELEM;
}

/// Warning! This function works even if g does not divide f.
//  It returns the remainder on division.
ring_elem PolyRing::divide(const ring_elem f, const ring_elem g) const
{
  ring_elem rem, d;
  rem = remainderAndQuotient(f, g, d);
  return d;
}

void PolyRing::imp_subtract_multiple_to(ring_elem &f,
                                        ring_elem a,
                                        const int *m,
                                        const ring_elem g) const
{
  ring_elem b = K_->negate(a);
  ring_elem h = mult_by_term(g, b, m);
  add_to(f, h);
}

bool PolyRing::imp_attempt_to_cancel_lead_term(ring_elem &f,
                                               ring_elem g,
                                               ring_elem &coeff,
                                               int *monom) const
{
  bool result;
  Nterm *t = f;
  Nterm *s = g;
  if (t == NULL || s == NULL) return true;
  if (K_->is_field())
    {
      coeff = K_->divide(t->coeff, s->coeff);
      result = true;
    }
  else if (K_ == globalZZ)
    {
      ring_elem r = globalZZ->remainderAndQuotient(t->coeff, s->coeff, coeff);
      result =
          (globalZZ->is_zero(r));  // true means lead term will be cancelled.
    }
  else
    {
      throw exc::internal_error("should not get to this code in imp_attempt_to_cancel_lead_term");
      coeff = K_->zero();
      result = false;
      // What do we do here??  Call divide, and hope for the best?
    }
  if (!K_->is_zero(coeff))
    {
      if (is_skew_)
        {
          exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
          exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
          exponents EXP3 = ALLOCATE_EXPONENTS(exp_size);
          M_->to_expvector(t->monom, EXP1);
          M_->to_expvector(s->monom, EXP2);
          int sign = skew_.divide(EXP1, EXP2, EXP3);
          M_->from_expvector(EXP3, monom);
          if (sign < 0) K_->negate_to(coeff);
          imp_subtract_multiple_to(f, coeff, monom, g);
        }
      else
        {
          M_->divide(t->monom, s->monom, monom);
          imp_subtract_multiple_to(f, coeff, monom, g);
        }
    }
  return result;
}

ring_elem PolyRing::gcd(const ring_elem ff, const ring_elem gg) const
{
  if (nvars_ != 1)
    {
      ERROR("multivariate gcd not yet implemented");
      return ZERO_RINGELEM;
    }
  ring_elem f = copy(ff);
  ring_elem g = copy(gg);
  ring_elem s, rem;
  while (!is_zero(g))
    {
      rem = remainderAndQuotient(f, g, s);
      f = g;
      g = rem;
    }
#if 0
// #warning commented out make monic in gcd
//   make_monic(f);
#endif
  return f;
}

ring_elem PolyRing::gcd_extended(const ring_elem f,
                                 const ring_elem g,
                                 ring_elem &u,
                                 ring_elem &v) const
// result == gcd(f,g) = u f + v g
{
  u = from_long(1);
  ring_elem result = copy(f);

  if (is_zero(g))
    {
      v = from_long(0);
      return result;
    }
  ring_elem v1 = ZERO_RINGELEM;
  ring_elem v3 = copy(g);
  ring_elem t1, t3;
  ring_elem temp1, temp2, temp3;
  while (!is_zero(v3))
    {
      ring_elem q;
      t3 = remainderAndQuotient(result, v3, q);

      // The following is: t1 = u - q*v1
      temp1 = mult(q, v1);
      subtract_to(u, temp1);
      t1 = u;

      u = v1;
      result = v3;
      v1 = t1;
      v3 = t3;
    }

  // make 'result' monic. (and divide 'u' by this as well)
  if (!is_zero(result))
    {
      Nterm *t = result;
      ring_elem c = K_->invert(t->coeff);
      mult_coeff_to(c, result);
      mult_coeff_to(c, u);
    }

  // The following is v = (result - f*u)/g
  temp1 = mult(f, u);
  temp2 = subtract(result, temp1);
  temp3 = remainderAndQuotient(temp2, g, v);

  return result;
}

void PolyRing::minimal_monomial(ring_elem f, int *&monom) const
{
  // Determines the minimal monomial which divides each term of f.
  // This monomial is placed into 'monom'.

  Nterm *t = f;
  if (t == NULL) return;
  M_->copy(t->monom, monom);
  for (t = t->next; t != NULL; t = t->next) M_->gcd(t->monom, monom, monom);
}

ring_elem PolyRing::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem;
  if (is_zero(g)) throw exc::internal_error("cannot use division algorithm dividing by zero");
  rem = remainderAndQuotient(f, g, quot);
  return rem;
}

ring_elem PolyRing::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem;
  if (is_zero(g)) throw exc::internal_error("cannot use division algorithm dividing by zero");
  rem = remainderAndQuotient(f, g, quot);
  return quot;
}

ring_elem PolyRing::remainderAndQuotient(const ring_elem f,
                                         const ring_elem g,
                                         ring_elem &quot) const
{
  if (K_->get_precision() > 0)
    {
      throw exc::engine_error(
          "polynomial division not yet implemented for RR or CC coefficients");
    }
  Nterm *q, *r;
  ring_elem rem;
  if (is_zero(f))
    {
      // In this case both the remainder and quotient are 0.
      quot = from_long(0);
      return from_long(0); 
    }
  if (is_zero(g))
    {
      quot = from_long(0);
      return copy(f);
    }
  else
    {
      bool has_negative_exponent_variables = getMonoid()->numInvertibleVariables() > 0;
      bool has_vars_lt_one = getMonoid()->numNonTermOrderVariables() > 0;
      
      if (has_negative_exponent_variables and not has_vars_lt_one)
        {
          Nterm* f1 = f;
          Nterm* g1 = g;
          r = division_algorithm_with_laurent_variables(f1, g1, q);
          quot = q;
          return r;
        }
      else if (has_vars_lt_one)
        {
          Nterm *f1 = f;
          Nterm *g1 = g;
          r = powerseries_division_algorithm(f1, g1, q);
          quot = q;
          return r;
        }
      else
        {
          rem = division_algorithm(f, g, q);
          quot = q;
          return rem;
        }
    }
  quot = from_long(0);
  return from_long(0);
}

void PolyRing::syzygy(const ring_elem a,
                      const ring_elem b,
                      ring_elem &x,
                      ring_elem &y) const
{
  // Do some special cases first.  After that: compute a GB

  // For the GB, we need to make a 1 by 2 matrix, and compute until one syzygy
  // is found.
  // create the matrix
  // create the gb comp
  // compute until one syz is found
  // grab the answer from the syz matrix.

  // Special situations:
  if (PolyRing::is_equal(b, one()))
    {
      x = PolyRing::copy(b);
      y = PolyRing::negate(a);
    }
  else if (PolyRing::is_equal(b, minus_one()))
    {
      x = one();
      y = PolyRing::copy(a);
    }
  else
    {
#if 0
// // MES Aug 2002 ifdef'ed because gb_comp is not back yet
//       intarray syzygy_stop_conditions;
//       syzygy_stop_conditions.append(0); // ngb
//       syzygy_stop_conditions.append(1); // nsyz
//       syzygy_stop_conditions.append(0); // npairs
//       syzygy_stop_conditions.append(0);
//       syzygy_stop_conditions.append(0);
//       syzygy_stop_conditions.append(0); // subring limit
//       syzygy_stop_conditions.append(0);
//
//       const FreeModule *F = make_FreeModule(1);
//       Matrix *m = new Matrix(F);
//       m->append(F->raw_term(a,0));
//       m->append(F->raw_term(b,0));
// #if 0
// //   buffer o;
// //   o << "constructing syzygy on ";
// //   elem_text_out(o,a);
// //   o << " and ";
// //   elem_text_out(o,b);
// //   emit_line(o.str());
// //   o.reset();
// //   o << "matrix is" << newline;
// //   m->text_out(o);
// //   emit_line(o.str());
// //   o.reset();
// #endif
//
//       gb_comp *g = gb_comp::make(m,true,-1,0);
//       g->calc(0, syzygy_stop_conditions);
//       Matrix *s = g->syz_matrix();
//
// #if 0
// //   if (s.n_cols() != 1)
// //     {
// //       o << "found " << s.n_cols() << " syzygies";
// //       emit_line(o.str());
// //     }
// #endif
//       x = s->elem(0,0);
//       y = s->elem(1,0);
//       ring_elem c = preferred_associate(x);
//       ring_elem x1 = mult(c,x);
//       ring_elem y1 = mult(c,y);
//       x = x1;
//       y = y1;
// #if 0
// //   o << "result: x = ";
// //   elem_text_out(o,x);
// //   o << " and y = ";
// //   elem_text_out(o,y);
// //   emit_line(o.str());
// #endif
//       freemem(g);
#endif
    }
}

ring_elem PolyRing::random() const
{
  return make_flat_term(K_->random(), M_->make_one());
}

void PolyRing::elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const
{
  Nterm *t = f;
  if (t == NULL)
    {
      o << '0';
      return;
    }

  int two_terms = (t->next != NULL);

  int needs_parens = p_parens && two_terms;
  if (needs_parens)
    {
      if (p_plus) o << '+';
      o << '(';
      p_plus = false;
    }

  for (t = f; t != NULL; t = t->next)
    {
      int isone = M_->is_one(t->monom);
      p_parens = !isone;
      bool p_one_this = (isone && needs_parens) || (isone && p_one);
      K_->elem_text_out(o, t->coeff, p_one_this, p_plus, p_parens);
      if (!isone)
        {
          M_->elem_text_out(o, t->monom, p_one_this);
        }
      p_plus = true;
    }
  if (needs_parens) o << ')';
}

ring_elem PolyRing::eval(const RingMap *map,
                         const ring_elem f,
                         int first_var) const
{
  // The way we collect the result depends on whether the target ring
  // is a polynomial ring: if so, use a heap structure.  If not, just add to the
  // result.

  intarray vp;
  const Ring *target = map->get_ring();
  SumCollector *H = target->make_SumCollector();

  for (Nterm *t = f; t != NULL; t = t->next)
    {
      vp.shrink(0);
      M_->to_varpower(t->monom, vp);
      ring_elem g = map->eval_term(K_, t->coeff, vp.raw(), first_var, n_vars());
      H->add(g);
    }
  ring_elem result = H->getValue();
  delete H;
  return result;
}

ring_elem PolyRing::zeroize_tiny(gmp_RR epsilon, const ring_elem f) const
{
  Nterm head;
  Nterm *result = &head;
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      ring_elem c = K_->zeroize_tiny(epsilon, a->coeff);
      if (!K_->is_zero(c))
        {
          result->next = new_term();
          result = result->next;
          result->coeff = c;
          M_->copy(a->monom, result->monom);
        }
    }
  result->next = NULL;
  return head.next;
}
void PolyRing::increase_maxnorm(gmp_RRmutable norm, const ring_elem f) const
{
  for (Nterm *a = f; a != NULL; a = a->next)
    K_->increase_maxnorm(norm, a->coeff);
}

/////////////////////////////////////////
// Useful division algorithm routines ///
/////////////////////////////////////////
// These are private routines, called from remainder
// or remainderAndQuotient or quotient.
/////////////////////////////////////////

///@brief Create an exponent vector whose i-th value is the minimum
/// of the exponents of that variable, if that variable is a Laurent variable
/// (i.e. allows negative exponents).

std::vector<int> PolyRing::setNegativeExponentMonomial(Nterm* f) const
{
  int *exp = new int[n_vars()];
  std::vector<int> result(n_vars(), 0);
  Nterm* t = f;
  getMonoid()->to_expvector(t->monom, exp);
  for (int i=0; i<n_vars(); i++)
    if (getMonoid()->isLaurentVariable(i))
      result[i] = exp[i];

  for (t = t->next; t != nullptr; t = t->next)
    {
      getMonoid()->to_expvector(t->monom, exp);
      for (int i=0; i<n_vars(); i++)
        if (getMonoid()->isLaurentVariable(i) and exp[i] < result[i])
          result[i] = exp[i];
    }
  delete [] exp;
  return result;
}

Nterm *PolyRing::division_algorithm_with_laurent_variables(Nterm *f, Nterm *g, Nterm *&quot) const
{
  // Step 1: replace f with f1, m1 (f = m1 * f1, f1 has only exponents >= 0, and no monomial factors)
  // Same: g = n1 * g1.
  // then if f1 = q * g1 + r, (q, r polynomials with only non-negative exponents)
  // then f = (q*m1*n1^(-1)) * g + m1*r
  //std::pair<Nterm*, int*> factor_out_inverse_variables(f);
  auto expf = setNegativeExponentMonomial(f);
  auto expg = setNegativeExponentMonomial(g);
  int* m = getMonoid()->make_one();
  int* n = getMonoid()->make_one();
  getMonoid()->from_expvector(expf.data(), m);
  getMonoid()->from_expvector(expg.data(), n);

  for (auto& a : expf) a = -a;
  for (auto& a : expg) a = -a;
  int* minv = getMonoid()->make_one();
  int* ninv = getMonoid()->make_one();
  getMonoid()->from_expvector(expf.data(), minv);
  getMonoid()->from_expvector(expg.data(), ninv);

  for (int i=0; i<n_vars(); ++i)
    expf[i] = -expf[i] + expg[i];
  int* mninv = getMonoid()->make_one();
  getMonoid()->from_expvector(expf.data(), mninv);
  
  ring_elem c = getCoefficientRing()->from_long(1);
  Nterm* f1 = mult_by_term(f, c, minv); // f1 = m^-1 * f, m only involves the variables which allow negative exponents
  Nterm* g1 = mult_by_term(g, c, ninv); // g1 = n^-1 * g.
  Nterm* quot1 = nullptr;
  Nterm* rem1 = division_algorithm(f1, g1, quot1);
  Nterm* rem = mult_by_term(rem1, c, m);
  quot = mult_by_term(quot1, c, mninv);
  return rem;
}

Nterm *PolyRing::division_algorithm(Nterm *f, Nterm *g, Nterm *&quot) const
{
  // This returns the remainder, and sets quot to be the quotient.

  // This does standard division by one polynomial.
  // However, it does work for Weyl algebra, skew commutative algebra,
  // This works if the coefficient ring is a field, or ZZ.

  ring_elem a = copy(f);
  Nterm *t = a;
  Nterm *b = g;
  Nterm divhead;
  Nterm remhead;
  Nterm *divt = &divhead;
  Nterm *remt = &remhead;
  Nterm *nextterm = new_term();

  //  buffer o;
  while (t != NULL)
    if (M_->divides_partial_order(b->monom, t->monom))
      {
        // o << "t = "; elem_text_out(o,t); o << newline;
        a = t;
        bool cancelled = imp_attempt_to_cancel_lead_term(
            a, g, nextterm->coeff, nextterm->monom);
        t = a;
        //      o << "  new t = "; elem_text_out(o,t); o << newline;
        //      o << "  cancelled = " << (cancelled ? "true" : "false") <<
        //      newline;
        //      o << "  coeff = "; K_->elem_text_out(o,nextterm->coeff); o <<
        //      newline;
        //      emit(o.str());
        if (!K_->is_zero(nextterm->coeff))
          {
            divt->next = nextterm;
            divt = divt->next;
            nextterm = new_term();
          }
        if (!cancelled)
          {
            remt->next = t;
            remt = remt->next;
            t = t->next;
          }
      }
    else
      {
        remt->next = t;
        remt = remt->next;
        t = t->next;
      }

  nextterm = NULL;
  remt->next = NULL;
  divt->next = NULL;
  quot = divhead.next;
  return remhead.next;
}

Nterm *PolyRing::division_algorithm(Nterm *f, Nterm *g) const
{
  // This does standard division by one polynomial, returning the remainder.
  // However, it does work for Weyl algebra, skew commutative algebra,
  // as long as the coefficient ring is a field.
  ring_elem a = copy(f);
  Nterm *t = a;
  Nterm *b = g;
  Nterm remhead;
  Nterm *remt = &remhead;
  ring_elem c;
  int *m = M_->make_one();
  while (t != NULL)
    if (M_->divides_partial_order(b->monom, t->monom))
      {
        a = t;
        bool cancelled = imp_attempt_to_cancel_lead_term(a, g, c, m);
        t = a;
        if (!cancelled)
          {
            remt->next = t;
            remt = remt->next;
            t = t->next;
          }
      }
    else
      {
        remt->next = t;
        remt = remt->next;
        t = t->next;
      }

  remt->next = NULL;
  return remhead.next;
}

Nterm *PolyRing::powerseries_division_algorithm(Nterm *f,
                                                Nterm *g,
                                                Nterm *&quot) const
{
  // This is intended for use when there is one variable, inverses are present,
  // and the coefficient ring is a field, or is ZZ.
  // The algorithm used is as follows.
  // Given a non-zero polynomial f = f(t,t^-1), let v(f) = top - bottom
  //   where top is the largest exponent in f, and bottom is the smallest.
  //   So if f is a monomial, v(f) = 0.  Also, v(fg) = v(f)+v(g) (at least if
  //   the
  //   coefficient ring is a domain), and v(f) >= 0 always.
  // The algorithm is as follows:
  //   Reduce f = f0 by lt(g) to obtain f1, then again to obtain f2, etc.
  //   So v(f1) >= v(f2) >= ... >= v(fi),
  //   and either fi = 0, v(fi) < v(g), or v(f(i+1)) > v(fi).
  //   In this case, the remainder returned is fi.
  //   (Note: the last case won't happen if the coefficients are a field, or the
  //   lead coefficient of g is a unit).

  // This is intended for use when the monomial order has an initial weight
  // vector and the first
  //   term of the divisor's weight value is strictly greater than the other
  //   terms.
  //   AND where (all) inverses are present,
  // and the coefficient ring is a field, or is ZZ.
  // The algorithm used is as follows.
  // Given a non-zero polynomial f = f(t,t^-1), let v(f) = top - bottom
  //   where top is the weight vec value of the first monomial of f, and bottom
  //   is the weight vec value
  //   for the last term of f.
  //   So if f is a monomial, v(f) = 0.  Also, v(fg) = v(f)+v(g) (at least if
  //   the
  //   coefficient ring is a domain), and v(f) >= 0 always.
  // The algorithm is as follows:
  //   Reduce f = f0 by lt(g) to obtain f1, then again to obtain f2, etc.
  //   So v(f1) >= v(f2) >= ... >= v(fi),
  //   and either fi = 0, v(fi) < v(g), or v(f(i+1)) > v(fi).
  //   In this case, the remainder returned is fi.
  //   (Note: the last case won't happen if the coefficients are a field, or the
  //   lead coefficient of g is a unit).

  // This returns the remainder, and sets quot to be the quotient.

  ring_elem a = copy(f);
  Nterm *t = a;
  Nterm *b = g;

  if (!M_->weight_value_exists())
    {
      throw exc::engine_error("no method for division in this ring");
    }

  Nterm divhead;
  Nterm remhead;
  Nterm *divt = &divhead;
  Nterm *remt = &remhead;
  Nterm *nextterm = new_term();
  long gval = 0, flast = 0;

  if (t != 0)
    {
      Nterm *z = t;
      for (; z->next != 0; z = z->next)
        ;

      flast = M_->first_weight_value(z->monom);
    }

  if (b != 0)
    {
      long gfirst, glast;
      Nterm *z = b;

      if (z->next == 0)
        gval = 0;
      else
        {
          gfirst = M_->first_weight_value(z->monom);
          long gsecond = M_->first_weight_value(z->next->monom);
          if (gfirst == gsecond)
            {
              // In this case, we return silently
              throw exc::internal_error("division algorithm for this element is not implemented");
            }
          for (; z->next != 0; z = z->next)
            ;
          glast = M_->first_weight_value(z->monom);
          gval = gfirst - glast;
        }
    }

  //  buffer o;
  while (t != NULL)
    {
      long ffirst;

      ffirst = M_->first_weight_value(t->monom);
      long fval = ffirst - flast;

      // std::cout << "f: "; dNterm(this, t); std::cout << std::endl;
      // std::cout << "g: "; dNterm(this, g); std::cout << std::endl;
      if (fval >= gval and M_->divides(g->monom, t->monom))
        //if (fval >= gval)
        {
          // o << "t = "; elem_text_out(o,t); o << newline;
          a = t;
          bool cancelled = imp_attempt_to_cancel_lead_term(
              a, g, nextterm->coeff, nextterm->monom);
          t = a;
          //    o << "  new t = "; elem_text_out(o,t); o << newline;
          //      o << "  cancelled = " << (cancelled ? "true" : "false") <<
          //      newline;
          //    o << "  coeff = "; K_->elem_text_out(o,nextterm->coeff); o <<
          //    newline;
          //    emit(o.str());
          if (!K_->is_zero(nextterm->coeff))
            {
              divt->next = nextterm;
              divt = divt->next;
              nextterm = new_term();
            }
          if (!cancelled)
            {
              remt->next = t;
              remt = remt->next;
              t = t->next;
            }
        }
      else
        {
          remt->next = t;
          remt = remt->next;
          t = t->next;
        }
    }

  nextterm = NULL;
  remt->next = NULL;
  divt->next = NULL;
  quot = divhead.next;
  return remhead.next;
}

////////////////////////////////////
// Working with logical terms //////
////////////////////////////////////

ring_elem PolyRing::get_logical_coeff(const Ring *coeffR, const Nterm *&f) const
// coeffR needs to be either:
//  (a) the actual coeff ring K_ of this, or
//  (b) one of the poly rings used to construct this, in the front end.
// This function DOES NOT check this!
// Given an Nterm f, return the coeff of its logical monomial, in the
// polynomial ring coeffR.  f is modified, in that it is replaced by
// the pointer to the first term of f not used (possibly 0).
// coeffR should be a coefficient ring of 'this'.
{
  if (f == 0) return coeffR->zero();
  if (coeffR == K_)
    {
      ring_elem result = f->coeff;
      f = f->next;
      return result;
    }
  const PolynomialRing *KR = coeffR->cast_to_PolynomialRing();
  assert(KR);
  const PolyRing *K = KR->getNumeratorRing();
  Nterm head;
  Nterm *inresult = &head;
  inresult->next = 0;
  int *exp = newarray_atomic(int, n_vars());
  int *exp2 = newarray_atomic(int, n_vars());
  int nvars = n_vars() - K->n_vars();
  M_->to_expvector(f->monom, exp);
  ntuple::copy(n_vars(), exp, exp2);
  do
    {
      Nterm *t = K->new_term();
      inresult->next = t;
      inresult = t;
      t->coeff = f->coeff;
      K->getMonoid()->from_expvector(exp2 + nvars, t->monom);

      f = f->next;
      if (f == 0) break;
      M_->to_expvector(f->monom, exp2);
    }
  while (EQ == ntuple::lex_compare(nvars, exp, exp2));
  inresult->next = 0;
  return head.next;
}

void PolyRing::lead_logical_exponents(int nvars0,
                                      const ring_elem f,
                                      int *result_exp) const
{
  Nterm *g = f;
  assert(g != NULL);
  int *exp = newarray_atomic(int, n_vars());
  M_->to_expvector(g->monom, exp);
  ntuple::copy(nvars0, exp, result_exp);
}

ring_elem PolyRing::lead_logical_coeff(const Ring *coeffR,
                                       const ring_elem f) const
{
  const Nterm *t = f;
  return get_logical_coeff(coeffR, t);
}

int PolyRing::n_logical_terms(int nvars0, const ring_elem f) const
{
  // nvars0 is the number of variables in the outer monoid
  if (nvars0 == n_vars()) return n_terms(f);
  Nterm *t = f;
  if (t == 0) return 0;
  int *exp1 = newarray_atomic(int, n_vars());
  int *exp2 = newarray_atomic(int, n_vars());
  M_->to_expvector(t->monom, exp1);
  int result = 1;
  for (; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp2);
      if (EQ == ntuple::lex_compare(nvars0, exp1, exp2)) continue;
      int *temp = exp1;
      exp1 = exp2;
      exp2 = temp;
      result++;
    }
  freemem(exp1);
  freemem(exp2);
  return result;
}

engine_RawArrayPairOrNull PolyRing::list_form(const Ring *coeffR,
                                              const ring_elem f) const
{
  // Either coeffR should be the actual coefficient ring (possible a "toField"ed
  // ring)
  // or a polynomial ring.  If not, NULL is returned and an error given
  // In the latter case, the last set of variables are part of
  // the coefficients.
  int nvars0 = check_coeff_ring(coeffR, this);
  if (nvars0 < 0) return 0;
  int n = n_logical_terms(nvars0, f);
  engine_RawMonomialArray monoms =
      GETMEM(engine_RawMonomialArray, sizeofarray(monoms, n));
  engine_RawRingElementArray coeffs =
      GETMEM(engine_RawRingElementArray, sizeofarray(coeffs, n));
  monoms->len = n;
  coeffs->len = n;
  engine_RawArrayPair result = newitem(struct engine_RawArrayPair_struct);
  result->monoms = monoms;
  result->coeffs = coeffs;

  int *exp = newarray_atomic(int, n_vars());
  intarray resultvp;
  const Nterm *t = f;
  for (int next = 0; next < n; next++)
    {
      getMonoid()->to_expvector(t->monom, exp);
      ring_elem c =
          get_logical_coeff(coeffR, t);  // increments t to the next term of f.
      varpower::from_ntuple(nvars0, exp, resultvp);
      monoms->array[next] = Monomial::make(resultvp.raw());
      coeffs->array[next] = RingElement::make_raw(coeffR, c);
      resultvp.shrink(0);

      assert(monoms->array[next] != NULL);
      assert(coeffs->array[next] != NULL);
    }
  freemem(exp);
  return result;
}

struct part_elem : public our_new_delete
{
  part_elem *next;
  long wt;
  Nterm head;
  Nterm *inresult;
};

ring_elem *PolyRing::get_parts(const M2_arrayint wts,
                               const ring_elem f,
                               long &result_len) const
{
  // (1) Make a hashtable: keys are weight values, values are indices into an
  // array
  // (2) Loop over all terms, inserting a copy of each term at the correct
  // weight value
  // (3) Sort the array, by increasing weight values.
  // (4) Make an array, copy the elems to it.

  return NULL;
}

ring_elem PolyRing::get_part(const M2_arrayint wts,
                             const ring_elem f,
                             bool lobound_given,
                             bool hibound_given,
                             long lobound,
                             long hibound) const
{
  Nterm head;
  Nterm *inresult = &head;

  int *exp = newarray_atomic(int, M_->n_vars());

  for (Nterm *t = f; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp);
      long wt = ntuple::weight(M_->n_vars(), exp, wts);
      if (lobound_given && wt < lobound) continue;
      if (hibound_given && wt > hibound) continue;
      inresult->next = copy_term(t);
      inresult = inresult->next;
    }

  inresult->next = 0;
  freemem(exp);
  return head.next;
}

ring_elem PolyRing::make_logical_term(const Ring *coeffR,
                                      const ring_elem a,
                                      const int *exp0) const
{
  const PolynomialRing *logicalK = coeffR->cast_to_PolynomialRing();

  int nvars0 = n_vars();
  if (K_ == coeffR)
    {
      int *m = M_->make_one();
      M_->from_expvector(exp0, m);
      return make_flat_term(a, m);
    }
  if (logicalK == 0)
    {
      ERROR("expected actual coefficient ring");
      return from_long(0);
    }
  nvars0 -= logicalK->n_vars();

  Nterm head;
  Nterm *inresult = &head;
  int *exp = newarray_atomic(int, M_->n_vars());
  ntuple::copy(nvars0, exp0, exp);  // Sets the first part of exp
  for (Nterm *f = a; f != 0; f = f->next)
    {
      Nterm *t = new_term();
      inresult->next = t;
      inresult = t;
      t->coeff = f->coeff;
      logicalK->getMonoid()->to_expvector(f->monom, exp + nvars0);
      M_->from_expvector(exp, t->monom);
    }
  inresult->next = 0;
  return head.next;
}

ring_elem PolyRing::get_terms(int nvars0,
                              const ring_elem f,
                              int lo,
                              int hi) const
{
  int nterms = n_logical_terms(nvars0, f);
  if (lo < 0) lo = nterms + lo;
  if (hi < 0) hi = nterms + hi;

  Nterm *t = f;
  if (t == 0) return t;
  Nterm head;
  Nterm *result = &head;

  int *exp1 = newarray_atomic(int, n_vars());
  int *exp2 = newarray_atomic(int, n_vars());
  M_->to_expvector(t->monom, exp1);
  int n = 0;
  while (t != NULL)
    {
      if (n > hi) break;
      if (n >= lo)
        {
          result->next = copy_term(t);
          result = result->next;
        }
      t = t->next;
      if (t == 0) break;
      M_->to_expvector(t->monom, exp2);
      if (EQ == ntuple::lex_compare(nvars0, exp1, exp2)) continue;
      int *temp = exp1;
      exp1 = exp2;
      exp2 = temp;
      n++;
    }
  result->next = NULL;
  return head.next;
}

int PolyRing::n_flat_terms(const ring_elem f) const
{
  int result = 0;
  for (Nterm *a = f; a != NULL; a = a->next) result++;
  return result;
}

ring_elem PolyRing::make_flat_term(const ring_elem a, const int *m) const
{
  if (K_->is_zero(a)) return ZERO_RINGELEM;
  Nterm *t = new_term();
  t->coeff = K_->copy(a);
  M_->copy(m, t->monom);
  t->next = NULL;
  return t;
}

ring_elem PolyRing::lead_flat_coeff(const ring_elem f) const
{
  Nterm *t = f;
  if (t == NULL) return K_->from_long(0);
  return K_->copy(t->coeff);
}

const int *PolyRing::lead_flat_monomial(const ring_elem f) const
{
  Nterm *t = f;
  assert(t != NULL);
  return t->monom;
}

ring_elem PolyRing::get_coeff(const Ring *coeffR,
                              const ring_elem f,
                              const int *vp) const
// note: vp is a varpower monomial.
{
  int nvars0 = check_coeff_ring(coeffR, this);
  if (nvars0 < 0) return from_long(0);

  int *exp = newarray_atomic(int, nvars0);
  int *exp2 = newarray_atomic(int, n_vars());  // FLAT number of variables
  varpower::to_ntuple(nvars0, vp, exp);

  // Now loop thru f until exponents match up.
  const Nterm *t = f;
  for (; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp2);
      if (EQ == ntuple::lex_compare(nvars0, exp, exp2)) break;
    }

  ring_elem result = get_logical_coeff(coeffR, t);
  freemem(exp2);
  freemem(exp);
  return result;
}

ring_elem PolyRing::diff(ring_elem a, ring_elem b, int use_coeff) const
{
  polyheap H(this);
  Nterm *d = new_term();
  for (Nterm *s = a; s != 0; s = s->next)
    {
      for (Nterm *t = b; t != 0; t = t->next)
        {
          d->coeff = diff_term(s->monom, t->monom, d->monom, use_coeff);
          if (!K_->is_zero(d->coeff))
            {
              K_->mult_to(d->coeff, s->coeff);
              K_->mult_to(d->coeff, t->coeff);
              d->next = 0;
              H.add(d);
              d = new_term();
            }
        }
    }
  return H.value();
}

ring_elem PolyRing::diff_term(const int *m,
                              const int *n,
                              int *resultmon,
                              int use_coeff) const
{
  int sign = 0;
  if (!M_->divides(m, n)) return K_->from_long(0);
  if (is_skew_ && use_coeff)
    {
      exponents EXP1 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP2 = ALLOCATE_EXPONENTS(exp_size);
      exponents EXP3 = ALLOCATE_EXPONENTS(exp_size);
      M_->to_expvector(m, EXP1);
      M_->to_expvector(n, EXP2);
      sign = skew_.diff(EXP1, EXP2, EXP3);
      M_->from_expvector(EXP3, resultmon);
    }
  else
    M_->divide(n, m, resultmon);
  ring_elem result = K_->from_long(1);
  if (!use_coeff) return result;
  intarray e1, e2;
  int *exp1 = e1.alloc(n_vars());
  int *exp2 = e2.alloc(n_vars());
  M_->to_expvector(m, exp1);
  M_->to_expvector(n, exp2);

  if (is_skew_ && sign < 0) K_->negate_to(result);

  for (int i = 0; i < n_vars(); i++)
    {
      for (int j = exp1[i] - 1; j >= 0; j--)
        {
          ring_elem g = K_->from_long(exp2[i] - j);
          K_->mult_to(result, g);
          if (K_->is_zero(result)) return result;
        }
    }
  return result;
}

void PolyRing::sort(Nterm *&f) const
{
  // Divide f into two lists of equal length, sort each,
  // then add them together.  This allows the same monomial
  // to appear more than once in 'f'.

  if (f == NULL || f->next == NULL) return;
  Nterm *f1 = NULL;
  Nterm *f2 = NULL;
  while (f != NULL)
    {
      Nterm *t = f;
      f = f->next;
      t->next = f1;
      f1 = t;

      if (f == NULL) break;
      t = f;
      f = f->next;
      t->next = f2;
      f2 = t;
    }

  sort(f1);
  sort(f2);
  ring_elem g = f1;
  ring_elem h = f2;
  add_to(g, h);
  f = g;
}

bool PolyRing::in_subring(int nslots, const ring_elem a) const
{
  for (Nterm *t = a; t != 0; t = t->next)
    if (!M_->in_subring(nslots, t->monom)) return false;
  return true;
}

void PolyRing::degree_of_var(int n, const ring_elem a, int &lo, int &hi) const
{
  Nterm *t = a;
  if (t == NULL)
    {
      ERROR("attempting to find degree of a zero element");
      return;
    }
  int *exp = newarray_atomic(int, n_vars());
  M_->to_expvector(t->monom, exp);
  lo = hi = exp[n];
  for (t = t->next; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp);
      if (exp[n] < lo)
        lo = exp[n];
      else if (exp[n] > hi)
        hi = exp[n];
    }
  freemem(exp);
}

void PolyRing::monomial_divisor(const ring_elem a, int *exp) const
// Replaces the flat exponent vector 'exp' with its gcd with the gcd of all
// monomials of 'a'.
{
  int *exp1 = newarray_atomic(int, n_vars());
  for (const Nterm *t = a; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp1);
      ntuple::gcd(n_vars(), exp1, exp, exp);
    }
}

ring_elem PolyRing::divide_by_var(int n, int d, const ring_elem a) const
// Divide each monomial of 'a' by x^d, where x is the n th variable.
// If a monomial is not divisible by x^d, then that monomial is not put
// into the result.
{
  if (d == 0) return a;
  Nterm head;
  Nterm *result = &head;
  int *exp = newarray_atomic(int, n_vars());
  for (Nterm *t = a; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp);
      if (exp[n] >= d)
        exp[n] -= d;
      else
        continue;
      result->next = new_term();
      result = result->next;
      result->coeff = t->coeff;
      M_->from_expvector(exp, result->monom);
    }
  freemem(exp);
  result->next = 0;
  return head.next;
}

ring_elem PolyRing::divide_by_expvector(const int *exp, const ring_elem a) const
{
  Nterm *result = 0;
  int *exp0 = newarray_atomic(int, n_vars());
  for (Nterm *t = a; t != 0; t = t->next)
    {
      M_->to_expvector(t->monom, exp0);
      ntuple::quotient(n_vars(), exp0, exp, exp0);
      Nterm *u = new_term();
      u->coeff = t->coeff;
      M_->from_expvector(exp0, u->monom);
      u->next = result;
      result = u;
    }
  freemem(exp0);
  sort(result);
  return result;
}

void PolyRing::lower_content(ring_elem &c, ring_elem g) const
// c is a content elem, g is in ring
{
  for (Nterm *t = g; t != 0; t = t->next) K_->lower_content(c, t->coeff);
}

ring_elem PolyRing::content(ring_elem f) const
{
  Nterm *t = f;
  if (t == 0) return K_->zero();
  ring_elem c = t->coeff;
  for (t = t->next; t != 0; t = t->next) K_->lower_content(c, t->coeff);
  return c;
}

ring_elem PolyRing::content(ring_elem f, ring_elem g) const
{
  Nterm *t = f;
  if (t == 0) return K_->zero();
  ring_elem c = t->coeff;
  for (t = t->next; t != 0; t = t->next) K_->lower_content(c, t->coeff);
  for (t = g; t != 0; t = t->next) K_->lower_content(c, t->coeff);
  return c;
}

ring_elem PolyRing::divide_by_given_content(ring_elem f, ring_elem c) const
{
  Nterm *a = f;
  if (a == 0) return f;

  Nterm head;
  Nterm *result = &head;
  for (; a != NULL; a = a->next)
    {
      result->next = new_term();
      result = result->next;
      result->coeff = K_->divide(a->coeff, c);
      M_->copy(a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

////////////////////////////////////
// Translation to/from arrays //////
////////////////////////////////////
const PolyRing * /* or null */ isUnivariateOverPrimeField(const Ring *R)
{
  const PolyRing *P = R->cast_to_PolyRing();
  if (P == 0) return 0;
  if (P->n_vars() != 1) return 0;
  if (P->characteristic() == 0) return 0;
  return P;
}

ring_elem PolyRing::fromSmallIntegerCoefficients(
    const std::vector<long> &coeffs,
    int var) const
{
  // create a poly
  SumCollector *H = make_SumCollector();
  exponents exp = ALLOCATE_EXPONENTS(EXPONENT_BYTE_SIZE(
      n_vars()));  // deallocates automatically at end of block
  ntuple::one(n_vars(), exp);
  for (long i = 0; i < coeffs.size(); i++)
    {
      exp[var] = static_cast<int>(i);
      ring_elem c = K_->from_long(coeffs[i]);
      if (K_->is_zero(c)) continue;
      Nterm *t = new_term();
      t->coeff = c;
      t->next = NULL;
      M_->from_expvector(exp, t->monom);
      H->add(t);
    }
  ring_elem result = H->getValue();
  delete H;
  return result;
}

///////////////////////////////////
// vec routines for polynomials ///
///////////////////////////////////
ring_elem PolyRing::lead_term(int nparts, const ring_elem f) const
{
  Nterm *lead = f;
  Nterm head;
  Nterm *result = &head;
  int nslots = M_->n_slots(nparts);
  for (Nterm *a = f; a != NULL; a = a->next)
    {
      if (M_->compare(nslots, lead->monom, a->monom) != EQ) break;
      result->next = new_term();
      result = result->next;
      result->coeff = a->coeff;
      M_->copy(a->monom, result->monom);
    }
  result->next = NULL;
  return head.next;
}

const vecterm *PolyRing::vec_locate_lead_term(const FreeModule *F, vec v) const
// Returns a pointer to the lead vector of v.
// This works if F has a Schreyer order, or an up/down order.
{
  if (v == 0) return v;
  const vecterm *lead = v;
  const SchreyerOrder *S = F->get_schreyer_order();
  if (S)
    {
      for (vec w = v->next; w != 0; w = w->next)
        {
          if (S->schreyer_compare(POLY(lead->coeff)->monom,
                                  lead->comp,
                                  POLY(w->coeff)->monom,
                                  w->comp) == LT)
            {
              lead = w;
            }
        }
    }
  else
    {
      for (vec w = v->next; w != 0; w = w->next)
        {
          if (M_->compare(POLY(lead->coeff)->monom,
                          lead->comp,
                          POLY(w->coeff)->monom,
                          w->comp) == LT)
            {
              lead = w;
            }
        }
    }
  return lead;
}

vec PolyRing::vec_lead_term(int nparts, const FreeModule *F, vec v) const
{
  // The first step is to find the lead monomial.

  if (v == 0) return 0;
  const vecterm *lead = vec_locate_lead_term(F, v);

  // Now that we have the lead term, use the first n parts of the monomial
  // ordering

  ring_elem r = PolyRing::lead_term(nparts, lead->coeff);
  return make_vec(lead->comp, r);
}

vec PolyRing::vec_coefficient_of_var(vec v, int x, int e) const
// Find the coefficient of x^e in v.
{
  int *exp = newarray_atomic(int, n_vars());
  vecterm vec_head;
  vecterm *vec_result = &vec_head;
  for (vecterm *t = v; t != NULL; t = t->next)
    {
      Nterm head;
      Nterm *result = &head;
      for (Nterm *f = t->coeff; f != NULL; f = f->next)
        {
          M_->to_expvector(f->monom, exp);
          if (exp[x] != e) continue;
          exp[x] = 0;
          result->next = new_term();
          result = result->next;
          result->coeff = f->coeff;
          M_->from_expvector(exp, result->monom);
        }
      result->next = NULL;
      vec_result->next = make_vec(t->comp, head.next);
      vec_result = vec_result->next;
    }
  freemem(exp);
  vec_result->next = NULL;
  return vec_head.next;
}

vec PolyRing::vec_top_coefficient(const vec v, int &x, int &e) const
// find the smallest index variable x which occurs in v, and also find e s.t.
// x^e is
// the largest power of x occurring in v.  Set x and e accordingly.
// Return the coefficient of x^e.
// IF v has no variables occurring in it, then set x to be #vars, e to be 0 and
// return v.
// If v is 0, then set x to -1, e to 0, and v to 0.
{
  x = n_vars();
  e = 0;
  if (v == NULL)
    {
      return NULL;
    }

  int *exp = newarray_atomic(int, n_vars());
  for (vec t = v; t != 0; t = t->next)
    for (Nterm *f = t->coeff; f != 0; f = f->next)
      {
        M_->to_expvector(f->monom, exp);
        for (int i = 0; i < x; i++)
          {
            if (exp[i] > 0)
              {
                x = i;
                e = exp[i];
                break;
              }
          }
        if (x < n_vars() && exp[x] > e) e = exp[x];
      }

  // Now we have the variable, and its exponent.
  freemem(exp);
  if (x == n_vars()) return v;
  return vec_coefficient_of_var(v, x, e);
}

///////////////////////////////////
// translation gbvector <--> vec //
///////////////////////////////////
#include "geovec.hpp"

void PolyRing::determine_common_denominator_QQ(ring_elem f,
                                               mpz_ptr denom_so_far) const
{
  // We assume that 'this' is QQ[M].
  if (getCoefficients() != globalQQ) return;

  for (Nterm *t = f; t != 0; t = t->next)
    {
      mpq_srcptr a = MPQ_VAL(t->coeff);
      mpz_lcm(denom_so_far, denom_so_far, mpq_denref(a));
    }
}

ring_elem PolyRing::get_denominator_QQ(ring_elem f) const
// returns an element c of getCoefficients(), s.t. f = c*g, and g has:
// over QQ: content(g)=1, leadmonomial(g) is positive, g has no denominators in
// QQ
// over ZZ: c is the content, leadmonomial(g) is positive.
// over a basic field: leadmonomial(g) is 1.
{
  assert(tpoly(f) != 0);
  assert(getCoefficients() == globalQQ);

  mpz_t denom;
  mpz_init_set_si(denom, 1);

  determine_common_denominator_QQ(f, denom);
  ring_elem result = globalZZ->RingZZ::from_int(denom);
  mpz_clear(denom);
  return result;
}

ring_elem PolyRing::vec_get_denominator_QQ(vec f) const
// returns an element c of getCoefficients(), s.t. f = c*g, and g has:
// over QQ: content(g)=1, leadmonomial(g) is positive, g has no denominators in
// QQ
// over ZZ: c is the content, leadmonomial(g) is positive.
// over a basic field: leadmonomial(g) is 1.
{
  assert(f != 0);
  assert(getCoefficients() == globalQQ);

  mpz_t denom;
  mpz_init_set_si(denom, 1);

  for (vec w = f; w != 0; w = w->next)
    determine_common_denominator_QQ(w->coeff, denom);
  ring_elem result = globalZZ->RingZZ::from_int(denom);
  mpz_clear(denom);
  return result;
}

gbvector *PolyRing::translate_gbvector_from_ringelem_QQ(ring_elem coeff) const
{
  // We assume that the ring elements here have no denominators
  GBRing *GR = get_gb_ring();
  gbvector head;
  gbvector *inresult = &head;
  for (Nterm *t = coeff; t != 0; t = t->next)
    {
      // make a gbvector node.
      ring_elem a = globalZZ->RingZZ::from_int(mpq_numref(MPQ_VAL(t->coeff)));
      gbvector *g = GR->gbvector_term(0, a, t->monom, 0);
      inresult->next = g;
      inresult = inresult->next;
    }
  return head.next;
}

gbvector *PolyRing::translate_gbvector_from_ringelem(ring_elem coeff) const
{
  if (getCoefficients() == globalQQ)
    return translate_gbvector_from_ringelem_QQ(coeff);
  GBRing *GR = get_gb_ring();
  gbvector head;
  gbvector *inresult = &head;
  for (Nterm *t = coeff; t != 0; t = t->next)
    {
      // make a gbvector node.
      gbvector *g = GR->gbvector_term(0, t->coeff, t->monom, 0);
      inresult->next = g;
      inresult = inresult->next;
    }
  return head.next;
}

gbvector *PolyRing::translate_gbvector_from_vec_QQ(
    const FreeModule *F,
    const vec v,
    ring_elem &result_denominator) const
{
  if (v == 0)
    {
      result_denominator = globalZZ->one();
      return 0;
    }
  GBRing *GR = get_gb_ring();
  result_denominator = vec_get_denominator_QQ(v);
  gbvectorHeap H(GR, F);
  gbvector head;
  gbvector *inresult;
  mpz_t a;
  mpz_init(a);
  for (vec w = v; w != 0; w = w->next)
    {
      inresult = &head;
      int comp = w->comp + 1;
      for (Nterm *t = w->coeff; t != 0; t = t->next)
        {
          // make a gbvector node.
          mpq_srcptr b = MPQ_VAL(t->coeff);
          mpz_mul(a, result_denominator.get_mpz(), mpq_numref(b));
          mpz_divexact(a, a, mpq_denref(b));
          gbvector *g = GR->gbvector_term(
              F, globalZZ->RingZZ::from_int(a), t->monom, comp);
          inresult->next = g;
          inresult = inresult->next;
        }
      H.add(head.next);
    }
  mpz_clear(a);
  return H.value();
}

gbvector *PolyRing::translate_gbvector_from_vec(
    const FreeModule *F,
    const vec v,
    ring_elem &result_denominator) const
{
  if (getCoefficients() == globalQQ)
    return translate_gbvector_from_vec_QQ(F, v, result_denominator);
  result_denominator = getCoefficients()->one();
  if (v == 0) return 0;
  GBRing *GR = get_gb_ring();
  gbvectorHeap H(GR, F);
  gbvector head;
  gbvector *inresult;
  for (vec w = v; w != 0; w = w->next)
    {
      inresult = &head;
      int comp = w->comp + 1;
      for (Nterm *t = w->coeff; t != 0; t = t->next)
        {
          // make a gbvector node.
          gbvector *g = GR->gbvector_term(F, t->coeff, t->monom, comp);
          inresult->next = g;
          inresult = inresult->next;
        }
      H.add(head.next);
    }

  return H.value();
}

// This next routine was physically lifted from the one right below it, and
// only a couple of lines changed.  Very poor programming on my part (MES),
// and I need to fix that

vec PolyRing::translate_gbvector_to_vec_QQ(const FreeModule *F,
                                           const gbvector *v,
                                           const ring_elem denom) const
{
  if (v == 0) return 0;

  GBRing *GR = get_gb_ring();

  int firstcomp = v->comp;
  int lastcomp = firstcomp;
  for (const gbvector *t = v->next; t != 0; t = t->next)
    {
      if (firstcomp > t->comp)
        firstcomp = t->comp;
      else if (lastcomp < t->comp)
        lastcomp = t->comp;
    }

  Nterm **vec_comps = newarray(Nterm *, lastcomp - firstcomp + 1);
  Nterm **vec_last = newarray(Nterm *, lastcomp - firstcomp + 1);
  for (int i = 0; i < lastcomp - firstcomp + 1; i++)
    {
      vec_comps[i] = 0;
      vec_last[i] = 0;
    }

  // Now make a list of Nterm's, copy gbvectors in (except comps)
  for (const gbvector *t = v; t != 0; t = t->next)
    {
      Nterm *s = new_term();
      GR->gbvector_get_lead_monomial(F, t, s->monom);
      s->coeff = globalQQ->fraction(t->coeff, denom);
      s->next = 0;
      int x = t->comp - firstcomp;
      if (!vec_comps[x])
        {
          vec_comps[x] = s;
          vec_last[x] = s;
        }
      else
        {
          vec_last[x]->next = s;
          vec_last[x] = s;
        }
    }

  // Now create the vecs
  vec result = 0;
  for (int x = 0; x < lastcomp - firstcomp + 1; x++)
    if (vec_comps[x])
      {
        vec w = make_vec(x + firstcomp - 1, vec_comps[x]);
        w->next = result;
        result = w;
      }

  // Finally, free vec_last, vec_comps;
  freemem(vec_comps);
  freemem(vec_last);

  return result;
}

vec PolyRing::translate_gbvector_to_vec(const FreeModule *F,
                                        const gbvector *v) const
{
  if (v == 0) return 0;

  if (getCoefficients() == globalQQ)
    return translate_gbvector_to_vec_QQ(F, v, globalZZ->one());
  GBRing *GR = get_gb_ring();

  int firstcomp = v->comp;
  int lastcomp = firstcomp;
  for (const gbvector *t = v->next; t != 0; t = t->next)
    {
      if (firstcomp > t->comp)
        firstcomp = t->comp;
      else if (lastcomp < t->comp)
        lastcomp = t->comp;
    }

  Nterm **vec_comps = newarray(Nterm *, lastcomp - firstcomp + 1);
  Nterm **vec_last = newarray(Nterm *, lastcomp - firstcomp + 1);
  for (int i = 0; i < lastcomp - firstcomp + 1; i++)
    {
      vec_comps[i] = 0;
      vec_last[i] = 0;
    }

  // Now make a list of Nterm's, copy gbvectors in (except comps)
  for (const gbvector *t = v; t != 0; t = t->next)
    {
      Nterm *s = new_term();
      GR->gbvector_get_lead_monomial(F, t, s->monom);
      s->coeff = t->coeff;
      s->next = 0;
      int x = t->comp - firstcomp;
      if (!vec_comps[x])
        {
          vec_comps[x] = s;
          vec_last[x] = s;
        }
      else
        {
          vec_last[x]->next = s;
          vec_last[x] = s;
        }
    }

  // Now create the vecs
  vec result = 0;
  for (int x = 0; x < lastcomp - firstcomp + 1; x++)
    if (vec_comps[x])
      {
        vec w = make_vec(x + firstcomp - 1, vec_comps[x]);
        w->next = result;
        result = w;
      }

  // Finally, free vec_last, vec_comps;
  freemem(vec_comps);
  freemem(vec_last);

  return result;
}

vec PolyRing::translate_gbvector_to_vec_denom(const FreeModule *F,
                                              const gbvector *v,
                                              const ring_elem denom) const
{
  if (getCoefficients() == globalQQ)
    return translate_gbvector_to_vec_QQ(F, v, denom);
  GBRing *GR = get_gb_ring();
  const ring_elem c = K_->invert(denom);
  gbvector *w = GR->gbvector_mult_by_coeff(v, c);
  vec result = translate_gbvector_to_vec(F, w);
  GR->gbvector_remove(w);
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
