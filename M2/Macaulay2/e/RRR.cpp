// Copyright 1995 Michael E. Stillman

#include "rand.h"
#include "ZZ.hpp"
#include "QQ.hpp"
#include "RRR.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "gbring.hpp"

#if 0
// #include "gmp.h"
// #define MPF_VAL(f) (mpfr_ptr ((f).poly_val))
// #define MPF_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))
#endif

bool RRR::initialize_RRR(unsigned long prec)
{
  initialize_ring(0);
  declare_field();
  precision = prec;
  _elem_size = sizeof(mpfr_t);
  _zero_elem = new_elem();

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  coeffR = new Ring_RRR(this);
  return true;
}

RRR *RRR::create(unsigned long prec)
{
  RRR *result = new RRR;
  result->initialize_RRR(prec);
  return result;
}

void RRR::text_out(buffer &o) const
{
  o << "RRR_" << precision;
}

mpfr_ptr RRR::new_elem() const
{
  mpfr_ptr result = getmemstructtype(mpfr_ptr);
  mpfr_init2(result,precision);
  return result;
}

ring_elem RRR::random() const
{
  gmp_RR result = rawRandomRR(precision);
  return MPF_RINGELEM(result);
}

void RRR::elem_text_out(buffer &o,
                        const ring_elem ap,
                        bool p_one,
                        bool p_plus,
                        bool p_parens) const
{
  mpfr_ptr a = MPF_VAL(ap);
  M2_string s = (*gmp_tostringRRpointer)(a);
  bool prepend_plus = p_plus && (s->array[0] != '-');
  bool strip_last = !p_one && (
                              (s->len == 1 && s->array[0] == '1')
                              || (s->len == 2 && s->array[1] == '1' && s->array[0] == '-'));

  if (prepend_plus)
    o << "+";
  if (strip_last)
    o.put(s->array, s->len-1);
  else
    o.put(s->array, s->len);

}

mpfr_ptr RRR::to_BigReal(ring_elem f) const
{
  return MPF_VAL(f);
}

ring_elem RRR::from_int(int n) const
{
  mpfr_ptr result = new_elem();
  mpfr_set_si(result, n, GMP_RNDN);

  return MPF_RINGELEM(result);
}

ring_elem RRR::from_int(mpz_ptr n) const
{
  mpfr_ptr result = new_elem();
  mpfr_set_z(result, n, GMP_RNDN);

  return MPF_RINGELEM(result);
}

ring_elem RRR::from_rational(mpq_ptr r) const
{
  mpfr_ptr result = new_elem();
  mpfr_set_q(result, r, GMP_RNDN);

  return MPF_RINGELEM(result);
}

bool RRR::from_BigReal(gmp_RR r, ring_elem &result1) const
{
  mpfr_ptr result = new_elem();
  mpfr_set(result, r, GMP_RNDN);

  result1 = MPF_RINGELEM(result);
  return true;
}

bool RRR::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  if (Rf == globalZZ)
    {
      result = RRR::from_int(f.get_mpz());
      return true;
    }
  if (Rf == globalQQ)
    {
      mpfr_ptr g = new_elem();
      mpfr_set_q(g, MPQ_VAL(f), GMP_RNDN);
      result = MPF_RINGELEM(g);
      return true;
    }
  if (Rf->is_RRR())
    {
      gmp_RR g = new_elem();
      mpfr_set(g, MPF_VAL(f), GMP_RNDN);
      result = MPF_RINGELEM(g);
      return true;
    }
  return false;
}

bool RRR::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  return false;
}

bool RRR::is_unit(const ring_elem f) const
{
  return !is_zero(f);
}

bool RRR::is_zero(const ring_elem f) const
{
  mpfr_ptr a = MPF_VAL(f);
  return mpfr_zero_p(a) != 0;
}

bool RRR::is_equal(const ring_elem f, const ring_elem g) const
{
  return mpfr_equal_p(MPF_VAL(f), MPF_VAL(g));
}

bool RRR::is_greater(const ring_elem f, const ring_elem g) const
{
  mpfr_ptr a = MPF_VAL(f);
  mpfr_ptr b = MPF_VAL(g);
  return mpfr_cmp(a,b) > 0;
}
bool RRR::is_less(const ring_elem f, const ring_elem g) const
{
  mpfr_ptr a = MPF_VAL(f);
  mpfr_ptr b = MPF_VAL(g);
  return mpfr_cmp(a,b) < 0;
}

int RRR::compare_elems(const ring_elem f, const ring_elem g) const
{
  mpfr_ptr a = MPF_VAL(f);
  mpfr_ptr b = MPF_VAL(g);
  int cmp = mpfr_cmp(a,b);
  if (cmp > 0) return 1;
  if (cmp == 0) return 0;
  return -1;
}

int RRR::is_positive(const ring_elem f) const
{
  mpfr_ptr a = MPF_VAL(f);
  return mpfr_sgn(a) > 0;
}

ring_elem RRR::absolute(const ring_elem f) const
{
  mpfr_ptr result = new_elem();
  mpfr_ptr a = MPF_VAL(f);
  mpfr_abs(result, a, GMP_RNDN);
  return MPF_RINGELEM(result);
}


ring_elem RRR::copy(const ring_elem f) const
{
  mpfr_ptr a = MPF_VAL(f);

  mpfr_ptr result = new_elem();
  mpfr_set(result, a, GMP_RNDN);
  return MPF_RINGELEM(result);
}

void RRR::remove(ring_elem &f) const
{
  // Intentionally empty
}

ring_elem RRR::preferred_associate(ring_elem f) const
{
  mpfr_ptr a = MPF_VAL(f);
  if (mpfr_sgn(a) >= 0)
    return from_int(1);
  return from_int(-1);
}

ring_elem RRR::negate(const ring_elem f) const
{
  mpfr_ptr result = new_elem();
  mpfr_neg(result, MPF_VAL(f), GMP_RNDN);
  return MPF_RINGELEM(result);
}

ring_elem RRR::add(const ring_elem f, const ring_elem g) const
{
  mpfr_ptr result = new_elem();
  mpfr_add(result, MPF_VAL(f), MPF_VAL(g), GMP_RNDN);
  return MPF_RINGELEM(result);
}

ring_elem RRR::subtract(const ring_elem f, const ring_elem g) const
{
  mpfr_ptr result = new_elem();
  mpfr_sub(result, MPF_VAL(f), MPF_VAL(g), GMP_RNDN);
  return MPF_RINGELEM(result);
}

ring_elem RRR::mult(const ring_elem f, const ring_elem g) const
{
  mpfr_ptr result = new_elem();
  mpfr_mul(result, MPF_VAL(f), MPF_VAL(g), GMP_RNDN);
  return MPF_RINGELEM(result);
}

ring_elem RRR::power(const ring_elem f, int n) const
{
  mpfr_ptr result = new_elem();
  mpfr_pow_si(result, MPF_VAL(f), n, GMP_RNDN);
  return MPF_RINGELEM(result);
}
ring_elem RRR::power(const ring_elem f, mpz_t n) const
{
  mpfr_ptr result = new_elem();
  mpfr_pow_z(result, MPF_VAL(f), n, GMP_RNDN);
  return MPF_RINGELEM(result);
}

ring_elem RRR::sqrt(const ring_elem f) const
{
  mpfr_ptr result = new_elem();
  mpfr_sqrt(result, MPF_VAL(f), GMP_RNDN);
  return MPF_RINGELEM(result);
}

ring_elem RRR::invert(const ring_elem f) const
{
  if (is_zero(f))
    return from_int(0);
  else {
    mpfr_ptr result = new_elem();
    mpfr_ui_div(result, 1, MPF_VAL(f), GMP_RNDN);
    return MPF_RINGELEM(result);
  }
}

ring_elem RRR::divide(const ring_elem f, const ring_elem g) const
{
  mpfr_ptr result = new_elem();
  mpfr_div(result, MPF_VAL(f), MPF_VAL(g), GMP_RNDN);
  return MPF_RINGELEM(result);
}

void RRR::syzygy(const ring_elem a, const ring_elem b,
               ring_elem &x, ring_elem &y) const
{
  if (is_zero(b))
    {
      x = from_int(0);
      y = from_int(1);
    }
  else
    {
      x = from_int(1);
      y = divide(negate(a),b);
    }
}

ring_elem RRR::eval(const RingMap *map, const ring_elem f, int) const
{
  ring_elem result;
  map->get_ring()->from_BigReal(MPF_VAL(f), result);
  return result;
}

ring_elem RRR::zeroize_tiny(gmp_RR epsilon, const ring_elem f) const
{
  mpfr_ptr f1 = MPF_VAL(f);
  if (mpfr_cmpabs(f1,epsilon) < 0)
    return zero();
  return f;
}
void RRR::increase_maxnorm(gmp_RR norm, const ring_elem f) const
  // If any real number appearing in f has larger absolute value than norm, replace norm.
{
  mpfr_ptr f1 = MPF_VAL(f);
  if (mpfr_cmpabs(norm,f1) < 0)
    mpfr_abs(norm,f1,GMP_RNDN);
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
