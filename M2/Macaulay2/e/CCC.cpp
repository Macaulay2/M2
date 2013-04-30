// Copyright 1995 Michael E. Stillman

#include "rand.h"
#include "ZZ.hpp"
#include "RRR.hpp"
#include "CCC.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "gbring.hpp"

bool CCC::initialize_CCC(unsigned long prec)
{
  initialize_ring(0);
  declare_field();
  precision = prec;
  _elem_size = static_cast<int>(sizeofstructtype(gmp_CC));
  _zero_elem = new_elem();

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  coeffR = new CoefficientRingCCC(this);
  return true;
}

CCC *CCC::create(unsigned long prec)
{
  CCC *result = new CCC;
  result->initialize_CCC(prec);
  return result;
}

void CCC::text_out(buffer &o) const
{
  o << "CCC_" << precision;
}

gmp_CC CCC::new_elem() const
{
  gmp_CC result = getmemstructtype(gmp_CC);
  result->re = getmemstructtype(gmp_RR);
  result->im = getmemstructtype(gmp_RR);
  mpfr_init2(result->re,precision);
  mpfr_init2(result->im,precision);
  return result;
}
void CCC::remove_elem(gmp_CC f) const
{
  // mpfr_clear(f);
}

ring_elem CCC::random() const
{
  gmp_CC result = rawRandomCC(precision);
  return BIGCC_RINGELEM(result);
}

void CCC::elem_text_out(buffer &o,
                        const ring_elem ap,
                        bool p_one,
                        bool p_plus,
                        bool p_parens) const
{
  M2_string s = (p_parens ? (*gmp_tonetCCparenpointer)(BIGCC_VAL(ap))
                 : (*gmp_tonetCCpointer)(BIGCC_VAL(ap)));

  // if: first char is a "-", and p_plus, o << "+"
  // if: an internal "+" or "-", then put parens around it.
  //   otherwise: if the string is "1" or "-1", and !p_one
  //              then leave out the last character.

  bool prepend_plus = p_plus && (s->array[0] != '-');
  bool strip_last = !p_one && (
                              (s->len == 1 && s->array[0] == '1')
                              || (s->len == 2 && s->array[1] == '1' && s->array[0] == '-'));

  if (prepend_plus) o << "+";
  if (strip_last)
    o.put(s->array, s->len-1);
  else
    o.put(s->array, s->len);
}

ring_elem CCC::from_int(int n) const
{
  gmp_CC result = new_elem();
  mpfr_set_si(result->re, n, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_int(mpz_ptr n) const
{
  gmp_CC result = new_elem();
  mpfr_set_z(result->re, n, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_rational(mpq_ptr r) const
{
  gmp_CC result = new_elem();
  mpfr_set_q(result->re, r, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

bool CCC::from_BigReal(gmp_RR r, ring_elem &result1) const
{
  gmp_CC result = new_elem();
  mpfr_set(result->re, r, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  result1 = BIGCC_RINGELEM(result);
  return true;
}

ring_elem CCC::from_BigReals(mpfr_ptr a, mpfr_ptr b) const
{
  gmp_CC result = new_elem();
  mpfr_set(result->re, a, GMP_RNDN);
  mpfr_set(result->im, b, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

bool CCC::from_BigComplex(gmp_CC z, ring_elem &result1) const
{
  gmp_CC result = new_elem();
  mpfr_set(result->re, z->re, GMP_RNDN);
  mpfr_set(result->im, z->im, GMP_RNDN);

  result1 = BIGCC_RINGELEM(result);
  return true;
}

mpfr_ptr CCC::to_BigReal(ring_elem f) const
{
  return BIGCC_RE(f);
}

bool CCC::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  if (Rf->is_ZZ())
    {
      gmp_CC g = new_elem();
      mpfr_set_z(g->re, f.get_mpz(), GMP_RNDN);
      mpfr_set_si(g->im, 0, GMP_RNDN);
      result = BIGCC_RINGELEM(g);
      return true;
    }
  if (Rf->is_RRR())
    {
      gmp_CC g = new_elem();
      mpfr_set(g->re, MPF_VAL(f), GMP_RNDN);
      mpfr_set_si(g->im, 0, GMP_RNDN);
      result = BIGCC_RINGELEM(g);
      return true;
    }
  if (Rf->is_CCC())
    {
      gmp_CC g = new_elem();
      mpfr_set(g->re, BIGCC_RE(f), GMP_RNDN);
      mpfr_set(g->im, BIGCC_IM(f), GMP_RNDN);
      result = BIGCC_RINGELEM(g);
      return true;
    }
  return false;
}

bool CCC::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  return false;
}

bool CCC::is_unit(const ring_elem f) const
{
  return !is_zero(f);
}

bool CCC::is_zero(const ring_elem f) const
{
  mpfr_ptr a = BIGCC_RE(f);
  mpfr_ptr b = BIGCC_IM(f);
  return (mpfr_zero_p(a) != 0 && mpfr_zero_p(b) != 0);
}

bool CCC::is_equal(const ring_elem f, const ring_elem g) const
{
  return mpfr_equal_p(BIGCC_RE(f), BIGCC_RE(g)) && mpfr_equal_p(BIGCC_IM(f), BIGCC_IM(g));
}

int CCC::compare_elems(const ring_elem f, const ring_elem g) const
{
  int cmp = mpfr_cmp(BIGCC_RE(f), BIGCC_RE(g));
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  cmp = mpfr_cmp(BIGCC_IM(f),BIGCC_IM(g));
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  return 0;
}

bool CCC::is_real(const ring_elem f) const
{
  mpfr_ptr im = BIGCC_IM(f);
  return mpfr_sgn(im) == 0;
}

ring_elem CCC::absolute(const ring_elem f) const
{
  gmp_CC result = new_elem();

  mpfr_pow_ui(result->re, BIGCC_RE(f), 2, GMP_RNDN);
  mpfr_pow_ui(result->im, BIGCC_IM(f), 2, GMP_RNDN);
  mpfr_add(result->re, result->re, result->im, GMP_RNDN);
  mpfr_sqrt(result->re, result->re, GMP_RNDN);
  mpfr_set_ui(result->im, 0, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::copy(const ring_elem f) const
{
  mpfr_ptr a = BIGCC_RE(f);
  mpfr_ptr b = BIGCC_IM(f);

  gmp_CC result = new_elem();
  mpfr_set(result->re, a, GMP_RNDN);
  mpfr_set(result->im, b, GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

void CCC::remove(ring_elem &f) const
{
#if 0
//   gmp_CC z = BIGCC_VAL(f);
//   remove_elem(z); // does nothing... get rid of this code?
//   f = BIGCC_RINGELEM(NULL);
#endif
}

// TO DO: MAKE IT SAME AS CC
ring_elem CCC::preferred_associate(ring_elem f) const
{
  return from_int(1);
}

void CCC::internal_negate_to(ring_elem &f) const
{
  mpfr_neg(BIGCC_RE(f), BIGCC_RE(f), GMP_RNDN);
  mpfr_neg(BIGCC_IM(f), BIGCC_IM(f), GMP_RNDN);
}

void CCC::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpfr_add(BIGCC_RE(f), BIGCC_RE(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_add(BIGCC_IM(f), BIGCC_IM(f), BIGCC_IM(g), GMP_RNDN);
  // remove(g); should this be removed?
}

void CCC::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpfr_sub(BIGCC_RE(f), BIGCC_RE(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_sub(BIGCC_IM(f), BIGCC_IM(f), BIGCC_IM(g), GMP_RNDN);
  // remove(g); should g be removed?
}

ring_elem CCC::negate(const ring_elem f) const
{
  gmp_CC result = new_elem();
  mpfr_neg(result->re, BIGCC_RE(f), GMP_RNDN);
  mpfr_neg(result->im, BIGCC_IM(f), GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::add(const ring_elem f, const ring_elem g) const
{
  gmp_CC result = new_elem();
  mpfr_add(result->re, BIGCC_RE(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_add(result->im, BIGCC_IM(f), BIGCC_IM(g), GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::subtract(const ring_elem f, const ring_elem g) const
{
  gmp_CC result = new_elem();
  mpfr_sub(result->re, BIGCC_RE(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_sub(result->im, BIGCC_IM(f), BIGCC_IM(g), GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::mult(const ring_elem f, const ring_elem g) const
{
  gmp_CC result = new_elem();
  gmp_CC tmp = new_elem();
  mpfr_mul(result->re, BIGCC_RE(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_mul(tmp->re, BIGCC_IM(f), BIGCC_IM(g), GMP_RNDN);
  mpfr_sub(result->re, result->re, tmp->re, GMP_RNDN);
  mpfr_mul(result->im, BIGCC_RE(f), BIGCC_IM(g), GMP_RNDN);
  mpfr_mul(tmp->im, BIGCC_IM(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_add(result->im, result->im, tmp->im, GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::power(const ring_elem f, int n) const
{
  ring_elem curr_pow;
  ring_elem result = from_int(1);
  if (n == 0)
    {
      return result;
    }
  else if (n < 0)
    {
      n = -n;
      curr_pow = invert(f);
    }
  else
    {
      curr_pow = copy(f);
    }

  while (n > 0)
    {
      if (n%2) {
        result = CCC::mult(result, curr_pow);
      }
      n = n/2;
      curr_pow = CCC::mult(curr_pow, curr_pow);
    }
  return result;
}

ring_elem CCC::power(const ring_elem f, mpz_t n) const
{
  int n1;
  if (!RingZZ::get_si(n1, n))
    {
      ERROR("exponent too large");
      return from_int(1);
    }
  return power(f, n1);
}

ring_elem CCC::invert(const ring_elem f) const
{
  if (is_zero(f))
    return from_int(0);
  else {
    gmp_CC result = new_elem();
    gmp_CC tmp = new_elem();
    mpfr_mul(tmp->re, BIGCC_RE(f), BIGCC_RE(f), GMP_RNDN);
    mpfr_mul(tmp->im, BIGCC_IM(f), BIGCC_IM(f), GMP_RNDN);
    mpfr_add(tmp->re, tmp->re, tmp->im, GMP_RNDN);
    mpfr_div(result->re, BIGCC_RE(f), tmp->re, GMP_RNDN);
    mpfr_div(result->im, BIGCC_IM(f), tmp->re, GMP_RNDN);
    mpfr_neg(result->im, result->im, GMP_RNDN);
    return BIGCC_RINGELEM(result);
  }
}

ring_elem CCC::divide(const ring_elem f, const ring_elem g) const
{
  ring_elem h = CCC::invert(g);
  return CCC::mult(f, h);
}

void CCC::syzygy(const ring_elem a, const ring_elem b,
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

ring_elem CCC::eval(const RingMap *map, const ring_elem f, int) const
{
  ring_elem result;
  map->get_ring()->from_BigComplex(BIGCC_VAL(f), result);
  return result;
}

ring_elem CCC::zeroize_tiny(gmp_RR epsilon, const ring_elem f) const
{
  // This is the one that needs to be rewritten: 14 Jan 2008
  mpfr_ptr f1 = BIGCC_RE(f);
  bool re_is_zero = (mpfr_cmpabs(f1,epsilon) < 0);
  mpfr_ptr f2 = BIGCC_IM(f);
  bool im_is_zero = (mpfr_cmpabs(f2,epsilon) < 0);

  if (!re_is_zero && !im_is_zero)
    return f;

  gmp_CC result = new_elem();
  if (re_is_zero)
    mpfr_set_si(result->re, 0, GMP_RNDN);
  else
    mpfr_set(result->re,f1,GMP_RNDN);

  if (im_is_zero)
    mpfr_set_si(result->im, 0, GMP_RNDN);
  else
    mpfr_set(result->im,f2,GMP_RNDN);

  return BIGCC_RINGELEM(result);
}
void CCC::increase_maxnorm(gmp_RR norm, const ring_elem f) const
  // If any real number appearing in f has larger absolute value than norm, replace norm.
{
  mpfr_t f1;
  mpfr_init2(f1,precision);
  mpfc_abs(f1, BIGCC_VAL(f));
  if (mpfr_cmpabs(norm,f1) < 0)
    mpfr_set(norm,f1,GMP_RNDN);
  mpfr_clear(f1);
}



// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
