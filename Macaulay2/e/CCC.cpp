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
#include "../d/M2mem.h"

bool CCC::initialize_CCC(int prec) 
{
  initialize_ring(0);
  declare_field();
  precision = prec;
  _elem_size = sizeof(M2_CCC_struct);
  _zero_elem = new_elem();

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  coeffR = new CoefficientRingCCC(this);
  return true;
}

CCC *CCC::create(int prec)
{
  CCC *result = new CCC;
  result->initialize_CCC(prec);
  return result;
}

void CCC::text_out(buffer &o) const
{
  o << "CCC_" << precision;
}

M2_CCC CCC::new_elem() const
{
  M2_CCC result = reinterpret_cast<M2_CCC>(getmem(_elem_size));
  result->re = reinterpret_cast<mpfr_ptr>(getmem(sizeof(mpfr_t)));
  result->im = reinterpret_cast<mpfr_ptr>(getmem(sizeof(mpfr_t)));
  mpfr_init2(result->re,precision);
  mpfr_init2(result->im,precision);
  return result;
}
void CCC::remove_elem(M2_CCC f) const
{
  // mpfr_clear(f);
}

ring_elem CCC::random() const
{
  M2_CCC result = rawRandomCC(precision);
  return BIGCC_RINGELEM(result);
}

void CCC::elem_text_out(buffer &o, const ring_elem ap) const
{
  mpfr_ptr a = BIGCC_RE(ap);
  mpfr_ptr b = BIGCC_IM(ap);
  mp_exp_t expptr;

  // easier to call RRR::elem_text_out here ???

  // size_t size = 1000;
  //  char *s = newarray_atomic(char,1000);
  //  char *str;

  o << gmp_tostringCC(BIGCC_VAL(ap));
  return;
#if 0
  bool is_neg = (mpfr_cmp_si(a, 0) == -1);
  bool is_one = (mpfr_cmp_si(a, 1) == 0 || mpfr_cmp_si(a, -1) == 0);

  // int size = mpfr_sizeinbase(a, 10) + 2;

  // char *allocstr = (size > 1000 ? newarray(char,size) : s);

  if (!is_neg && p_plus) o << '+';
  if (is_one) 
    {  
      if (is_neg) o << '-';
      if (p_one) o << '1'; 
    }
  else
    {
      str = mpfr_get_str(s, &expptr, 10, 0, a, GMP_RNDN);
      o << "." << str << "*10^" << expptr;
    }

  is_neg = (mpfr_cmp_si(b, 0) == -1);
  is_one = (mpfr_cmp_si(b, 1) == 0 || mpfr_cmp_si(b, -1) == 0);

  str = mpfr_get_str(s, &expptr, 10, 0, b, GMP_RNDN);
  o << "+ (." << str << "*10^" << expptr << ") ii";

  // if (size > 1000) deletearray(allocstr);
#endif
}

ring_elem CCC::from_int(int n) const
{
  M2_CCC result = new_elem();
  mpfr_set_si(result->re, n, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_int(mpz_ptr n) const
{
  M2_CCC result = new_elem();
  mpfr_set_z(result->re, n, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_rational(mpq_ptr r) const
{
  M2_CCC result = new_elem();
  mpfr_set_q(result->re, r, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

bool CCC::from_BigReal(M2_RRR r, ring_elem &result1) const
{
  M2_CCC result = new_elem();
  mpfr_set(result->re, r, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);

  result1 = BIGCC_RINGELEM(result);
  return true;
}

ring_elem CCC::from_BigReals(mpfr_ptr a, mpfr_ptr b) const
{
  M2_CCC result = new_elem();
  mpfr_set(result->re, a, GMP_RNDN);
  mpfr_set(result->im, b, GMP_RNDN);

  return BIGCC_RINGELEM(result);
}

bool CCC::from_BigComplex(M2_CCC z, ring_elem &result1) const
{
  M2_CCC result = new_elem();
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
      M2_CCC g = new_elem();
      mpfr_set_z(g->re, f.get_mpz(), GMP_RNDN);
      mpfr_set_si(g->im, 0, GMP_RNDN);
      result = BIGCC_RINGELEM(g);
      return true;
    }
  if (Rf->is_RRR())
    {
      M2_CCC g = new_elem();
      mpfr_set(g->re, MPF_VAL(f), GMP_RNDN);
      mpfr_set_si(g->im, 0, GMP_RNDN);
      result = BIGCC_RINGELEM(g);
      return true;
    }
  if (Rf->is_CCC())
    {
      M2_CCC g = new_elem();
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
  return (mpfr_sgn(a) == 0 && mpfr_sgn(b) == 0);
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
  M2_CCC result = new_elem();

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

  M2_CCC result = new_elem();
  mpfr_set(result->re, a, GMP_RNDN);
  mpfr_set(result->im, b, GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

void CCC::remove(ring_elem &f) const
{
#if 0
//   M2_CCC z = BIGCC_VAL(f);
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
  M2_CCC result = new_elem();
  mpfr_neg(result->re, BIGCC_RE(f), GMP_RNDN);
  mpfr_neg(result->im, BIGCC_IM(f), GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::add(const ring_elem f, const ring_elem g) const
{
  M2_CCC result = new_elem();
  mpfr_add(result->re, BIGCC_RE(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_add(result->im, BIGCC_IM(f), BIGCC_IM(g), GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::subtract(const ring_elem f, const ring_elem g) const
{
  M2_CCC result = new_elem();
  mpfr_sub(result->re, BIGCC_RE(f), BIGCC_RE(g), GMP_RNDN);
  mpfr_sub(result->im, BIGCC_IM(f), BIGCC_IM(g), GMP_RNDN);
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::mult(const ring_elem f, const ring_elem g) const
{
  M2_CCC result = new_elem();
  M2_CCC tmp = new_elem();
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
    M2_CCC result = new_elem();
    M2_CCC tmp = new_elem();
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




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
