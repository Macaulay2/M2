// Copyright 1995 Michael E. Stillman

#include "rand.h"
#include "ZZ.hpp"
#include "QQ.hpp"
#include "RRR.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "gbring.hpp"
#include "../d/M2mem.h"

#if 0
// #include "gmp.h"
// #define MPF_VAL(f) (mpfr_ptr ((f).poly_val))
// #define MPF_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))
#endif

bool RRR::initialize_RRR(int prec) 
{
  initialize_ring(0);
  declare_field();
  precision = prec;
  _elem_size = sizeof(mpfr_t);
  _zero_elem = new_elem();

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  coeffR = new CoefficientRingRRR(this);
  return true;
}

RRR *RRR::create(int prec)
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
  mpfr_ptr result = reinterpret_cast<mpfr_ptr>(getmem(_elem_size));
  mpfr_init2(result,precision);
  return result;
}

void RRR::remove_elem(mpfr_ptr f) const
{
  // mpfr_clear(f);
}

ring_elem RRR::random() const
{
  M2_RRR result = rawRandomRR(precision);
  return MPF_RINGELEM(result);
}

void RRR::elem_text_out(buffer &o, const ring_elem ap) const
{
  mpfr_ptr a = MPF_VAL(ap);
  mp_exp_t expptr;

  // size_t size = 1000;
#warning this all looks wrong
  char *s = newarray_atomic(char,1000);
  char *str;

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
      o << gmp_tostringRR(a);
      //str = mpfr_get_str(s, &expptr, 10, 0, a, GMP_RNDN);
      //o << "." << str << "*10^" << expptr;
    }
  // if (size > 1000) deletearray(allocstr);
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

bool RRR::from_BigReal(M2_RRR r, ring_elem &result1) const
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
  return mpfr_sgn(a) == 0;
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
#if 0
//   mpfr_ptr a = MPF_VAL(f);
//   remove_elem(a);  // does nothing... remove this code?
//   f = MPF_RINGELEM(NULL);
#endif
}

ring_elem RRR::preferred_associate(ring_elem f) const
{
  mpfr_ptr a = MPF_VAL(f);
  if (mpfr_sgn(a) >= 0)
    return from_int(1);
  return from_int(-1);
}

void RRR::internal_negate_to(ring_elem &f) const
{
  mpfr_neg(MPF_VAL(f), MPF_VAL(f), GMP_RNDN);
}

void RRR::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpfr_add(MPF_VAL(f), MPF_VAL(f), MPF_VAL(g), GMP_RNDN);
  // remove(g); should this be removed?
}

void RRR::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpfr_sub(MPF_VAL(f), MPF_VAL(f), MPF_VAL(g), GMP_RNDN);
  // remove(g); should g be removed?
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
  mpfr_pow_ui(result, MPF_VAL(f), n, GMP_RNDN);
  return MPF_RINGELEM(result);
}
ring_elem RRR::power(const ring_elem f, mpz_t n) const
{
  mpfr_ptr result = new_elem();
  int n1;
  if (!RingZZ::get_si(n1, n)) 
    { ERROR("exponent too large"); }
  else
    mpfr_pow_ui(result, MPF_VAL(f), n1, GMP_RNDN);
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
    mpfr_ptr unit_elem = new_elem();
    mpfr_set_si(unit_elem, 1, GMP_RNDN);
    mpfr_div(result, unit_elem, MPF_VAL(f), GMP_RNDN);
    remove_elem(unit_elem);
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




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
