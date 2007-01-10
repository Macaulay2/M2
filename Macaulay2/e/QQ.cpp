
// Copyright 1995 Michael E. Stillman

#include "QQ.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "gbring.hpp"
#include "../d/M2mem.h"

bool QQ::initialize_QQ() 
{
  initialize_ring(0);
  _elem_size = sizeof(mpq_t);
  _zero_elem = new_elem();// this sets the element to 0.
#if 0
//   trans_one = globalZZ->from_int(1);
#endif
  declare_field();
  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  return true;
}

QQ *QQ::create()
{
  QQ *result = new QQ;
  result->initialize_QQ();
  return result;
}

void QQ::text_out(buffer &o) const
{
  o << "QQ";
}

M2_Rational QQ::new_elem() const
{
  M2_Rational result = reinterpret_cast<M2_Rational>(getmem(_elem_size));
  mpq_init(result);
  return result;
}

void QQ::remove_elem(M2_Rational f) const
{
}

int QQ::coerce_to_int(ring_elem a) const
{ 
  return mpz_get_si(mpq_numref(MPQ_VAL(a)));
}

ring_elem QQ::random() const
{
  M2_Rational result = QQ::new_elem();
  Random::random_integer(mpq_numref(result));
  do {
    Random::random_integer(mpq_denref(result));
  } while (!mpz_cmp_si(mpq_denref(result),0));
  mpq_canonicalize(result);
  return MPQ_RINGELEM(result);
}

void QQ::elem_text_out(buffer &o, const ring_elem ap) const
{
  M2_Rational a = MPQ_VAL(ap);

  char s[1000];
  char *str;

  bool is_neg = (mpq_sgn(a) == -1);
  bool is_one = (mask_mpq_cmp_si(a, 1, 1) == 0 || mask_mpq_cmp_si(a, -1, 1) == 0);

  int size = mpz_sizeinbase (mpq_numref(a), 10)
    + mpz_sizeinbase (mpq_denref(a), 10) + 3;

  char *allocstr = (size > 1000 ? newarray_atomic(char,size) : s);

  if (!is_neg && p_plus) o << '+';
  if (is_one) 
    {  
      if (is_neg) o << '-';
      if (p_one) o << '1'; 
    }
  else
    {
      str = mpq_get_str(allocstr, 10, a);
      o << str;
    }
  if (size > 1000) deletearray(allocstr);
}

ring_elem QQ::numerator(ring_elem q) const
{
  return globalZZ->from_int(mpq_numref(MPQ_VAL(q)));
}

ring_elem QQ::denominator(ring_elem q) const
{
  return globalZZ->from_int(mpq_denref(MPQ_VAL(q)));
}

ring_elem QQ::fraction(ring_elem top, ring_elem bottom) const
{
  M2_Rational result = QQ::new_elem();
  mpz_set(mpq_numref(result),top.get_mpz());
  mpz_set(mpq_denref(result),bottom.get_mpz());
  mpq_canonicalize(result);
  return MPQ_RINGELEM(result);
}

ring_elem QQ::from_int(int n) const
{
  M2_Rational result = QQ::new_elem();
  mpq_set_si(result, n, 1);

  return MPQ_RINGELEM(result);
}

ring_elem QQ::from_int(mpz_ptr n) const
{
  M2_Rational result = QQ::new_elem();
  mpz_set(mpq_numref(result), n); // denominator is still 1.

  return MPQ_RINGELEM(result);
}

ring_elem QQ::from_rational(mpq_ptr a) const
{
  M2_Rational result = QQ::new_elem();
  mpq_set(result, a);
  return MPQ_RINGELEM(result);
}

bool QQ::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  // Rf = ZZ ---> QQ
  if (Rf->is_ZZ())
    {
      result = QQ::from_int(f.get_mpz());
      return true;
    }
  return false;
}

bool QQ::lift(const Ring *Rg, const ring_elem f, ring_elem &result) const
{
  // Rg = ZZ ---> QQ
  // f is an element of QQ

  if (Rg->is_ZZ())
    {
      M2_Rational h = MPQ_VAL(f);
      if (mask_mpz_cmp_si(mpq_denref(h),1) == 0)
	{
	  result = globalZZ->RingZZ::from_int(mpq_numref(h));
	  return true;
	}
    }
  return false;
}

bool QQ::is_unit(const ring_elem f) const
{
  M2_Rational a = MPQ_VAL(f);
  return mpq_sgn(a) != 0;
}

bool QQ::is_zero(const ring_elem f) const
{
  M2_Rational a = MPQ_VAL(f);
  return mpq_sgn(a) == 0;
}

bool QQ::is_equal(const ring_elem f, const ring_elem g) const
{
  M2_Rational a = MPQ_VAL(f);
  M2_Rational b = MPQ_VAL(g);

  return mpq_equal(a, b);
}
int QQ::compare_elems(const ring_elem f, const ring_elem g) const
{
  M2_Rational a = MPQ_VAL(f);
  M2_Rational b = MPQ_VAL(g);

  int cmp = mpq_cmp(a,b);
  if (cmp > 0) return 1;
  if (cmp == 0) return 0;
  return -1;
}
int QQ::is_positive(const ring_elem f) const
{
  M2_Rational a = MPQ_VAL(f);
  return mpq_sgn(a) > 0;
}

ring_elem QQ::copy(const ring_elem f) const
{
  return QQ::from_rational(MPQ_VAL(f));
}

void QQ::remove(ring_elem &f) const
{
#if 0
//   f = MPQ_RINGELEM(NULL);
#endif
}

ring_elem QQ::preferred_associate(ring_elem f) const
{
  M2_Rational a = MPQ_VAL(f);
  if (mpq_sgn(a) >= 0)
    return QQ::from_int(1);
  return QQ::from_int(-1);
}

bool QQ::lower_associate_divisor(ring_elem &f, const ring_elem g) const
{
  M2_Rational a = MPQ_VAL(f);
  M2_Rational b = MPQ_VAL(g);
  int sa = mpq_sgn(a);
  int sb = mpq_sgn(b);
  int s = (sa == 0 ? sb : sa);
  M2_Rational result = QQ::new_elem();

  mpz_gcd(mpq_numref(result), mpq_numref(a), mpq_numref(b));
  mpz_lcm(mpq_denref(result), mpq_denref(a), mpq_denref(b));
  if (s != mpq_sgn(result))
    mpq_neg(result,result);
  f = MPQ_RINGELEM(result);
  return true; // the answer could become lower, if a newer g has a larger denom
}

void QQ::internal_negate_to(ring_elem &f) const
{
  mpq_sub(MPQ_VAL(f), _zero_elem, MPQ_VAL(f));
}

void QQ::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpq_add(MPQ_VAL(f), MPQ_VAL(f), MPQ_VAL(g));
  remove(g);
}

void QQ::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpq_sub(MPQ_VAL(f), MPQ_VAL(f), MPQ_VAL(g));
  remove(g);
}

ring_elem QQ::negate(const ring_elem f) const
{
  M2_Rational result = QQ::new_elem();
  mpq_sub(result, _zero_elem, MPQ_VAL(f));
  return MPQ_RINGELEM(result);
}

ring_elem QQ::add(const ring_elem f, const ring_elem g) const
{
  M2_Rational result = QQ::new_elem();
  mpq_add(result, MPQ_VAL(f), MPQ_VAL(g));
  return MPQ_RINGELEM(result);
}

ring_elem QQ::subtract(const ring_elem f, const ring_elem g) const
{
  M2_Rational result = QQ::new_elem();
  mpq_sub(result, MPQ_VAL(f), MPQ_VAL(g));
  return MPQ_RINGELEM(result);
}

ring_elem QQ::mult(const ring_elem f, const ring_elem g) const
{
  M2_Rational result = QQ::new_elem();
  mpq_mul(result, MPQ_VAL(f), MPQ_VAL(g));
  return MPQ_RINGELEM(result);
}

ring_elem QQ::power(const ring_elem f, int n) const
{
  M2_Rational result = QQ::new_elem();
  if (n > 0)
    {
      mpz_pow_ui(mpq_numref(result), mpq_numref(MPQ_VAL(f)), n);
      mpz_pow_ui(mpq_denref(result), mpq_denref(MPQ_VAL(f)), n);
    }
  else if (n < 0)
    {
      mpz_pow_ui(mpq_numref(result), mpq_denref(MPQ_VAL(f)), -n);
      mpz_pow_ui(mpq_denref(result), mpq_numref(MPQ_VAL(f)), -n);
      if (mpz_sgn(mpq_denref(result)) < 0)
	{
	  mpz_neg(mpq_numref(result), mpq_numref(result));
	  mpz_neg(mpq_denref(result), mpq_denref(result));
	}
      else if (mpz_sgn(mpq_denref(result)) == 0)
	{
	  ERROR("attempted to divide by zero");
	  mpz_set_si(mpq_denref(result), 1);
	}
    }
  else 
    mpq_set_si(result, 1, 1);
  return MPQ_RINGELEM(result);
}

ring_elem QQ::power(const ring_elem f, mpz_t n) const
{
  int n1;
  if (!RingZZ::get_si(n1, n)) 
    { 
      ERROR("exponent too large");
      return QQ::from_int(1);
    }
  else
    return QQ::power(f,n1);
}

ring_elem QQ::invert(const ring_elem f) const
{
  if (is_zero(f))
    return QQ::from_int(0);
  else
    {
      M2_Rational result = QQ::new_elem();
      mpq_inv(result, MPQ_VAL(f));
      return MPQ_RINGELEM(result);
    }
}

ring_elem QQ::divide(const ring_elem f, const ring_elem g) const
{
  M2_Rational result = QQ::new_elem();
  mpq_div(result, MPQ_VAL(f), MPQ_VAL(g));
  return MPQ_RINGELEM(result);
}

void QQ::syzygy(const ring_elem a, const ring_elem b,
	       ring_elem &x, ring_elem &y) const
{
  x = QQ::from_int(1);
  y = QQ::divide(a,b);
  QQ::internal_negate_to(y);
}

ring_elem QQ::eval(const RingMap *map, const ring_elem a, int) const
{
  const Ring *S = map->get_ring();
  return S->from_rational(MPQ_VAL(a));
#if 0
//   const PolynomialRing *SP = S->cast_to_PolynomialRing();
//   M2_Rational f = MPQ_VAL(a);
//   if (S == globalQQ || (SP != 0 && SP->getCoefficients() == globalQQ))
//     {
//       return S->from_QQ(a);
//     }
//   ring_elem top = globalZZ->eval(map, MPZ_RINGELEM(mpq_numref(f)));
//   if (S->is_zero(top)) return top;
//   ring_elem bottom = globalZZ->eval(map, MPZ_RINGELEM(mpq_denref(f)));
//   if (S->is_zero(bottom))
//     {
//       ERROR("division by zero!");
//       S->remove(bottom);
//       bottom = S->from_int(1);
//     }
//   ring_elem result = S->divide(top, bottom);
//   S->remove(top);
//   S->remove(bottom);
//   return result;
#endif
}

#if 0
// ///////////////////////////////////
// // translation gbvector <--> vec //
// ///////////////////////////////////
// ring_elem QQ::trans_to_ringelem(ring_elem coeff, 
// 				const int *exp) const
// {
//   ring_elem a = globalZZ->trans_to_ringelem(coeff,exp);
//   return this->fraction(a, trans_one);
// }
// 
// ring_elem QQ::trans_to_ringelem_denom(ring_elem coeff, 
// 				      ring_elem denom, 
// 				      int *exp) const
// {
//   ring_elem a = globalZZ->trans_to_ringelem(coeff,exp);
//   return this->fraction(a, denom);
// }
// 
// void QQ::trans_from_ringelem(gbvectorHeap &H, 
// 			     ring_elem coeff, 
// 			     int comp, 
// 			     int *exp,
// 			     int firstvar) const
// {
//   ring_elem a = this->numerator(coeff);
//   globalZZ->trans_from_ringelem(H, a, comp, exp, firstvar);
// }
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:

