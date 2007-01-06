// Copyright 1995 Michael E. Stillman

#include "ZZ.hpp"
#include "text_io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "gbring.hpp"
#include "../d/M2mem.h"

#include "coeffrings.hpp"

#if 0
#include "gmp.h"
#define MPZ_VAL(f) (mpz_ptr ((f).poly_val))
#define MPZ_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))
#endif

bool RingZZ::initialize_ZZ(const PolynomialRing *deg_ring) 
{
  initialize_ring(0);
  _elem_size = sizeof(mpz_t);
  _zero_elem = new_elem();
  mpz_init_set_si(_zero_elem, 0);

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  degree_ring = deg_ring;
  coeffR = new CoefficientRingZZ_NTL(this);
  return true;
}

void RingZZ::text_out(buffer &o) const
{
  o << "ZZ";
}

mpz_ptr RingZZ::new_elem() const
{
  mpz_ptr result = reinterpret_cast<mpz_ptr>(getmem(_elem_size));
  mpz_init(result);
  return result;
}
void RingZZ::remove_elem(mpz_ptr f) const
{
}

bool RingZZ::get_ui(unsigned int &result, mpz_t n)
{
  result = mpz_get_ui(n);
  return mpz_fits_ulong_p(n);
}

bool RingZZ::get_si(int &result, mpz_t n)
{
  result = mpz_get_si(n);
  return mpz_fits_slong_p(n);
}

unsigned int RingZZ::mod_ui(mpz_t n, unsigned int p)
{
  mpz_t ans;
  mpz_init(ans);
  unsigned int exp = mpz_mod_ui(ans, n, p);
  mpz_clear(ans);
  return exp;
}

int RingZZ::coerce_to_int(ring_elem a) const
{ 
  return mpz_get_si(MPZ_VAL(a)); 
}

ring_elem RingZZ::random() const
{
  M2_Integer result = RingZZ::new_elem();
  Random::random_integer(result);
  return MPZ_RINGELEM(result);
}

void RingZZ::elem_text_out(buffer &o, const ring_elem ap) const
{
  mpz_ptr a = MPZ_VAL(ap);

  char s[1000];
  char *str;

  bool is_neg = (mask_mpz_cmp_si(a, 0) == -1);
  bool is_one = (mask_mpz_cmp_si(a, 1) == 0 || mask_mpz_cmp_si(a, -1) == 0);

  int size = mpz_sizeinbase(a, 10) + 2;

  char *allocstr = (size > 1000 ? newarray_atomic(char,size) : s);

  if (!is_neg && p_plus) o << '+';
  if (is_one) 
    {  
      if (is_neg) o << '-';
      if (p_one) o << '1'; 
    }
  else
    {
      str = mpz_get_str(allocstr, 10, a);
      o << str;
    }
  if (size > 1000) deletearray(allocstr);
}

ring_elem RingZZ::from_int(int n) const
{
  mpz_ptr result = new_elem();
  mpz_set_si(result, n);

  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::from_int(mpz_ptr n) const
{
  mpz_ptr result = new_elem();
  mpz_set(result, n);

  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::from_rational(mpq_ptr q) const
{
  return RingZZ::from_int(mpq_numref(q));
}

bool RingZZ::promote(const Ring *R, const ring_elem a, ring_elem &result) const
{
  return false;
}

bool RingZZ::lift(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool RingZZ::is_unit(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  return (mask_mpz_cmp_si(a, 1)==0 ||
	  mask_mpz_cmp_si(a, -1)==0);
}

bool RingZZ::is_zero(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  return mpz_sgn(a) == 0;
}

bool RingZZ::is_equal(const ring_elem f, const ring_elem g) const
{
  mpz_ptr a = MPZ_VAL(f);
  mpz_ptr b = MPZ_VAL(g);

  return mpz_cmp(a, b) == 0;
}
int RingZZ::compare_elems(const ring_elem f, const ring_elem g) const
{
  mpz_ptr a = MPZ_VAL(f);
  mpz_ptr b = MPZ_VAL(g);
  int cmp = mpz_cmp(a,b);
  if (cmp > 0) return 1;
  if (cmp == 0) return 0;
  return -1;
}
int RingZZ::is_positive(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  return mpz_sgn(a) > 0;
}

ring_elem RingZZ::copy(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);

  mpz_ptr result = new_elem();
  mpz_set(result, a);
  return MPZ_RINGELEM(result);
}

void RingZZ::remove(ring_elem &f) const
{
#if 0
  mpz_ptr a = MPZ_VAL(f);
  remove_elem(a);
  f = MPZ_RINGELEM(NULL);
#endif
}

ring_elem RingZZ::preferred_associate(ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  if (mpz_sgn(a) >= 0)
    return from_int(1);
  return from_int(-1);
}

bool RingZZ::lower_associate_divisor(ring_elem &f, const ring_elem g) const
{
  // if f is 0, do f=sign(g), else f=sign(f)
  // return whether f is zero
  M2_Integer result = RingZZ::new_elem();
  mpz_ptr a = MPZ_VAL(f);
  mpz_ptr b = MPZ_VAL(g);
  int sa = mpz_sgn(a);
  int sb = mpz_sgn(b);
  int s = (sa == 0 ? sb : sa);

  mpz_set_si(result,s);
  f = MPZ_RINGELEM(result);
  return !RingZZ::is_zero(f);
}

void RingZZ::internal_negate_to(ring_elem &f) const
{
  mpz_sub(MPZ_VAL(f), _zero_elem, MPZ_VAL(f));
}

void RingZZ::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpz_add(MPZ_VAL(f), MPZ_VAL(f), MPZ_VAL(g));
  remove(g);
}

void RingZZ::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpz_sub(MPZ_VAL(f), MPZ_VAL(f), MPZ_VAL(g));
  remove(g);
}

ring_elem RingZZ::negate(const ring_elem f) const
{
  mpz_ptr result = new_elem();
  mpz_sub(result, _zero_elem, MPZ_VAL(f));
  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::add(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_add(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::subtract(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_sub(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::mult(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_mul(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::power(const ring_elem f, int n) const
{
  mpz_ptr result = new_elem();
  mpz_pow_ui(result, MPZ_VAL(f), n);
  return MPZ_RINGELEM(result);
}
ring_elem RingZZ::power(const ring_elem f, mpz_t n) const
{
  mpz_ptr result = new_elem();
  int n1;
  if (!get_si(n1, n)) 
    { ERROR("exponent too large"); }
  else
    mpz_pow_ui(result, MPZ_VAL(f), n1);
  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::invert(const ring_elem f) const
{
  if (is_unit(f))
    return copy(f);
  else
    return from_int(0);
}

ring_elem RingZZ::divide(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_div(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				  ring_elem &quot) const
{
  mpz_ptr q = new_elem();
  mpz_ptr r = new_elem();
  int gsign = mpz_sgn(MPZ_VAL(g));
  mpz_t gg, ghalf;
  mpz_init(gg);
  mpz_init(ghalf);
  mpz_abs(gg,MPZ_VAL(g));
  mpz_fdiv_qr(q, r, MPZ_VAL(f), gg);
  mpz_tdiv_q_2exp(ghalf, gg, 1);
  if (mpz_cmp(r,ghalf) > 0)  // r > ghalf
    {
      mpz_sub(r,r,gg);
      mpz_add_ui(q,q,1);
    }
  if (gsign < 0)
    mpz_neg(q,q);

  mpz_clear(gg);
  mpz_clear(ghalf);
  quot = MPZ_RINGELEM(q);
  return MPZ_RINGELEM(r);
}

ring_elem RingZZ::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem = RingZZ::remainderAndQuotient(f,g,quot);
  remove(quot);
  return rem;
}

ring_elem RingZZ::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem = RingZZ::remainderAndQuotient(f,g,quot);
  remove(rem);
  return quot;
}

ring_elem RingZZ::gcd(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_gcd(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem RingZZ::gcd_extended(const ring_elem f, const ring_elem g, 
			    ring_elem &u, ring_elem &v) const
{
  mpz_ptr result = new_elem();
  mpz_ptr u1 = new_elem();
  mpz_ptr v1 = new_elem();
  mpz_gcdext(result, u1, v1, MPZ_VAL(f), MPZ_VAL(g));
  u = MPZ_RINGELEM(u1);
  v = MPZ_RINGELEM(v1);
  return MPZ_RINGELEM(result);
}

void RingZZ::syzygy(const ring_elem a, const ring_elem b,
	       ring_elem &x, ring_elem &y) const
{
  // First check the special cases a = 0, b = 1, -1.  Other cases: use gcd.
  if (RingZZ::is_zero(a))
    {
      x = RingZZ::from_int(1);
      y = RingZZ::from_int(0);
      return;
    }
  mpz_ptr bb = MPZ_VAL(b);
  if (mpz_cmp_ui(bb,1) == 0)
    {
      x = RingZZ::from_int(1);
      y = RingZZ::negate(a);
      return;
    }
  if (mask_mpz_cmp_si(bb,-1) == 0)
    {
      x = RingZZ::from_int(1);
      y = RingZZ::copy(a);
      return;
    }
  ring_elem g = RingZZ::gcd(a,b);
  y = RingZZ::divide(a,g);
  x = RingZZ::divide(b,g);
  RingZZ::remove(g);
  if (mpz_sgn(MPZ_VAL(x)) > 0)
    RingZZ::internal_negate_to(y);
  else
    RingZZ::internal_negate_to(x);
}

ring_elem RingZZ::eval(const RingMap *map, const ring_elem f,int) const
{
  return map->get_ring()->from_int(MPZ_VAL(f));
}




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
