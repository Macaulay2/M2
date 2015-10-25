// Copyright 1995 Michael E. Stillman

#include "ZZ.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "gbring.hpp"

#include "aring-zz-gmp.hpp"
#if 0
// #include "gmp.h"
// #define MPZ_VAL(f) (mpz_ptr ((f).poly_val))
// #define MPZ_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))
#endif

unsigned int computeHashValue_mpz(mpz_ptr a)
{
  return static_cast<unsigned int>(mpz_get_si(a));
}

bool RingZZ::initialize_ZZ(const PolynomialRing *deg_ring)
{
  initialize_ring(0);
  _elem_size = static_cast<int>(sizeof(mpz_t));
  _zero_elem = new_elem();
  mpz_init_set_si(_zero_elem, 0);

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);

  degree_ring = deg_ring;
  coeffR = new M2::ARingZZGMP;
  return true;
}

void RingZZ::text_out(buffer &o) const
{
  o << "ZZ";
}

mpz_ptr RingZZ::new_elem() const
{
  mpz_ptr result = getmemstructtype(mpz_ptr);
  mpz_init(result);
  return result;
}
void RingZZ::remove_elem(mpz_ptr f) const
{
}

unsigned int RingZZ::computeHashValue(const ring_elem a) const
{
  return computeHashValue_mpz(a.get_mpz());
}

bool RingZZ::get_ui(unsigned int &result, mpz_t n)
{
  result = static_cast<unsigned int>(mpz_get_ui(n));
  return mpz_fits_ulong_p(n);
}

bool RingZZ::get_si(int &result, mpz_t n)
{
  result = static_cast<int>(mpz_get_si(n));
  return mpz_fits_slong_p(n);
}

unsigned int RingZZ::mod_ui(mpz_t n, unsigned int p)
{
  mpz_t ans;
  mpz_init(ans);
  unsigned int exp = static_cast<unsigned int>(mpz_mod_ui(ans, n, p));
  mpz_clear(ans);
  return exp;
}

std::pair<bool, long> RingZZ::coerceToLongInteger(ring_elem a) const
{
  return std::pair<bool, long>(mpz_fits_slong_p(a.get_mpz()), 
                               mpz_get_si(a.get_mpz()));
}

ring_elem RingZZ::random() const
{
  return rawRandomInteger(0);
}

void RingZZ::elem_text_out(buffer &o,
                           const ring_elem ap,
                           bool p_one,
                           bool p_plus,
                           bool p_parens) const
{
  mpz_ptr a = ap.get_mpz();

  char s[1000];
  char *str;

  bool is_neg = (mask_mpz_cmp_si(a, 0) == -1);
  bool is_one = (mask_mpz_cmp_si(a, 1) == 0 || mask_mpz_cmp_si(a, -1) == 0);

  int size = static_cast<int>(mpz_sizeinbase(a, 10)) + 2;

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

ring_elem RingZZ::from_long(long n) const
{
  mpz_ptr result = new_elem();
  mpz_set_si(result, n);

  return ring_elem(result);
}

ring_elem RingZZ::from_int(mpz_ptr n) const
{
  mpz_ptr result = new_elem();
  mpz_set(result, n);

  return ring_elem(result);
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
  mpz_ptr a = f.get_mpz();
  return (mask_mpz_cmp_si(a, 1)==0 ||
          mask_mpz_cmp_si(a, -1)==0);
}

bool RingZZ::is_zero(const ring_elem f) const
{
  mpz_ptr a = f.get_mpz();
  return mpz_sgn(a) == 0;
}

bool RingZZ::is_equal(const ring_elem f, const ring_elem g) const
{
  mpz_ptr a = f.get_mpz();
  mpz_ptr b = g.get_mpz();

  return mpz_cmp(a, b) == 0;
}
int RingZZ::compare_elems(const ring_elem f, const ring_elem g) const
{
  mpz_ptr a = f.get_mpz();
  mpz_ptr b = g.get_mpz();
  int cmp = mpz_cmp(a,b);
  if (cmp > 0) return 1;
  if (cmp == 0) return 0;
  return -1;
}
int RingZZ::is_positive(const ring_elem f) const
{
  mpz_ptr a = f.get_mpz();
  return mpz_sgn(a) > 0;
}

ring_elem RingZZ::copy(const ring_elem f) const
{
  mpz_ptr a = f.get_mpz();

  mpz_ptr result = new_elem();
  mpz_set(result, a);
  return ring_elem(result);
}

void RingZZ::remove(ring_elem &f) const
{
#if 0
//   mpz_ptr a = f.get_mpz();
//   remove_elem(a);
//   f = MPZ_RINGELEM(NULL);
#endif
}

ring_elem RingZZ::preferred_associate(ring_elem f) const
{
  mpz_ptr a = f.get_mpz();
  if (mpz_sgn(a) >= 0)
    return from_long(1);
  return from_long(-1);
}

bool RingZZ::lower_associate_divisor(ring_elem &f, const ring_elem g) const
{
  // This sets f to either 0, 1 or -1.
  // if f is 0, do f=sign(g), else f=sign(f)
  // return whether f is zero
  gmp_ZZ result = RingZZ::new_elem();
  mpz_ptr a = f.get_mpz();
  mpz_ptr b = g.get_mpz();
  int sa = mpz_sgn(a);
  int sb = mpz_sgn(b);
  int s = (sa == 0 ? sb : sa);

  mpz_set_si(result,s);
  f = ring_elem(result);
  return !RingZZ::is_zero(f);
}

void RingZZ::lower_content(ring_elem &c, ring_elem g) const
// c is a content elem, g is in ring
{
  if (is_zero(c))
    {
      c = g;
      return;
    }
  gmp_ZZ result = RingZZ::new_elem();
  mpz_ptr a = c.get_mpz();
  mpz_ptr b = g.get_mpz();
  mpz_gcd(result,a,b);
  c = ring_elem(result);
}


void RingZZ::internal_negate_to(ring_elem &f) const
{
  mpz_sub(f.get_mpz(), _zero_elem, f.get_mpz());
}

void RingZZ::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpz_add(f.get_mpz(), f.get_mpz(), g.get_mpz());
  remove(g);
}

void RingZZ::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpz_sub(f.get_mpz(), f.get_mpz(), g.get_mpz());
  remove(g);
}

ring_elem RingZZ::negate(const ring_elem f) const
{
  mpz_ptr result = new_elem();
  mpz_sub(result, _zero_elem, f.get_mpz());
  return ring_elem(result);
}

ring_elem RingZZ::add(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_add(result, f.get_mpz(), g.get_mpz());
  return ring_elem(result);
}

ring_elem RingZZ::subtract(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_sub(result, f.get_mpz(), g.get_mpz());
  return ring_elem(result);
}

ring_elem RingZZ::mult(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_mul(result, f.get_mpz(), g.get_mpz());
  return ring_elem(result);
}

ring_elem RingZZ::power(const ring_elem f, int n) const
{
  mpz_ptr result = new_elem();
  mpz_pow_ui(result, f.get_mpz(), n);
  return ring_elem(result);
}
ring_elem RingZZ::power(const ring_elem f, mpz_t n) const
{
  mpz_ptr result = new_elem();
  int n1;
  if (!get_si(n1, n))
    { ERROR("exponent too large"); }
  else
    mpz_pow_ui(result, f.get_mpz(), n1);
  return ring_elem(result);
}

ring_elem RingZZ::invert(const ring_elem f) const
{
  if (is_unit(f))
    return copy(f);
  else
    return from_long(0);
}

ring_elem RingZZ::divide(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_fdiv_q(result, f.get_mpz(), g.get_mpz());
  return ring_elem(result);
}

ring_elem RingZZ::remainderAndQuotient(const ring_elem f, const ring_elem g,
                                  ring_elem &quot) const
{
  mpz_ptr q = new_elem();
  mpz_ptr r = new_elem();
  int gsign = mpz_sgn(g.get_mpz());
  mpz_t gg, ghalf;
  mpz_init(gg);
  mpz_init(ghalf);
  mpz_abs(gg,g.get_mpz());
  mpz_fdiv_qr(q, r, f.get_mpz(), gg);
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
  quot = ring_elem(q);
  return ring_elem(r);
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
  mpz_gcd(result, f.get_mpz(), g.get_mpz());
  return ring_elem(result);
}

ring_elem RingZZ::gcd_extended(const ring_elem f, const ring_elem g,
                            ring_elem &u, ring_elem &v) const
{
  mpz_ptr result = new_elem();
  mpz_ptr u1 = new_elem();
  mpz_ptr v1 = new_elem();
  mpz_gcdext(result, u1, v1, f.get_mpz(), g.get_mpz());
  u = ring_elem(u1);
  v = ring_elem(v1);
  return ring_elem(result);
}

void RingZZ::syzygy(const ring_elem a, const ring_elem b,
               ring_elem &x, ring_elem &y) const
{
  ASSERT(!is_zero(b));
  // First check the special cases a = 0, b = 1, -1.  Other cases: use gcd.
  if (RingZZ::is_zero(a))
    {
      x = RingZZ::from_long(1);
      y = RingZZ::from_long(0);
      return;
    }
  mpz_ptr bb = b.get_mpz();
  if (mpz_cmp_ui(bb,1) == 0)
    {
      x = RingZZ::from_long(1);
      y = RingZZ::negate(a);
      return;
    }
  if (mask_mpz_cmp_si(bb,-1) == 0)
    {
      x = RingZZ::from_long(1);
      y = RingZZ::copy(a);
      return;
    }
  ring_elem g = RingZZ::gcd(a,b);
  y = RingZZ::divide(a,g);
  x = RingZZ::divide(b,g);
  RingZZ::remove(g);
  if (mpz_sgn(x.get_mpz()) > 0)
    RingZZ::internal_negate_to(y);
  else
    RingZZ::internal_negate_to(x);
}

ring_elem RingZZ::eval(const RingMap *map, const ring_elem f,int) const
{
  return map->get_ring()->from_int(f.get_mpz());
}




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
