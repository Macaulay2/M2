// Copyright 1995 Michael E. Stillman

#include "ZZ.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "gbring.hpp"

#include "aring-zz-gmp.hpp"
#include <utility>
#include "error.h"

unsigned int computeHashValue_mpz(mpz_srcptr a)
{
  return static_cast<unsigned int>(mpz_get_si(a));
}

bool RingZZ::initialize_ZZ(const PolynomialRing *deg_ring)
{
  initialize_ring(0);

  zeroV = from_long(0);
  oneV = from_long(1);
  minus_oneV = from_long(-1);

  degree_ring = deg_ring;
  coeffR = new M2::ARingZZGMP;
  return true;
}

void RingZZ::text_out(buffer &o) const { o << "ZZ"; }

mpz_ptr RingZZ::new_elem() const
{
  mpz_ptr result = getmemstructtype(mpz_ptr);
  mpz_init(result);
  return result;
}

unsigned int RingZZ::computeHashValue(const ring_elem a) const
{
  return computeHashValue_mpz(a.get_mpz());
}

std::pair<bool, int> RingZZ::get_si(mpz_srcptr n)
{
  if (not mpz_fits_slong_p(n)) return std::make_pair<bool, int>(false, 0);
  long a = mpz_get_si(n);
  int b = static_cast<int>(a);
  if (a == b) return std::make_pair<bool, int>(true, std::move(b));
  return std::make_pair<bool, int>(false, 0);
}

unsigned int RingZZ::mod_ui(mpz_srcptr n, unsigned int p)
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

ring_elem RingZZ::random() const { return ring_elem(rawRandomInteger(0)); }

void RingZZ::elem_text_out(buffer &o,
                           const ring_elem ap,
                           bool p_one,
                           bool p_plus,
                           bool p_parens) const
{
  mpz_srcptr a = ap.get_mpz();

  char s[1000];
  char *str;

  bool is_neg = (mask_mpz_cmp_si(a, 0) == -1);
  bool is_one = (mask_mpz_cmp_si(a, 1) == 0 || mask_mpz_cmp_si(a, -1) == 0);

  int size = static_cast<int>(mpz_sizeinbase(a, 10)) + 2;

  char *allocstr = (size > 1000 ? newarray_atomic(char, size) : s);

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
  if (size > 1000) freemem(allocstr);
}

ring_elem RingZZ::from_long(long n) const
{
  mpz_ptr result = new_elem();
  mpz_set_si(result, n);
  mpz_reallocate_limbs(result);

  return ring_elem(result);
}

ring_elem RingZZ::from_int(mpz_srcptr n) const
{
  mpz_ptr result = new_elem();
  mpz_set(result, n);
  mpz_reallocate_limbs(result);
  
  return ring_elem(result);
}

bool RingZZ::from_rational(mpq_srcptr q, ring_elem &result) const
{
  bool ok = mpz_cmp_si(mpq_denref(q), 1) == 0;
  if (not ok) return false;
  result = RingZZ::from_int(mpq_numref(q));
  return true;
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
  mpz_srcptr a = f.get_mpz();
  return (mask_mpz_cmp_si(a, 1) == 0 || mask_mpz_cmp_si(a, -1) == 0);
}

bool RingZZ::is_zero(const ring_elem f) const
{
  mpz_srcptr a = f.get_mpz();
  return mpz_sgn(a) == 0;
}

bool RingZZ::is_equal(const ring_elem f, const ring_elem g) const
{
  mpz_srcptr a = f.get_mpz();
  mpz_srcptr b = g.get_mpz();

  return mpz_cmp(a, b) == 0;
}
int RingZZ::compare_elems(const ring_elem f, const ring_elem g) const
{
  mpz_srcptr a = f.get_mpz();
  mpz_srcptr b = g.get_mpz();
  int cmp = mpz_cmp(a, b);
  if (cmp > 0) return 1;
  if (cmp == 0) return 0;
  return -1;
}
int RingZZ::is_positive(const ring_elem f) const
{
  mpz_srcptr a = f.get_mpz();
  return mpz_sgn(a) > 0;
}

ring_elem RingZZ::copy(const ring_elem f) const
{
  mpz_srcptr a = f.get_mpz();

  mpz_ptr result = new_elem();
  mpz_set(result, a);
  mpz_reallocate_limbs(result);
  
  return ring_elem(result);
}

void RingZZ::remove(ring_elem &f) const
{
  // NOTHING
}

ring_elem RingZZ::preferred_associate(ring_elem f) const
{
  mpz_srcptr a = f.get_mpz();
  if (mpz_sgn(a) >= 0) return from_long(1);
  return from_long(-1);
}

bool RingZZ::lower_associate_divisor(ring_elem &f, const ring_elem g) const
{
  // This sets f to either 0, 1 or -1.
  // if f is 0, do f=sign(g), else f=sign(f)
  // return whether f is zero
  mpz_ptr result = RingZZ::new_elem();
  mpz_srcptr a = f.get_mpz();
  mpz_srcptr b = g.get_mpz();
  int sa = mpz_sgn(a);
  int sb = mpz_sgn(b);
  int s = (sa == 0 ? sb : sa);

  mpz_set_si(result, s);
  mpz_reallocate_limbs(result);  

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
  mpz_ptr result = RingZZ::new_elem();
  mpz_srcptr a = c.get_mpz();
  mpz_srcptr b = g.get_mpz();
  mpz_gcd(result, a, b);
  if(mpz_sgn(a) == -1)
    mpz_neg(result, result);
  mpz_reallocate_limbs(result);  
  c = ring_elem(result);
}

ring_elem RingZZ::negate(const ring_elem f) const
{
  mpz_ptr result = new_elem();
  mpz_neg(result, f.get_mpz());
  mpz_reallocate_limbs(result);
  return ring_elem(result);
}

ring_elem RingZZ::add(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_add(result, f.get_mpz(), g.get_mpz());
  mpz_reallocate_limbs(result);
  return ring_elem(result);
}

ring_elem RingZZ::subtract(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_sub(result, f.get_mpz(), g.get_mpz());
  mpz_reallocate_limbs(result);
  return ring_elem(result);
}

ring_elem RingZZ::mult(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_mul(result, f.get_mpz(), g.get_mpz());
  mpz_reallocate_limbs(result);
  return ring_elem(result);
}

ring_elem RingZZ::power(const ring_elem f, int n) const
{
  mpz_ptr result = new_elem();
  if (n<0) ERROR("can only raise to a nonnegative power"); else {
    mpz_pow_ui(result, f.get_mpz(), n);
    mpz_reallocate_limbs(result);
  }
  return ring_elem(result);
}
ring_elem RingZZ::power(const ring_elem f, mpz_srcptr n) const
{
  std::pair<bool, int> n1 = RingZZ::get_si(n);
  if (n1.first)
    return power(f, n1.second);
  else
    throw exc::engine_error("exponent too large");
}

ring_elem RingZZ::invert(const ring_elem f) const
{
  if (RingZZ::is_unit(f))
    return RingZZ::copy(f);
  else
    return RingZZ::from_long(0);
}

ring_elem RingZZ::divide(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_fdiv_q(result, f.get_mpz(), g.get_mpz());
  mpz_reallocate_limbs(result);
  return ring_elem(result);
}

ring_elem RingZZ::remainderAndQuotient(const ring_elem f,
                                       const ring_elem g,
                                       ring_elem &quot) const
{
  mpz_ptr q = new_elem();
  mpz_ptr r = new_elem();
  int gsign = mpz_sgn(g.get_mpz());
  mpz_t gg, ghalf;
  mpz_init(gg);
  mpz_init(ghalf);
  mpz_abs(gg, g.get_mpz());
  mpz_fdiv_qr(q, r, f.get_mpz(), gg);
  mpz_tdiv_q_2exp(ghalf, gg, 1);
  if (mpz_cmp(r, ghalf) > 0)  // r > ghalf
    {
      mpz_sub(r, r, gg);
      mpz_add_ui(q, q, 1);
    }
  if (gsign < 0) mpz_neg(q, q);

  mpz_clear(gg);
  mpz_clear(ghalf);
  mpz_reallocate_limbs(q);
  mpz_reallocate_limbs(r);
  quot = ring_elem(q);
  return ring_elem(r);
}

ring_elem RingZZ::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem = RingZZ::remainderAndQuotient(f, g, quot);
  remove(quot);
  return rem;
}

ring_elem RingZZ::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem quot;
  ring_elem rem = RingZZ::remainderAndQuotient(f, g, quot);
  remove(rem);
  return quot;
}

ring_elem RingZZ::gcd(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_gcd(result, f.get_mpz(), g.get_mpz());
  mpz_reallocate_limbs(result);
  return ring_elem(result);
}

ring_elem RingZZ::gcd_extended(const ring_elem f,
                               const ring_elem g,
                               ring_elem &u,
                               ring_elem &v) const
{
  mpz_ptr result = new_elem();
  mpz_ptr u1 = new_elem();
  mpz_ptr v1 = new_elem();
  mpz_gcdext(result, u1, v1, f.get_mpz(), g.get_mpz());
  mpz_reallocate_limbs(u1);
  mpz_reallocate_limbs(v1);
  mpz_reallocate_limbs(result);
  u = ring_elem(u1);
  v = ring_elem(v1);
  return ring_elem(result);
}

void RingZZ::syzygy(const ring_elem a,
                    const ring_elem b,
                    ring_elem &x,
                    ring_elem &y) const
{
  assert(!is_zero(b));
  // First check the special cases a = 0, b = 1, -1.  Other cases: use gcd.
  if (RingZZ::is_zero(a))
    {
      x = RingZZ::from_long(1);
      y = RingZZ::from_long(0);
      return;
    }
  mpz_srcptr bb = b.get_mpz();
  if (mpz_cmp_ui(bb, 1) == 0)
    {
      x = RingZZ::from_long(1);
      y = RingZZ::negate(a);
      return;
    }
  if (mask_mpz_cmp_si(bb, -1) == 0)
    {
      x = RingZZ::from_long(1);
      y = RingZZ::copy(a);
      return;
    }
  ring_elem g = RingZZ::gcd(a, b);
  y = RingZZ::divide(a, g);
  x = RingZZ::divide(b, g);
  RingZZ::remove(g);
  if (mpz_sgn(x.get_mpz()) > 0)
    y = RingZZ::negate(y);
  else
    x = RingZZ::negate(x);
}

ring_elem RingZZ::eval(const RingMap *map, const ring_elem f, int) const
{
  return map->get_ring()->from_int(f.get_mpz());
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
