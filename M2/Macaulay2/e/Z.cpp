// Copyright 1995 Michael E. Stillman

#include "Z.hpp"
#include "text_io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "gbring.hpp"

#if 0
#include "gmp.h"
#define MPZ_VAL(f) (mpz_ptr ((f).poly_val))
#define MPZ_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))
#endif

bool Z::initialize_ZZ(const Monoid *D) 
{
  initialize_ring(0,0,0,this,
		  Monoid::get_trivial_monoid(),
		  D);
  _elem_size = sizeof(mpz_t);
  _zero_elem = new_elem();
  mpz_init_set_si(_zero_elem, 0);
  return true;
}

Z *Z::create(const Monoid *D)
{
  Z *result = new Z;
  result->initialize_ZZ(D);
  result->_grtype = GRType::make_BASE(result);
  return result;
}

void Z::text_out(buffer &o) const
{
  o << "ZZ";
}

mpz_ptr Z::new_elem() const
{
  mpz_ptr result = (mpz_ptr) getmem(_elem_size);
  mpz_init(result);
  return result;
}
void Z::remove_elem(mpz_ptr f) const
{
}

bool Z::get_ui(unsigned int &result, mpz_t n)
{
  if (n->_mp_size > 1 || n->_mp_size < -1) return false;
  result = mpz_get_ui(n);
  return true;
}

bool Z::get_si(int &result, mpz_t n)
{
  if (n->_mp_size > 1 || n->_mp_size < -1) return false;
  result = mpz_get_si(n);
  return true;
}

unsigned int Z::mod_ui(mpz_t n, unsigned int p)
{
  mpz_t ans;
  mpz_init(ans);
  unsigned int exp = mpz_mod_ui(ans, n, p);
  mpz_clear(ans);
  return exp;
}

int Z::coerce_to_int(ring_elem a) const
{ 
  return mpz_get_si(MPZ_VAL(a)); 
}

ring_elem Z::random() const
{
  M2_Integer result = Z::new_elem();
  Random::random_integer(result);
  return MPZ_RINGELEM(result);
}

void Z::elem_text_out(buffer &o, const ring_elem ap) const
{
  mpz_ptr a = MPZ_VAL(ap);

  char s[1000];
  char *str;

  bool is_neg = (mpz_cmp_si(a, 0) == -1);
  bool is_one = (mpz_cmp_si(a, 1) == 0 || mpz_cmp_si(a, -1) == 0);

  int size = mpz_sizeinbase(a, 10) + 2;

  char *allocstr = (size > 1000 ? new char[size] : s);

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
  if (size > 1000) delete [] allocstr;
}

ring_elem Z::from_int(int n) const
{
  mpz_ptr result = new_elem();
  mpz_set_si(result, n);

  return MPZ_RINGELEM(result);
}

ring_elem Z::from_int(mpz_ptr n) const
{
  mpz_ptr result = new_elem();
  mpz_set(result, n);

  return MPZ_RINGELEM(result);
}

ring_elem Z::var(int v, int) const
{
  if (v >= 0) return from_int(0);
  return from_int(1);
}
bool Z::promote(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool Z::lift(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool Z::is_unit(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  return (mpz_cmp_si(a, 1)==0 ||
	  mpz_cmp_si(a, -1)==0);
}

bool Z::is_zero(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  return mpz_sgn(a) == 0;
}

bool Z::is_equal(const ring_elem f, const ring_elem g) const
{
  mpz_ptr a = MPZ_VAL(f);
  mpz_ptr b = MPZ_VAL(g);

  return mpz_cmp(a, b) == 0;
}
int Z::compare(const ring_elem f, const ring_elem g) const
{
  mpz_ptr a = MPZ_VAL(f);
  mpz_ptr b = MPZ_VAL(g);
  return mpz_cmp(a,b);
}
int Z::is_positive(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  return mpz_sgn(a) > 0;
}

ring_elem Z::copy(const ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);

  mpz_ptr result = new_elem();
  mpz_set(result, a);
  return MPZ_RINGELEM(result);
}

void Z::remove(ring_elem &f) const
{
  mpz_ptr a = MPZ_VAL(f);
  remove_elem(a);
  f = MPZ_RINGELEM(NULL);
}

ring_elem Z::preferred_associate(ring_elem f) const
{
  mpz_ptr a = MPZ_VAL(f);
  if (mpz_sgn(a) >= 0)
    return from_int(1);
  return from_int(-1);
}

void Z::negate_to(ring_elem &f) const
{
  mpz_sub(MPZ_VAL(f), _zero_elem, MPZ_VAL(f));
}

void Z::add_to(ring_elem &f, ring_elem &g) const
{
  mpz_add(MPZ_VAL(f), MPZ_VAL(f), MPZ_VAL(g));
  remove(g);
}

void Z::subtract_to(ring_elem &f, ring_elem &g) const
{
  mpz_sub(MPZ_VAL(f), MPZ_VAL(f), MPZ_VAL(g));
  remove(g);
}

ring_elem Z::negate(const ring_elem f) const
{
  mpz_ptr result = new_elem();
  mpz_sub(result, _zero_elem, MPZ_VAL(f));
  return MPZ_RINGELEM(result);
}

ring_elem Z::add(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_add(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem Z::subtract(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_sub(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem Z::mult(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_mul(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem Z::power(const ring_elem f, int n) const
{
  mpz_ptr result = new_elem();
  mpz_pow_ui(result, MPZ_VAL(f), n);
  return MPZ_RINGELEM(result);
}
ring_elem Z::power(const ring_elem f, mpz_t n) const
{
  mpz_ptr result = new_elem();
  int n1;
  if (!get_si(n1, n)) 
    { ERROR("exponent too large"); }
  else
    mpz_pow_ui(result, MPZ_VAL(f), n1);
  return MPZ_RINGELEM(result);
}

ring_elem Z::invert(const ring_elem f) const
{
  if (is_unit(f))
    return copy(f);
  else
    return from_int(0);
}

ring_elem Z::divide(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_div(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}
ring_elem Z::divide(const ring_elem f, const ring_elem g, ring_elem &rem) const
{
#if 0
  mpz_ptr result = new_elem();
  mpz_ptr resultmod = new_elem();
  mpz_divmod(result, resultmod, MPZ_VAL(f), MPZ_VAL(g));
  rem = MPZ_RINGELEM(resultmod);
  return MPZ_RINGELEM(result);
#endif
#if 0
  mpz_ptr result = new_elem();
  mpz_ptr resultmod = new_elem();

  mpz_tdiv_qr(result, resultmod, MPZ_VAL(f), MPZ_VAL(g));
  mpz_t ghalf;
  mpz_init(ghalf);
  mpz_tdiv_q_2exp(ghalf, MPZ_VAL(g), 1);

  if (mpz_cmp(ghalf,resultmod) == -1)  // ghalf < rem
    {
      mpz_add_ui(result, result, 1);
      mpz_sub(resultmod, resultmod, MPZ_VAL(g));
    }
  else 
    {
      mpz_neg(ghalf, ghalf);
      int cmp = mpz_cmp(ghalf,resultmod);
      int odd = mpz_mod_ui(ghalf, MPZ_VAL(g),2);  // The ghalf is a dummy
      if (cmp == 1 || (!odd & cmp == 0))
	{
	  mpz_sub_ui(result, result, 1);
	  mpz_add(resultmod, resultmod, MPZ_VAL(g));
	}
    }
  mpz_clear(ghalf);
  rem = MPZ_RINGELEM(resultmod);
  return MPZ_RINGELEM(result);
#endif
  // Divide f by |g|, to get q, r, where r is non-negative.
  // If r is > |g|/2 then: r -= |g|, q++.
  // If g<0 then: q = -q.

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
  rem = MPZ_RINGELEM(r);
  return MPZ_RINGELEM(q);
}

ring_elem Z::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem quot = Z::divide(f,g,rem);
  remove(quot);
  return rem;
}

ring_elem Z::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem quot = Z::divide(f,g,rem);
  remove(rem);
  return quot;
}

ring_elem Z::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				  ring_elem &quot) const
{
  ring_elem result;
  quot = Z::divide(f,g,result);
  return result;
}


ring_elem Z::gcd(const ring_elem f, const ring_elem g) const
{
  mpz_ptr result = new_elem();
  mpz_gcd(result, MPZ_VAL(f), MPZ_VAL(g));
  return MPZ_RINGELEM(result);
}

ring_elem Z::gcd_extended(const ring_elem f, const ring_elem g, 
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

void Z::syzygy(const ring_elem a, const ring_elem b,
	       ring_elem &x, ring_elem &y) const
{
  // First check the special cases a = 0, b = 1, -1.  Other cases: use gcd.
  if (Z::is_zero(a))
    {
      x = Z::from_int(1);
      y = Z::from_int(0);
      return;
    }
  mpz_ptr bb = MPZ_VAL(b);
  if (mpz_cmp_ui(bb,1) == 0)
    {
      x = Z::from_int(1);
      y = Z::negate(a);
      return;
    }
  if (mpz_cmp_si(bb,-1) == 0)
    {
      x = Z::from_int(1);
      y = Z::copy(a);
      return;
    }
  ring_elem g = Z::gcd(a,b);
  y = Z::divide(a,g);
  x = Z::divide(b,g);
  Z::remove(g);
  if (mpz_sgn(MPZ_VAL(x)) > 0)
    Z::negate_to(y);
  else
    Z::negate_to(x);
}

ring_elem Z::eval(const RingMap *map, const ring_elem f) const
{
  return map->get_ring()->from_int(MPZ_VAL(f));
}




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
