// Copyright 1995 Michael E. Stillman

#include "Z.hpp"
#include "text_io.hpp"
#include "bin_io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "serial.hpp"
#if 0
#include "gmp.h"
#define MPZ_VAL(f) (mpz_ptr ((f).poly_val))
#define MPZ_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))
#endif

Z::Z(const Monoid *D) : Ring(0,0,0,this /* Visual C WARNING */,trivial_monoid,D)
{
  mpz_stash = new stash("ZZ", sizeof(mpz_t));
  zero_elem = new_elem();
  mpz_init_set_si(zero_elem, 0);
}

Z *Z::create(const Monoid *D)
{
  Z *obj = new Z(D);
  return (Z *) intern(obj);
}

void Z::write_object(object_writer &o) const
{
  o << class_id() << D;
}

Z *Z::read_object(object_reader &i)
{
  object_element *obj;
  i >> obj;
  Monoid *D = obj->cast_to_Monoid();
  return new Z(D);
}

void Z::text_out(buffer &o) const
{
  o << "ZZ";
}

mpz_ptr Z::new_elem() const
{
  mpz_ptr result = (mpz_ptr) mpz_stash->new_elem();
  mpz_init(result);
  return result;
}
void Z::remove_elem(mpz_ptr f) const
{
  if (f == NULL) return;
  mpz_clear(f);
  mpz_stash->delete_elem(f);
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
  // uugh.  This should not need to go through RingElement
  RingElement f = Random::random();
  return copy(f.get_value());
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

void Z::elem_bin_out(buffer &o, const ring_elem a) const
{
  bin_mpz_out(o, MPZ_VAL(a));
}

void Z::write_element(object_writer &o, const ring_elem f) const
{
  o << MPZ_VAL(f);
}
void Z::read_element(object_reader &i, ring_elem &result) const
{
  mpz_ptr m = new_elem();
  i >> m;
  result = MPZ_RINGELEM(m);
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

void Z::negate_to(ring_elem &f) const
{
  mpz_sub(MPZ_VAL(f), zero_elem, MPZ_VAL(f));
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
  mpz_sub(result, zero_elem, MPZ_VAL(f));
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
    { gError << "exponent too large"; }
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

bool Z::is_homogeneous(const ring_elem) const
{
  return true;
}

void Z::degree(const ring_elem, int *d) const
{
  degree_monoid()->one(d);
}
void Z::degree_weights(const ring_elem, const int *, int &lo, int &hi) const
{
  lo = hi = 0;
}
int Z::primary_degree(const ring_elem) const
{
  return 0;
}

ring_elem Z::homogenize(const ring_elem f, int, int deg, const int *) const
{
  if (deg != 0) 
    gError << "homogenize: no homogenization exists";
  return f;
}

ring_elem Z::homogenize(const ring_elem f, int, const int *) const
{
  return f;
}

int Z::n_terms(const ring_elem) const
{
  return 1;
}
ring_elem Z::term(const ring_elem a, const int *) const
{
  return copy(a);
}
ring_elem Z::lead_coeff(const ring_elem f) const
{
  return f;
}
ring_elem Z::get_coeff(const ring_elem f, const int *) const
{
  return f;
}
ring_elem Z::get_terms(const ring_elem f, int, int) const
{
  return f;
}
