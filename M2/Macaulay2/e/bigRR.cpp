// Copyright 1995 Michael E. Stillman

#include "bigRR.hpp"
#include "ZZ.hpp"
#include "text_io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "gbring.hpp"
#include "../d/M2mem.h"

#if 0
#include "gmp.h"
#define MPF_VAL(f) (mpf_ptr ((f).poly_val))
#define MPF_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))
#endif

mpf_ptr bigRR::_epsilon = NULL;

bool bigRR::initialize_bigRR() 
{
  initialize_ring(0,0,0,this,
		  Monoid::get_trivial_monoid(),
		  Monoid::get_trivial_monoid());
  _elem_size = sizeof(mpf_t);
  _zero_elem = new_elem();
  if (_epsilon == NULL) {
    _epsilon = new_elem();
    mpf_init(_epsilon);
  }

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  return true;
}

bigRR *bigRR::create()
{
  bigRR *result = new bigRR;
  result->initialize_bigRR();
  return result;
}

void bigRR::text_out(buffer &o) const
{
  o << "bigRR";
}

mpf_ptr bigRR::new_elem() const
{
  mpf_ptr result = reinterpret_cast<mpf_ptr>(getmem(_elem_size));
  mpf_init(result);
  return result;
}
void bigRR::remove_elem(mpf_ptr f) const
{
  // mpf_clear(f);
}

ring_elem bigRR::random() const
{
  return from_int(0);
}

void bigRR::elem_text_out(buffer &o, const ring_elem ap) const
{
  mpf_ptr a = MPF_VAL(ap);
  mp_exp_t expptr;

  // size_t size = 1000;
  char *s = newarray(char,1000);
  char *str;

  bool is_neg = (mpf_cmp_si(a, 0) == -1);
  bool is_one = (mpf_cmp_si(a, 1) == 0 || mpf_cmp_si(a, -1) == 0);

  // int size = mpf_sizeinbase(a, 10) + 2;

  // char *allocstr = (size > 1000 ? newarray(char,size) : s);

  if (!is_neg && p_plus) o << '+';
  if (is_one) 
    {  
      if (is_neg) o << '-';
      if (p_one) o << '1'; 
    }
  else
    {
      str = mpf_get_str(s, &expptr, 10, 0, a);
      o << "." << str << "*10^" << expptr;
    }
  // if (size > 1000) deletearray(allocstr);
}

void bigRR::set_epsilon(mpf_ptr epsilon)
{
  if (_epsilon == NULL) {
    _epsilon = reinterpret_cast<mpf_ptr>(getmem(sizeof(mpf_t)));
    mpf_init(_epsilon);
  }  
  mpf_set(_epsilon, epsilon);
}

mpf_ptr bigRR::get_epsilon()
{
  mpf_ptr epsilon = reinterpret_cast<mpf_ptr>(getmem(sizeof(mpf_t)));
  mpf_init(epsilon);
  mpf_set(epsilon, _epsilon);
  return epsilon;
}

mpf_ptr bigRR::to_BigReal(ring_elem f) const
{
  return MPF_VAL(f);
}

ring_elem bigRR::from_int(int n) const
{
  mpf_ptr result = new_elem();
  mpf_set_si(result, n);

  return MPF_RINGELEM(result);
}

ring_elem bigRR::from_int(mpz_ptr n) const
{
  mpf_ptr result = new_elem();
  mpf_set_z(result, n);

  return MPF_RINGELEM(result);
}

ring_elem bigRR::from_double(double r) const
{
  mpf_ptr result = new_elem();
  mpf_set_d(result, r);

  return MPF_RINGELEM(result);
}

ring_elem bigRR::from_rational(mpq_ptr r) const
{
  mpf_ptr result = new_elem();
  mpf_set_q(result, r);

  return MPF_RINGELEM(result);
}

ring_elem bigRR::from_BigReal(mpf_ptr r) const
{
  mpf_ptr result = new_elem();
  mpf_set(result, r);

  return MPF_RINGELEM(result);
}

ring_elem bigRR::var(int v, int) const
{
  if (v >= 0) return from_int(0);
  return from_int(1);
}
bool bigRR::promote(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool bigRR::lift(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool bigRR::is_unit(const ring_elem f) const
{
  return !is_zero(f);
}

bool bigRR::is_zero(const ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);
  return mpf_sgn(a) == 0;
}

bool bigRR::is_equal(const ring_elem f, const ring_elem g) const
{
  mpf_ptr a = MPF_VAL(f);
  mpf_ptr b = MPF_VAL(g);

  if (mpf_sgn(_epsilon) == 0) 
    {
      return mpf_cmp(a, b) == 0;
    }
  else
    {
      mpf_ptr c = new_elem();
      mpf_ptr d = new_elem();
      mpf_sub(c, a, b);
      mpf_sub(d, b, a);
      return (mpf_cmp(c, _epsilon) < 0 && mpf_cmp(d, _epsilon) < 0);
    }
}
bool bigRR::is_greater(const ring_elem f, const ring_elem g) const
{
  mpf_ptr a = MPF_VAL(f);
  mpf_ptr b = MPF_VAL(g);
  return mpf_cmp(a,b) > 0;
}
bool bigRR::is_less(const ring_elem f, const ring_elem g) const
{
  mpf_ptr a = MPF_VAL(f);
  mpf_ptr b = MPF_VAL(g);
  return mpf_cmp(a,b) < 0;
}
int bigRR::is_positive(const ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);
  return mpf_sgn(a) > 0;
}


void bigRR::zeroize_tiny_lead_components(vec &v, mpf_ptr epsilon) const
{
  while (v != NULL) {
    mpf_ptr r = new_elem();
    mpf_abs(r, MPF_VAL(v->coeff));
    if (epsilon != NULL && mpf_cmp(r, epsilon) < 0) {
      v = v->next;
    } else return;
  }
}

ring_elem bigRR::absolute(const ring_elem f) const
{
  mpf_ptr result = new_elem();
  mpf_ptr a = MPF_VAL(f);
  mpf_abs(result, a);
  return MPF_RINGELEM(result);
}


ring_elem bigRR::copy(const ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);

  mpf_ptr result = new_elem();
  mpf_set(result, a);
  return MPF_RINGELEM(result);
}

void bigRR::remove(ring_elem &f) const
{
#if 0
  mpf_ptr a = MPF_VAL(f);
  remove_elem(a);  // does nothing... remove this code?
  f = MPF_RINGELEM(NULL);
#endif
}

ring_elem bigRR::preferred_associate(ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);
  if (mpf_sgn(a) >= 0)
    return from_int(1);
  return from_int(-1);
}

void bigRR::internal_negate_to(ring_elem &f) const
{
  mpf_neg(MPF_VAL(f), MPF_VAL(f));
}

void bigRR::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpf_add(MPF_VAL(f), MPF_VAL(f), MPF_VAL(g));
  // remove(g); should this be removed?
}

void bigRR::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpf_sub(MPF_VAL(f), MPF_VAL(f), MPF_VAL(g));
  // remove(g); should g be removed?
}

ring_elem bigRR::negate(const ring_elem f) const
{
  mpf_ptr result = new_elem();
  mpf_neg(result, MPF_VAL(f));
  return MPF_RINGELEM(result);
}

ring_elem bigRR::add(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_add(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}

ring_elem bigRR::subtract(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_sub(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}

ring_elem bigRR::mult(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_mul(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}

ring_elem bigRR::power(const ring_elem f, int n) const
{
  mpf_ptr result = new_elem();
  mpf_pow_ui(result, MPF_VAL(f), n);
  return MPF_RINGELEM(result);
}
ring_elem bigRR::power(const ring_elem f, mpz_t n) const
{
  mpf_ptr result = new_elem();
  int n1;
  if (!ZZ::get_si(n1, n)) 
    { ERROR("exponent too large"); }
  else
    mpf_pow_ui(result, MPF_VAL(f), n1);
  return MPF_RINGELEM(result);
}

ring_elem bigRR::sqrt(const ring_elem f) const
{
  mpf_ptr result = new_elem();
  mpf_sqrt(result, MPF_VAL(f));
  return MPF_RINGELEM(result);
}

ring_elem bigRR::invert(const ring_elem f) const
{
  if (is_zero(f))
    return from_int(0);
  else {
    mpf_ptr result = new_elem();
    mpf_ptr unit_elem = new_elem();
    mpf_set_si(unit_elem, 1);
    mpf_div(result, unit_elem, MPF_VAL(f));
    remove_elem(unit_elem);
    return MPF_RINGELEM(result);
  }
}

ring_elem bigRR::divide(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_div(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}
ring_elem bigRR::divide(const ring_elem f, const ring_elem g, ring_elem &rem) const
{
  // If g == 0.0 then rem = f, return 0.
  // If g != 0.0 then rem = 0, return f/g
  if (is_zero(g))
    {
      rem = copy(f);
      return from_int(0);
    }
  else
    {
      rem = from_int(0);
      return divide(f,g);
    }
}

ring_elem bigRR::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem quot = divide(f,g,rem);
  remove(quot);
  return rem;
}

ring_elem bigRR::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem quot = bigRR::divide(f,g,rem);
  remove(rem);
  return quot;
}

ring_elem bigRR::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				  ring_elem &quot) const
{
  ring_elem result;
  quot = bigRR::divide(f,g,result);
  return result;
}


ring_elem bigRR::gcd(const ring_elem f, const ring_elem g) const
{
  if (is_zero(g) && is_zero(f))
    return from_int(0);
  else
    return from_int(1);
}

ring_elem bigRR::gcd_extended(const ring_elem f, const ring_elem g, 
			    ring_elem &u, ring_elem &v) const
{
  if (!is_zero(g))
    {
      u = from_int(0);
      ring_elem one_elem = from_int(1);
      v = divide(one_elem, g);
      return one_elem;
    }
  else if (!is_zero(f))
    {
      ring_elem one_elem = from_int(1);
      u = divide(one_elem, f);
      v = from_int(0);
      return one_elem;
    }
  else
    {
      u = from_int(0);
      v = from_int(0);
      return from_int(0);
    }
}


void bigRR::syzygy(const ring_elem a, const ring_elem b,
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

ring_elem bigRR::eval(const RingMap *map, const ring_elem f) const
{
  return map->get_ring()->from_BigReal(MPF_VAL(f));
}




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
