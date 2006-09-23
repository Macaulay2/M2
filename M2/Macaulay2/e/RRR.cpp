// Copyright 1995 Michael E. Stillman

#include "RRR.hpp"
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

mpf_ptr RRR::_epsilon = NULL;

bool RRR::initialize_RRR() 
{
  initialize_ring(0);
  declare_field();
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

RRR *RRR::create()
{
  RRR *result = new RRR;
  result->initialize_RRR();
  return result;
}

void RRR::text_out(buffer &o) const
{
  o << "RRR";
}

mpf_ptr RRR::new_elem() const
{
  mpf_ptr result = reinterpret_cast<mpf_ptr>(getmem(_elem_size));
  mpf_init(result);
  return result;
}
void RRR::remove_elem(mpf_ptr f) const
{
  // mpf_clear(f);
}

ring_elem RRR::random() const
{
  return from_int(0);
}

void RRR::elem_text_out(buffer &o, const ring_elem ap) const
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

void RRR::set_epsilon(mpf_ptr epsilon)
{
  if (_epsilon == NULL) {
    _epsilon = reinterpret_cast<mpf_ptr>(getmem(sizeof(mpf_t)));
    mpf_init(_epsilon);
  }  
  mpf_set(_epsilon, epsilon);
}

mpf_ptr RRR::get_epsilon()
{
  mpf_ptr epsilon = reinterpret_cast<mpf_ptr>(getmem(sizeof(mpf_t)));
  mpf_init(epsilon);
  mpf_set(epsilon, _epsilon);
  return epsilon;
}

bool RRR::from_string(M2_string s, ring_elem &f) const
  // returns false if an error has occurred.  f is initialized and set with value
  // only if true is returned.
{
  char *s1 = tocharstar(s);
  mpf_ptr result = new_elem();
  if (!mpf_set_str(result, s1, 10))
    return false;

  f = MPF_RINGELEM(result);
  return true;
}

mpf_ptr RRR::to_BigReal(ring_elem f) const
{
  return MPF_VAL(f);
}

ring_elem RRR::from_int(int n) const
{
  mpf_ptr result = new_elem();
  mpf_set_si(result, n);

  return MPF_RINGELEM(result);
}

ring_elem RRR::from_int(mpz_ptr n) const
{
  mpf_ptr result = new_elem();
  mpf_set_z(result, n);

  return MPF_RINGELEM(result);
}

ring_elem RRR::from_double(double r) const
{
  mpf_ptr result = new_elem();
  mpf_set_d(result, r);

  return MPF_RINGELEM(result);
}

ring_elem RRR::from_rational(mpq_ptr r) const
{
  mpf_ptr result = new_elem();
  mpf_set_q(result, r);

  return MPF_RINGELEM(result);
}

ring_elem RRR::from_BigReal(mpf_ptr r) const
{
  mpf_ptr result = new_elem();
  mpf_set(result, r);

  return MPF_RINGELEM(result);
}

bool RRR::promote(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool RRR::lift(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool RRR::is_unit(const ring_elem f) const
{
  return !is_zero(f);
}

bool RRR::is_zero(const ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);
  return mpf_sgn(a) == 0;
}

bool RRR::is_equal(const ring_elem f, const ring_elem g) const
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
bool RRR::is_greater(const ring_elem f, const ring_elem g) const
{
  mpf_ptr a = MPF_VAL(f);
  mpf_ptr b = MPF_VAL(g);
  return mpf_cmp(a,b) > 0;
}
bool RRR::is_less(const ring_elem f, const ring_elem g) const
{
  mpf_ptr a = MPF_VAL(f);
  mpf_ptr b = MPF_VAL(g);
  return mpf_cmp(a,b) < 0;
}

int RRR::compare_elems(const ring_elem f, const ring_elem g) const
{
  mpf_ptr a = MPF_VAL(f);
  mpf_ptr b = MPF_VAL(g);
  int cmp = mpf_cmp(a,b);
  if (cmp > 0) return 1;
  if (cmp == 0) return 0;
  return -1;
}

int RRR::is_positive(const ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);
  return mpf_sgn(a) > 0;
}


void RRR::zeroize_tiny_lead_components(vec &v, mpf_ptr epsilon) const
{
  while (v != NULL) {
    mpf_ptr r = new_elem();
    mpf_abs(r, MPF_VAL(v->coeff));
    if (epsilon != NULL && mpf_cmp(r, epsilon) < 0) {
      v = v->next;
    } else return;
  }
}

ring_elem RRR::absolute(const ring_elem f) const
{
  mpf_ptr result = new_elem();
  mpf_ptr a = MPF_VAL(f);
  mpf_abs(result, a);
  return MPF_RINGELEM(result);
}


ring_elem RRR::copy(const ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);

  mpf_ptr result = new_elem();
  mpf_set(result, a);
  return MPF_RINGELEM(result);
}

void RRR::remove(ring_elem &f) const
{
#if 0
  mpf_ptr a = MPF_VAL(f);
  remove_elem(a);  // does nothing... remove this code?
  f = MPF_RINGELEM(NULL);
#endif
}

ring_elem RRR::preferred_associate(ring_elem f) const
{
  mpf_ptr a = MPF_VAL(f);
  if (mpf_sgn(a) >= 0)
    return from_int(1);
  return from_int(-1);
}

void RRR::internal_negate_to(ring_elem &f) const
{
  mpf_neg(MPF_VAL(f), MPF_VAL(f));
}

void RRR::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpf_add(MPF_VAL(f), MPF_VAL(f), MPF_VAL(g));
  // remove(g); should this be removed?
}

void RRR::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpf_sub(MPF_VAL(f), MPF_VAL(f), MPF_VAL(g));
  // remove(g); should g be removed?
}

ring_elem RRR::negate(const ring_elem f) const
{
  mpf_ptr result = new_elem();
  mpf_neg(result, MPF_VAL(f));
  return MPF_RINGELEM(result);
}

ring_elem RRR::add(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_add(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}

ring_elem RRR::subtract(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_sub(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}

ring_elem RRR::mult(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_mul(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}

ring_elem RRR::power(const ring_elem f, int n) const
{
  mpf_ptr result = new_elem();
  mpf_pow_ui(result, MPF_VAL(f), n);
  return MPF_RINGELEM(result);
}
ring_elem RRR::power(const ring_elem f, mpz_t n) const
{
  mpf_ptr result = new_elem();
  int n1;
  if (!RingZZ::get_si(n1, n)) 
    { ERROR("exponent too large"); }
  else
    mpf_pow_ui(result, MPF_VAL(f), n1);
  return MPF_RINGELEM(result);
}

ring_elem RRR::sqrt(const ring_elem f) const
{
  mpf_ptr result = new_elem();
  mpf_sqrt(result, MPF_VAL(f));
  return MPF_RINGELEM(result);
}

ring_elem RRR::invert(const ring_elem f) const
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

ring_elem RRR::divide(const ring_elem f, const ring_elem g) const
{
  mpf_ptr result = new_elem();
  mpf_div(result, MPF_VAL(f), MPF_VAL(g));
  return MPF_RINGELEM(result);
}
ring_elem RRR::divide(const ring_elem f, const ring_elem g, ring_elem &rem) const
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

ring_elem RRR::remainder(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem quot = divide(f,g,rem);
  remove(quot);
  return rem;
}

ring_elem RRR::quotient(const ring_elem f, const ring_elem g) const
{
  ring_elem rem;
  ring_elem quot = RRR::divide(f,g,rem);
  remove(rem);
  return quot;
}

ring_elem RRR::remainderAndQuotient(const ring_elem f, const ring_elem g, 
				  ring_elem &quot) const
{
  ring_elem result;
  quot = RRR::divide(f,g,result);
  return result;
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
  return map->get_ring()->from_BigReal(MPF_VAL(f));
}




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
