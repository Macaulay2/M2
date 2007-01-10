// Copyright 1995 Michael E. Stillman

#include "ZZ.hpp"
#include "RRR.hpp"
#include "CCC.hpp"
#include "text-io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "gbring.hpp"
#include "../d/M2mem.h"

mpf_ptr CCC::_epsilon = NULL;

bool CCC::initialize_CCC() 
{
  initialize_ring(0);
  declare_field();
  _elem_size = sizeof(M2_CCC_struct);
  _zero_elem = new_elem();
  if (_epsilon == NULL) {
    _epsilon = reinterpret_cast<mpf_ptr>(getmem(sizeof(mpf_t)));
    mpf_init(_epsilon);
  }

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  return true;
}

CCC *CCC::create()
{
  CCC *result = new CCC;
  result->initialize_CCC();
  return result;
}

void CCC::text_out(buffer &o) const
{
  o << "CCC";
}

mpf_ptr CCC::new_mpf() const
{
  mpf_ptr result = reinterpret_cast<mpf_ptr>(getmem(_elem_size));
  mpf_init(result);
  return result;
}

M2_CCC CCC::new_elem() const
{
  M2_CCC result = reinterpret_cast<M2_CCC>(getmem(_elem_size));
  mpf_init(&result->re);
  mpf_init(&result->im);
  return result;
}
void CCC::remove_elem(M2_CCC f) const
{
  // mpf_clear(f);
}

ring_elem CCC::random() const
{
  return from_int(0);
}

void CCC::elem_text_out(buffer &o, const ring_elem ap) const
{
  mpf_ptr a = BIGCC_RE(ap);
  mpf_ptr b = BIGCC_IM(ap);
  mp_exp_t expptr;

  // easier to call RRR::elem_text_out here ???

  // size_t size = 1000;
  char *s = newarray_atomic(char,1000);
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

  is_neg = (mpf_cmp_si(b, 0) == -1);
  is_one = (mpf_cmp_si(b, 1) == 0 || mpf_cmp_si(b, -1) == 0);

  str = mpf_get_str(s, &expptr, 10, 0, b);
  o << "+ (." << str << "*10^" << expptr << ") ii";

  // if (size > 1000) deletearray(allocstr);
}

void CCC::set_epsilon(mpf_ptr epsilon)
{
  if (_epsilon == NULL) {
    _epsilon = reinterpret_cast<mpf_ptr>(getmem(sizeof(mpf_t)));
    mpf_init(_epsilon);
  }
  mpf_set(_epsilon, epsilon);
}

mpf_ptr CCC::get_epsilon()
{
  mpf_ptr epsilon = reinterpret_cast<mpf_ptr>(getmem(sizeof(mpf_t)));
  mpf_init(epsilon);
  mpf_set(epsilon, _epsilon);
  return epsilon;
}

ring_elem CCC::from_int(int n) const
{
  M2_CCC result = new_elem();
  mpf_set_si(&result->re, n);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_int(mpz_ptr n) const
{
  M2_CCC result = new_elem();
  mpf_set_z(&result->re, n);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_double(double r) const
{
  M2_CCC result = new_elem();
  mpf_set_d(&result->re, r);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_doubles(double r, double s) const
{
  M2_CCC result = new_elem();
  mpf_set_d(&result->re, r);
  mpf_set_d(&result->im, s);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_rational(mpq_ptr r) const
{
  M2_CCC result = new_elem();
  mpf_set_q(&result->re, r);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_complex(M2_CC z) const
{
  M2_CCC result = new_elem();
  mpf_set_d(&result->re, z->re);
  mpf_set_d(&result->im, z->im);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_BigReal(mpf_ptr r) const
{
  M2_CCC result = new_elem();
  mpf_set(&result->re, r);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_BigReals(mpf_ptr a, mpf_ptr b) const
{
  M2_CCC result = new_elem();
  mpf_set(&result->re, a);
  mpf_set(&result->im, b);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::from_BigComplex(M2_CCC z) const
{
  M2_CCC result = new_elem();
  mpf_set(&result->re, &z->re);
  mpf_set(&result->im, &z->im);

  return BIGCC_RINGELEM(result);
}

mpf_ptr CCC::to_BigReal(ring_elem f) const
{
  return BIGCC_RE(f);
}

bool CCC::promote(const Ring *Rf, const ring_elem f, ring_elem &result) const
{
  if (Rf == globalRRR)
    {
      M2_CCC g = new_elem();
      mpf_set(&g->re, MPF_VAL(f));
      mpf_init(&g->im);
      
      result = BIGCC_RINGELEM(g);
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
  mpf_ptr a = BIGCC_RE(f);
  mpf_ptr b = BIGCC_IM(f);
  return (mpf_sgn(a) == 0 && mpf_sgn(b) == 0);
}

bool CCC::is_equal(const ring_elem f, const ring_elem g) const
{
  mpf_ptr ar = BIGCC_RE(f);
  mpf_ptr ai = BIGCC_IM(f);
  mpf_ptr br = BIGCC_RE(g);
  mpf_ptr bi = BIGCC_IM(g);

  if (mpf_sgn(_epsilon) == 0) 
    {
      return (mpf_cmp(ar, br) == 0 && mpf_cmp(ai, bi) == 0);
    }    
  else
    {
      mpf_ptr cr = new_mpf();
      mpf_ptr ci = new_mpf();
      mpf_ptr dr = new_mpf();
      mpf_ptr di = new_mpf();
      mpf_sub(cr, ar, br);
      mpf_sub(ci, ai, bi);
      mpf_sub(dr, br, ar);
      mpf_sub(di, bi, ai);
      return (mpf_cmp(cr, _epsilon) < 0 && mpf_cmp(ci, _epsilon) < 0 &&
	      mpf_cmp(dr, _epsilon) < 0 && mpf_cmp(di, _epsilon) < 0);
    }
}
bool CCC::is_greater(const ring_elem f, const ring_elem g) const
{
  M2_CCC a = new_elem();
  M2_CCC b = new_elem();

  mpf_pow_ui(&a->re, BIGCC_RE(f), 2);
  mpf_pow_ui(&a->im, BIGCC_IM(f), 2);
  mpf_pow_ui(&b->re, BIGCC_RE(g), 2);
  mpf_pow_ui(&b->im, BIGCC_IM(g), 2);
  mpf_add(&a->re, &a->re, &a->im);
  mpf_add(&b->re, &b->re, &b->im);
  return mpf_cmp(&a->re,&b->re) > 0;
}

int CCC::compare_elems(const ring_elem f, const ring_elem g) const
{
  int cmp = mpf_cmp(BIGCC_RE(f), BIGCC_RE(g));
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  cmp = mpf_cmp(BIGCC_IM(f),BIGCC_IM(g));
  if (cmp > 0) return 1;
  if (cmp < 0) return -1;
  return 0;
}

bool CCC::is_real(const ring_elem f) const
{
  mpf_ptr im = BIGCC_IM(f);
  return mpf_sgn(im) == 0;
}

void CCC::zeroize_tiny_lead_components(vec &v, mpf_ptr epsilon) const
{
  while (v != NULL) {
    mpf_ptr re = new_mpf();
    mpf_ptr im = new_mpf();
    mpf_abs(re, BIGCC_RE(v->coeff));
    mpf_abs(im, BIGCC_IM(v->coeff));
    if (epsilon != NULL && mpf_cmp(re, epsilon) < 0 && mpf_cmp(im, epsilon)) {
      v = v->next;
    } else return;    
  }
}

ring_elem CCC::absolute(const ring_elem f) const
{
  M2_CCC result = new_elem();

  mpf_pow_ui(&result->re, BIGCC_RE(f), 2);
  mpf_pow_ui(&result->im, BIGCC_IM(f), 2);
  mpf_add(&result->re, &result->re, &result->im);
  mpf_sqrt(&result->re, &result->re);
  mpf_set_ui(&result->im, 0);

  return BIGCC_RINGELEM(result);
}

ring_elem CCC::copy(const ring_elem f) const
{
  mpf_ptr a = BIGCC_RE(f);
  mpf_ptr b = BIGCC_IM(f);

  M2_CCC result = new_elem();
  mpf_set(&result->re, a);
  mpf_set(&result->im, b);
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
  mpf_neg(BIGCC_RE(f), BIGCC_RE(f));
  mpf_neg(BIGCC_IM(f), BIGCC_IM(f));
}

void CCC::internal_add_to(ring_elem &f, ring_elem &g) const
{
  mpf_add(BIGCC_RE(f), BIGCC_RE(f), BIGCC_RE(g));
  mpf_add(BIGCC_IM(f), BIGCC_IM(f), BIGCC_IM(g));
  // remove(g); should this be removed?
}

void CCC::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  mpf_sub(BIGCC_RE(f), BIGCC_RE(f), BIGCC_RE(g));
  mpf_sub(BIGCC_IM(f), BIGCC_IM(f), BIGCC_IM(g));
  // remove(g); should g be removed?
}

ring_elem CCC::negate(const ring_elem f) const
{
  M2_CCC result = new_elem();
  mpf_neg(&result->re, BIGCC_RE(f));
  mpf_neg(&result->im, BIGCC_IM(f));
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::add(const ring_elem f, const ring_elem g) const
{
  M2_CCC result = new_elem();
  mpf_add(&result->re, BIGCC_RE(f), BIGCC_RE(g));
  mpf_add(&result->im, BIGCC_IM(f), BIGCC_IM(g));
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::subtract(const ring_elem f, const ring_elem g) const
{
  M2_CCC result = new_elem();
  mpf_sub(&result->re, BIGCC_RE(f), BIGCC_RE(g));
  mpf_sub(&result->im, BIGCC_IM(f), BIGCC_IM(g));
  return BIGCC_RINGELEM(result);
}

ring_elem CCC::mult(const ring_elem f, const ring_elem g) const
{
  M2_CCC result = new_elem();
  M2_CCC tmp = new_elem();
  mpf_mul(&result->re, BIGCC_RE(f), BIGCC_RE(g));
  mpf_mul(&tmp->re, BIGCC_IM(f), BIGCC_IM(g));
  mpf_sub(&result->re, &result->re, &tmp->re);
  mpf_mul(&result->im, BIGCC_RE(f), BIGCC_IM(g));
  mpf_mul(&tmp->im, BIGCC_IM(f), BIGCC_RE(g));
  mpf_add(&result->im, &result->im, &tmp->im);
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
    mpf_mul(&tmp->re, BIGCC_RE(f), BIGCC_RE(f));
    mpf_mul(&tmp->im, BIGCC_IM(f), BIGCC_IM(f));
    mpf_add(&tmp->re, &tmp->re, &tmp->im);
    mpf_div(&result->re, BIGCC_RE(f), &tmp->re);
    mpf_div(&result->im, BIGCC_IM(f), &tmp->re);
    mpf_neg(&result->im, &result->im);
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
  return map->get_ring()->from_BigComplex(BIGCC_VAL(f));
}




// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
