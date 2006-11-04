// Copyright 2001 Michael E. Stillman

#include "ZZ.hpp"
#include "CC.hpp"
#include <stdio.h>
#include <math.h>
#include "text_io.hpp"
#include "monoid.hpp"
#include "relem.hpp"
#include "ringmap.hpp"
#include "random.hpp"
#include "gbring.hpp"
#include "../d/M2mem.h"
#include "coeffrings.hpp"


extern "C" M2_CC make_M2_Complex(double re, double im)
{
  M2_CC z = newitem_atomic(M2_CC_struct);
  z->re = re;
  z->im = im;
  return z;
}

CC::~CC()
{
}

bool CC::initialize_CC(double epsilon) 
{
  initialize_ring(0);
  declare_field();
  _epsilon = epsilon;

  zeroV = from_int(0);
  oneV = from_int(1);
  minus_oneV = from_int(-1);

  coeffR = new CoefficientRingCC;
  return true;
}

CC *CC::create(double epsilon)
{
  CC *result = new CC;
  result->initialize_CC(epsilon);
  return result;
}

void CC::text_out(buffer &o) const
{
  o << "CC";
}

M2_CC CC::new_elem() const
{
  M2_CC result = reinterpret_cast<M2_CC>(getmem(sizeof(M2_CC_struct)));
  result->re = 0.0;
  result->im = 0.0;
  return result;
}
ring_elem CC::from_double(double a) const
{
  M2_CC result = reinterpret_cast<M2_CC>(getmem(sizeof(M2_CC_struct)));
  result->re = a;
  result->im = 0.0;
  return CC_RINGELEM(result);
}
double CC::to_double(ring_elem a) const
{
  return CC_RE(a);
}

ring_elem CC::from_rational(mpq_ptr r) const
{
  M2_CC result = reinterpret_cast<M2_CC>(getmem(sizeof(M2_CC_struct)));
  result->re = mpq_get_d(r);
  result->im = 0.0;
  return CC_RINGELEM(result);
}

ring_elem CC::from_BigReal(mpf_ptr a) const
{
  M2_CC result = reinterpret_cast<M2_CC>(getmem(sizeof(M2_CC_struct)));
  result->re = mpf_get_d(a);
  result->im = 0.0;
  return CC_RINGELEM(result);
}

ring_elem CC::from_complex(M2_CC z) const
{
  M2_CC result = reinterpret_cast<M2_CC>(getmem(sizeof(M2_CC_struct)));
  result->re = z->re;
  result->im = z->im;
  return CC_RINGELEM(result);
}

ring_elem CC::from_doubles(double a, double b) const
{
  M2_CC result = reinterpret_cast<M2_CC>(getmem(sizeof(M2_CC_struct)));
  result->re = a;
  result->im = b;
  return CC_RINGELEM(result);
}

ring_elem CC::from_BigComplex(M2_CCC z) const
{
  M2_CC result = reinterpret_cast<M2_CC>(getmem(sizeof(M2_CC_struct)));
  result->re = mpf_get_d(&z->re);
  result->im = mpf_get_d(&z->im);
  return CC_RINGELEM(result);
}

void CC::remove_elem(M2_CC f) const
{
  //  if (f == NULL) return;
  //  CC_stash->delete_elem(f);
}

ring_elem CC::random() const
{
  int d = 1000000;
  long r = Random::random0((int32_t)(2*d));
  long s = Random::random0((int32_t)(2*d));
  double a = (r*1.0)/d - 1.0;
  double b = (s*1.0)/d - 1.0;
  return CC::from_doubles(a, b);
}

int CC::compare_CC(double a, double b) const
  // This is our notion of equality
{
  double c = a-b;
  if (c > _epsilon) return GT;
  if (c < -_epsilon) return LT;
  return EQ;
}

void CC::elem_text_out(buffer &o, const ring_elem ap) const
{
  double a = CC_RE(ap);
  double b = CC_IM(ap);

  char s[100];

  bool is_real = compare_CC(b,0.0) == EQ;
  bool is_imag = compare_CC(a,0.0) == EQ;
  bool is_neg = compare_CC(a,0.0) == LT;
  bool is_one = (compare_CC(fabs(a),1.0) == EQ &&
		 compare_CC(b,0.0) == EQ);

  if (is_one) 
    {  
      if (!is_neg && p_plus) o << '+';
      if (is_neg) o << '-';
      if (p_one) o << '1'; 
    }
  else if (is_real)
    {
      if (!is_neg && p_plus) o << '+';
      sprintf(s, "%f", a);
      o << s;
    }
  else if (is_imag)
    {
      if ((compare_CC(b,0.0) != LT) && p_plus) o << '+';
      sprintf(s, "%fii", b);
      o << s;
    }
  else 
    {
      if (p_plus) o << "+";
      if (compare_CC(b,0.0) == LT)
	{
	  sprintf(s, "(%f%fii)", a, b);
	  o << s;
	}
      else
	{
	  sprintf(s, "(%f+%fii)", a,b);
	  o << s;
	}
    }
}

ring_elem CC::from_int(int n) const
{
  double a = n;
  return CC::from_double(a);
}

ring_elem CC::from_int(mpz_ptr n) const
{
  double a = mpz_get_d(n);
  return CC::from_double(a);
}

bool CC::promote(const Ring *S, const ring_elem f, ring_elem &result) const
{
  const RingRR *T = S->cast_to_RingRR();
  if (T)
    {
      result = CC::from_double(T->to_double(f));
      return true;
    }
  return false;
}

bool CC::lift(const Ring *, const ring_elem, ring_elem &) const
{
  return false;
}

bool CC::is_zero_CC(double a) const
{
  return compare_CC(a,0.0) == EQ;
}

// Do it componentwise.
bool CC::is_zero(const ring_elem f) const
{
  return compare_CC(CC_NORM(f), 0.0) == EQ;
}

bool CC::is_unit(const ring_elem f) const
{
  return !CC::is_zero(f);
}

bool CC::is_equal(const ring_elem f, const ring_elem g) const
{
  return (compare_CC(sqrt((CC_RE(f)-CC_RE(g))*(CC_RE(f)-CC_RE(g)) +
			  (CC_IM(f)-CC_IM(g))*(CC_IM(f)-CC_IM(g))),
		     0.0) == EQ);
}

int CC::compare_elems(const ring_elem f, const ring_elem g) const
{
  return compare_CC(CC_NORM(f),CC_NORM(g));
}
int CC::is_positive(const ring_elem f) const
{
  return (compare_CC(CC_RE(f),0.0) == GT
	  && compare_CC(CC_IM(f),0.0) == GT);
}

ring_elem CC::copy(const ring_elem f) const
{
  return CC::from_complex(CCELEM_VAL(f));
}

void CC::remove(ring_elem &f) const
{
#if 0
  M2_CC a = CCELEM_VAL(f);
  remove_elem(a);
  f = CC_RINGELEM(NULL);
#endif
}

ring_elem CC::preferred_associate(ring_elem f) const
{
  if (CC::is_positive(f) >= 0) return CC::from_double(1.0);
  return CC::from_double(-1.0);
}

void CC::internal_negate_to(ring_elem &f) const
{
  M2_CC a = CCELEM_VAL(f);
  a->re = - a->re;
  a->im = - a->im;
}

void CC::internal_add_to(ring_elem &f, ring_elem &g) const
{
  M2_CC a = CCELEM_VAL(f);
  a->re += CC_RE(g);
  a->im += CC_IM(g);
  remove(g);
}

void CC::internal_subtract_to(ring_elem &f, ring_elem &g) const
{
  M2_CC a = CCELEM_VAL(f);
  a->re -= CC_RE(g);
  a->im -= CC_IM(g);
  remove(g);
}

ring_elem CC::negate(const ring_elem f) const
{
  return CC::from_doubles(-CC_RE(f), -CC_IM(f));
}

ring_elem CC::add(const ring_elem f, const ring_elem g) const
{
  return CC::from_doubles(CC_RE(f)+CC_RE(g), CC_IM(f)+CC_IM(g));
}

ring_elem CC::subtract(const ring_elem f, const ring_elem g) const
{
  return CC::from_doubles(CC_RE(f)-CC_RE(g), CC_IM(f)-CC_IM(g));
}

ring_elem CC::mult(const ring_elem f, const ring_elem g) const
{
  return CC::from_doubles(CC_RE(f)*CC_RE(g) - CC_IM(f)*CC_IM(g),
			  CC_RE(f)*CC_IM(g) + CC_IM(f)*CC_RE(g));
}

ring_elem CC::power(const ring_elem f, int n) const
{
  double a1 = CC_RE(f);
  double a2 = CC_IM(f);
  double b1 = a1;
  double b2 = a2;
  double c1, c2;
  for (int i=0; i<n; i++) {
    c1 = a1*b1 - a2*b2;
    c2 = a1*b2 + a2*b1;
    b1 = c1;
    b2 = c2;
  }
  return CC::from_doubles(b1, b2);
}
ring_elem CC::power(const ring_elem f, mpz_t n) const
{
  int n1;
  if (!RingZZ::get_si(n1, n)) 
    { ERROR("exponent too large"); }
  return CC::power(f,n1);
}

ring_elem CC::invert(const ring_elem f) const
{
  double a = CC_RE(f);
  double b = CC_IM(f);
  double det = a*a + b*b;
  return CC::from_doubles(a/det, -b/det);
}

ring_elem CC::divide(const ring_elem f, const ring_elem g) const
{
  double a = CC_RE(f);
  double b = CC_IM(f);
  double c = CC_RE(g);
  double d = CC_IM(g);
  double det = a*a + b*b;
  return CC::from_doubles((a*c+b*d)/det, (-b*c+a*d)/det);
}

void CC::syzygy(const ring_elem a, const ring_elem b,
	       ring_elem &x, ring_elem &y) const
{
  if (CC::is_zero(b))
    {
      x = CC::from_double(0.0);
      y = CC::from_double(1.0);
    }
  else 
    {
      x = CC::from_double(1.0);
      y = CC::divide(a,b);
    }
}

ring_elem CC::eval(const RingMap *map, const ring_elem f, int) const
{
  return map->get_ring()->from_complex(CCELEM_VAL(f));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
