// Copyright 2007  Michael E. Stillman

#ifndef _complex_hpp_
#define _complex_hpp_
#include "../d/M2types.h"

class CCArithmetic
{
public:
  typedef M2_CC_struct elem; // components are re, im

  static void init_set(elem &result, elem a)
  { 
    result = a; 
  }

  static void set_zero(elem &result)
  { 
    result.re = 0.0; result.im = 0.0; 
  }

  static bool is_zero(elem result)
  { 
    // This code should change...?  to allow nearby values to be 0
    return result.re == 0.0 && result.im == 0.0; 
  }

  static void invert(elem &result, elem v)
  {
    if (fabs(v.re) >= fabs(v.im))
      {
	double p = v.im/v.re;
	double denom = v.re + p * v.im;
	result.re = 1.0/denom;
	result.im = - p/denom;
      }
    else
      {
	double p = v.re/v.im;
	double denom = v.im + p * v.re;
	result.re = p/denom;
	result.im = -1.0/denom;
      }
  }

  static void subtract_multiple(elem &result, elem a, elem b)
  {
    result.re -= a.re*b.re - a.im*b.im;
    result.im -= a.re*b.im + a.im*b.re;
  }

  static void add(elem &result, elem a, elem b)
  {
    result.re = a.re + b.re;
    result.im = a.im + b.im;
  }

  static void subtract(elem &result, elem a, elem b)
  {
    result.re = a.re - b.re;
    result.im = a.im - b.im;
  }

  static void mult(elem &result, elem a, elem b)
  {
    result.re = a.re*b.re - a.im*b.im;
    result.im = a.re*b.im + a.im*b.re;
  }

  static void divide(elem &result, elem u, elem v)
  {
    if (fabs(v.re) >= fabs(v.im))
      {
	// for u = c + d*i,
	// p = d/c
	// c+di = c(1+p*i), so denom is c(1+p^2)
	// which is c + d*p
	double p = v.im/v.re;
	double denom = v.re + p * v.im;
	result.re = (u.re + p*u.im)/denom;
	result.im = (u.im - p*u.re)/denom;
      }
    else
      {
	double p = v.re/v.im;
	double denom = v.im + p * v.re;
	result.re = (u.re * p + u.im)/denom;
	result.im = (-u.re + p * u.im)/denom;
      }
  }

  // Routines more specific to complex arithmetic:
  static void conjugate(elem &result, elem a)
  {
    result.re = a.re;
    result.im = -a.im;
  }
  
  static void abs(double &result, elem c)
  {
    double a = fabs(c.re);
    double b = fabs(c.im);
    if (a == 0.0) result = b;
    else if (b == 0.0) result = a;
    else if (a > b)
      {
	double d = b/a;
	result = a * ::sqrt(1.0 + d*d);
      }
    else
      {
	double d = a/b;
	result = b * ::sqrt(1.0 + d*d);
      }
  }
  
  static void sqrt(elem &result, elem a)
  {
    // The idea is: write a = a1 + i * a2
    // first make it numerically more stable by dividing by the larger
    //   answer will be multiplied by larger afterwords, if needed
    // To take the square root of 1+di, or -1+di
    //   it is enough to solve (if sqrt is e+fi), e^2+f^2 = sqrt(1+d^2),
    //   and e^2-f^2 = 1 (or -1).
    double b,c,d,e;
    if (a.re == 0.0 && a.im == 0.0)
      {
	result.re = 0.0;
	result.im = 0.0;
      }
    else 
      {
	b = fabs(a.re);
	c = fabs(a.im);
	if (b > c)
	  {
	    d = c/b;
	    e = ::sqrt(b) * ::sqrt(0.5 * (1.0 + ::sqrt(1.0 + d*d)));
	  }
	else
	  {
	    d = b/c;
	    e = ::sqrt(c) * ::sqrt(0.5 * (d + ::sqrt(1.0 + d*d)));
	  }
	// Now be careful with the signs:
	if (a.re >= 0.0)
	  {
	    result.re = e;
	    result.im = a.im/(2.0 * e);
	  }
	else
	  {
	    result.im = (a.im >= 0.0 ? e : -e);
	    result.re = a.re/(2.0*result.im);
	  }
      }
  }
};

class CCCArithmetic
{
public:
  typedef M2_CCC_struct elem; // components are re, im

  static void init_set(elem &result, elem a)
  { 
    mpfr_init_set(result.re, a.re, GMP_RNDN);
    mpfr_init_set(result.im, a.im, GMP_RNDN);
  }

  static void set_zero(elem &result)
  { 
    mpfr_set_si(result.re, 0, GMP_RNDN);
    mpfr_set_si(result.im, 0, GMP_RNDN);
  }

  static bool is_zero(elem result)
  { 
    return mpfr_cmp_si(result.re, 0) == 0 && mpfr_cmp_si(result.im, 0) == 0;
  }

  static void add(elem &result, elem a, elem b)
  {
    mpfr_add(result.re, a.re, b.re, GMP_RNDN);
    mpfr_add(result.im, a.im, b.im, GMP_RNDN);
  }

  static void subtract(elem &result, elem a, elem b)
  {
    mpfr_sub(result.re, a.re, b.re, GMP_RNDN);
    mpfr_sub(result.im, a.im, b.im, GMP_RNDN);
  }

  static void mult(elem &result, elem a, elem b)
  {
    mpfr_t tmp;
    mpfr_init2(tmp, mpfr_get_prec(a.re));

    // result.re = a.re*b.re - a.im*b.im;
    mpfr_mul(tmp,a.re,b.re,GMP_RNDN);
    mpfr_add(result.re,result.re,tmp,GMP_RNDN);
    mpfr_mul(tmp,a.im,b.im,GMP_RNDN);
    mpfr_sub(result.re,result.re,tmp,GMP_RNDN);

    // result.im = a.re*b.im + a.im*b.re;
    mpfr_mul(tmp,a.re,b.im,GMP_RNDN);
    mpfr_add(result.im,result.im,tmp,GMP_RNDN);
    mpfr_mul(tmp,a.im,b.re,GMP_RNDN);
    mpfr_add(result.im,result.im,tmp,GMP_RNDN);

    mpfr_clear(tmp);
  }

  static void invert(elem &result, elem v)
  {
    mpfr_t p, denom;
    mpfr_init2(p, mpfr_get_prec(v.re));
    mpfr_init2(denom, mpfr_get_prec(v.re));

    if (mpfr_cmpabs(v.re,v.im) >= 0)
      {
	// double p = v.im/v.re;
	// double denom = v.re + p * v.im;
	// result.re = 1.0/denom;
	// result.im = - p/denom;

	mpfr_div(p,v.im,v.re,GMP_RNDN);
	mpfr_mul(denom,p,v.im,GMP_RNDN);
	mpfr_add(denom,denom,v.re,GMP_RNDN);
	mpfr_si_div(result.re,1,denom,GMP_RNDN);
	mpfr_div(result.im,p,denom,GMP_RNDN);
	mpfr_neg(result.im,result.im,GMP_RNDN);
      }
    else
      {
	// double p = v.re/v.im;
	// double denom = v.im + p * v.re;
	// result.re = p/denom;
	// result.im = -1.0/denom;

	mpfr_div(p,v.re,v.im,GMP_RNDN);
	mpfr_mul(denom,p,v.re,GMP_RNDN);
	mpfr_add(denom,denom,v.im,GMP_RNDN);
	mpfr_si_div(result.im,1,denom,GMP_RNDN);
	mpfr_neg(result.im,result.im,GMP_RNDN);
	mpfr_div(result.re,p,denom,GMP_RNDN);
      }

    mpfr_clear(p);
    mpfr_clear(denom);
  }

  static void subtract_multiple(elem &result, elem a, elem b)
  {
    // result.re -= a.re*b.re - a.im*b.im;
    // result.im -= a.re*b.im + a.im*b.re;

    mpfr_t tmp;
    mpfr_init2(tmp, mpfr_get_prec(a.re));

    mpfr_mul(tmp,a.re,b.re,GMP_RNDN);
    mpfr_add(result.re,result.re,tmp,GMP_RNDN);
    mpfr_mul(tmp,a.im,b.im,GMP_RNDN);
    mpfr_sub(result.re,result.re,tmp,GMP_RNDN);

    mpfr_mul(tmp,a.re,b.im,GMP_RNDN);
    mpfr_add(result.im,result.im,tmp,GMP_RNDN);
    mpfr_mul(tmp,a.im,b.re,GMP_RNDN);
    mpfr_add(result.im,result.im,tmp,GMP_RNDN);

    mpfr_clear(tmp);
  }

  static void divide(elem &result, elem u, elem v)
  {
#warning "commented out 'divide'"
#if 0
    mpfr_t tmp1, tmp2;
    if (mpfr_cmpabs(v.re,v.im) >= 0)
      {
	// for u = c + d*i,
	// p = d/c
	// c+di = c(1+p*i), so denom is c(1+p^2)
	// which is c + d*p

	mpfr_init(tmp1, mpfr_precision(v.re), GMP_RNDN);
	mpfr_set(tmp2,v.re, GMP_RNDN);
	mpfr_div(tmp1,v.im,v.re);

	double p = v.im/v.re;
	double denom = v.re + p * v.im;
	result.re = (u.re + p*u.im)/denom;
	result.im = (u.im - p*u.re)/denom;
      }
    else
      {
	double p = v.re/v.im;
	double denom = v.im + p * v.re;
	result.re = (u.re * p + u.im)/denom;
	result.im = (-u.re + p * u.im)/denom;
      }
#endif
  }

  // Routines more specific to complex arithmetic:
  static void conjugate(elem &result, elem a)
  {
    mpfr_set(result.re, a.re, GMP_RNDN);
    mpfr_neg(result.im, a.im, GMP_RNDN);
  }

#warning "commented out JUST FOR NOW these complex arithmetic routines"
#if 0
  static void abs(double &result, elem c)
  {
    double a = fabs(c.re);
    double b = fabs(c.im);
    if (a == 0.0) result = b;
    else if (b == 0.0) result = a;
    else if (a > b)
      {
	double d = b/a;
	result = a * ::sqrt(1.0 + d*d);
      }
    else
      {
	double d = a/b;
	result = b * ::sqrt(1.0 + d*d);
      }
  }
  
  static void sqrt(elem &result, elem a)
  {
    // The idea is: write a = a1 + i * a2
    // first make it numerically more stable by dividing by the larger
    //   answer will be multiplied by larger afterwords, if needed
    // To take the square root of 1+di, or -1+di
    //   it is enough to solve (if sqrt is e+fi), e^2+f^2 = sqrt(1+d^2),
    //   and e^2-f^2 = 1 (or -1).
    double b,c,d,e;
    if (a.re == 0.0 && a.im == 0.0)
      {
	result.re = 0.0;
	result.im = 0.0;
      }
    else 
      {
	b = fabs(a.re);
	c = fabs(a.im);
	if (b > c)
	  {
	    d = c/b;
	    e = ::sqrt(b) * ::sqrt(0.5 * (1.0 + ::sqrt(1.0 + d*d)));
	  }
	else
	  {
	    d = b/c;
	    e = ::sqrt(c) * ::sqrt(0.5 * (d + ::sqrt(1.0 + d*d)));
	  }
	// Now be careful with the signs:
	if (a.re >= 0.0)
	  {
	    result.re = e;
	    result.im = a.im/(2.0 * e);
	  }
	else
	  {
	    result.im = (a.im >= 0.0 ? e : -e);
	    result.re = a.re/(2.0*result.im);
	  }
      }
  }
#endif
};
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
