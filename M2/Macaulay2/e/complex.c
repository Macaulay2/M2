// Copyright 2007  Michael E. Stillman

// get declarations of outofmem and getmem
#include "../d/M2mem.h"

#include <gc/gc.h>
#include "../d/debug.h"

#include "../d/M2types.h"
#include "complex.h"

void mpfc_init_set(M2_CCC result, M2_CCC a)
{ 
  mpfr_init_set(result->re, a->re, GMP_RNDN);
  mpfr_init_set(result->im, a->im, GMP_RNDN);
}

void mpfc_init(M2_CCC result, long precision)
{ 
  result->re = (__mpfr_struct *) getmem(sizeof(__mpfr_struct));
  result->im = (__mpfr_struct *) getmem(sizeof(__mpfr_struct));
  mpfr_init2(result->re, precision);
  mpfr_init2(result->im, precision);
}
void mpfc_clear(M2_CCC result)
{
  mpfr_clear(result->re);
  mpfr_clear(result->im);
  GC_FREE(result->re);
  GC_FREE(result->im);
}
void mpfc_set_si(M2_CCC result, long re)
{ 
  mpfr_set_si(result->re, re, GMP_RNDN);
  mpfr_set_si(result->im, 0, GMP_RNDN);
}
int mpfc_is_zero(M2_CCC a)
{ 
  return mpfr_cmp_si(a->re, 0) == 0 && mpfr_cmp_si(a->im, 0) == 0;
}
void mpfc_add(M2_CCC result, M2_CCC a, M2_CCC b)
{
  mpfr_add(result->re, a->re, b->re, GMP_RNDN);
  mpfr_add(result->im, a->im, b->im, GMP_RNDN);
}
void mpfc_sub(M2_CCC result, M2_CCC a, M2_CCC b)
{
  mpfr_sub(result->re, a->re, b->re, GMP_RNDN);
  mpfr_sub(result->im, a->im, b->im, GMP_RNDN);
}
void mpfc_mul(M2_CCC result, M2_CCC a, M2_CCC b)
{
  mpfr_t tmp;
  mpfr_init2(tmp, mpfr_get_prec(a->re));
  
  // result->re = a->re*b->re - a->im*b->im;
  mpfr_mul(tmp,a->re,b->re,GMP_RNDN);
  mpfr_add(result->re,result->re,tmp,GMP_RNDN);
  mpfr_mul(tmp,a->im,b->im,GMP_RNDN);
  mpfr_sub(result->re,result->re,tmp,GMP_RNDN);
  
  // result->im = a->re*b->im + a->im*b->re;
  mpfr_mul(tmp,a->re,b->im,GMP_RNDN);
  mpfr_add(result->im,result->im,tmp,GMP_RNDN);
  mpfr_mul(tmp,a->im,b->re,GMP_RNDN);
  mpfr_add(result->im,result->im,tmp,GMP_RNDN);
  
  mpfr_clear(tmp);
}
void mpfc_invert(M2_CCC result, M2_CCC v)
{
  mpfr_t p, denom;
  mpfr_init2(p, mpfr_get_prec(v->re));
  mpfr_init2(denom, mpfr_get_prec(v->re));
  
  if (mpfr_cmpabs(v->re,v->im) >= 0)
    {
      // double p = v->im/v->re;
      // double denom = v->re + p * v->im;
      // result->re = 1.0/denom;
      // result->im = - p/denom;
      
      mpfr_div(p,v->im,v->re,GMP_RNDN);
      mpfr_mul(denom,p,v->im,GMP_RNDN);
      mpfr_add(denom,denom,v->re,GMP_RNDN);
      mpfr_si_div(result->re,1,denom,GMP_RNDN);
      mpfr_div(result->im,p,denom,GMP_RNDN);
      mpfr_neg(result->im,result->im,GMP_RNDN);
    }
  else
    {
      // double p = v->re/v->im;
      // double denom = v->im + p * v->re;
      // result->re = p/denom;
      // result->im = -1.0/denom;
      
      mpfr_div(p,v->re,v->im,GMP_RNDN);
      mpfr_mul(denom,p,v->re,GMP_RNDN);
      mpfr_add(denom,denom,v->im,GMP_RNDN);
      mpfr_si_div(result->im,1,denom,GMP_RNDN);
      mpfr_neg(result->im,result->im,GMP_RNDN);
      mpfr_div(result->re,p,denom,GMP_RNDN);
    }
  
  mpfr_clear(p);
  mpfr_clear(denom);
}
void mpfc_div(M2_CCC result, M2_CCC a, M2_CCC b)
{
  /* To be written !! */
#if 0
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
#endif
}
void mpfc_sub_mult(M2_CCC result, M2_CCC a, M2_CCC b)
  {
    // result->re -= a->re*b->re - a->im*b->im;
    // result->im -= a->re*b->im + a->im*b->re;

    mpfr_t tmp;
    mpfr_init2(tmp, mpfr_get_prec(a->re));

    mpfr_mul(tmp,a->re,b->re,GMP_RNDN);
    mpfr_add(result->re,result->re,tmp,GMP_RNDN);
    mpfr_mul(tmp,a->im,b->im,GMP_RNDN);
    mpfr_sub(result->re,result->re,tmp,GMP_RNDN);

    mpfr_mul(tmp,a->re,b->im,GMP_RNDN);
    mpfr_add(result->im,result->im,tmp,GMP_RNDN);
    mpfr_mul(tmp,a->im,b->re,GMP_RNDN);
    mpfr_add(result->im,result->im,tmp,GMP_RNDN);

    mpfr_clear(tmp);
  }
void mpfc_conj(M2_CCC result, M2_CCC a)
  {
    mpfr_set(result->re, a->re, GMP_RNDN);
    mpfr_neg(result->im, a->im, GMP_RNDN);
  }
void mpfc_abs(M2_CCC result, M2_CCC a)
{
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
#endif
}
void mpfc_sqrt(M2_CCC result, M2_CCC a)
{
#if 0  
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
#endif
  }


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
