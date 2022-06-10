// Copyright 2007  Michael E. Stillman

#include "complex.h"
#include "engine-includes.hpp"
#include <stdio.h>

void mpfc_init_set(gmp_CCmutable result, gmp_CCmutable a)
{
  result->re = getmemstructtype(gmp_RRmutable);
  result->im = getmemstructtype(gmp_RRmutable);
  mpfr_init_set(result->re, a->re, MPFR_RNDN);
  mpfr_init_set(result->im, a->im, MPFR_RNDN);
}

void mpfc_init(gmp_CCmutable result, long precision)
{
  result->re = getmemstructtype(gmp_RRmutable);
  result->im = getmemstructtype(gmp_RRmutable);
  mpfr_init2(result->re, precision);
  mpfr_init2(result->im, precision);
}
void mpfc_set(gmp_CCmutable result, gmp_CCmutable a)
{
  mpfr_set(result->re, a->re, MPFR_RNDN);
  mpfr_set(result->im, a->im, MPFR_RNDN);
}

void mpfc_clear(gmp_CCmutable result)
{
  mpfr_clear(result->re);
  mpfr_clear(result->im);
  freemem(result->re);
  freemem(result->im);
  //  GC_FREE(result->re);
  //  GC_FREE(result->im);
}
void mpfc_set_si(gmp_CCmutable result, long re)
{
  mpfr_set_si(result->re, re, MPFR_RNDN);
  mpfr_set_si(result->im, 0, MPFR_RNDN);
}
int mpfc_is_zero(gmp_CCmutable a)
{
  return mpfr_cmp_si(a->re, 0) == 0 && mpfr_cmp_si(a->im, 0) == 0;
}

int mpfc_is_equal(gmp_CCmutable a, gmp_CCmutable b)
{
  return mpfr_cmp(a->re, b->re) == 0 && mpfr_cmp(a->im, b->im) == 0;
}
void mpfc_add(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b)
{
  mpfr_add(result->re, a->re, b->re, MPFR_RNDN);
  mpfr_add(result->im, a->im, b->im, MPFR_RNDN);
}
void mpfc_neg(gmp_CCmutable result, gmp_CCmutable a)
{
  mpfr_neg(result->re, a->re, MPFR_RNDN);
  mpfr_neg(result->im, a->im, MPFR_RNDN);
}
void mpfc_sub(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b)
{
  mpfr_sub(result->re, a->re, b->re, MPFR_RNDN);
  mpfr_sub(result->im, a->im, b->im, MPFR_RNDN);
}
void mpfc_mul(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b)
{
  mpfr_t tmp;
  mpfr_init2(tmp, mpfr_get_prec(a->re));

  // result->re = a->re*b->re - a->im*b->im;
  mpfr_mul(tmp, a->re, b->re, MPFR_RNDN);
  mpfr_set(result->re, tmp, MPFR_RNDN);
  mpfr_mul(tmp, a->im, b->im, MPFR_RNDN);
  mpfr_sub(result->re, result->re, tmp, MPFR_RNDN);

  // result->im = a->re*b->im + a->im*b->re;
  mpfr_mul(tmp, a->re, b->im, MPFR_RNDN);
  mpfr_set(result->im, tmp, MPFR_RNDN);
  mpfr_mul(tmp, a->im, b->re, MPFR_RNDN);
  mpfr_add(result->im, result->im, tmp, MPFR_RNDN);

  mpfr_clear(tmp);
}
void mpfc_invert(gmp_CCmutable result, gmp_CCmutable v)
{
  mpfr_t p, denom;
  mpfr_init2(p, mpfr_get_prec(v->re));
  mpfr_init2(denom, mpfr_get_prec(v->re));

  if (mpfr_cmpabs(v->re, v->im) >= 0)
    {
      // double p = v->im/v->re;
      // double denom = v->re + p * v->im;
      // result->re = 1.0/denom;
      // result->im = - p/denom;

      mpfr_div(p, v->im, v->re, MPFR_RNDN);
      mpfr_mul(denom, p, v->im, MPFR_RNDN);
      mpfr_add(denom, denom, v->re, MPFR_RNDN);
      mpfr_si_div(result->re, 1, denom, MPFR_RNDN);
      mpfr_div(result->im, p, denom, MPFR_RNDN);
      mpfr_neg(result->im, result->im, MPFR_RNDN);
    }
  else
    {
      // double p = v->re/v->im;
      // double denom = v->im + p * v->re;
      // result->re = p/denom;
      // result->im = -1.0/denom;

      mpfr_div(p, v->re, v->im, MPFR_RNDN);
      mpfr_mul(denom, p, v->re, MPFR_RNDN);
      mpfr_add(denom, denom, v->im, MPFR_RNDN);
      mpfr_si_div(result->im, 1, denom, MPFR_RNDN);
      mpfr_neg(result->im, result->im, MPFR_RNDN);
      mpfr_div(result->re, p, denom, MPFR_RNDN);
    }

  mpfr_clear(p);
  mpfr_clear(denom);
}
void mpfc_div(gmp_CCmutable result, gmp_CCmutable u, gmp_CCmutable v)
{
  mpfr_t p, denom;
  mpfr_init2(p, mpfr_get_prec(u->re));
  mpfr_init2(denom, mpfr_get_prec(u->re));

  printf("not expected to be used -- we have a bug here\n");
  abort();
  if (mpfr_cmpabs(v->re, v->im) >= 0)
    {
      // for v = c + d*i,
      // p = d/c
      // c+di = c(1+p*i), so denom is c(1+p^2)
      // which is c + d*p

      // double p = v.im/v.re;
      // double denom = v.re + p * v.im;
      // result.re = (u.re + p*u.im)/denom;
      // result.im = (u.im - p*u.re)/denom;

      mpfr_div(p, v->im, v->re, MPFR_RNDN);
      mpfr_mul(denom, p, v->im, MPFR_RNDN);
      mpfr_add(denom, denom, v->re, MPFR_RNDN);

      mpfr_mul(result->re, p, u->im, MPFR_RNDN);
      mpfr_add(result->re, result->re, u->re, MPFR_RNDN);
      mpfr_div(result->re, result->re, denom, MPFR_RNDN);

      mpfr_mul(result->im, p, u->re, MPFR_RNDN);
      mpfr_neg(result->im, result->re, MPFR_RNDN);
      mpfr_add(result->im, result->re, u->im, MPFR_RNDN);
      mpfr_div(result->im, result->re, denom, MPFR_RNDN);
    }
  else
    {
      // double p = v.re/v.im;
      // double denom = v.im + p * v.re;
      // result.re = (u.re * p + u.im)/denom;
      // result.im = (-u.re + p * u.im)/denom;

      mpfr_div(p, v->re, v->im, MPFR_RNDN);
      mpfr_mul(denom, p, v->re, MPFR_RNDN);
      mpfr_add(denom, denom, v->im, MPFR_RNDN);

      mpfr_mul(result->re, p, u->re, MPFR_RNDN);
      mpfr_add(result->re, result->re, u->im, MPFR_RNDN);
      mpfr_div(result->re, result->re, denom, MPFR_RNDN);

      mpfr_mul(result->im, p, u->im, MPFR_RNDN);
      mpfr_sub(result->im, result->re, u->re, MPFR_RNDN);
      mpfr_div(result->im, result->re, denom, MPFR_RNDN);
    }

  mpfr_clear(p);
  mpfr_clear(denom);
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
void mpfc_sub_mult(gmp_CCmutable result, gmp_CCmutable a, gmp_CCmutable b)
{
  // result->re -= a->re*b->re - a->im*b->im;
  // result->im -= a->re*b->im + a->im*b->re;

  mpfr_t tmp;
  mpfr_init2(tmp, mpfr_get_prec(a->re));

  mpfr_mul(tmp, a->re, b->re, MPFR_RNDN);
  mpfr_add(result->re, result->re, tmp, MPFR_RNDN);
  mpfr_mul(tmp, a->im, b->im, MPFR_RNDN);
  mpfr_sub(result->re, result->re, tmp, MPFR_RNDN);

  mpfr_mul(tmp, a->re, b->im, MPFR_RNDN);
  mpfr_add(result->im, result->im, tmp, MPFR_RNDN);
  mpfr_mul(tmp, a->im, b->re, MPFR_RNDN);
  mpfr_add(result->im, result->im, tmp, MPFR_RNDN);

  mpfr_clear(tmp);
}
void mpfc_conj(gmp_CCmutable result, gmp_CCmutable a)
{
  mpfr_set(result->re, a->re, MPFR_RNDN);
  mpfr_neg(result->im, a->im, MPFR_RNDN);
}
void mpfc_abs(gmp_RRmutable result, gmp_CCmutable c)
{
  mpfr_t a, b;

  mpfr_init2(a, mpfr_get_prec(c->re));
  mpfr_init2(b, mpfr_get_prec(c->re));
  mpfr_abs(a, c->re, MPFR_RNDN);
  mpfr_abs(b, c->im, MPFR_RNDN);
  if (mpfr_zero_p(a))
    mpfr_set(result, b, MPFR_RNDN);
  else if (mpfr_zero_p(b))
    mpfr_set(result, a, MPFR_RNDN);
  else if (mpfr_greater_p(a, b))
    {
      // double d = b/a;
      // But use b for d, as it is not needed later.
      // result = a * ::sqrt(1.0 + d*d);
      mpfr_div(b, b, a, MPFR_RNDN);
      mpfr_sqr(b, b, MPFR_RNDN);
      mpfr_add_si(b, b, 1, MPFR_RNDN);
      mpfr_sqrt(b, b, MPFR_RNDN);
      mpfr_mul(result, a, b, MPFR_RNDN);
    }
  else
    {
      // double d = a/b;
      // But use a for d, as it is not needed later.
      // result = b * ::sqrt(1.0 + d*d);
      mpfr_div(a, a, b, MPFR_RNDN);
      mpfr_sqr(a, a, MPFR_RNDN);
      mpfr_add_si(a, a, 1, MPFR_RNDN);
      mpfr_sqrt(a, a, MPFR_RNDN);
      mpfr_mul(result, b, a, MPFR_RNDN);
    }
  mpfr_clear(a);
  mpfr_clear(b);

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
void mpfc_sqrt(gmp_CCmutable result, gmp_CC a)
{
  // The idea is: write a = a1 + i * a2
  // first make it numerically more stable by dividing by the larger
  //   answer will be multiplied by larger afterwords, if needed
  // To take the square root of 1+di, or -1+di
  //   it is enough to solve (if sqrt is e+fi), e^2+f^2 = sqrt(1+d^2),
  //   and e^2-f^2 = 1 (or -1).

  if (mpfr_zero_p(a->re) && mpfr_zero_p(a->im))
    {
      mpfr_set_si(result->re, 0, MPFR_RNDN);
      mpfr_set_si(result->im, 0, MPFR_RNDN);
      return;
    }

  mpfr_t b, c, d, d2;

  unsigned long p = mpfr_get_prec(a->re);
  mpfr_init2(b, p);
  mpfr_init2(c, p);
  mpfr_init2(d, p);
  mpfr_init2(d2, p);

  // b = fabs(a.re);
  // c = fabs(a.im);
  mpfr_abs(b, a->re, MPFR_RNDN);
  mpfr_abs(c, a->im, MPFR_RNDN);
  if (mpfr_greater_p(b, c))
    {
      // d = c/b;
      // e = ::sqrt(b) * ::sqrt(0.5 * (1.0 + ::sqrt(1.0 + d*d)));
      // but use c as d, and also as e
      mpfr_div(d, c, b, MPFR_RNDN);
      mpfr_sqr(d, d, MPFR_RNDN);
      mpfr_add_si(d, d, 1, MPFR_RNDN);
      mpfr_sqrt(d, d, MPFR_RNDN);
      mpfr_add_si(d, d, 1, MPFR_RNDN);
      mpfr_div_2ui(d, d, 1, MPFR_RNDN);
      mpfr_sqrt(d, d, MPFR_RNDN);
      mpfr_sqrt(b, b, MPFR_RNDN);
      mpfr_mul(d, d, b, MPFR_RNDN); /* this is now e ! */
    }
  else
    {
      // d = b/c;
      // e = ::sqrt(c) * ::sqrt(0.5 * (d + ::sqrt(1.0 + d*d)));
      mpfr_div(d, b, c, MPFR_RNDN);
      mpfr_sqr(d2, d, MPFR_RNDN);
      mpfr_add_si(d2, d2, 1, MPFR_RNDN);
      mpfr_sqrt(d2, d2, MPFR_RNDN);
      mpfr_add(d, d2, d, MPFR_RNDN);
      mpfr_div_2ui(d, d, 1, MPFR_RNDN);
      mpfr_sqrt(d, d, MPFR_RNDN);
      mpfr_sqrt(c, c, MPFR_RNDN);
      mpfr_mul(d, d, c, MPFR_RNDN); /* this is now e ! */
    }
  if (mpfr_sgn(a->re) >= 0)
    {
      // result.re = e;
      // result.im = a.im/(2.0 * e);
      mpfr_set(result->re, d, MPFR_RNDN);
      mpfr_mul_2ui(d, d, 1, MPFR_RNDN);
      mpfr_div(result->im, a->im, d, MPFR_RNDN);
    }
  else
    {
      // result.im = (a.im >= 0.0 ? e : -e);
      // result.re = a.re/(2.0*result.im);
      mpfr_set(result->im, d, MPFR_RNDN);
      if (mpfr_sgn(a->im) < 0) mpfr_neg(result->im, result->im, MPFR_RNDN);
      mpfr_mul_2ui(d, result->im, 1, MPFR_RNDN);
      mpfr_div(result->re, a->im, d, MPFR_RNDN);
    }

  mpfr_clear(b);
  mpfr_clear(c);
  mpfr_clear(d);
  mpfr_clear(d2);

#if 0
  static void sqrt(elem &result, elem a)
  {
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
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
