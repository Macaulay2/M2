/* Copyright 2006-2016 by Michael E. Stillman */

#include "schreyer-resolution/res-varpower-monomial.hpp"
#include "schreyer-resolution/res-monomial-types.hpp"  // for res_varpower_word

/* The following are also in ../style.hpp */
#define LT (-1)
#define EQ 0
#define GT 1

res_varpower_word res_varpower_monomials::simple_degree(
    res_const_varpower_monomial m)
{
  res_varpower_word i;
  res_varpower_word sum = 0;
  res_varpower_word npairs = *m;
  m += 2;
  for (i = npairs; i > 0; i--, m += 2) sum += *m;
  return sum;
}

#if 0
res_varpower_word res_varpower_monomials::weight(res_const_varpower_monomial m, M2_arrayint wts)
{
  res_varpower_word i;
  res_varpower_word sum = 0;
  res_varpower_word npairs = *m;
  m += 1;
  for (i=npairs; i>0; i--, m += 2)
    if (*m >= wts->len)
      sum += m[1];
    else
      sum += m[1] * wts->array[*m];
  return sum;
}
#endif

int res_varpower_monomials::equal(res_const_varpower_monomial m1,
                                  res_const_varpower_monomial m2)
{
  res_varpower_word npairs = *m1++;
  if (npairs != *m2++) return 0;
  npairs *= 2;
  for (; npairs > 0; npairs--)
    if (*m1++ != *m2++) return 0;
  return 1;
}

#define INCR(m, n, v, e) \
  if (n > 0)             \
    {                    \
      v = *m++;          \
      e = *m++;          \
      n--;               \
    }                    \
  else                   \
  v = -1

int res_varpower_monomials::compare(res_const_varpower_monomial a,
                                    res_const_varpower_monomial b)
// return EQ, LT, or GT for a == b, a < b, or a > b.
{
  res_varpower_word i;
  res_varpower_word alen = 2 * (*a++);
  res_varpower_word blen = 2 * (*b++);
  if (alen > blen)
    {
      for (i = 0; i < blen; i++)
        {
          res_varpower_word c = *a++ - *b++;
          if (c == 0) continue;
          if (c > 0) return GT;
          return LT;
        }
      return GT;
    }
  for (i = 0; i < alen; i++)
    {
      res_varpower_word c = *a++ - *b++;
      if (c == 0) continue;
      if (c > 0) return GT;
      return LT;
    }
  if (alen == blen) return EQ;
  return LT;
}

void res_varpower_monomials::lcm(res_const_varpower_monomial m1,
                                 res_const_varpower_monomial m2,
                                 res_varpower_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for const_res_varpower_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
  */

  res_varpower_word v1, v2, e1 = 0, e2 = 0;
  res_varpower_word n1 = *m1++;
  res_varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  res_varpower_word *r = result + 1;
  for (;;)
    {
      if (v1 > v2)
        {
          *r++ = v1;
          *r++ = e1;
          (*result)++;
          INCR(m1, n1, v1, e1);
        }
      else if (v2 > v1)
        {
          *r++ = v2;
          *r++ = e2;
          (*result)++;
          INCR(m2, n2, v2, e2);
        }
      else
        {
          if (v1 < 0) break;
          if (e1 < e2) e1 = e2;
          *r++ = v1;
          *r++ = e1;
          (*result)++;
          INCR(m1, n1, v1, e1);
          INCR(m2, n2, v2, e2);
        }
    }
}

void res_varpower_monomials::mult(res_const_varpower_monomial m1,
                                  res_const_varpower_monomial m2,
                                  res_varpower_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for const_res_varpower_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
  */

  /*
    As for overflows, we assume that we only consider degrees which will cover
    all
    monomials.
  */

  res_varpower_word v1, v2, e1 = 0, e2 = 0;
  res_varpower_word n1 = *m1++;
  res_varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  res_varpower_word *r = result + 1;
  for (;;)
    {
      if (v1 > v2)
        {
          *r++ = v1;
          *r++ = e1;
          (*result)++;
          INCR(m1, n1, v1, e1);
        }
      else if (v2 > v1)
        {
          *r++ = v2;
          *r++ = e2;
          (*result)++;
          INCR(m2, n2, v2, e2);
        }
      else
        {
          if (v1 < 0) break;
          res_varpower_word z =
              e1 + e2; /* we assume here that the powers are all >= 0?? */
          *r++ = v1;
          *r++ = z;
          (*result)++;
          INCR(m1, n1, v1, e1);
          INCR(m2, n2, v2, e2);
        }
    }
}

void res_varpower_monomials::quotient(res_const_varpower_monomial m1,
                                      res_const_varpower_monomial m2,
                                      res_varpower_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for const_res_varpower_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
     result is set to m1:m2.  No overflows can occur
  */

  res_varpower_word v1, v2, e1 = 0, e2 = 0;
  res_varpower_word n1 = *m1++;
  res_varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  res_varpower_word *r = result + 1;
  for (;;)
    {
      if (v1 > v2)
        {
          *r++ = v1;
          *r++ = e1;
          (*result)++;
          INCR(m1, n1, v1, e1);
        }
      else if (v2 > v1)
        {
          INCR(m2, n2, v2, e2);
        }
      else
        {
          if (v1 < 0) break;
          res_varpower_word z = e1 - e2;
          if (z > 0)
            {
              *r++ = v1;
              *r++ = z;
              (*result)++;
            }
          INCR(m1, n1, v1, e1);
          INCR(m2, n2, v2, e2);
        }
    }
}

int res_varpower_monomials::divides(res_const_varpower_monomial m1,
                                    res_const_varpower_monomial m2,
                                    res_varpower_monomial result)
{
  /* the result is only valid if 1 is returned.
     If 1 is returned: m1 divides m2, and result is set to m2-m1.
     0 is returned if m1 does not divide m2.
  */

  res_varpower_word v1, v2, e1 = 0, e2 = 0;
  res_varpower_word n1 = *m1++;
  res_varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  res_varpower_word *r = result + 1;
  for (;;)
    {
      if (v1 > v2)
        {
          return 0; /* m1 has a term that m2 does not */
        }
      else if (v2 > v1)
        {
          *r++ = v2;
          *r++ = e2;
          (*result)++;
          INCR(m2, n2, v2, e2);
        }
      else
        {
          if (v1 < 0) return 1;
          if (e1 > e2) return 0;
          if (e1 != e2)
            {
              *r++ = v1;
              *r++ = e2 - e1;
              (*result)++;
            }
          INCR(m1, n1, v1, e1);
          INCR(m2, n2, v2, e2);
        }
    }
}

void res_varpower_monomials::elem_text_out(FILE *fil,
                                           res_const_varpower_monomial m)
{
  res_varpower_word v, e;
  res_varpower_word n = *m++;

  if (n == 0)
    {
      fprintf(fil, "1");
      return;
    }

  while (n > 0)
    {
      INCR(m, n, v, e);
      if (v < 26)
        {
          int c = 'a' + static_cast<int>(v);
          fprintf(fil, "%c", c);
        }
      else if (v < 52)
        {
          int c = 'A' + static_cast<int>(v);
          fprintf(fil, "%c", c);
        }
      else
        fprintf(fil, "x[%d]", v);
      if (e > 1)
        fprintf(fil, "%d", e);
      else if (e < 0)
        fprintf(fil, "^(%d)", e);
    }
}

/*
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
*/
