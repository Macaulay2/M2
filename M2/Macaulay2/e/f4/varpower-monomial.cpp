/* Copyright 2006 by Michael E. Stillman */

#include "f4/varpower-monomial.hpp"
#include "engine-exports.h"  // for M2_arrayint_struct, M2_arrayint
#include "style.hpp"

varpower_word varpower_monomials::simple_degree(const_varpower_monomial m)
{
  varpower_word i;
  varpower_word sum = 0;
  varpower_word npairs = *m;
  m += 2;
  for (i = npairs; i > 0; i--, m += 2) sum += *m;
  return sum;
}

varpower_word varpower_monomials::weight(const_varpower_monomial m,
                                         M2_arrayint wts)
{
  varpower_word i;
  varpower_word sum = 0;
  varpower_word npairs = *m;
  m += 1;
  for (i = npairs; i > 0; i--, m += 2)
    if (*m >= wts->len)
      sum += m[1];
    else
      sum += m[1] * wts->array[*m];
  return sum;
}

int varpower_monomials::equal(const_varpower_monomial m1,
                              const_varpower_monomial m2)
{
  varpower_word npairs = *m1++;
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

int varpower_monomials::compare(const_varpower_monomial a,
                                const_varpower_monomial b)
// return EQ, LT, or GT for a == b, a < b, or a > b.
{
  varpower_word i;
  varpower_word alen = 2 * (*a++);
  varpower_word blen = 2 * (*b++);
  if (alen > blen)
    {
      for (i = 0; i < blen; i++)
        {
          varpower_word c = *a++ - *b++;
          if (c == 0) continue;
          if (c > 0) return GT;
          return LT;
        }
      return GT;
    }
  for (i = 0; i < alen; i++)
    {
      varpower_word c = *a++ - *b++;
      if (c == 0) continue;
      if (c > 0) return GT;
      return LT;
    }
  if (alen == blen) return EQ;
  return LT;
}

void varpower_monomials::lcm(const_varpower_monomial m1,
                             const_varpower_monomial m2,
                             varpower_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for const_varpower_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
  */

  varpower_word v1, v2, e1 = 0, e2 = 0;
  varpower_word n1 = *m1++;
  varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  varpower_word *r = result + 1;
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

void varpower_monomials::mult(const_varpower_monomial m1,
                              const_varpower_monomial m2,
                              varpower_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for const_varpower_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
  */

  /*
    As for overflows, we assume that we only consider degrees which will cover
    all
    monomials.
  */

  varpower_word v1, v2, e1 = 0, e2 = 0;
  varpower_word n1 = *m1++;
  varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  varpower_word *r = result + 1;
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
          varpower_word z =
              e1 + e2; /* we assume here that the powers are all >= 0?? */
          *r++ = v1;
          *r++ = z;
          (*result)++;
          INCR(m1, n1, v1, e1);
          INCR(m2, n2, v2, e2);
        }
    }
}

void varpower_monomials::quotient(const_varpower_monomial m1,
                                  const_varpower_monomial m2,
                                  varpower_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for const_varpower_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
     result is set to m1:m2.  No overflows can occur
  */

  varpower_word v1, v2, e1 = 0, e2 = 0;
  varpower_word n1 = *m1++;
  varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  varpower_word *r = result + 1;
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
          varpower_word z = e1 - e2;
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

int varpower_monomials::divides(const_varpower_monomial m1,
                                const_varpower_monomial m2,
                                varpower_monomial result)
{
  /* the result is only valid if 1 is returned.
     If 1 is returned: m1 divides m2, and result is set to m2-m1.
     0 is returned if m1 does not divide m2.
  */

  varpower_word v1, v2, e1 = 0, e2 = 0;
  varpower_word n1 = *m1++;
  varpower_word n2 = *m2++;

  INCR(m1, n1, v1, e1);
  INCR(m2, n2, v2, e2);

  *result = 0;
  varpower_word *r = result + 1;
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

void varpower_monomials::elem_text_out(FILE *fil, const_varpower_monomial m)
{
  varpower_word v, e;
  varpower_word n = *m++;

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
        fprintf(fil, "x[%ld]", v);
      if (e > 1)
        fprintf(fil, "%ld", e);
      else if (e < 0)
        fprintf(fil, "^(%ld)", e);
    }
}

int buchberger_moeller_keep(const_varpower_monomial m,
                            const_varpower_monomial p,
                            const_varpower_monomial q,
                            const_varpower_monomial pq)
/* These should satisfy: lcm(p,q) == pq */
/* returns 0 if the pair (p,q) should be REMOVED */
/* Returns 1 iff either (a) m does not divide pq,
   or (b) m does divide pq, and lcm(m,p) == lcm(m,q) */
{
#ifdef DEVELOPMENT
#warning "buchberger-moeller still to write"
#endif
  return 0;
}

/*
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
*/
