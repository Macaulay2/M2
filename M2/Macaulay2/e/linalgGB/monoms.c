#include "monoms.h"
/* The following are also in ../style.hpp */
#define LT (-1)
#define EQ 0
#define GT 1

/**********************************
 * uninterned monomial operations *
 **********************************/

int monomial_simple_degree(uninterned_monomial m)
{
  int i;
  int sum = 0;
  int npairs = *m;
  m += 2;
  for (i=npairs; i>0; i--, m += 2)
    sum += *m;
  return sum;
}

int monomial_weight(uninterned_monomial m, M2_arrayint wts)
{
  int i;
  int sum = 0;
  int npairs = *m;
  m += 1;
  for (i=npairs; i>0; i--, m += 2)
    if (*m >= wts->len)
      sum += m[1];
    else
      sum += m[1] * wts->array[*m];
  return sum;
}

int monomial_equal(uninterned_monomial m1,
		   uninterned_monomial m2)
{
  int npairs = *m1++;
  if (npairs != *m2++) return 0;
  npairs *= 2;
  for ( ; npairs > 0; npairs--)
    if (*m1++ != *m2++) return 0;
  return 1;
}

#define INCR(m,n,v,e) \
  if (n > 0)                     \
    {                            \
      v = *m++;                  \
      e = *m++;                  \
      n--;                       \
  } else                         \
      v = -1

int monomial_compare(uninterned_monomial a, uninterned_monomial b)
    // return EQ, LT, or GT for a == b, a < b, or a > b.
{
  int i;
  int alen = 2*(*a++);
  int blen = 2*(*b++);
  if (alen > blen)
    {
      for (i=0; i<blen; i++)
	{
	  int c = *a++ - *b++;
	  if (c == 0) continue;
	  if (c > 0) return GT;
	  return LT;
	}
      return GT;
    }
  for (i=0; i<alen; i++)
    {
      int c = *a++ - *b++;
      if (c == 0) continue;
      if (c > 0) return GT;
      return LT;
    }
  if (alen == blen)
    return EQ;
  return LT;
}  
  
void monomial_lcm(uninterned_monomial m1, 
		  uninterned_monomial m2,
		  uninterned_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for uninterned_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
  */

  int v1,v2,e1,e2;
  int n1 = *m1++;
  int n2 = *m2++;

  INCR(m1,n1,v1,e1);
  INCR(m2,n2,v2,e2);

  *result = 0;
  int *r = result+1;
  for(;;)
  {
    if (v1 > v2)
      {
	*r++ = v1;
	*r++ = e1;
	(*result)++;
	INCR(m1,n1,v1,e1);
      }
    else if (v2 > v1)
      {
	*r++ = v2;
	*r++ = e2;
	(*result)++;
	INCR(m2,n2,v2,e2);
      }
    else
      {
	if (v1 < 0) break;
	if (e1 < e2) e1 = e2;
	*r++ = v1;
	*r++ = e1;
	(*result)++;
	INCR(m1,n1,v1,e1);
	INCR(m2,n2,v2,e2);
      }
  }
}

void monomial_mult(uninterned_monomial m1, 
		   uninterned_monomial m2,
		   uninterned_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for uninterned_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
  */

  /*
    As for overflows, we assume that we only consider degrees which will cover all
    monomials.
  */

  int v1,v2,e1,e2;
  int n1 = *m1++;
  int n2 = *m2++;

  INCR(m1,n1,v1,e1);
  INCR(m2,n2,v2,e2);

  *result = 0;
  int *r = result+1;
  for(;;)
  {
    if (v1 > v2)
      {
	*r++ = v1;
	*r++ = e1;
	(*result)++;
	INCR(m1,n1,v1,e1);
      }
    else if (v2 > v1)
      {
	*r++ = v2;
	*r++ = e2;
	(*result)++;
	INCR(m2,n2,v2,e2);
      }
    else
      {
	if (v1 < 0) break;
	long z = e1 + e2; /* we assume here that the powers are all >= 0?? */
	*r++ = v1;
	*r++ = z;
	(*result)++;
	INCR(m1,n1,v1,e1);
	INCR(m2,n2,v2,e2);
      }
  }
}

void monomial_quotient(uninterned_monomial m1, 
		       uninterned_monomial m2,
		       uninterned_monomial result)
{
  /* result: should be a pointer to a spot with enough space for the result */
  /* the format for uninterned_monomials:
     [npairs, v1, e1, v2, e2, ..., vn,en]
     so: length is 2*npairs+1 (in ints)
     result is set to m1:m2.  No overflows can occur
  */

  int v1,v2,e1,e2;
  int n1 = *m1++;
  int n2 = *m2++;

  INCR(m1,n1,v1,e1);
  INCR(m2,n2,v2,e2);

  *result = 0;
  int *r = result+1;
  for(;;)
  {
    if (v1 > v2)
      {
	*r++ = v1;
	*r++ = e1;
	(*result)++;
	INCR(m1,n1,v1,e1);
      }
    else if (v2 > v1)
      {
	INCR(m2,n2,v2,e2);
      }
    else
      {
	if (v1 < 0) break;
	long z = e1-e2;
	if (z > 0)
	  {
	    *r++ = v1;
	    *r++ = z;
	    (*result)++;
	  }
	INCR(m1,n1,v1,e1);
	INCR(m2,n2,v2,e2);
      }
  }
}

int monomial_divides(uninterned_monomial m1, 
		     uninterned_monomial m2,
		     uninterned_monomial result)
{
  /* the result is only valid if 1 is returned.
     If 1 is returned: m1 divides m2, and result is set to m2-m1.
     0 is returned if m1 does not divide m2.
  */

  int v1,v2,e1,e2;
  int n1 = *m1++;
  int n2 = *m2++;

  INCR(m1,n1,v1,e1);
  INCR(m2,n2,v2,e2);

  *result = 0;
  int *r = result+1;
  for(;;)
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
	INCR(m2,n2,v2,e2);
      }
    else
      {
	if (v1 < 0) return 1;
	if (e1 > e2) return 0;
	if (e1 != e2)
	  {
	    *r++ = v1;
	    *r++ = e2-e1;
	    (*result)++;
	  }
	INCR(m1,n1,v1,e1);
	INCR(m2,n2,v2,e2);
      }
  }
}

void monomial_elem_text_out(FILE *fil, uninterned_monomial m)
{
  int v,e;
  int n = *m++;

  if (n == 0)
    {
      fprintf(fil, "1");
      return;
    }

  while (n > 0) {
    INCR(m,n,v,e);
    if (v < 26) fprintf(fil, "%c", 'a'+v);
    else if (v < 52) fprintf(fil, "%c", 'A'+v);
    else fprintf(fil, "x[%d]",v);
    if (e > 1)
      fprintf(fil, "%d", e);
    else if (e < 0)
      fprintf(fil, "^(%d)", e);
  }
}

int buchberger_moeller_keep(uninterned_monomial m,
			    uninterned_monomial p,
			    uninterned_monomial q,
			    uninterned_monomial pq)
     /* These should satisfy: lcm(p,q) == pq */
     /* returns 0 if the pair (p,q) should be REMOVED */
     /* Returns 1 iff either (a) m does not divide pq,
	or (b) m does divide pq, and lcm(m,p) == lcm(m,q) */
{
#warning "buchberger-moeller still to write"
  return 0;
}

/*
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB "
//  End:
*/
