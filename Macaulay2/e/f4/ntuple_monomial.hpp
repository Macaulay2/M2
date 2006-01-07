// Copyright 1997-2006 Michael E. Stillman

#ifndef _ntuple_monomial_hpp_
#define _ntuple_monomial_hpp_

#include "../style.hpp"
#include "../engine.h"

// This class implements monomial operations on ntuples:
// i.e. exponent vectors.  No allocation is done by any of these
// routines.  The format for an exponent vector is simply:
//  an array 0..nvars-1 of long's.  The 'nvars' information
// is not stored with the ntuple_monomial.

// CAVEAT: NO overflow checking is done with this class.

typedef long *ntuple_monomial;
typedef const long *const_ntuple_monomial;

class ntuple_monomials
{
public:
  static void copy(int nvars, 
		   const_ntuple_monomial a,
		   ntuple_monomial result);

  static void one(int nvars, 
		  ntuple_monomial result);

  static bool is_one(int nvars, 
		     const_ntuple_monomial a);

  static void mult(int nvars, 
		   const_ntuple_monomial a, 
		   const_ntuple_monomial b, 
		   ntuple_monomial result);

  static void power(int nvars, 
		    const_ntuple_monomial a, 
		    int n, 
		    ntuple_monomial result);

  static void divide(int nvars, 
		     const_ntuple_monomial a, 
		     const_ntuple_monomial b, 
		     ntuple_monomial result);
    // result = a - b

  static void quotient(int nvars, 
		       const_ntuple_monomial a, 
		       const_ntuple_monomial b, 
		       ntuple_monomial result);
    // result = max(a-b,0)

  static void lcm(int nvars, 
		  const_ntuple_monomial a, 
		  const_ntuple_monomial b, 
		  ntuple_monomial result);

  static void gcd(int nvars, 
		  const_ntuple_monomial a, 
		  const_ntuple_monomial b, 
		  ntuple_monomial result);

  static void syz(int nvars, 
		  const_ntuple_monomial a, 
		  const_ntuple_monomial b,
		  ntuple_monomial a1, 
		  ntuple_monomial a2);

  static bool divides(int nvars, 
		      const_ntuple_monomial a, 
		      const_ntuple_monomial b);

  static unsigned long mask(int nvars, 
			    const_ntuple_monomial a);

  static int lex_compare(int nvars, 
			 const_ntuple_monomial a, 
			 const_ntuple_monomial b);

  static long weight(int nvars, 
		     const_ntuple_monomial a, 
		     const M2_arrayint wt);

  static long simple_degree(int nvars, 
			   const_ntuple_monomial a);

  static void elem_text_out(buffer &o, 
			    unsigned int nvars,
			    const_ntuple_monomial a, 
			    M2_stringarray varnames);
};

inline 
void ntuple_monomials::one(int nvars, ntuple_monomial result)
{
  for (int i=0; i<nvars; i++) *result++ = 0;
}

inline 
bool ntuple_monomials::is_one(int nvars, const_ntuple_monomial a)
{
  for (int i=0; i<nvars; i++) 
    if (*a++ != 0) return false;
  return true;
}

inline
void ntuple_monomials::mult(int nvars, 
		  const_ntuple_monomial a, 
		  const_ntuple_monomial b, 
		  ntuple_monomial result)
{
  for (int i=0; i<nvars; i++)
    {
      long x = *a++;
      long y = *b++;
      long z = x+y;
      *result++ = z;
    }
}

inline
void ntuple_monomials::power(int nvars, 
			     const_ntuple_monomial a, 
			     int n, 
			     ntuple_monomial result)
{
  for (int i=0; i<nvars; i++)
    {
      long e = *a++;
      long long e1 = e;
      e *= n;
      e1 *= n;
      *result++ = e1;
    }
}

inline
void ntuple_monomials::divide(int nvars, 
			      const_ntuple_monomial a, 
			      const_ntuple_monomial b, 
			      ntuple_monomial result)
{
  for (int i=0; i<nvars; i++)
    {
      long x = *a++;
      long y = *b++;
      long z = x-y;
      *result++ = z;
    }
}

inline
void ntuple_monomials::quotient(int nvars, 
				const_ntuple_monomial a, 
				const_ntuple_monomial b, 
				ntuple_monomial result)
{
  for (int i=0; i<nvars; i++)
    {
      long x = *a++;
      long y = *b++;
      long z;
      if (x <= y) z = 0;
      else
	{
	  z = x-y;
	}
      *result++ = z;
    }
}

inline
void ntuple_monomials::lcm(int nvars, 
			   const_ntuple_monomial a, 
			   const_ntuple_monomial b, 
			   ntuple_monomial result)
{
  for (int i=0; i<nvars; i++)
    {
      long c = *a++;
      long d = *b++;
      *result++ = (c > d ? c : d);
    }
}

inline
void ntuple_monomials::gcd(int nvars, 
			   const_ntuple_monomial a, 
			   const_ntuple_monomial b, 
			   ntuple_monomial result)
{
  for (int i=0; i<nvars; i++)
    {
      long c = *a++;
      long d = *b++;
      *result++ = (c < d ? c : d);
    }
}

inline
bool ntuple_monomials::divides(int nvars, 
			       const_ntuple_monomial a, 
			       const_ntuple_monomial b)
  // Does a divide b?
{
  for (int i=0; i<nvars; i++)
    if (a[i] > b[i]) return false;
  return true;
}

inline
unsigned long ntuple_monomials::mask(int nvars, const_ntuple_monomial a)
{
  unsigned long result = 0;
  int i;
  unsigned int j;
  for (i=0, j=0; i<nvars; i++, j++)
    {
      if (j == 8*sizeof(unsigned int)) j=0;
      if (a[i] > 0)
	result |= (1 << j);
    }
  return result;
}

inline
int ntuple_monomials::lex_compare(int nvars, 
				  const_ntuple_monomial a, 
				  const_ntuple_monomial b)
{
  for (int i=0; i<nvars; i++)
    if (a[i] > b[i]) return GT;
    else if (a[i] < b[i]) return LT;
  return EQ;
}

inline
void ntuple_monomials::copy(int nvars, 
			    const_ntuple_monomial a, 
			    ntuple_monomial result)
{
  memcpy(result, a, nvars*sizeof(long));
}


inline
long ntuple_monomials::weight(int nvars, const_ntuple_monomial a, M2_arrayint wt)
{
  long sum = 0;
  int top = wt->len;
  if (nvars < top) top = nvars;
  for (int i=0; i<top; i++)
    sum += a[i] * wt->array[i];
  return sum;
}

inline
long ntuple_monomials::simple_degree(int nvars, 
				    const_ntuple_monomial a)
{
  long sum = 0;
  for (int i=0; i<nvars; i++)
    sum += a[i];
  return sum;
}

inline
void ntuple_monomials::syz(int nvars, 
			   const_ntuple_monomial a, 
			   const_ntuple_monomial b,
			   ntuple_monomial a1, 
			   ntuple_monomial b1)
{
  for (int i=0; i<nvars; i++)
    {
      long c = a[i] - b[i];
      if (c >= 0)
	{
	  a1[i] = 0;
	  b1[i] = c;
	}
      else
	{
	  a1[i] = -c;
	  b1[i] = 0;
	}
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
