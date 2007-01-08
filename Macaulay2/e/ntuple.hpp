// Copyright 1997 Michael E. Stillman

#ifndef _ntuple_hpp_
#define _ntuple_hpp_

#include "style.hpp"
#include "engine.h"
#include "exceptions.hpp"

typedef int *exponents;

class ntuple
{
public:
  static void one(int nvars, int *result);
  static bool is_one(int nvars, int *a);
  static void mult(int nvars, const int *a, const int *b, int *result);
  static void power(int nvars, const int *a, int n, int *result);
  static void divide(int nvars, const int *a, const int *b, int *result);
    // result = a - b
  static void quotient(int nvars, const int *a, const int *b, int *result);
    // result = max(a-b,0)
  static void lcm(int nvars, const int *a, const int *b, int *result);
  static void gcd(int nvars, const int *a, const int *b, int *result);
  static void syz(int nvars, const int *a, const int *b,
		  int *a1, int *a2);
  static bool divides(int nvars, const int *a, const int *b);
  static unsigned int mask(int nvars, const int *a);

  static int lex_compare(int nvars, const int *a, const int *b);
  static void copy(int nvars, const int *a, int *result);
  static int weight(int nvars, const int *a, M2_arrayint wt);
  static int degree(int nvars, const int *a);
  static void elem_text_out(buffer &o, 
			    unsigned int nvars,
			    const int *a, 
			    M2_stringarray varnames);
};

inline 
void ntuple::one(int nvars, int *result)
{
  for (int i=0; i<nvars; i++) *result++ = 0;
}

inline 
bool ntuple::is_one(int nvars, int *a)
{
  for (int i=0; i<nvars; i++) 
    if (*a++ != 0) return false;
  return true;
}

inline
void ntuple::mult(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    {
      long x = *a++;
      long y = *b++;
      long z = x+y;
      if ((x < 0) == (y < 0) && (x < 0) != (z < 0))
	{
	  throw(exc::overflow_error("monomial overflow"));
	}
      *result++ = z;
    }
}

inline
void ntuple::power(int nvars, const int *a, int n, int *result)
{
  for (int i=0; i<nvars; i++)
    {
      long e = *a++;
      long long e1 = e;
      e *= n;
      e1 *= n;
      if (e != e1)
	   throw(exc::overflow_error("monomial overflow"));
      *result++ = e1;
    }
}

inline
void ntuple::divide(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    {
      long x = *a++;
      long y = *b++;
      long z = x-y;
      if ((x < 0) == (y > 0) && (x < 0) != (z < 0))
	{
	  throw(exc::overflow_error("monomial overflow"));
	}
      *result++ = z;
    }
}

inline
void ntuple::quotient(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    {
      long x = *a++;
      long y = *b++;
      assert(x >= 0 && y > 0);		// Dan thinks this routine wouldn't ever be called with negative exponents!  Or in that case it should/could always return 0.
      long z;
      if (x <= y) z = 0;
      else
	{
	  z = x-y;
	  if (z < 0)
	       throw(exc::overflow_error("monomial overflow"));
	}
      *result++ = z;
    }
}

inline
void ntuple::lcm(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    {
      int c = *a++;
      int d = *b++;
      *result++ = (c > d ? c : d);
    }
}

inline
void ntuple::gcd(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    {
      int c = *a++;
      int d = *b++;
      *result++ = (c < d ? c : d);
    }
}

inline
bool ntuple::divides(int nvars, const int *a, const int *b)
  // Does a divide b?
{
  for (int i=0; i<nvars; i++)
    if (a[i] > b[i]) return false;
  return true;
}

inline
unsigned int ntuple::mask(int nvars, const int *a)
{
  unsigned int result = 0;
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
int ntuple::lex_compare(int nvars, const int *a, const int *b)
{
  for (int i=0; i<nvars; i++)
    if (a[i] > b[i]) return GT;
    else if (a[i] < b[i]) return LT;
  return EQ;
}

inline
void ntuple::copy(int nvars, const int *a, int *result)
{
  memcpy(result, a, nvars*sizeof(int));
}


inline
int ntuple::weight(int nvars, const int *a, M2_arrayint wt)
{
  int sum = 0;
  int top = wt->len;
  if (nvars < top) top = nvars;
  for (int i=0; i<top; i++)
       //warning: check for overflow here
    sum += a[i] * wt->array[i];
  return sum;
}

inline
int ntuple::degree(int nvars, const int *a)
{
  int sum = 0;
  for (int i=0; i<nvars; i++)
       //warning: check for overflow here
    sum += a[i];
  return sum;
}

inline
void ntuple::syz(int nvars, const int *a, const int *b,
		 int *a1, int *b1)
{
     for (int i=0; i<nvars; i++) {
	  if ((a[i] < 0 || b[i] < 0) && !(a[i] < 0 && b[i] < 0)) {
	       a1[i] = -a[i];
	       b1[i] = -b[i];
	       //warning: check better for overflow here
	       //warning: Mike: does this routine ever get called with negative exponents?
	       if (a1[i] == a[i] && a[i] < 0 || b1[i] == b[i] && b[i] < 0) { // yes, we overflow a bit too often here
		    throw(exc::overflow_error("monomial overflow"));
	       }
	  }
	  else {
	       int c = a[i] - b[i];
	       if (c >= 0) a1[i] = 0, b1[i] = c;
	       else a1[i] = -c, b1[i] = 0;
	  }
     }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
