// Copyright 1997 Michael E. Stillman

#ifndef _ntuple_hpp_
#define _ntuple_hpp_

#include "style.hpp"

class ntuple
{
public:
  static void one(int nvars, int *result);
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
  static int weight(int nvars, const int *a, const int *wt);
  static int degree(int nvars, const int *a);
  static void elem_text_out(buffer &o, 
			    int nvars,
			    const int *a, 
			    const array<char *> &varnames);
  static void elem_bin_out(buffer &o, int nvars, const int *a);
};

inline 
void ntuple::one(int nvars, int *result)
{
  for (int i=0; i<nvars; i++) *result++ = 0;
}

inline
void ntuple::mult(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    *result++ = *a++ + *b++;  // What about overflow checking here?
}

inline
void ntuple::power(int nvars, const int *a, int n, int *result)
{
  for (int i=0; i<nvars; i++)
    *result++ = *a++ * n;  // What about overflow checking here?
}

inline
void ntuple::divide(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    *result++ = *a++ - *b++;  // What about overflow checking here?
}

inline
void ntuple::quotient(int nvars, const int *a, const int *b, int *result)
{
  for (int i=0; i<nvars; i++)
    {
      int c = *a++ - *b++;  // What about overflow checking here?
      if (c < 0) c = 0;
      *result++ = c;
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
int ntuple::weight(int nvars, const int *a, const int *wt)
{
  int sum = 0;
  for (int i=0; i<nvars; i++)
    sum += a[i] * wt[i];
  return sum;
}

inline
int ntuple::degree(int nvars, const int *a)
{
  int sum = 0;
  for (int i=0; i<nvars; i++)
    sum += a[i];
  return sum;
}

inline
void ntuple::syz(int nvars, const int *a, const int *b,
		 int *a1, int *b1)
{
  for (int i=0; i<nvars; i++)
    {
      int c = a[i] - b[i];
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
