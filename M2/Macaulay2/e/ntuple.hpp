// Copyright 1997 Michael E. Stillman

#ifndef _ntuple_hpp_
#define _ntuple_hpp_

#include "buffer.hpp"
#include "engine-includes.hpp"
#include "overflow.hpp"
#include "style.hpp"  // for LT, GT, EQ.

typedef int *exponents;

class ntuple
{
 public:
  static void one(int nvars, int *result);
  static bool is_one(int nvars, int *a);
  static void mult(int nvars, const int *a, const int *b, int *result);
  static void power(int nvars, const int *a, int n, int *result);
  static void multpower(int nvars,
                        const int *a,
                        const int *b,
                        int n,
                        int *result);
  static void divide(int nvars, const int *a, const int *b, int *result);
  // result = a - b
  static void quotient(int nvars, const int *a, const int *b, int *result);
  // result = max(a-b,0)
  static void lcm(int nvars, const int *a, const int *b, int *result);
  static void gcd(int nvars, const int *a, const int *b, int *result);
  static bool divides(int nvars, const int *a, const int *b);
  static unsigned int mask(int nvars, const int *a);

  static int lex_compare(int nvars, const int *a, const int *b);
  static void copy(int nvars, const int *a, int *result);
  static int weight(int nvars, const int *a, M2_arrayint wt);
  static int weight(int nvars, const int *a, const std::vector<int>& wt);
  static int degree(int nvars, const int *a);
  static void elem_text_out(buffer &o,
                            unsigned int nvars,
                            const int *a,
                            M2_ArrayString varnames,
                            bool p_one = true);
};

inline void ntuple::one(int nvars, int *result)
{
  for (int i = 0; i < nvars; i++) *result++ = 0;
}

inline bool ntuple::is_one(int nvars, int *a)
{
  for (int i = 0; i < nvars; i++)
    if (*a++ != 0) return false;
  return true;
}

inline void ntuple::mult(int nvars, const int *a, const int *b, int *result)
{
  for (int i = nvars; i > 0; i--) *result++ = safe::add(*a++, *b++);
}

inline void ntuple::power(int nvars, const int *a, int n, int *result)
{
  for (int i = nvars; i > 0; i--) *result++ = safe::mult(*a++, n);
}

inline void ntuple::multpower(int nvars,
                              const int *a,
                              const int *b,
                              int n,
                              int *result)
{
  for (int i = nvars; i > 0; i--)
    *result++ = safe::add(*a++, safe::mult(*b++, n));
}

inline void ntuple::divide(int nvars, const int *a, const int *b, int *result)
{
  for (int i = nvars; i > 0; i--) *result++ = safe::sub(*a++, *b++);
}

inline void ntuple::quotient(int nvars, const int *a, const int *b, int *result)
{
  for (int i = nvars; i > 0; i--)
    {
      assert(*a >= 0 && *b >= 0);  // Dan thinks this routine wouldn't ever be
                                   // called with negative exponents!  Or in
                                   // that case it should/could always return 0.
      *result++ =
          safe::sub_pos(*a++, *b++);  // if so, there would never be an overflow
    }
}

inline void ntuple::lcm(int nvars, const int *a, const int *b, int *result)
{
  for (int i = nvars; i > 0; i--)
    {
      int c = *a++;
      int d = *b++;
      *result++ = (c > d ? c : d);
    }
}

inline void ntuple::gcd(int nvars, const int *a, const int *b, int *result)
{
  for (int i = nvars; i > 0; i--)
    {
      int c = *a++;
      int d = *b++;
      *result++ = (c < d ? c : d);
    }
}

inline bool ntuple::divides(int nvars, const int *a, const int *b)
// Does a divide b?
{
  for (int i = 0; i < nvars; i++)  // we go upward, because some rings have
                                   // unused variables at the end
    if (a[i] > b[i]) return false;
  return true;
}

inline unsigned int ntuple::mask(int nvars, const int *exp)
{
  unsigned int result = 0, bit = 1;
  for (int i = nvars - 1; i >= 0; i--)
    {
      if (exp[i] > 0) result |= bit;
      bit <<= 1;
      if (bit == 0) bit = 1;
    }
  return result;
}

inline int ntuple::lex_compare(int nvars, const int *a, const int *b)
{
  for (int i = 0; i < nvars; i++)
    if (a[i] > b[i])
      return GT;
    else if (a[i] < b[i])
      return LT;
  return EQ;
}

inline void ntuple::copy(int nvars, const int *a, int *result)
{
  memcpy(result, a, nvars * sizeof(int));
}

inline int ntuple::weight(int nvars, const int *a, M2_arrayint wt)
{
  int top = wt->len;
  if (nvars < top) top = nvars;
  if (top == 0) return 0;
  int sum = safe::mult(a[0], wt->array[0]);
  for (int i = 1; i < top; i++)
    sum = safe::add(sum, safe::mult(a[i], wt->array[i]), "weight overflow");
  return sum;
}

inline int ntuple::weight(int nvars, const int *a, const std::vector<int>& wt)
{
  int top = wt.size();
  if (nvars < top) top = nvars;
  if (top == 0) return 0;
  int sum = safe::mult(a[0], wt[0]);
  for (int i = 1; i < top; i++)
    sum = safe::add(sum, safe::mult(a[i], wt[i]), "weight overflow");
  return sum;
}

inline int ntuple::degree(int nvars, const int *a)
{
  if (nvars == 0) return 0;
  int sum = a[0];
  for (int i = 1; i < nvars; i++) sum = safe::add(sum, a[i], "degree overflow");
  return sum;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
