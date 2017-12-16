// (c) 1995 Michael E. Stillman

#ifndef _varpower_hh_
#define _varpower_hh_

#include "buffer.hpp"
#include "engine-includes.hpp"
#include "intarray.hpp"

class varpower
{
  friend class index_varpower;

  static int degree_of(int n, const int *a);
  static bool is_nonneg(const int *a);
  static int max_mon_size(int n);
  static int compare(const int *a, const int *b);
  // return EQ, LT, or GT for a == b, a < b, or a > b.

  // [len, v1, e1, ..., vn, en], where len=n
public:
  varpower() {}
  ~varpower() {}
  static unsigned int computeHashValue(const int *vp);

  static void elem_text_out(buffer &o, const int *a, bool p_one = true);
  static void elem_text_out(buffer &o,
                            const int *a,
                            M2_ArrayString varnames,
                            bool p_one = true);

  static bool is_one(const int *a);
  static bool is_equal(const int *a, const int *b);
  static int topvar(const int *a);

  static void one(intarray &result);
  static void var(int v, int e, intarray &result);
  static int *copy(const int *vp, intarray &result);

  static void to_ntuple(int n, const int *a, int *result_exponents);
  static void from_ntuple(int n, const int *a, intarray &result);

  static M2_arrayint to_arrayint(const int *vp);
  static void from_arrayint(M2_arrayint m, intarray &result);

  static int simple_degree(const int *a);

  static void mult(const int *a, const int *b, intarray &result);
  static void quotient(const int *a, const int *b, intarray &result);
  // compute the quotient a:b
  static void power(const int *a, int n, intarray &result);
  static bool divides(const int *a, const int *b);
  // Is a divisible by b?
  static void monsyz(const int *a, const int *b, intarray &sa, intarray &sb);
  static void lcm(const int *a, const int *b, intarray &result);
  static void gcd(const int *a, const int *b, intarray &result);
  static void erase(const int *a, const int *b, intarray &result);
  // divide a by b^infinity
  static void radical(const int *a, intarray &result);

  static bool is_pure_power(const int *a, int &v, int &e);
  // if a is a pure power, then set v, e so that v^e is a.
  // otherwise return false.
};

class index_varpower
{
  const int *loc;
  const int *hi;

 public:
  index_varpower() : loc(0), hi(0) {}
  index_varpower(const int *m) : loc(m + 1), hi(m + *m) {}
  //  index_varpower(const int *m, int)
  //    : lo(m+1), hi(m+*m-2) { loc = hi; }

  index_varpower(const index_varpower &i) : loc(i.loc), hi(i.hi) {}
  int valid() { return loc < hi; }
  index_varpower &operator++()
  {
    loc += 2;
    return *this;
  }

  //  index_varpower &operator--() { loc -= 2; return *this; }

  int var() { return *loc; }
  int exponent() { return loc[1]; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
