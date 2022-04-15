// Copyright 2011 Michael E. Stillman

#ifndef _aring_zzp_hpp_
#define _aring_zzp_hpp_

#include "interface/random.h"
#include "aring.hpp"
#include "buffer.hpp"
#include "ringelem.hpp"
#include "error.h"           // for ERROR

class Z_mod;
class RingMap;

namespace M2 {
/**
\ingroup rings
*/
class ARingZZp : public RingInterface
{
  // Integers mod p, implemented as
  // exponents of a primitive element a
  // Representation:
  // 0  means 0
  // 1 <= n <= p-1  means a^n (mod p)

  // 0 represents 0
  // p-1 represents 1
  // 1..p-2 represent numbers in range 2..p-1

 public:
  static const RingID ringID = ring_ZZp;
  typedef Z_mod ring_type;
  typedef int ElementType;
  typedef int elem;
  typedef std::vector<elem> ElementContainerType;

  ARingZZp(size_t prime);

  ~ARingZZp();

  // ring informational
  size_t characteristic() const { return charac; }
  size_t cardinality() const { return charac; }
  static int findPrimitiveRoot(int P);

  void text_out(buffer &o) const;

  /////////////////////////////////////////////////////////
  // Routines to help in switch from coeffrings to aring //
  // these will be renamed or go away (hopefully) /////////
  /////////////////////////////////////////////////////////
  unsigned int computeHashValue(const elem &a) const { return a; }
  void init_set(elem &result, elem a) const { result = a; }
  void set(elem &result, elem a) const { result = a; }
  /////////////////////////////////
  // ElementType informational ////
  /////////////////////////////////

  bool is_unit(ElementType f) const { return f != 0; }
  bool is_zero(ElementType f) const { return f == 0; }
  bool is_equal(ElementType f, ElementType g) const { return f == g; }
  int compare_elems(ElementType f, ElementType g) const
  {
    int a = exp_table[f];
    int b = exp_table[g];
    if (a < b) return -1;
    if (a > b) return 1;
    return 0;
  }

  ////////////////////////////
  // to/from ringelem ////////
  ////////////////////////////
  // These simply repackage the element as either a ringelem or an
  // 'ElementType'.
  // No reinitialization is done.
  // Do not take the same element and store it as two different ring_elem's!!
  void to_ring_elem(ring_elem &result, const ElementType &a) const
  {
    if (a == 0)
      result = ring_elem(p1);
    else if (a == p1)
      result = ring_elem(0);
    else
      result = ring_elem(a);
  }

  void from_ring_elem(ElementType &result, const ring_elem &a) const
  {
    if (a.get_int() == 0)
      result = p1;
    else if (a.get_int() == p1)
      result = 0;
    else
      result = a.get_int();
  }

  // 'init', 'init_set' functions

  void init(elem &result) const { result = 0; }
  void clear(elem &result) const { /* nothing */}

  void set_zero(elem &result) const { result = 0; }
  void set_from_long(elem &result, long a) const
  {
    a = a % p;
    if (a < 0) a += p;
    result = log_table[a];
  }

  void set_var(elem &result, int v) const { result = 1; }
  void set_from_mpz(elem &result, mpz_srcptr a) const
  {
    int b = static_cast<int>(mpz_fdiv_ui(a, p));
    result = log_table[b];
  }

  bool set_from_mpq(elem &result, mpq_srcptr a) const
  {
    ElementType n, d;
    set_from_mpz(n, mpq_numref(a));
    set_from_mpz(d, mpq_denref(a));
    if (is_zero(d)) return false;
    divide(result, n, d);
    return true;
  }

  bool set_from_BigReal(elem &result, gmp_RR a) const { return false; }
  // arithmetic
  void negate(elem &result, elem a) const
  {
    if (a != 0)
      {
        result = minus_one + a;
        if (result > p1) result -= p1;
      }
    else
      result = 0;
  }

  void invert(elem &result, elem a) const
  // we silently assume that a != 0.  If it is, result is set to a^0, i.e. 1
  {
    result = p1 - a;
    if (result == 0) result = p1;
  }

  void add(elem &result, elem a, elem b) const
  {
    int e1 = exp_table[a];
    int e2 = exp_table[b];
    int n = e1 + e2;
    if (n >= p) n -= p;
    result = log_table[n];
  }

  void subtract(elem &result, elem a, elem b) const
  {
    int e1 = exp_table[a];
    int e2 = exp_table[b];
    int n = e1 - e2;
    if (n < 0) n += p;
    result = log_table[n];
  }

  inline int modulus_add(int a, int b, int p) const
  {
    int t = a + b;
    return (t <= p ? t : t - p);
  }

  inline int modulus_sub(int a, int b, int p) const
  {
    int t = a - b;
    return (t < 0 ? t + p : t);
  }

  void subtract_multiple(elem &result, elem a, elem b) const
  {
    // we assume: a, b are NONZERO!!
    // result -= a*b
    
    // the change in code below mimics that of coeffrings.cpp which was 15-20% faster in
    // testing for some reason (in small characteristics).  The assembly generated is much more
    // clean than it was previously.

    //int ab = a + b;
    //if (ab > p1) ab -= p1;
    int ab = modulus_add(a,b,p1);
    //int n = exp_table[result] - exp_table[ab];
    //if (n < 0) n += p;
    int n = modulus_sub(exp_table[result],exp_table[ab],p);
    result = log_table[n];
  }

  void mult(elem &result, elem a, elem b) const
  {
    if (a != 0 && b != 0)
      {
        int c = a + b;
        if (c > p1) c -= p1;
        result = c;
      }
    else
      result = 0;
  }

  void divide(elem &result, elem a, elem b) const
  {
    //    assert(b != 0);
    if (b == 0) ERROR("division by zero");
    if (a != 0)
      {
        int c = a - b;
        if (c <= 0) c += p1;
        result = c;
      }
    else
      result = 0;
  }

  void power(elem &result, elem a, int n) const
  {
    if (a != 0)
      {
        result = (a * n) % p1;
        if (result <= 0) result += p1;
      }
    else
      {
        // a == 0
        if (n == 0) result = p1; // the element 1 in this ring.
        else if (n < 0) ERROR("division by zero");
        else result = 0;
      }
  }

  void power_mpz(elem &result, elem a, mpz_srcptr n) const
  {
    if (a != 0)
      {
        int n1 = static_cast<int>(mpz_fdiv_ui(n, p1));
        power(result, a, n1);
      }
    else
      {
        if (mpz_sgn(n) == 0) result = p1; // the element 1 in this ring.
        else if (mpz_sgn(n) < 0) ERROR("division by zero");
        else result = 0; // result is 0 in the ring.
      }
  }

  void swap(ElementType &a, ElementType &b) const
  {
    ElementType tmp = a;
    a = b;
    b = tmp;
  }

  void elem_text_out(buffer &o,
                     ElementType a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;

  void syzygy(ElementType a,
              ElementType b,
              ElementType &x,
              ElementType &y) const
  // returns x,y s.y. x*a + y*b == 0.
  // if possible, x is set to 1.
  // no need to consider the case a==0 or b==0.
  {
    x = p1;
    divide(y, a, b);
    negate(y, y);
  }

  void random(ElementType &result) const { result = rawRandomInt((int32_t)p); }
  void eval(const RingMap *map,
            const elem f,
            int first_var,
            ring_elem &result) const;

  long coerceToLongInteger(const elem &f) const
  {
    int n = exp_table[f];
    if (n > p / 2) n -= p;
    return n;
  }
  long coerceToNonnegativeLongInteger(const elem &f) const
  {
    int n = exp_table[f];
    return n;
  }

 private:
  void initialize_tables();

  size_t charac;
  int p;   // charac == p ??
  int p1;  // p-1
  int minus_one;
  int prim_root;   // element we will use for our primitive root
  int *log_table;  // 0..p-1
  int *exp_table;  // 0..p-1
};
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
