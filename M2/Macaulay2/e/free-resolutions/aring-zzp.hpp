// Copyright 2011 Michael E. Stillman

#ifndef _aring_zzp_hpp_
#define _aring_zzp_hpp_

#include <iosfwd>

namespace M2 {
/**
\ingroup rings
*/
  class ARingZZp
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
    typedef int ElementType;
    typedef int elem;

    ARingZZp(size_t prime);

    ~ARingZZp();

    // ring informational
    size_t characteristic() const { return charac; }

    size_t cardinality() const { return charac; }

    static int findPrimitiveRoot(int P);

    void text_out(std::ostream& o) const;

    /////////////////////////////////////////////////////////
    // Routines to help in switch from coeffrings to aring //
    // these will be renamed or go away (hopefully) /////////
    /////////////////////////////////////////////////////////
    unsigned int computeHashValue(const elem& a) const { return a; }

    void init_set(elem &result, elem a) const { result = a; }
    void set(elem &result, elem a) const { result = a; }

    /////////////////////////////////
    // ElementType informational ////
    /////////////////////////////////

    bool is_unit(ElementType f) const { return f != 0; }
    bool is_zero(ElementType f) const { return f == 0; }
    bool is_equal(ElementType f, ElementType g) const { return f == g; }

    int compare_elems(ElementType f, ElementType g) const {
      int a = exp_table[f];
      int b = exp_table[g];
      if (a < b) return -1;
      if (a > b) return 1;
      return 0;
    }

    // 'init', 'init_set' functions

    void init(elem &result) const { result = 0; }

    void clear(elem &result) const { /* nothing */ }

    void set_zero(elem &result) const { result = 0; }

    void set_from_long(elem &result, long a) const {
      a = a % p;
      if (a < 0) a += p;
      result = log_table[a];
    }

    void set_var(elem &result, int v) const { result = 1; }

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
      int n = e1+e2;
      if (n >= p) n -= p;
      result = log_table[n];
    }

    void subtract(elem &result, elem a, elem b) const
    {
      int e1 = exp_table[a];
      int e2 = exp_table[b];
      int n = e1-e2;
      if (n < 0) n += p;
      result = log_table[n];
    }

    void subtract_multiple(elem &result, elem a, elem b) const
    {
      // we assume: a, b are NONZERO!!
      // result -= a*b
      int ab = a+b;
      if (ab > p1) ab -= p1;
      int n = exp_table[result] - exp_table[ab];
      if (n < 0) n += p;
      result = log_table[n];
    }

    void mult(elem &result, elem a, elem b) const
    {
      if (a != 0 && b != 0)
        {
          int c = a+b;
          if (c > p1) c -= p1;
          result = c;
        }
      else
        result = 0;
    }

    void divide(elem &result, elem a, elem b) const
    {
      M2_ASSERT(b != 0);
      if (a != 0 && b != 0)
        {
          int c = a-b;
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
          result = (a*n) % p1;
          if (result <= 0) result += p1;
        }
      else
        result = 0;
    }

    void swap(ElementType &a, ElementType &b) const
    {
      ElementType tmp = a;
      a = b;
      b = tmp;
    }

    void elem_text_out(std::ostream& o,
                       ElementType a,
                       bool p_one=true,
                       bool p_plus=false,
                       bool p_parens=false) const;

    void random(ElementType &result) const
    {
      // TODO: call what?? Or remove this function...
    }

    long coerceToLongInteger(const elem& f) const
    {
      int n = exp_table[f];
      if (n > p/2) n -= p;
      return n;
    }
    long coerceToNonnegativeLongInteger(const elem& f) const
    {
      int n = exp_table[f];
      return n;
    }
  private:
    void initialize_tables();
    
    size_t charac;
    int p; // charac == p ??
    int p1; // p-1
    int minus_one;
    int prim_root; // element we will use for our primitive root
    int *log_table; // 0..p-1
    int *exp_table; // 0..p-1
    
  };

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/free-resolutions  "
// indent-tabs-mode: nil
// End:
