// Copyright 2011 Michael E. Stillman

#ifndef _aring_zzp_hpp_
#define _aring_zzp_hpp_

#include "aring.hpp"

namespace M2 {
  class ARingZZp : RingInterface
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
    typedef unsigned int ElementType;

    typedef int elem;
    
    void initialize_tables();
    
    ARingZZp(int p0);

  private:
    unsigned long charac;
    int p; // charac == p ??
    int p1; // p-1
    int minus_one;
    int prim_root; // element we will use for our primitive root
    int *log_table; // 0..p-1
    int *exp_table; // 0..p-1
  public:
    // ring informational
    unsigned long characteristic() const { return p; }

    static int findPrimitiveRoot(int P);

    // 'get' functions

    int get_int(elem f) const { return exp_table[f]; }

    int get_repr(elem f) const { return f; }

    void get_str(char *s, size_t max_size, elem f);

    // 'init', 'init_set' functions

    void init(elem &result) { result = 0; }

    void init_set(elem &result, elem a) const { result = a; }
    
    void set_zero(elem &result) const { result = 0; }
    
    void set(elem &result, elem a) const { result = a; }

    void set_from_int(elem &result, int a) const { 
      a = a % p; 
      if (a < 0) a += p;
      result = log_table[a];
    }

    // arithmetic
    bool is_zero(elem result) const { return result == 0; }
    
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
      if (a != 0 && b != 0)
	{
	  int c = a-b;
	  if (c <= 0) c += p1;
	  result = c;
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
  };
  
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// End:
