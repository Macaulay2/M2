// Copyright 2005  Michael E. Stillman

#ifndef _coeffrings_hpp_
#define _coeffrings_hpp_

class Z_mod;
#include "ringelem.hpp"

class CoefficientRingZZp : public our_new_delete
{
  int p;
  int p1; // p-1
  int minus_one;
  int zero;
  int *log_table; // 0..p-1
  int *exp_table; // 0..p-1

  static inline int modulus_add(int a, int b, int p)
  {
    int t = a+b;
    return (t < p ? t : t-p);
  }

  static inline int modulus_sub(int a, int b, int p)
  {
    int t = a-b;
    return (t < 0 ? t+p : t);
  }
public:
  typedef Z_mod ring_type;
  typedef int elem;

  CoefficientRingZZp(int p0, int *log, int *exps)
    : p(p0),
      p1(p-1),
      zero(p-1),
      log_table(log),
      exp_table(exps)
  {
    if (p==2)
      minus_one = 0;
    else
      minus_one = (p-1)/2;
  }

  void init_set(elem &result, elem a) const { result = a; }
  void set_zero(elem &result) const { result = zero; }
  bool is_zero(elem result) const { return result == zero; }
  void invert(elem &result, elem a) const;
  void subtract_multiple(elem &result, elem a, elem b);
    // result -= a*b

  void add(elem &result, elem a, elem b) const
  {
    if (a == zero) result = b;
    else if (b == zero) result = a;
    else
      {
	int n = modulus_add(exp_table[a], exp_table[b], p);
	result = log_table[n];
      }
  }

  void mult(elem &result, elem a, elem b) const
  {
    if (a == zero || b == zero) 
      result = zero;
    else
      result = modulus_add(a,b,p1);
  }

  void divide(elem &result, elem a, elem b) const
  {
    if (a == zero || b == zero) 
      result = zero;
    else
      result = modulus_sub(a,b,p1);
  }

  void to_ring_elem(const elem a, ring_elem &result) const
  {
    result.int_val = a;
  }

  void from_ring_elem(const ring_elem &a, elem &result) const
  {
    result = a.int_val;
  }
};

#include "RR.hpp"
class CoefficientRingRR : public our_new_delete
{
public:
  typedef RR ring_type;
  typedef double elem;

  CoefficientRingRR() {}

  void init_set(elem &result, elem a) const { result = a; }
  void set_zero(elem &result) const { result = 0.0; }
  bool is_zero(elem result) const { return result == 0.0; }

  void invert(elem &result, elem a) const;

  void subtract_multiple(elem &result, elem a, elem b);
    // result -= a*b

  void add(elem &result, elem a, elem b) const
  {
    result = a + b;
  }

  void mult(elem &result, elem a, elem b) const
  {
    result = a*b;
  }

  void divide(elem &result, elem a, elem b) const
  {
    result = a/b;
  }

  void to_ring_elem(const elem a, ring_elem &result) const
  {
    result = globalRR->from_double(a);
  }

  void from_ring_elem(const ring_elem &a, elem &result) const
  {
    result = (reinterpret_cast<RR::RRelem>(a.poly_val))->val;
  }
};

#include "CC.hpp"
class CoefficientRingCC : public our_new_delete
{
public:
  typedef CC ring_type;
  typedef M2_CC_struct elem; // components are re, im

  CoefficientRingCC() {}

  void init_set(elem &result, elem a) const { result = a; }
  void set_zero(elem &result) const { result.re = 0.0; result.im = 0.0; }
  bool is_zero(elem result) const { return result.re == 0.0 && result.im == 0.0; }

  void invert(elem &result, elem a) const;

  void subtract_multiple(elem &result, elem a, elem b);
    // result -= a*b

  void add(elem &result, elem a, elem b) const
  {
    result.re = a.re + b.re;
    result.im = a.im + b.im;
  }

  void mult(elem &result, elem a, elem b) const
  {
    result.re = a.re*b.re - a.im*b.im;
    result.im = a.re*b.im + a.im*b.re;
  }

  void divide(elem &result, elem a, elem b) const
  {
    double bot = b.re*b.re + b.im*b.im;
    result.re = a.re*b.re + a.im*b.im;
    result.re /= bot;
    result.im = a.im*b.re - a.re*b.im;
    result.im /= bot;
  }

  void to_ring_elem(elem a, ring_elem &result) const
  {
    result = globalCC->from_complex(&a);
  }

  void from_ring_elem(const ring_elem &a, elem &result) const
  {
    M2_CC b = reinterpret_cast<M2_CC>(a.poly_val);
    result = *b;
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
