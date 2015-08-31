// Copyright 2014 Michael E. Stillman.

#ifndef _res__gausser_hpp_
#define _res__gausser_hpp_

#include "res-f4-mem.hpp"

class ResF4Mem;
typedef int FieldElement;
typedef int ComponentIndex;

class CoefficientRingZZp
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

  int to_int(int f) const { return exp_table[f]; }

  void init(elem& result) const {}

  void init_set(elem &result, elem a) const { result = a; }

  void set_zero(elem &result) const { result = zero; }

  void set(elem &result, elem a) const { result = a; }

  bool is_zero(elem result) const { return result == zero; }

  bool is_equal(elem a, elem b ) const { return a == b; }

  void invert(elem &result, elem a) const
  {
    if (a == 0)
      result = 0; // this is the case a == ONE
    else
      result = p - 1 - a;
  }


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

  void negate(elem &result, elem a) const
  {
    result = modulus_add(a, minus_one, p1);
  }
  
  void subtract(elem &result, elem a, elem b) const
  {
    if (b == zero) result = a;
    else if (a == zero) result = modulus_add(b, minus_one, p1);
    else
      {
        int n = modulus_sub(exp_table[a], exp_table[b], p);
        result = log_table[n];
      }
  }

  void subtract_multiple(elem &result, elem a, elem b) const
  {
    // we assume: a, b are NONZERO!!
    // result -= a*b
    elem ab = modulus_add(a,b,p1);
    subtract(result, result, ab);
    return;
    // if (result==zero)
    //   result = ab;
    // else
    //   {
    //  int n = modulus_sub(exp_table[result], exp_table[ab], p);
    //  result = log_table[n];
    //   }
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

  void swap(elem &a, elem &b) const
  {
    elem tmp = a;
    a = b;
    b = tmp;
  }
};


class ResGausser
{
  enum {ZZp} typ;
  const CoefficientRingZZp *Kp;
  mutable ResF4Mem Mem;

  ResGausser(const CoefficientRingZZp* K);
public:
  typedef FieldElement* CoefficientArray;

  struct dense_row {
    ComponentIndex len; // coeffs is an array 0..len-1
    CoefficientArray coeffs;
  };

  ~ResGausser() {}

  const CoefficientRingZZp* get_coeff_ring() const { return Kp; }

  void set_one(FieldElement& one) const { one = 0; } // exponent for 1

  void negate(FieldElement a, FieldElement& result) const
  {
    Kp->negate(result, a);
  }
  
  // leading coefficient
  FieldElement lead_coeff(CoefficientArray coeffs) const
  {
    return coeffs[0];
  }

  // other routines:
  void deallocate_F4CCoefficientArray(CoefficientArray &F, ComponentIndex len) const;

  // reduce mod p. (QQ --> ZZ/p) (place in double's??)

  // other reductions

  // combine them (ZZ/p1, ..., ZZ/pn --> QQ)

  // Hensel lift routine?

  // evaluation of multivariate poly's or fcn's.

  void dense_row_allocate(dense_row &r, ComponentIndex nelems) const;
  // create a row of 0's (over K).

  void dense_row_clear(dense_row &r, ComponentIndex first, ComponentIndex last) const;

  void dense_row_deallocate(dense_row &r) const;

  ComponentIndex dense_row_next_nonzero(dense_row &r, ComponentIndex first, ComponentIndex last) const;

  void dense_row_fill_from_sparse(dense_row& r,
                                  ComponentIndex len,
                                  CoefficientArray sparse,
                                  ComponentIndex* comps) const;
  // Fills 'r' from 'sparse' (and 'comps')

  void dense_row_cancel_sparse(dense_row& r,
                               ComponentIndex len,
                               CoefficientArray sparse,
                               ComponentIndex* comps) const;
  // dense += c * sparse, where c is chosen to cancel column comps[0].
  // There should also be a version of this routine which records this value c
  // into a CoefficientArray.

  int coeff_to_int(FieldElement f) const //anton
  {
    return Kp->to_int(f);
  }

  mutable long n_dense_row_cancel;
  mutable long n_subtract_multiple;
};

#endif

// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
//  End:
