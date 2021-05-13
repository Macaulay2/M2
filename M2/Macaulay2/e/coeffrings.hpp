// Copyright 2005  Michael E. Stillman

#ifndef _coeffrings_hpp_
#define _coeffrings_hpp_

class Z_mod;
#include "ringelem.hpp"
#include "ZZ.hpp"

/**
 * \ingroup coeffrings
 */
class CoefficientRingZZp : public our_new_delete
{
  int p;
  int p1;  // p-1
  int minus_one;
  int zero;
  int *log_table;  // 0..p-1
  int *exp_table;  // 0..p-1

  static inline int modulus_add(int a, int b, int p)
  {
    int t = a + b;
    return (t < p ? t : t - p);
  }

  static inline int modulus_sub(int a, int b, int p)
  {
    int t = a - b;
    return (t < 0 ? t + p : t);
  }

 public:
  typedef Z_mod ring_type;
  typedef int elem;
  typedef elem ElementType;

  typedef std::vector<elem> ElementContainerType;

  CoefficientRingZZp(int p0, int *log, int *exps)
      : p(p0), p1(p - 1), zero(p - 1), log_table(log), exp_table(exps)
  {
    if (p == 2)
      minus_one = 0;
    else
      minus_one = (p - 1) / 2;


#if 0
    fprintf(stderr, "char %d\n", p);
    fprintf(stderr, "exp: ");
    for (int i=0; i<p; i++)
      fprintf(stderr, "%d ", exp_table[i]);
    fprintf(stderr, "\nlog: ");
    for (int i=0; i<p; i++)
      fprintf(stderr, "%d ", log_table[i]);
    fprintf(stderr, "\n");
#endif
  }

  void set_from_long(elem &result, long a) const
  {
    a = a % p;
    if (a < 0) a += p;
    result = log_table[a];
  }

  long coerceToLongInteger(const elem &f) const
  {
    int n = exp_table[f];
    if (n > p / 2) n -= p;
    return n;
  }
  
  int to_int(int f) const { return exp_table[f]; }
  void init(elem &result) const {}
  void clear(elem &result) const { /* nothing */}
  void init_set(elem &result, elem a) const { result = a; }
  void set_zero(elem &result) const { result = zero; }
  void set(elem &result, elem a) const { result = a; }
  bool is_zero(elem result) const { return result == zero; }
  bool is_equal(elem a, elem b) const { return a == b; }
  void invert(elem &result, elem a) const
  {
    if (a == 0)
      result = 0;  // this is the case a == ONE
    else
      result = p - 1 - a;
  }

  void add(elem &result, elem a, elem b) const
  {
    if (a == zero)
      result = b;
    else if (b == zero)
      result = a;
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
    if (b == zero)
      result = a;
    else if (a == zero)
      result = modulus_add(b, minus_one, p1);
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
    elem ab = modulus_add(a, b, p1);
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
      result = modulus_add(a, b, p1);
  }

  void divide(elem &result, elem a, elem b) const
  {
    if (a == zero || b == zero)
      result = zero;
    else
      result = modulus_sub(a, b, p1);
  }

  void to_ring_elem(ring_elem &result, const elem a) const
  {
    result = ring_elem(a);
  }

  void from_ring_elem(elem &result, const ring_elem &a) const
  {
    result = a.get_int();
  }

  void swap(elem &a, elem &b) const
  {
    elem tmp = a;
    a = b;
    b = tmp;
  }

  void elem_text_out(buffer &o,
                     ElementType a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;
};

/**
 * \ingroup coeffrings
 */
class CoefficientRingR : public our_new_delete
{
  const Ring *R;

 public:
  typedef Ring ring_type;
  typedef ring_elem elem;
  typedef elem ElementType;
  typedef VECTOR(elem) ElementContainerType;

  CoefficientRingR(const Ring *R0) : R(R0) {}
  void init_set(elem &result, elem a) const { result = a; }
  void init(elem &result) const { result = R->zero(); }
  void clear(elem &result) const { /* do nothing */}

  void set_zero(elem &result) const { result = R->zero(); }
  void set(elem &result, elem a) const { result = a; }
  void set_from_long(elem &result, long a) const { result = R->from_long(a); }
  bool is_zero(elem result) const { return R->is_zero(result); }
  bool is_equal(elem a, elem b) const { return R->is_equal(a, b); }
  bool is_unit(elem f) const { return R->is_unit(f); }
  void invert(elem &result, elem a) const { result = R->invert(a); }
  void subtract_multiple(elem &result, elem a, elem b) const 
  {
    // result -= a*b
    elem tmp = R->mult(a,b);
    result = R->subtract(result,tmp);
  }

  void add(elem &result, elem a, elem b) const { result = R->add(a, b); }
  void negate(elem &result, elem a) const { result = R->negate(a); }
  void subtract(elem &result, elem a, elem b) const
  {
    result = R->subtract(a, b);
  }

  void mult(elem &result, elem a, elem b) const { result = R->mult(a, b); }
  void divide(elem &result, elem a, elem b) const { result = R->divide(a, b); }
  void to_ring_elem(ring_elem &result, const elem &a) const { result = a; }
  void from_ring_elem(elem &result, const ring_elem &a) const { result = a; }
  void swap(elem &a, elem &b) const
  {
    elem tmp = a;
    a = b;
    b = tmp;
  }

  void elem_text_out(buffer &o,
                     ElementType a,
                     bool p_one = true,
                     bool p_plus = false,
                     bool p_parens = false) const;

  void text_out(buffer &o) const { o << "CoefficientRingR"; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
