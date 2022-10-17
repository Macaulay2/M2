// Copyright 2004.  Michael E. Stillman

#ifndef _monoid_hpp_
#define _monoid_hpp_

#include <vector>

#include "engine-includes.hpp"

#include "hash.hpp"
#include "imonorder.hpp"
#include "newdelete.hpp"
#include "style.hpp"

class PolynomialRing;
class buffer;
class intarray;
struct MonomialOrdering;

typedef int *exponents;
typedef int *graded_exponents;
typedef int *partial_sums;
typedef int *monomial;

typedef const int *const_exponents;
typedef const int *const_graded_exponents;
typedef const int *const_partial_sums;
typedef const int *const_monomial;
typedef const int *const_varpower;

#define ALLOCATE_EXPONENTS(byte_len) static_cast<exponents>(alloca(byte_len))
#define EXPONENT_BYTE_SIZE(nvars) static_cast<int>((sizeof(int) * (nvars)))

#define ALLOCATE_MONOMIAL(byte_len) static_cast<monomial>(alloca(byte_len))
#define MONOMIAL_BYTE_SIZE(mon_size) \
  static_cast<int>((sizeof(int) * (mon_size)))

class Monoid : public MutableEngineObject
{
  int nvars_;
  M2_ArrayString varnames_;
  M2_arrayint degvals_;
  M2_arrayint heftvals_;
  VECTOR(const_monomial)
      degree_of_var_;  // [0]..[nvars-1] are the multi-degrees of the
                       // variables, and [nvars] = zero element in the
                       // degree monoid.
  M2_arrayint heft_degree_of_var_;

  const PolynomialRing *degree_ring_;
  const Monoid *degree_monoid_;

  const MonomialOrdering *mo_;
  MonomialOrder *monorder_;  // Internal version, with encoding information
  enum overflow_type { OVER, OVER1, OVER2, OVER4 } * overflow;

  size_t exp_size;  // in bytes

  int monomial_size_;  // in ints
  int monomial_bound_;

  int n_invertible_vars_;
  int n_before_component_;
  int n_after_component_;
  bool component_up_;

  M2_arrayint local_vars;  // These are the variables which are < 1 in the
                           // monomial order.

  int first_weights_slot_;  // < 0 if none, otherwise the location of the first
                            // weight vec value
                            // in each monomial

  std::vector<bool> mLaurentVariablesPredicate;
  
  VECTOR(int) nslots_;

  void set_degrees();
  void set_overflow_flags();

  static Monoid *trivial_monoid;

  Monoid();

  Monoid(const MonomialOrdering *mo,
         M2_ArrayString names,
         const PolynomialRing *DR, /* degree ring */
         M2_arrayint degs,
         M2_arrayint hefts);

 public:
  static Monoid *create(const MonomialOrdering *mo,
                        M2_ArrayString names,
                        const PolynomialRing *DR, /* degree ring */
                        M2_arrayint degs,
                        M2_arrayint hefts);

  static Monoid *create(const MonomialOrdering *mo,
                        const std::vector<std::string>& names,
                        const PolynomialRing *DR, /* degree ring */
                        const std::vector<int>& degs,
                        const std::vector<int>& hefts);
  
  ~Monoid();

  static void set_trivial_monoid_degree_ring(const PolynomialRing *DR);
  // ONLY to be called by PolyRing::get_trivial_poly_ring()

  static Monoid *get_trivial_monoid();

  const MonomialOrdering *getMonomialOrdering() const { return mo_; }
  M2_arrayint getNonTermOrderVariables() const { return local_vars; }
  const PolynomialRing *get_degree_ring() const { return degree_ring_; }
  const Monoid *degree_monoid() const { return degree_monoid_; }
  const_monomial degree_of_var(int v) const { return degree_of_var_[v]; }
  int primary_degree_of_var(int v) const
  {
    return heft_degree_of_var_->array[v];
  }
  M2_arrayint primary_degree_of_vars() const { return heft_degree_of_var_; }
  M2_arrayint get_heft_vector() const { return heftvals_; }
  bool primary_degrees_of_vars_positive() const;

  bool is_group() const { return n_invertible_vars_ == nvars_; }
  bool has_monomials_lt_one() const
  {
    return (n_invertible_vars_ > 0 || local_vars->len > 0);
  }

  int numInvertibleVariables() const { return n_invertible_vars_; }
  int numNonTermOrderVariables() const { return local_vars->len; }
  // returns an empty vector if the first part of the monomial ordering is
  // not a weight vector
  std::vector<int> getFirstWeightVector() const;

  std::vector<int> getPrimaryDegreeVector() const;

  bool isLaurentVariable(int i) const {
    return monorder_->is_laurent[i];
  }

  std::vector<bool> laurentVariables() const {
    return mLaurentVariablesPredicate;
  }

  void text_out(buffer &o) const;

  int n_vars() const { return nvars_; }
  int max_degree() const { return monomial_bound_; }
  int monomial_size() const { return monomial_size_; }
  int n_slots(int nparts) const;
  int num_parts() const;

  unsigned int computeHashValue(const_monomial m) const;

  /////////////////////////
  // Monomial arithmetic //
  /////////////////////////
  void from_varpower(const_varpower vp, monomial result) const;
  void to_varpower(const_monomial m, intarray &result_vp) const;

  void from_expvector(const_exponents exp, monomial result) const;
  void to_expvector(const_monomial m, exponents result_exp) const;

  M2_arrayint to_arrayint(
      const_monomial monom) const; /* Returns an exponent vector representation
                                  of the monomial */
  bool in_subring(int nslots, const_monomial m) const;
  inline int compare(int nslots, const_monomial m, const_monomial n) const
  {
    for (int i = nslots; i != 0; --i)
      {
        if (*m > *n) return GT;
        if (*m < *n) return LT;
        m++;
        n++;
      }
    return EQ;
  }

  monomial make_new(const_monomial d) const;
  monomial make_one() const;
  void remove(monomial d) const;

  bool is_one(const_monomial m) const;
  bool is_invertible(const_monomial m) const;  // is every variable that occurs
  // in 'm' allowed to be negative?

  void one(monomial result) const;
  void copy(const_monomial m, monomial result) const;

  void mult(const_monomial m, const_monomial n, monomial result) const;
  void power(const_monomial m, int n, monomial result) const;
  inline int compare(const_monomial m, const_monomial n) const
  {
    int i = monomial_size_;
    if (i == 0) return EQ;
    while (1)
      {
        if (*m > *n) return GT;
        if (*m < *n) return LT;
        if (--i == 0) return EQ;
        m++, n++;
      }
  }
  int partial_compare(int num, const_monomial m, const_monomial n) const;
  int compare(const_monomial m, int mcomp, const_monomial n, int ncomp) const;
  bool is_equal(const_monomial m1, const_monomial m2) const { return compare(m1, m2) == EQ; }

  bool divides_partial_order(const_monomial m, const_monomial n) const; // s.t. n/m only has >= exponents.
  bool divides(const_monomial m, const_monomial n) const; // s.t. n/m might have negative exponents, for Laurent variables.
  void divide(const_monomial m, const_monomial n, monomial result) const;
  void lcm(const_monomial m, const_monomial n, monomial result) const;
  void gcd(const_monomial m, const_monomial n, monomial result) const;
  void monsyz(const_monomial m,
              const_monomial n,
              monomial result_sm,
              monomial result_sn) const;

  void elem_text_out(buffer &o, const_monomial m, bool p_one = true) const;

  void multi_degree(const_monomial m, monomial result) const;
  int primary_degree(const_monomial m) const;
  int degree_weights(const_monomial m, M2_arrayint wts) const;

  template<typename T>
  T degree_weights(const_monomial m, const std::vector<T>& wts) const;
  
  int simple_degree(const_monomial m) const;  // simply sum of exponents
  void degree_of_varpower(const_varpower vp, monomial result) const;

  template<typename T>
  void degree_of_expvector(const T* expvector, monomial result) const
  {
    degree_monoid()->one(result);
    monomial mon1 = degree_monoid()->make_one();
    for (int i=0; i<n_vars(); i++)
      {
        if (expvector[i] != 0)
          {
            degree_monoid()->power(degree_of_var(i), expvector[i], mon1);
            degree_monoid()->mult(result, mon1, result);
          }
      }
    degree_monoid()->remove(mon1);
  }

  bool weight_value_exists() const { return first_weights_slot_ >= 0; }
  // True if the first part of the order has a weight vector.
  // MUST be true in order for first_weight_value to not give erroneous value

  long first_weight_value(const_monomial m) const
  {
    return m[first_weights_slot_];
  }
  // Returns the value of the first weight vector in the monomial order.
  // If none, returns 0.  First call weight_value_exists() before using.
};

#if 0
// // These will become the unsafe versions, I guess, if they are still
// // needed...
// inline void Monoid::mult(const_monomial m, const_monomial n, monomial result) const
//     { for (int i=0; i<monomial_size_; i++) *result++ = *m++ + *n++; }
//
// inline void Monoid::power(const_monomial m, int n, monomial result) const
//     { for (int i=0; i<monomial_size_; i++) *result++ = *m++ * n; }
#endif

// WARNING!! 'divide' assumes that division is possible
inline void Monoid::divide(const_monomial m,
                           const_monomial n,
                           monomial result) const
{
  for (int i = monomial_size_; i > 0; i--) *result++ = *m++ - *n++;
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
