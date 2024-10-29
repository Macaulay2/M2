// Copyright 2004.  Michael E. Stillman
#pragma once

#ifdef HAVE_ALLOCA_H
#include <alloca.h>  // for alloca
#endif
#include <stddef.h>  // for size_t
#include <string>    // for string
#include <vector>    // for vector

#include "ExponentList.hpp"
#include "ExponentVector.hpp"
#include "hash.hpp"
#include "imonorder.hpp"
#include "newdelete.hpp"
#include "style.hpp"

class PolynomialRing;
class buffer;
struct MonomialOrdering;

// monomial is an encoded array of size monomial_size()
typedef int *monomial;
typedef const int *const_monomial;


#define ALLOCATE_EXPONENTS(byte_len) static_cast<exponents_t>(alloca(byte_len))
#define EXPONENT_BYTE_SIZE(nvars) static_cast<int>((sizeof(int) * (nvars)))

#define ALLOCATE_MONOMIAL(byte_len) static_cast<monomial>(alloca(byte_len))
#define MONOMIAL_BYTE_SIZE(mon_size) \
  static_cast<int>((sizeof(int) * (mon_size)))

// TODO: rename and document all variables
// (e.g. see NCAlgebras/FreeMonoid.hpp and mathicgb/MonoMonoid.hpp)
// TODO: make sure monoid is deconstructed and not garbage collected
class Monoid : public MutableEngineObject
{
  const Monoid *mDegreeMonoid;
  const PolynomialRing *mDegreeRing;

  /// the monomial ordering of the variables
  const MonomialOrdering *mo_;
  // Internal version, with encoding information
  MonomialOrder *monorder_;

  /// number of variables
  const int mVariableCount;
  /// names of variables
  const std::vector<std::string> mVariableNames;
  /// length mVariableCount * (length of a single degree vector)
  const std::vector<int> mDegrees;
  /// length of a single degree vector
  const std::vector<int> mHeftVector;
  /// length mVariableCount
  // TODO: make this const
  std::vector<int> mHeftDegrees;

  /// The following two Should be constant after construction
  /// [0]..[mVariableCount-1] are the multi-degrees of the variables,
  /// and [mVariableCount] = zero element in the degree monoid.
  gc_vector<const_monomial> mDegreeOfVar;

  /// sets mHeftDegrees and mDegreeOfVar
  void set_degrees();

  /// the number bytes in an unpacked exponent vector
  /// length VariableCount * (number of bytes per exponent)
  size_t exp_size;

  /// size of an encoded monomial
  int monomial_size_;  // in ints
  int monomial_bound_; // not yet used

  // TODO: the following should be handled by the monomial order
  /// the location of the first weight vec value in each monomial
  /// or < 0 if there is none
  int first_weights_slot_;
  /// number of invertible variables
  int n_invertible_vars_;
  /// indicates where the free module components are in the monomial order
  int n_before_component_;
  int n_after_component_;
  /// indicates whether free module components are ordered lexicographically
  bool component_up_;
  /// These are the variables which are < 1 in the monomial order.
  std::vector<int> local_vars;
  /// These are the variables which can have negative exponents
  std::vector<bool> mLaurentVariablesPredicate;
  /// number of slots per monomial order block
  // TODO: why is this one a gc_vector?
  gc_vector<int> nslots_;
  /// used for preventing overflows
  void set_overflow_flags();
  enum overflow_type { OVER, OVER1, OVER2, OVER4 } * overflow;

  /// the trivial monoid
  static Monoid *trivial_monoid;

  /// constructors
  Monoid();
  Monoid(const MonomialOrdering *mo,
         const PolynomialRing *DR, /* degree ring */
         const std::vector<std::string> names,
         const std::vector<int> degs,
         const std::vector<int> hefts);

 public:
  static Monoid *create(const MonomialOrdering *mo,
                        const PolynomialRing *DR, /* degree ring */
                        const std::vector<std::string> &names,
                        const std::vector<int> &degs,
                        const std::vector<int> &hefts);

  ~Monoid();

  static void set_trivial_monoid_degree_ring(const PolynomialRing *DR);
  // ONLY to be called by PolyRing::get_trivial_poly_ring()

  static Monoid *get_trivial_monoid();

  const MonomialOrdering *getMonomialOrdering() const { return mo_; }
  const PolynomialRing *get_degree_ring() const { return mDegreeRing; }
  const Monoid *degree_monoid() const { return mDegreeMonoid; }
  const_monomial degree_of_var(int v) const { return mDegreeOfVar[v]; }
  int primary_degree_of_var(int v) const { return mHeftDegrees[v]; }
  const std::vector<int> &primary_degree_of_vars() const { return mHeftDegrees; }
  const std::vector<int> &get_heft_vector() const { return mHeftVector; }
  bool primary_degrees_of_vars_positive() const;

  bool is_group() const { return n_invertible_vars_ == mVariableCount; }
  bool is_local() const { return local_vars.size() > 0; }
  bool has_monomials_lt_one() const
  {
    return (n_invertible_vars_ > 0 || local_vars.size() > 0);
  }

  int numInvertibleVariables() const { return n_invertible_vars_; }
  int numNonTermOrderVariables() const { return local_vars.size(); }
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

  int n_vars() const { return mVariableCount; }
  int max_degree() const { return monomial_bound_; }
  int monomial_size() const { return monomial_size_; }
  int n_slots(int nparts) const;
  int num_parts() const;

  unsigned int computeHashValue(const_monomial m) const;

  /////////////////////////
  // Monomial arithmetic //
  /////////////////////////
  void from_varpower(const_varpower vp, monomial result) const;
  void to_varpower(const_monomial m, gc_vector<int>& result_vp) const;

  // convert to and from the exponent vector representation
  void from_expvector(const_exponents exp, monomial result) const;
  void to_expvector(const_monomial m, exponents_t result_exp) const;

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

  // TODO: define all three
  void elem_text_out(buffer &o, const_monomial m, bool p_one = true) const;
  //void elem_text_out(buffer &o, const_exponents m, bool p_one = true) const;
  //void elem_text_out(buffer &o, const_varpower m, bool p_one = true) const;

  void multi_degree(const_monomial m, monomial result) const;
  int primary_degree(const_monomial m) const;

  template<typename T>
  T degree_weights(const_monomial m, const std::vector<T>& wts) const;
  int degree_weights(const_monomial m, const std::vector<int> &wts) const;
  
  int simple_degree(const_monomial m) const;  // simply sum of exponents
  void degree_of_varpower(const_varpower vp, monomial result) const;

  template<typename T>
  void degree_of_expvector(const T* expvector, monomial result) const
  {
    mDegreeMonoid->one(result);
    monomial mon1 = mDegreeMonoid->make_one();
    for (int i=0; i<n_vars(); i++)
      {
        if (expvector[i] != 0)
          {
            mDegreeMonoid->power(mDegreeOfVar[i], expvector[i], mon1);
            mDegreeMonoid->mult(result, mon1, result);
          }
      }
    mDegreeMonoid->remove(mon1);
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

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
