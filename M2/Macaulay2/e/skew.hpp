// Copyright 2010 Michael E. Stillman

#ifndef _skew_hpp_
#define _skew_hpp_

/**
 * @file skew.hpp
 * @brief `SkewMultiplication` --- configuration object naming the skew-commuting variables of a ring.
 *
 * Declares `SkewMultiplication`, the pure-data record a
 * `PolyRing` (and `f4/moninfo.hpp`'s `MonomialInfo`) attaches
 * when the user declares a subset of the variables to
 * skew-commute. The record carries the total variable count
 * `_n_vars`, the skew-variable count `_n_skew`, an ordered
 * `_skew_list` of skew-variable indices for iteration, and a
 * `_skew_exp` boolean bitmap indexed by variable for O(1)
 * lookup --- the two representations exist for the different
 * hot-loop patterns engine code walks.
 *
 * Beyond the lookup helpers `is_skew_var`, `skew_variable`,
 * `skew_degree(exp)` (number of skew variables active in
 * `exp`), and `skew_vars(exp, result)` (their indices), the
 * class also computes `mult_sign(exp1, exp2)` --- the +/-1
 * sign produced by the transpositions needed to interleave the
 * skew variables of the two factors --- plus `diff` / `divide`
 * exponent helpers and `exp_is_zero(exp)`, which detects
 * `x_i^2 = 0` collapses on the skew side. Full polynomial
 * multiplication still lives in `skewpoly.cpp`, `f4/`, and the
 * resolution code in `schreyer-resolution/`, which consult
 * `SkewMultiplication` for these signs and predicates.
 *
 * @see polyring.hpp
 */

/**
 * @brief Sign-rule helper used by every ring that has a skew-commutative
 * subset of variables (exterior factor, full skew ring, ...).
 *
 * @details Stores which of the `_n_vars` variables are skew
 * (`_skew_list[0.._n_skew-1]`, with `_skew_exp[i]` a fast
 * "is variable `i` skew?" lookup), plus the byte size used to
 * cache exponent vectors. Hands out the inversion count needed by
 * the wrapping ring's `mult_by_term1` to decide the sign of a
 * product, and detects `x_i^2 = 0` collapses on the skew side.
 * Full polynomial multiplication still lives in the rings that
 * embed this helper (`SkewPolynomialRing`, `PolyRing` with skew
 * factors, F4 / resolution code).
 */
class SkewMultiplication
{
 public:
  int _n_vars;
  int _n_skew;
  int *_skew_list;  // An array 0.._n_skew-1, listing the indices of the skew
                    // comm variables.
  bool *_skew_exp;  // 0.._n_vars-1

  unsigned long skew_byte_size;

  SkewMultiplication();
  SkewMultiplication(int nvars, int nskew, int *skew_list);
  ~SkewMultiplication() {}
  int n_skew_vars() const { return _n_skew; }
  bool is_skew_var(int i) const { return _skew_exp[i]; }
  int skew_variable(int i) const { return _skew_list[i]; }
  // number of variables occurring in 'exp' which are skew variables
  int skew_degree(const int *exp) const;

  int skew_vars(const int *exp, int *result) const;
  int skew_vars(const long *exp, int *result) const;
  // The number s of skew variables in 'exp' is returned, and their
  // indices are placed in result[0], ..., result[s-1].
  // The space that 'result' points to MUST hold at least 'nskew' ints.

  int mult_sign(const int *exp1, const int *exp2) const;
  int mult_sign(const long *exp1, const long *exp2) const;

  int diff(const int *exp1, const int *exp2, int *result) const;
  int divide(const int *exp1, const int *exp2, int *result) const;
  bool exp_is_zero(const int *exp) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
