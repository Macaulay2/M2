// Copyright 1997 Michael E. Stillman
// TODO: utilize ExponentVector instead of int *

#ifndef _weylalg_hh_
#define _weylalg_hh_

/**
 * @file weylalg.hpp
 * @brief `WeylAlgebra` --- ring of polynomial differential operators with `[d_i, x_i] = 1`.
 *
 * Declares the `PolyRing` subclass for the Weyl algebra: each
 * pair `(x_i, d_i)` of coordinate and derivative satisfies
 * `d_i x_i = x_i d_i + 1`, while every other generator commutes
 * normally. The user supplies the assignment via the
 * `_derivative` / `_commutative` arrays
 * (`_derivative[i] = j` means generator `i` is `d_j`;
 * `_derivative[i] = -1` marks a plain commuting variable),
 * so the multiplication routine can recognise out-of-order
 * `d_i x_i` products and unfold them via the Leibniz expansion
 * `d^a x^b = sum_k C(a, k) C(b, k) k! x^{b - k} d^{a - k}`.
 * Two static caches `binomtable` and `diffcoeffstable` (sized
 * up to `binomtop` / `diffcoeffstop`) memoise the binomial and
 * Leibniz coefficients so the inner loop avoids recomputation.
 *
 * Setting `_homogeneous_weyl_algebra` and `_homog_var` selects
 * the homogenised variant in which `d_i` shares its degree
 * with `x_i` (via the extra homogeneity variable absorbing the
 * difference); the result is graded and lets ordinary GB
 * machinery run unchanged on characteristic-variety and
 * D-module intersection problems. The TODO at the top of the
 * header flags a planned migration from raw `int *` arrays to
 * `ExponentVector`.
 *
 * @see poly.hpp
 * @see solvable.hpp
 */

#include "poly.hpp"
#include "gbring.hpp"

///// Ring Hierarchy ///////////////////////////////////

/**
 * @brief `PolyRing` subclass for Weyl algebras: polynomial rings with the
 * `[d_i, x_i] = 1` derivative-variable commutation relations.
 *
 * @details Tracks a `_derivative[i]` / `_commutative[i]` index pair so the
 * ring knows which variables are partial derivatives and which are
 * the variables they differentiate. Optional homogeneous mode
 * (`_homogeneous_weyl_algebra`) adds an extra homogenising variable
 * `_homog_var` used to keep degrees compatible across commutator
 * expansions. The static `binomtable` / `diffcoeffstable` cache
 * binomial coefficients and derivative-coefficient products used by
 * term-by-term multiplication.
 */
class WeylAlgebra : public PolyRing
{
  int _nderivatives;
  bool _homogeneous_weyl_algebra;
  int _homog_var;    // Only used if 'homogeneous_weyl_algebra' is true.
  int *_derivative;  // a value _derivative[i] = r >= 0 means that i is diff(r).
                     // If < 0 : the variable i does not have a diff op.
  int *_commutative;  // Same as above, but in opposite direction.

  static int binomtop;
  static int diffcoeffstop;
  static int **binomtable;
  static int **diffcoeffstable;

  void initialize1();
  bool initialize_weyl(M2_arrayint derivs, M2_arrayint comms, int homog_var);
  WeylAlgebra() {}
  virtual ~WeylAlgebra() {}
 protected:
  void extractDerivativePart(const_exponents exp, int *result) const;
  void extractCommutativePart(const_exponents exp, int *result) const;
  ring_elem binomial(int top, int bottom) const;
  ring_elem multinomial(const ring_elem a,
                        const_exponents exptop,
                        const_exponents expbottom) const;
  bool increment(int *current_derivative, const int *top_derivative) const;

  bool divides(const_exponents expbottom, const_exponents exptop) const;
  ring_elem diff_coefficients(const ring_elem c,
                              const int *derivatives,
                              const_exponents exp) const;

  Nterm *weyl_diff(const ring_elem c,
                   const_exponents expf,
                   const int *derivatives,
                   const Nterm *g) const;  // An entire polynomial
  vec weyl_diff(const ring_elem c,
                const_exponents expf,
                const int *derivatives,
                const vec g) const;  // An entire polynomial
  vec weyl_diff(const FreeModule *resultF,
                const ring_elem c,
                const_exponents expf,
                int component,
                const int *derivatives,
                const Nterm *g) const;  // An entire polynomial

  gbvector *gbvector_weyl_diff(
      GBRing *GR,
      const ring_elem c,  // in K
      int comp,           // adds this component to each component of g.
      const_exponents expf,
      const int *derivatives,
      const FreeModule *Fg,      // Free module of g, unless g is a ring element
      const gbvector *g) const;  // An entire polynomial
 public:
  static WeylAlgebra *create(const Ring *K,
                             const Monoid *M,
                             M2_arrayint derivs,
                             M2_arrayint comms,
                             int homog_var);

  virtual bool is_commutative_ring() const { return false; }
  virtual bool is_weyl_algebra() const { return true; }
  virtual const WeylAlgebra *cast_to_WeylAlgebra() const { return this; }
  virtual void text_out(buffer &o) const;

  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

  ring_elem multinomial(const_exponents exptop, const_exponents exp) const;

 public:
  virtual ring_elem mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const_monomial m) const;

  gbvector *gbvector_mult_by_term(
      gbvectorHeap &result,
      const gbvector *f,
      const ring_elem c,  // in the base K
      const_monomial m,   // monomial, in M
      int comp) const;    // comp is either 0 or a real component.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
