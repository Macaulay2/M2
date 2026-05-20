// Copyright 1996  Michael E. Stillman
#ifndef _ringmap_hh_
#define _ringmap_hh_

/**
 * @file ringmap.hpp
 * @brief `RingMap` --- engine representation of a ring homomorphism.
 *
 * Declares `RingMap`, an `EngineObject` that stores a
 * homomorphism via the target ring `R` (with `P` set to its
 * polynomial side and `K` to its coefficient ring) and an
 * array `_elem` of `nvars` image records, one per generator of
 * the (matrix-supplied) source. Each `var` record carries the
 * pre-factored image `coeff * monom * bigelem` together with
 * `is_zero`, `coeff_is_one`, `monom_is_one`, and
 * `bigelem_is_one` flags so the inner-loop evaluator can skip
 * trivial multiplications. A top-level `is_monomial` flag
 * unlocks a much faster evaluation path when every image is a
 * single monomial of the target. Application of a `RingMap` to
 * a polynomial walks each term, substitutes the image of every
 * variable, and accumulates the result in the target, threading
 * coefficient maps through where source and target differ in
 * coefficient ring (the `eval_term` entry point).
 *
 * `RingMap`s are typically built ad hoc by `RingMap::make(m)`
 * from a matrix giving the images of the source variables
 * (quotient projection in `qring.cpp`, M2's `map(S, R, ...)`,
 * coercion-style maps, ring-extension morphisms) and
 * short-lived. Composition is not a first-class operation:
 * callers evaluate one map on the image list of another to
 * produce a new image list.
 *
 * @see ring.hpp
 * @see qring.hpp
 */

#include "ring.hpp"

class RingElement;
class Matrix;

/**
 * @brief Engine-side ring homomorphism: stores, for each source-ring
 * variable, the target-ring element it maps to.
 *
 * @details Built from a row matrix of target-ring elements (`make(m)`): the
 * `i`-th column gives the image of the `i`-th source variable, kept
 * in pre-factored form as `coeff * monom * bigelem`. The
 * `is_monomial` flag records whether every variable maps to a
 * single term, enabling a fast monomial-by-monomial evaluation
 * path. `R` / `P` / `K` / `M` cache the target ring (and its
 * polynomial / coefficient / monoid layers, when present) so
 * `evaluate` can dispatch without re-querying.
 */
class RingMap : public EngineObject
{
  /**
   * @brief Per-source-variable image record: a factored representation of
   * the target-ring element that variable maps to.
   *
   * @details The image is `coeff * monom * bigelem`. Three independent
   * `is_one` flags let the evaluator skip whichever factors are
   * trivially 1, and `is_zero` short-circuits the whole multiplication
   * when the variable maps to 0.
   */
  struct var : public our_new_delete
  {
    bool is_zero;  // Does this variable map to 0?

    bool coeff_is_one;
    bool monom_is_one;
    bool bigelem_is_one;

    ring_elem coeff;  // this variable maps to coeff*monom*bigelem
                      // where coeff is 1 if isone is true.
                      // and   monom is 1 if mon_isone is true,
                      // and   bigelem is 1 if bigone_isone is true.
                      // coeff is an element of type K.
    int *monom;       // This is an exponent vector in R.
    ring_elem bigelem;
  };

  const Ring *R;  // This is the target ring.

  const PolynomialRing *P;  // P is either R (if it is a poly ring) or 0, ifnot
  const Ring *K;            // K is either coeffs of P, or R (if P==0).
  const Monoid *M;

  bool is_monomial;  // True, if each term maps to a term in the
                     // target ring.

  int nvars;   // Number of variables in the source ring.
  var *_elem;  // elem[i] is the structure representing the image of
               // the i th variable.
  RingMap(const Matrix *m);

 protected:
  virtual unsigned int computeHashValue() const;

 public:
  ~RingMap();

  static const RingMap *make(const Matrix *m);

  const Ring *get_ring() const { return R; }
  bool is_equal(const RingMap *phi) const;

  const ring_elem elem(int i) const
  {
    assert(i >= 0);
    assert(i < nvars);
    return _elem[i].bigelem;
  }

  ring_elem eval_term(const Ring *coeff_ring,
                      const ring_elem coeff,
                      const int *vp,
                      int first_var,
                      int nvars_in_source) const;

  RingElement /* or null */ *eval(const RingElement *r) const;
  Matrix /* or null */ *eval(const FreeModule *newrows, const Matrix *m) const;

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
