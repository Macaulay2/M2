// Copyright 1995 Michael E. Stillman

#ifndef _ring_elem_hh_
#define _ring_elem_hh_

/**
 * @file relem.hpp
 * @brief `RingElement` --- tagged `(Ring*, ring_elem)` pair, the engine's universal element type.
 *
 * Declares `RingElement`, the `EngineObject` subclass that
 * stores one element of a ring as a pair `(R, val)`: the
 * `Ring*` pointer tags the value and the opaque `ring_elem`
 * carries it. Arithmetic is exposed through operator overloads
 * (unary `operator-`, binary `operator+` / `operator-` /
 * `operator*` / `operator/`, plus `operator*(int)`,
 * `power(mpz_srcptr)` / `power(int)`, and `invert()`), each
 * delegating to the corresponding virtual on the ring; the same
 * `RingElement` shape thus represents integers, rationals,
 * polynomials, quotient-ring elements, Weyl operators, and any
 * other algebraic object the engine knows about. The
 * `make_raw(R, f)` factory wraps a freshly-built `ring_elem`
 * with its ring; direct construction via the public constructor
 * is rare.
 *
 * Beyond core arithmetic, the class offers `promote(S, result)`
 * / `lift(S, result)` for the canonical inter-ring maps
 * (`R -> R[x]`, `R -> R/I`, `R -> frac R`, `Z/p[x]/F -> GF`),
 * polynomial accessors (`lead_term`, `rest`, `n_terms`,
 * `get_terms`, `get_coeff`, `lead_coeff`, `lead_monom`,
 * `is_homogeneous`, `homogenize`, `degree_weights`, `degree`,
 * `multi_degree`), fraction-field helpers (`numerator`,
 * `denominator`, `fraction`), content extraction (`content`,
 * `remove_content`, `split_off_content`), and the univariate-
 * over-finite-field helper `getSmallIntegerCoefficients`. The
 * `ring_elem` union itself lives in `ringelem.hpp` --- small
 * values (`int`, Z/p element) stay inline, large values
 * (`mpz_ptr`, polynomial pointer) reference heap-allocated
 * storage, and each ring is the only thing that knows what its
 * `val` means.
 *
 * @see ring.hpp
 * @see ringelem.hpp
 */

#include "ring.hpp"

class EngineMonomial;

/**
 * @brief Front-end-visible "ring element" value: an engine `ring_elem`
 * paired with the `Ring*` that gives it meaning.
 *
 * @details The interpreter handles ring elements as opaque
 * `RingElement*` pointers and asks the engine to do arithmetic
 * by dispatching through `get_ring()`. Inherits from
 * `EngineObject` so the hash is content-based (computed via the
 * ring's `computeHashValue`) and the element is immutable once
 * exposed to the front end. `make_raw` is the standard factory
 * the interface layer uses to wrap a freshly produced
 * `ring_elem`.
 */
class RingElement : public EngineObject
{
  const Ring *R;
  ring_elem val;

 protected:
  virtual unsigned int computeHashValue() const;

 public:
  RingElement(const Ring *R, ring_elem f);

  static RingElement *make_raw(const Ring *R, ring_elem f);

  ring_elem get_value() const { return val; }

  const Ring *get_ring() const { return R; }

  // ring arithmetic

  bool is_zero() const;
  bool is_equal(const RingElement &b) const;
  bool is_unit() const;

  RingElement *operator-() const;
  RingElement *operator+(const RingElement &b) const;
  RingElement *operator-(const RingElement &b) const;
  RingElement *operator*(const RingElement &b) const;
  RingElement *operator*(int n) const;
  RingElement *power(mpz_srcptr n) const;
  RingElement *power(int n) const;

  RingElement *operator/(const RingElement &b) const;
  RingElement *invert() const;

  static RingElement *random(const Ring *R);

  void text_out(buffer &o) const;

  // We have several ways of moving from one ring to the next:
  //    R ---> R[x1..xn]
  //    R ---> R/I
  //    R ---> frac R
  //    Z/p[x]/F(x) ---> GF(p,n)
  //    R ---> local(R,I)    (much later...)

  // Both of the following routines assume that S ---> 'this'
  // is one of these construction steps.  Promote takes an element of
  // S, and maps it into 'this', while lift goes the other way.

  bool promote(const Ring *S, const RingElement *&result) const;
  bool lift(const Ring *S, const RingElement *&result) const;

  // functions primarily for polynomial ring elements

  RingElement *lead_term(int n = -1) const;
  RingElement *rest() const;

  ////////////////////////////////////
  // The following take extra arguments, using logical monoid and coefficient
  // ring
  // nvars is the number of variables in an outermost monoid
  // coeffR is the coefficient ring the result coefficients will be in
  int n_terms(int nvars) const;
  RingElement /* or null */ *get_terms(int nvars, int lo, int hi) const;
  RingElement /* or null */ *get_coeff(const Ring *coeffR,
                                       const EngineMonomial *m) const;
  RingElement /* or null */ *lead_coeff(const Ring *coeffR) const;
  EngineMonomial /* or null */ *lead_monom(int nvars) const;
  ////////////////////////////////////

  bool is_homogeneous() const;
  RingElement *homogenize(int v, const std::vector<int> &wts) const;
  RingElement *homogenize(int v, int deg, const std::vector<int> &wts) const;
  void degree_weights(const std::vector<int> &wts, int &lo, int &hi) const;
  const_monomial degree() const { return R->degree(val); }
  bool multi_degree(monomial d) const { return R->multi_degree(val, d); }

  // See engine.h for the definition of 'content' here
  const RingElement /* or null */ *content() const;
  const RingElement /* or null */ *remove_content() const;
  const RingElement /* or null */ *split_off_content(
      const RingElement /* or null */ *&result) const;

  // functions for fraction fields

  RingElement *numerator() const;
  RingElement *denominator() const;
  RingElement *fraction(const Ring *R, const RingElement *bottom) const;

  // functions for univariate polynomials

  // raises an exception if ring is not a univariate polynomial ring
  // returns false if the polynomial coefficients cannot be coerced to long
  // ints.
  // otherwise: returns true, and sets result_coeffs.
  // (Mainly useful for univariate poly rings over finite fields)
  bool getSmallIntegerCoefficients(std::vector<long> &result_coeffs) const;
  M2_arrayintOrNull getSmallIntegerCoefficients() const;
};

inline RingElement::RingElement(const Ring *R0, ring_elem f) : R(R0), val(f) {}
inline bool RingElement::is_zero() const { return get_ring()->is_zero(val); }
inline bool RingElement::is_unit() const { return get_ring()->is_unit(val); }
inline bool RingElement::is_equal(const RingElement &b) const
{
  if (this == &b) return true;
  if (get_ring() != b.get_ring())
    {
      ERROR("cannot compare ring elements from different rings");
      return false;
    }
  return get_ring()->is_equal(val, b.val);
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
