// Copyright 1995 Michael E. Stillman

#ifndef _ring_elem_hh_
#define _ring_elem_hh_

#include "ring.hpp"

class Monomial;

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
                                       const Monomial *m) const;
  RingElement /* or null */ *lead_coeff(const Ring *coeffR) const;
  Monomial /* or null */ *lead_monom(int nvars) const;
  ////////////////////////////////////

  bool is_homogeneous() const;
  RingElement *homogenize(int v, M2_arrayint wts) const;
  RingElement *homogenize(int v, int deg, M2_arrayint wts) const;
  void degree_weights(M2_arrayint wts, int &lo, int &hi) const;
  M2_arrayint multi_degree() const;
  //  intarray  degree() const;

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
