/* Copyright 2017 Mahrud Sayrafi and Michael E. Stillman
   Mahrud Sayrafi's code in this file is in the public domain. */

#ifndef _localring_hh_
#define _localring_hh_

#include "ring.hpp"
#include "poly.hpp"
#include "polyring.hpp"
#include "comp-gb.hpp"

struct local_elem
{
  ring_elem numer;
  ring_elem denom;
};

class LocalRing : public Ring
{
 private:
  const PolyRing *mRing;
  GBComputation *mPrime;

  LocalRing() {}
  virtual ~LocalRing() {}

  bool initialize_local(const PolyRing *R, GBComputation *P);
  local_elem *make_elem(ring_elem a, ring_elem b) const;
  local_elem *new_local_elem() const;

  bool is_in_prime(const ring_elem f) const;
  void simplify(local_elem *f) const;

  // FIXME remove:
  bool use_gcd_simplify = true;
  ring_elem set_non_unit_frac(ring_elem top) const;

 public:
  LocalRing *cast_to_LocalRing() { return this; }
  const LocalRing *cast_to_LocalRing() const { return this; }

  const PolyRing *get_ring() const { return mRing; }
  unsigned long get_precision() const { return mRing->get_precision(); }

  virtual bool is_local_ring() const { return true; }
  virtual bool is_graded() const { return mRing->is_graded(); }
  virtual CoefficientType coefficient_type() const;

  static LocalRing *create(const PolyRing *R, GBComputation *P);

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;

  ring_elem numerator(ring_elem f) const;
  ring_elem denominator(ring_elem f) const;
  ring_elem fraction(const ring_elem top, const ring_elem bottom) const;

  virtual void lift_up(const Ring *R, const Matrix *m, Matrix *&result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool promote(const Ring *R,
                       const ring_elem f,
                       ring_elem &result) const;

  virtual bool from_rational(mpq_srcptr n, ring_elem &result) const;
  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_srcptr n) const;
  virtual ring_elem var(int v) const;

  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  void lower_content(ring_elem &c, const ring_elem g) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, monomial d) const;
  virtual bool multi_degree(const ring_elem f, monomial d) const;
  virtual void degree_weights(const ring_elem f,
                              M2_arrayint wts,
                              int &lo,
                              int &hi) const;
  virtual ring_elem homogenize(const ring_elem f,
                               int v,
                               int deg,
                               M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, M2_arrayint wts) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;
  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;

  virtual void syzygy(const ring_elem a,
                      const ring_elem b,
                      ring_elem &x,
                      ring_elem &y) const;

  virtual ring_elem random() const;

  virtual ring_elem eval(const RingMap *map,
                         const ring_elem f,
                         int first_var) const;

  virtual int n_fraction_vars() const;  // FIXME
  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const_monomial m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const_monomial m) const;
  virtual ring_elem get_terms(int nvars0,
                              const ring_elem f,
                              int lo,
                              int hi) const;

  virtual void text_out(buffer &o) const;
  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one = true,
                             bool p_plus = false,
                             bool p_parens = false) const;
  virtual unsigned int computeHashValue(const ring_elem a) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
