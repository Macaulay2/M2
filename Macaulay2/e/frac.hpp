// Copyright 1996 Michael E. Stillman.
#ifndef _frac_hh_
#define _frac_hh_

#include "ring.hpp"
#include "polyring.hpp"

struct frac_elem
{
  ring_elem numer;
  ring_elem denom;
};

class FractionField : public Ring
{
  const PolyRingFlat *R_;		// Base ring.  Assumed to be a domain.
  ring_elem _MINUS_ONE;		// this is -1 in the ring R.
  bool use_gcd_simplify;     // Use built in gcd only if this is frac(ZZ[xs]) or frac(ZZ/p[xs])
                             // When we (if we) change fractions to be flat, then this
                             // restriction should go away.

  ring_elem set_non_unit_frac(ring_elem top) const;
  frac_elem *new_frac_elem() const;
  void simplify(frac_elem *f) const;
  frac_elem *make_elem(ring_elem a, ring_elem b) const;

protected:
  FractionField() {}
  virtual ~FractionField() {}
  bool initialize_frac(const PolyRingFlat *R);
public:
  static FractionField * create(const PolyRingFlat *R);

  FractionField * cast_to_FractionField() { return this; }
  const FractionField * cast_to_FractionField() const { return this; }

  const Ring *get_ring() const { return R_; }

  ring_elem numerator(ring_elem f) const;
  ring_elem denominator(ring_elem f) const;
  ring_elem fraction(const ring_elem top, const ring_elem bottom) const;

// The following are all the routines required by 'ring'
  virtual bool is_fraction_field() const { return true; }

  virtual bool is_graded() const    { return R_->is_graded(); }

  virtual CoefficientType coefficient_type() const;
  virtual int n_fraction_vars() const;

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_rational(mpq_ptr n) const;
  virtual ring_elem var(int v) const;

  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;
  virtual void degree_weights(const ring_elem f, M2_arrayint wts, 
			      int &lo, int &hi) const;
  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, M2_arrayint wts) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

  void internal_negate_to(ring_elem &f) const;
  void internal_add_to(ring_elem &f, ring_elem &g) const;
  void internal_subtract_to(ring_elem &f, ring_elem &g) const;

  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  virtual ring_elem power(const ring_elem f, mpz_t n) const;
  virtual ring_elem power(const ring_elem f, int n) const;
  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(int nvars0, const ring_elem f, int lo, int hi) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
