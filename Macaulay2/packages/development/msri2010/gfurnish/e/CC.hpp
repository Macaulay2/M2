// Copyright 2001 Michael E. Stillman.

#ifndef _CC_hh_
#define _CC_hh_

#include "ring.hpp"

class CoefficientRingCC;

class CC : public Ring
// Elements of this ring are two real numbers: 'double's representing real and imaginary parts
{
  ring_elem from_M2_CC_struct(M2_CC_struct &a) const;

  M2_CC new_elem() const;

  CoefficientRingCC *coeffR;

protected:
  CC() {}
 ~CC() {}
  bool initialize_CC();
public:
  static CC * create();

  CC * cast_to_CC() { return this; }
  const CC * cast_to_CC() const { return this; }

  CoefficientRingCC *get_CoeffRing() const { return coeffR; }

  ring_elem from_doubles(double r, double s) const;

  double to_double(ring_elem a) const;

// The following are all the routines required by 'ring'
  virtual bool is_CC() const         { return true; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_double(double r) const;
  virtual ring_elem from_rational(mpq_ptr r) const;
  virtual ring_elem from_BigReal(mpfr_ptr a) const;
  virtual ring_elem from_complex(M2_CC z) const;
  virtual bool from_BigComplex(M2_CCC z, ring_elem &result) const;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;

  int is_positive(const ring_elem a) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem a, const ring_elem b) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

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
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
