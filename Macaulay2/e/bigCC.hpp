// Copyright 1995 Michael E. Stillman.

// need to be careful that exponent does not overflow, according to GMP

#ifndef _bigCC_hh_
#define _bigCC_hh_

#include "ring.hpp"
#include <gmp.h>

#define BIGCC_VAL(f) (M2_BigComplex ((f).poly_val))
#define BIGCC_RE(f) (&BIGCC_VAL(f)->re)  // returns actual value, not copy
#define BIGCC_IM(f) (&BIGCC_VAL(f)->im)
#define BIGCC_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))

class bigCC : public Ring
{
  int _elem_size;

  M2_BigComplex _zero_elem;

  mpf_ptr new_mpf() const;
  M2_BigComplex new_elem() const;
  void remove_elem(M2_BigComplex f) const;

  static mpf_ptr _epsilon;  // Components (real or imag) less than this are considered zero.

protected:
  bigCC() {}
  virtual ~bigCC() {}
  bool initialize_bigCC();
public:
  static bigCC * create();

  bigCC * cast_to_bigCC() { return this; }
  const bigCC * cast_to_bigCC() const { return this; }

  static void set_epsilon(mpf_ptr epsilon);
  static mpf_ptr get_epsilon();

  // should there be a complex conjugation function?

  ring_elem from_doubles(double r, double s) const;
  ring_elem from_BigReals(mpf_ptr a, mpf_ptr b) const;

  virtual mpf_ptr to_BigReal(ring_elem f) const;

  bool is_real(const ring_elem f) const;  // see if |f| is purely real
  bool is_greater(const ring_elem f, const ring_elem g) const;  // compares |f| and |g|
  ring_elem absolute(const ring_elem f) const;  // norm |f| of f

  void zeroize_tiny_lead_components(vec &v, mpf_ptr epsilon) const; 
  // zeroizes coeffs until imag or real part of lead coeff greater than epsilon in abs value

  // The following are all the routines required by 'ring'
  virtual bool is_bigCC() const         { return true; }

  virtual bool is_pid() const       { return 1; }
  virtual bool has_gcd() const      { return 1; }
  virtual bool is_graded() const    { return 1; }
  virtual bool is_expensive() const { return 1; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_double(double r) const;
  virtual ring_elem from_rational(mpq_ptr r) const;
  virtual ring_elem from_complex(M2_CC z) const;
  virtual ring_elem from_BigReal(mpf_ptr r) const;
  virtual ring_elem from_BigComplex(M2_BigComplex z) const;
  virtual ring_elem var(int v, int n) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  // TO DO: MAKE IT SAME AS CC
  virtual ring_elem preferred_associate(ring_elem f) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

  virtual void negate_to(ring_elem &f) const;
  virtual void add_to(ring_elem &f, ring_elem &g) const;
  virtual void subtract_to(ring_elem &f, ring_elem &g) const;
  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  virtual ring_elem power(const ring_elem f, mpz_t n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

  virtual ring_elem invert(const ring_elem f) const;

  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g, ring_elem &rem) const;

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;
  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
					 ring_elem &quot) const;

  virtual ring_elem gcd(const ring_elem f, const ring_elem g) const;
  virtual ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const;

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
