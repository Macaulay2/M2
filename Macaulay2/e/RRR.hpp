// Copyright 1995 Michael E. Stillman.

// need to be careful that exponent does not overflow, according to GMP

#ifndef _RRR_hh_
#define _RRR_hh_

#include "ZZ.hpp"

class RRR : public Ring
{
  int _elem_size;
  mpfr_ptr _zero_elem;

  mpfr_ptr new_elem() const;
  void remove_elem(mpfr_ptr f) const;

  static mpfr_ptr _epsilon;  // Elements less than this are considered zero.

protected:
  RRR() {}
  virtual ~RRR() {}
  bool initialize_RRR();
public:
  static RRR * create();

  RRR * cast_to_RRR() { return this; }
  const RRR * cast_to_RRR() const { return this; }

  static void set_epsilon(mpfr_ptr epsilon);
  static mpfr_ptr get_epsilon();

  void zeroize_tiny_lead_components(vec &v, mpfr_ptr epsilon) const; 
  // zeroizes coeffs until lead coeff no longer less than epsilon in abs value

  bool is_greater(const ring_elem a, const ring_elem b) const;
  bool is_less(const ring_elem a, const ring_elem b) const;
  ring_elem absolute(const ring_elem f) const;
  ring_elem sqrt(const ring_elem f) const;

  bool from_string(M2_string s, ring_elem &f) const;
  // returns false if an error has occurred.  f is initialized and set with value
  // only if true is returned.

  virtual mpfr_ptr to_BigReal(ring_elem f) const;

// The following are all the routines required by 'ring'
  virtual bool is_RRR() const         { return true; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_double(double r) const;
  virtual ring_elem from_rational(mpq_ptr r) const;
  virtual ring_elem from_BigReal(mpfr_ptr r) const;
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

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
