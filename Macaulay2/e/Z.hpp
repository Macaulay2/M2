// Copyright 1995 Michael E. Stillman.

#ifndef _Z_hh_
#define _Z_hh_

#include "ring.hpp"
#include <gmp.h>

#define MPZ_VAL(f) (mpz_ptr ((f).poly_val))
#define MPZ_RINGELEM(a) ((ring_elem) ((Nterm *) (a)))

class Z : public Ring
{
  int _elem_size;
  mpz_ptr _zero_elem;

  mpz_ptr new_elem() const;
  void remove_elem(mpz_ptr f) const;

protected:
  Z() {}
  virtual ~Z() {}
  bool initialize_ZZ(const Monoid *D);
public:
  static Z * create(const Monoid *D);

  Z * cast_to_Z() { return this; }
  const Z * cast_to_Z() const { return this; }

// The following are all the routines required by 'ring'
  virtual bool is_ZZ() const         { return true; }

  virtual bool is_pid() const       { return 1; }
  virtual bool has_gcd() const      { return 1; }
  virtual bool is_graded() const    { return 1; }
  virtual bool is_expensive() const { return 1; }

  virtual void text_out(buffer &o) const;

  static unsigned int mod_ui(mpz_t n, unsigned int p);
  static bool get_ui(unsigned int &result, mpz_t n);
  static bool get_si(int &result, mpz_t n);

  // If the base ring of a is Z:
  // To get a bignum from a RingElement a, use: MPZ_VAL(a.get_value())
  // To get a bignum from an ring_elem  a, use: MPZ_VAL(a)

  virtual int coerce_to_int(ring_elem a) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;

  int compare(const ring_elem a, const ring_elem b) const;
  int is_positive(const ring_elem a) const;

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
