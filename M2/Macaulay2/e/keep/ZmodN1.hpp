// Copyright 1995 Michael E. Stillman.

#ifndef _ZmodN1_hh_
#define _ZmodN1_hh_

#include "Z.hh"

class ZmodN1 : public Z
{
public:
  ZmodN1(const monoid *D);
  ~ZmodN1() {}

  ZmodN1 * cast_to_ZmodN1() { return this; }
  const ZmodN1 * cast_to_ZmodN1() const { return this; }

// The following are all the routines required by 'ring'
  virtual int is_field() const     { return 0; }
  virtual int is_pid() const       { return 0; }
  virtual int has_gcd() const      { return 0; }
  virtual int is_Z() const         { return 0; }
  virtual int is_poly_ring() const { return 0; }
  virtual int is_quotient_poly_ring() const { return 0; }
  virtual int is_graded() const    { return 1; }
  virtual int is_expensive() const { return 1; }

  virtual void text_out(ostream &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;
  virtual int promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual int lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual int is_unit(const ring_elem f) const;

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
  virtual ring_elem gcd(ring_elem f, ring_elem g) const;
  virtual ring_elem gcd_extended(ring_elem f, ring_elem g, 
				  ring_elem &u, ring_elem &v) const;

  virtual void Nelem_text_out(ostream &o, const ring_elem f) const;
  virtual void Nelem_bin_out(ostream &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap &map, const ring_elem f) const;
};

#endif
