// Copyright 1996 Michael E. Stillman.
#ifndef _frac_hh_
#define _frac_hh_

#include "ring.hpp"

struct frac_elem
{
  ring_elem numer;
  ring_elem denom;
};

class FractionField : public Ring
{
  const Ring *R;		// Base ring.  Assumed to be a domain.
  stash *frac_stash;

  ring_elem MINUS_ONE;		// this is -1 in the ring R.

  frac_elem *new_frac_elem() const;
  void simplify(frac_elem *f) const;
  frac_elem *make_elem(ring_elem a, ring_elem b) const;
  
  FractionField(const Ring *R);
protected:
  virtual ~FractionField();
public:
  static FractionField *create(const Ring *R);

  class_identifier class_id() const { return CLASS_FractionField; }

  // serialize
  virtual void write_object(object_writer &o) const;
  static FractionField *read_object(object_reader &i);
  void write_element(object_writer &o, const ring_elem f) const;
  void read_element(object_reader &i, ring_elem &result) const;

  FractionField * cast_to_FractionField() { return this; }
  const FractionField * cast_to_FractionField() const { return this; }

  const Ring *get_ring() const { return R; }

  ring_elem numerator(ring_elem f) const;
  ring_elem denominator(ring_elem f) const;
  ring_elem fraction(const ring_elem top, const ring_elem bottom) const;

// The following are all the routines required by 'ring'
  virtual bool is_field() const     { return 1; }
  virtual bool is_pid() const       { return 1; }
  virtual bool has_gcd() const      { return 1; }
  virtual bool is_Z() const         { return 0; }
  virtual bool is_poly_ring() const { return 0; }
  virtual bool is_quotient_poly_ring() const { return 0; }
  virtual bool is_graded() const    { return R->is_graded(); }
  virtual bool is_expensive() const { return 1; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual int primary_degree(const ring_elem f) const;
  virtual void degree_weights(const ring_elem f, const int *wts, int &lo, int &hi) const;
  virtual ring_elem homogenize(const ring_elem f, int v, int deg, const int *wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, const int *wts) const;

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
  virtual ring_elem gcd(const ring_elem f, const ring_elem g) const;
  virtual ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;
  virtual ring_elem random(int homog, const int *deg) const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;
  virtual void elem_bin_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;
};

#endif
