// Copyright 1995 Michael E. Stillman.
#ifndef _z_mod_p_hh_
#define _z_mod_p_hh_

#include "ring.hpp"

class Z_mod : public Ring
{
  //int P; // this is defined in class Ring
  int _P1;	// = P-1
  int _ZERO;     // = p-1, log of zero...

  int _prim_root;
  int _minus_one;
  int *_exp_table;
  int *_log_table;

  int int_to_exp(int a) const;

protected:
  Z_mod() {}
  virtual ~Z_mod() {}
  bool initialize_Z_mod(int p);
public:
  static Z_mod * create(int p);

  Z_mod * cast_to_Z_mod() { return this; }
  const Z_mod * cast_to_Z_mod() const { return this; }

  virtual int coerce_to_int(ring_elem a) const;

  int to_int(int a) const;

// The following are all the routines required by 'ring'
  virtual bool is_pid() const       { return 1; }
  virtual bool has_gcd() const      { return 1; }
  virtual bool is_graded() const    { return 1; }
  virtual bool is_expensive() const { return 0; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

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
  virtual ring_elem gcd(const ring_elem f, const ring_elem g) const;
  virtual ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const;

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;
  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
					 ring_elem &quot) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual int primary_degree(const ring_elem f) const;
  virtual void degree_weights(const ring_elem f, const M2_arrayint wts, 
			      int &lo, int &hi) const;
  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       const M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, const M2_arrayint wts) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
