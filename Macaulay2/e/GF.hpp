// Copyright 1995 Michael E. Stillman.
#ifndef _GF_hh_
#define _GF_hh_

#include "relem.hpp"

class GF : public Ring
{
  //int P; // this is defined in class Ring
  const PolynomialRing *_originalR; // This should be the ring ((Z/p)[t])/f(t).

  const RingElement *_primitive_element;  // An element of K
  int _x_exponent;			// (primitive_element)^(x_exponent) = x,
					// the given generator of K.
  int Q_;        // this is GF(Q) = GF(P^Qexp)
  int Qexp_;	// P^Qexp = Q
  int Q1_;	// Q1 = Q-1
  int _ZERO;     // = 0   is our representation of 0.
  int _ONE;	// = Q-1 is our representation of 1.
  int _MINUS_ONE; // = (Q-1)/2 if Q odd, = ONE if Q even.

  int *_one_table;   // Indexed from 0..Q1
  int *_from_int_table;

  //  GF(const RingElement *prim);
protected:
  GF();
  virtual ~GF();
  bool initialize_GF(const RingElement *prim);
public:
  static GF * create(const RingElement *prim);

  int to_int(int a) const;

  GF * cast_to_GF() { return this; }
  const GF * cast_to_GF() const { return this; }

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

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual int primary_degree(const ring_elem f) const;
  virtual void degree_weights(const ring_elem f, const M2_arrayint wts, 
			      int &lo, int &hi) const;

  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       const M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, const M2_arrayint wts) const;

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

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;
};

#endif
