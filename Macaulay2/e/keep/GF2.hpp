// Copyright 1995 Michael E. Stillman.
#ifndef _GF2_hh_
#define _GF2_hh_

#include "relem.hh"

#undef setbit			// setbit is a macro on some systems!

typedef unsigned char byte;

class GF2 : public Ring
{
  //int P; // this is defined in class Ring
  const PolynomialRing *K; // This should be the ring ((Z/p)[t])/f(t).

  int nwords;
  int nbytes;
  int nbits;			// = n-1, if Q = P^n

  byte *reductum;

  byte *reductum_bits;
  int nreductum_bits;

  byte **reduce_table;	// reduce_table[b] is the nbytes length array which is
				// x^(8*nbytes) (8 bit value) reduced.

  stash *GFstash;		// Only set if nwords > 1

  byte *new_elem() const;

  void xorbit(int d, byte *f) const;
  void setbit(int d, byte *f, int val) const;
  int bitval(int d, byte *f) const;
  
  void make_tables();
  void display0(ostream &o, byte *p, int n) const;
  unsigned int mult0(unsigned int a, unsigned int b) const;
  void mult1(unsigned int *aa, unsigned int *bb, unsigned int *res) const;
  void multx0(byte *a, byte *result) const;
  void reduce0(byte *a, byte *result) const;

  //void mult0(unsigned char *n1, unsigned char *n2, unsigned char *result) const;
  //void reduce0(unsigned char *n, unsigned char *result) const;
public:
  GF2(const PolynomialRing *K);
  ~GF2();

  GF2 * cast_to_GF2() { return this; }
  const GF2 * cast_to_GF2() const { return this; }

// The following are all the routines required by 'ring'
  virtual int is_field() const     { return 1; }
  virtual int is_pid() const       { return 1; }
  virtual int has_gcd() const      { return 1; }
  virtual int is_Z() const         { return 0; }
  virtual int is_poly_ring() const { return 0; }
  virtual int is_quotient_poly_ring() const { return 0; }
  virtual int is_graded() const    { return 1; }
  virtual int is_expensive() const { return 0; }

  virtual void text_out(ostream &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;
  virtual int promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual int lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual int is_unit(const ring_elem f) const;
  virtual int is_zero(const ring_elem f) const;
  virtual int is_equal(const ring_elem f, const ring_elem g) const;

  virtual int is_homogeneous(const ring_elem f) const;
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
  virtual ring_elem gcd(ring_elem f, ring_elem g) const;
  virtual ring_elem gcd_extended(ring_elem f, ring_elem g, 
				  ring_elem &u, ring_elem &v) const;

  virtual void Nelem_text_out(ostream &o, const ring_elem f) const;
  virtual void Nelem_bin_out(ostream &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap &map, const ring_elem f) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;
};

#endif
