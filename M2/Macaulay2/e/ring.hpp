// Copyright 1995 Michael E. Stillman

#ifndef _ring_hh_
#define _ring_hh_

#include "ringelem.hpp"
#include "monoid.hpp"

///// Ring Hierarchy ///////////////////////////////////

class WeylAlgebra;

class Ring : public type
{
  friend class FreeModule;
  friend class res_poly;
  friend class res2_poly;
protected:
  int P;
  int nvars;
  int totalvars;		// The total number of variables, inclduing all base rings
  const Ring *K;		// For a base ring, this will point to self
  const Monoid *M;
  const Monoid *D;
  const PolynomialRing *HRing;	// Hilbert function ring, if D has >= 1 variables.
				// Otherwise, this will be NULL.
  stash *vecstash;		// Stash used to allocate vectors over this ring.
  stash *resstash;		// For resolutions

  Ring(int charac, int nvars, int totalvars, const Ring *K, 
	const Monoid *M, const Monoid *D);
  Ring(const Ring &R);
public:
  virtual ~Ring();

  int charac() const { return P; }
  int n_vars() const { return nvars; }
  int total_n_vars() const { return totalvars; }

  const Ring * get_ring() const { return this; }
  const Ring *  Ncoeffs()       const { return K; }
  const Monoid * Nmonoms()       const { return M; }
  const Monoid * degree_monoid() const { return D; }
  const PolynomialRing *HilbertRing() const { return HRing; }

  virtual FreeModule *make_FreeModule() const;
  virtual FreeModule *make_FreeModule(int n) const;

  virtual const Z * cast_to_Z() const         { return 0; }
  virtual       Z * cast_to_Z()               { return 0; }
  virtual const Z_mod * cast_to_Z_mod() const         { return 0; }
  virtual       Z_mod * cast_to_Z_mod()               { return 0; }
  virtual const PolynomialRing * cast_to_PolynomialRing()  const      { return 0; }
  virtual       PolynomialRing * cast_to_PolynomialRing()             { return 0; }
  virtual const FractionField * cast_to_FractionField() const    { return 0; }
  virtual       FractionField * cast_to_FractionField()          { return 0; }
  virtual const SchurRing * cast_to_SchurRing() const { return 0; }
  virtual       SchurRing * cast_to_SchurRing()       { return 0; }
  virtual const WeylAlgebra *cast_to_WeylAlgebra() const { return 0; }

  virtual bool is_field() const = 0;
  virtual bool is_pid() const = 0;
  virtual bool has_gcd() const = 0;
  virtual bool is_Z() const = 0;
  virtual bool is_poly_ring() const = 0;
  virtual bool is_weyl_algebra() const { return false; }
  virtual bool is_graded() const = 0;
  virtual bool is_expensive() const = 0;
  virtual bool is_quotient_poly_ring() const = 0;
  virtual bool is_commutative_ring() const { return true; }

  virtual void text_out(buffer &o) const = 0;

  virtual int coerce_to_int(ring_elem a) const;

  virtual ring_elem from_int(int n) const = 0;
  virtual ring_elem from_int(mpz_ptr n) const = 0;
  virtual ring_elem var(int v, int n) const = 0;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const = 0;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const = 0;

  virtual bool is_unit(const ring_elem f) const = 0;
  virtual bool is_zero(const ring_elem f) const = 0;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const = 0;

  virtual bool is_homogeneous(const ring_elem f) const = 0;
  virtual void degree(const ring_elem f, int *d) const = 0;
  virtual int primary_degree(const ring_elem f) const = 0;
  virtual void degree_weights(const ring_elem f, const int *wts, int &lo, int &hi) const = 0;
  virtual ring_elem homogenize(const ring_elem f, int v, int deg, const int *wts) const = 0;
  virtual ring_elem homogenize(const ring_elem f, int v, const int *wts) const = 0;

  virtual ring_elem copy(const ring_elem f) const = 0;
  virtual void remove(ring_elem &f) const = 0;

  virtual void negate_to(ring_elem &f) const = 0;
  virtual void add_to(ring_elem &f, ring_elem &g) const = 0;
  virtual void subtract_to(ring_elem &f, ring_elem &g) const = 0;
          void mult_to(ring_elem &f, const ring_elem g) const;
  virtual ring_elem negate(const ring_elem f) const = 0;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem power(const ring_elem f, mpz_t n) const = 0;
  virtual ring_elem power(const ring_elem f, int n) const = 0;
  virtual ring_elem invert(const ring_elem f) const = 0;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem divide(const ring_elem f, const ring_elem g, ring_elem &rem) const = 0;
  virtual ring_elem gcd(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const = 0;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const = 0;
  // Constructs elements x and y in the ring s.t. ax + by = 0.  This syzygy is
  // chosen as simply as possible.  For example, over QQ, x is chosen 
  // to be positive.  The routine must handle the case when a=0, but can
  // ignore the case when b=0... (Really?)

  virtual ring_elem random() const;
  virtual ring_elem random(int homog, const int *deg) const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const = 0;
  virtual void elem_bin_out(buffer &o, const ring_elem f) const = 0;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const = 0;

  // Polynomial routines
  virtual int n_terms(const ring_elem f) const = 0;
  virtual ring_elem term(const ring_elem a, const int *m) const = 0;
  virtual ring_elem lead_coeff(const ring_elem f) const = 0;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const = 0;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const = 0;

  // Routines special to polynomial rings
  // these include: #monomials, leadMonomial, findMonomial, term(a,m)
  // in_subring, div_degree, divide_by_var
  // possibly others?
  // Rideal, exterior_vars.
  // nbits
  // heap merge of elements...?

  // Routines special to PID's
  // these include: gcd, gcd_extended.

  // Routines special to fields (anything else?)
  
  // Infrastructure here
  virtual void write_element(object_writer &o, const ring_elem f) const = 0;
  virtual void read_element(object_reader &i, ring_elem &result) const = 0;

  int          length_of() const      { return n_vars(); }
  const Ring * cast_to_Ring() const { return this; }
  Ring *       cast_to_Ring()       { return this; }

  class_identifier class_id() const { return CLASS_Ring; }
  type_identifier  type_id () const { return TY_RING; }
  const char * type_name   () const { return "Ring"; }
};

#include "Z.hpp"
extern Z *ZZ;
#endif
