// Copyright 1995 Michael E. Stillman

#ifndef _ring_hh_
#define _ring_hh_

#include "ringelem.hpp"
#include "monoid.hpp"

///// Ring Hierarchy ///////////////////////////////////

class Z;
class QQ;
class RR;
class Z_mod;
class GF;
class FractionField;
class PolynomialRing;
class SkewPolynomialRing;
class SchurRing;
class WeylAlgebra;
class SolvableAlgebra;

class FreeModule;
class RingMap;

class GRType; // Used in GBRing, but each ring is tagged with a GBType

class Ring : public mutable_object
{
protected:
  int P;
  int _nvars;
  int _totalvars;		// The total number of variables, inclduing all base rings
  const Ring *_K;		// For a base ring, this will point to self
  const Monoid *_M;
  const Monoid *_D;
  const PolynomialRing *_HRing;	// Hilbert function ring, if D has >= 1 variables.
				// Otherwise, this will be NULL.
  const GRType *_grtype;	// Used for translation of gbvectors to/from ringelems and vecs.
  const Ring *_flattened_ring;	// Either 'this', or a polynomial ring whose coeff ring is 
                                // 'basic', eg ZZ, ZZ/p, RR, or some other 
                                // non fraction, non poly ring.

  ring_elem _zero_divisor;
  bool _isfield;		// true means yes, or declared yes.
				// If a zero divisor is found, isfield is set to false.

  bool _isquotientring;		// True if constructed via Ring(const Ring &R).
				// If true, then vecstash,resstash are not owned.

  bool _is_ZZ_quotient;		// true if this is a quotient of a polynomial ring over ZZ, AND
				// there is an integer in the factored ideal.
  ring_elem _ZZ_quotient_value;	// This is the integer in the factor ideal, if is_ZZ_quotient is set.

  void initialize_ring(int charac, int nvars, int totalvars, const Ring *K, 
	const Monoid *M, const Monoid *D);
  Ring() {}
public:
  virtual ~Ring();

  int charac() const { return P; }
  int n_vars() const { return _nvars; }
  int total_n_vars() const { return _totalvars; }

  const Ring * get_ring() const { return this; }
  const Ring *  Ncoeffs()       const { return _K; }
  const Monoid * Nmonoms()       const { return _M; }
  const Monoid * degree_monoid() const { return _D; }
  const PolynomialRing *HilbertRing() const { return _HRing; }
  const GRType * get_GRType() const { return _grtype; }
  const Ring * get_flattened_ring() const {  return _flattened_ring; }

  virtual FreeModule *make_FreeModule() const;
  virtual FreeModule *make_FreeModule(int n) const;

  virtual const Z * cast_to_Z() const         { return 0; }
  virtual       Z * cast_to_Z()               { return 0; }
  virtual const QQ * cast_to_QQ() const         { return 0; }
  virtual       QQ * cast_to_QQ()               { return 0; }
  virtual const RR * cast_to_RR() const         { return 0; }
  virtual       RR * cast_to_RR()               { return 0; }
  virtual const Z_mod * cast_to_Z_mod() const         { return 0; }
  virtual       Z_mod * cast_to_Z_mod()               { return 0; }
  virtual const GF * cast_to_GF() const         { return 0; }
  virtual       GF * cast_to_GF()               { return 0; }
  virtual const PolynomialRing * cast_to_PolynomialRing()  const      { return 0; }
  virtual       PolynomialRing * cast_to_PolynomialRing()             { return 0; }
  virtual const FractionField * cast_to_FractionField() const    { return 0; }
  virtual       FractionField * cast_to_FractionField()          { return 0; }
  virtual const SchurRing * cast_to_SchurRing() const { return 0; }
  virtual       SchurRing * cast_to_SchurRing()       { return 0; }
  virtual const SkewPolynomialRing * cast_to_SkewPolynomialRing()  const      { return 0; }
  virtual       SkewPolynomialRing * cast_to_SkewPolynomialRing()             { return 0; }
  virtual const SolvableAlgebra * cast_to_SolvableAlgebra()  const      { return 0; }
  virtual       SolvableAlgebra * cast_to_SolvableAlgebra()             { return 0; }
  virtual const WeylAlgebra *cast_to_WeylAlgebra() const { return 0; }

  virtual bool is_basic_ring() const { return true; } // The default is to be a basic ring.
  virtual bool is_ZZ() const { return false; }
  virtual bool is_QQ() const { return false; }
  virtual bool is_fraction_field() const { return false; }
  virtual bool is_poly_ring() const { return false; }
  virtual bool is_quotient_poly_ring() const { return false; }
  virtual bool is_commutative_ring() const { return true; }
  virtual bool is_weyl_algebra() const { return false; }
  virtual bool is_skew_commutative_ring() const { return false; }
  virtual bool is_solvable_algebra() const { return false; }

  virtual bool is_pid() const = 0;
  virtual bool has_gcd() const = 0;
  virtual bool is_graded() const = 0;
  virtual bool is_expensive() const = 0;

  bool is_field() const;
  void declare_field();
  ring_elem get_zero_divisor() const;

  virtual void text_out(buffer &o) const = 0;

  virtual int coerce_to_int(ring_elem a) const;

  virtual ring_elem from_int(int n) const = 0;
  virtual ring_elem from_int(mpz_ptr n) const = 0;
  virtual ring_elem from_double(double a) const;  // The default version converts to a mpz_ptr,
				// then uses from_int.
  virtual ring_elem from_rational(mpq_ptr q) const;  
  // The default version calls from_int(0). Change it?
  virtual ring_elem from_complex(M2_CC z) const;
  // The default version calls from_int(0). Change it?
  virtual ring_elem from_BigReal(mpf_ptr a) const;  
  // The default version calls from_int(0). Change it?
  virtual ring_elem from_BigComplex(M2_BigComplex z) const;  
  // The default version calls from_int(0). Change it?

  virtual ring_elem var(int v, int n) const = 0;

  virtual ring_elem preferred_associate(ring_elem f) const;
  // Returns an invertible element c of the same ring such that c*f is the
  // preferred associate of the element f.
  // WARNING: The default implementation is for a field.

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const = 0;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const = 0;

  virtual bool is_unit(const ring_elem f) const = 0;
  virtual bool is_zero(const ring_elem f) const = 0;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const = 0;

  virtual ring_elem copy(const ring_elem f) const = 0;
  virtual void remove(ring_elem &f) const = 0;
  void remove_vector(vec &v) const;

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

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
					 ring_elem &quot) const = 0;
  // These three routines: remainder, quotient and remainderAndQuotient
  // satisfy these properties:
  // If r = remainder(f,g), q = quotient(f,g), then
  // (1) f = q*g + r
  // (2) If f is in ideal(g), then r = 0.
  // (3) If g is invertible, then r = 0, and q = f * g^(-1).
  // (4) If the ring is ZZ, then the remainder is "balanced": -[g/2] < r <= [g/2]
  // remainderAndQuotient combines remainder and quotient into one routine.

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

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const = 0;

  // Polynomial routines
  // The default implementation is for non-polynomial rings
  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const;
  virtual bool in_subring(int n, const ring_elem a) const;
  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const;
  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const;
  virtual ring_elem divide_by_expvector(const int *exp, const ring_elem a) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;

  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       const M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, const M2_arrayint wts) const;

  // Routines expecting a grading.  The default implementation
  // is that the only degree is 0.
  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;
    // returns true iff f is homogeneous
  virtual int primary_degree(const ring_elem f) const;
  virtual void degree_weights(const ring_elem f, const M2_arrayint wts, int &lo, int &hi) const;

};

#include "Z.hpp"
extern Z *ZZ;
extern QQ *globalQQ;
extern RR *RRR;
#endif
