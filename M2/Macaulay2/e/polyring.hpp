// Copyright 1995 Michael E. Stillman

#ifndef _polyring_hh_
#define _polyring_hh_

#include "ring.hpp"
#include "ringelem.hpp"
#include "skew.hpp"

///// Ring Hierarchy ///////////////////////////////////

class TermIdeal;
class Matrix;
class GBRing;
class GBRingSkew;
class GBComputation;

#include "poly.hpp"

class PolyRing : public PolynomialRing
{
  friend class GBRingSkew;
  friend class FreeModule;

  void initialize_poly_ring(const Ring *K, const Monoid *M, 
			    const Ring *originalK, const Monoid *originalM, 
			    const PolynomialRing *deg_ring);
  // Only to be called from initialize_poly_ring and make_trivial_ZZ_poly_ring

  static const PolyRing *create_poly_ring(const Ring *K, const Monoid *M,
				   const Ring *originalK, const Monoid *originalM);
protected:
  virtual const PolyRing *createPolyRing(const Monoid *M) const;
  // Create the PolyRing this[M].  This is virtual, since it must be the same type of noncomm
  // as 'this'.  e.g., if K has skew comm variables, so does the result.

  void initialize_poly_ring(const Ring *K, const Monoid *M,
			    const Ring *originalK, const Monoid *originalM);

  virtual ~PolyRing();
  PolyRing() {}

  static PolyRing *trivial_poly_ring;
  static void make_trivial_ZZ_poly_ring();
public:
  static const PolyRing *create(const Ring *K, const Monoid *M);

protected:
  const Ring *K_;
  const Monoid *M_;
  const Ring *logicalK_;
  const Monoid *logicalM_;

  int _poly_size;

  bool _is_ZZ_quotient;		// true if this is a quotient of a polynomial ring over ZZ, AND
				// there is an integer in the factored ideal.
  ring_elem _ZZ_quotient_value;	// This is the integer in the factor ideal, if is_ZZ_quotient is set.

  
  GBRing *_gb_ring;

  bool _coefficients_are_ZZ;

  // Most skew-mult specific poly code is in skewpoly.{hpp,cpp}.  However, var, homogenize,
  //   and diff_term all have a small amount of skew commutative specific code.
  bool _is_skew;
  SkewMultiplication _skew; // Completely ignored if _is_skew is false.

  int *_EXP1, *_EXP2, *_EXP3;
public:
  virtual const PolyRing * getAmbientRing() const { return this; }

  virtual const RingOrNull *getDenominatorRing() const { return 0; }

  virtual const Ring *getCoefficients() const { return K_; }
  // The implementation coeff ring of 'this'.  This is either a basic ring (field, ZZ), or
  // is another PolyRing.

  virtual const Ring *getLogicalCoefficients() const { return logicalK_; }
  // The logical coefficient ring of 'this'.  
  // This is either a non polynomial ring, or it is a PolyRing.

  virtual const Monoid *getMonoid() const { return M_; }
  // The implementation monoid of this ring.

  virtual const Monoid *getLogicalMonoid() const { return logicalM_; }
  // The logical monoid of this polynomial ring.

  static const PolyRing *get_trivial_poly_ring();

  virtual const PolyRing * cast_to_PolyRing()  const { return this; }
  virtual       PolyRing * cast_to_PolyRing()        { return this; }

  virtual bool is_basic_ring() const { return false; }
  GBRing *get_gb_ring() const { return _gb_ring; }

  // Quotient ring information
  bool        is_quotient_ring() const { return false; }
  MonomialIdeal *  get_quotient_monomials() const { return 0; }
  
  // skew commutativity 
  bool is_skew_commutative() const { return _is_skew; }
  int n_skew_commutative_vars() const { return _skew.n_skew_vars(); }
  int skew_variable(int i) const { return _skew.skew_variable(i); }
  bool is_skew_var(int v) const { return _skew.is_skew_var(v); }
  const SkewMultiplication & getSkewInfo() const { return _skew; }

  virtual bool is_pid() const       { return (_nvars == 1 && K_->is_field())
				       || (_nvars == 0 && K_->is_pid()); }
  virtual bool has_gcd() const      { return (_nvars == 1 && K_->is_field())
				       || (_nvars == 0 && K_->has_gcd()); }

  virtual bool is_poly_ring() const { return true; }
  virtual bool is_quotient_poly_ring() const { return false; }

  virtual CoefficientType coefficient_type() const 
  { return Ncoeffs()->coefficient_type(); }

  virtual int n_fraction_vars() const
  { return Ncoeffs()->n_fraction_vars(); }

  virtual void text_out(buffer &o) const;

  /////////////////////////
  // Arithmetic ///////////
  /////////////////////////

  virtual ring_elem from_double(double n) const;
  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;

  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;
  virtual int primary_degree(const ring_elem f) const;
  virtual void degree_weights(const ring_elem f, const M2_arrayint wts, 
			      int &lo, int &hi) const;
  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       const M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, const M2_arrayint wts) const;

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
  virtual ring_elem gcd(const ring_elem f, const ring_elem g) const;
  virtual ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const;

protected:
  void minimal_monomial(ring_elem f, int *&monom) const;
  Nterm *division_algorithm(Nterm *f, Nterm *g, Nterm *&quot) const;
  Nterm *division_algorithm(Nterm *f, Nterm *g) const;
  Nterm *powerseries_division_algorithm(Nterm *f, Nterm *g, Nterm *&quot) const;

public:
  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;
  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
					 ring_elem &quot) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;
  virtual ring_elem random(int homog, const int *deg) const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const;

  virtual ring_elem mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;

  virtual int n_flat_terms(const ring_elem f) const;
  virtual int n_logical_terms(const ring_elem f) const;

  virtual ring_elem get_coeff(const ring_elem f, const int *vp) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;

  virtual ring_elem make_flat_term(const ring_elem a, const int *m) const;
  virtual ring_elem make_logical_term(const ring_elem a, const int *m) const;

  virtual ring_elem lead_flat_coeff(const ring_elem f) const;
  virtual ring_elem lead_logical_coeff(const ring_elem f) const;

  virtual const int * lead_flat_monomial(const ring_elem f) const;
  virtual const int * lead_logical_monomial(const ring_elem f) const;

  ring_elem lead_term(const ring_elem f) const; // copies the lead term
  int compare(const ring_elem f, const ring_elem g) const; // compares the lead terms

  virtual ArrayPairOrNull list_form(const ring_elem f) const;

  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const;


protected:
  ring_elem diff_term(const int *m, const int *n, 
		      int *resultmon,
		      int use_coeff) const;


  void get_logical_monomial(const Nterm *f, int *& result_monomial) const;

  ring_elem get_logical_coeff(const Ring *coeffR, const Nterm *&f) const;
  // Given an Nterm f, return the coeff of its logical monomial, in the
  // polynomial ring coeffR.  f is modified, in that it is replaced by
  // the pointer to the first term of f not used (possibly 0).

public:

  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const;
  virtual bool in_subring(int n, const ring_elem a) const;
  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const;
  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const;
  virtual ring_elem divide_by_expvector(const int *exp, const ring_elem a) const;

  // Routines special to polynomial rings
  // possibly others?
  // Rideal, exterior_vars.
  // nbits
  // heap merge of elements...?

  // Routines special to PID's
  // these include: gcd, gcd_extended.

  // Routines special to fields (anything else?)
protected:
  Nterm *new_term() const;
  Nterm *copy_term(const Nterm *t) const;

  bool imp_attempt_to_cancel_lead_term(ring_elem &f, 
				      ring_elem g, 
				      ring_elem &coeff, 
				      int *monom) const;

protected:
  ring_elem imp_skew_mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;
  void imp_subtract_multiple_to(ring_elem &f, 
				ring_elem a, const int *m, const ring_elem g) const;
public:
  void sort(Nterm *&f) const;

  ///////////////////////////////////////////////////////
  // Used in gbvector <--> vector/ringelem translation //
  ///////////////////////////////////////////////////////
  // These are only meant to be called by Ring's.
public:
  gbvector *translate_gbvector_from_ringelem(ring_elem coeff) const;
  
  gbvector * translate_gbvector_from_vec(const FreeModule *F, 
					 const vec v, 
					 ring_elem &result_denominator) const;
  
  vec translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const;
  
  vec translate_gbvector_to_vec_denom(const FreeModule *F, 
				      const gbvector *v,
				      const ring_elem denom) const;
  // Translate v/denom to a vector in F.  denom does not need to be positive,
  // although it had better be non-zero.
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
