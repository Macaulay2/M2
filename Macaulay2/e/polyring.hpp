// Copyright 1995 Michael E. Stillman

#ifndef _polyring_hh_
#define _polyring_hh_

#include "ring.hpp"
#include "ringelem.hpp"
#include "monideal.hpp"
#include "skew.hpp"

///// Ring Hierarchy ///////////////////////////////////

class TermIdeal;
class Matrix;
class GBRing;
class GBRingSkew;

#if 0
class QuotientInfo
{
public:
  bool is_quotient() const;
  vector <gbvector *> elements;
  const PolynomialRing *ambient_ring;
#if 0
  MonomialIdeal * _Rideal;	// This is used if the coeff ring is not ZZ.
  TermIdeal *_RidealZZ;		// This is used if the coeff ring is ZZ.
#endif
};
#endif

#if 0
// These were in PolynomialRing
  static PolynomialRing *create_quotient_ring(const PolynomialRing *R, const array<ring_elem> &I);
  MonomialIdeal *  get_quotient_monomials() const { return _Rideal; }
  const TermIdeal *get_quotient_monomials_ZZ() const { return _RidealZZ; }
  const FreeModule *get_Rsyz() const;

  Matrix     get_ideal() const;
  ring_elem get_quotient_elem(int i) const { return _quotient_ideal[i]; }
  int        get_quotient_elem_length() const { return _quotient_ideal.length(); }


  void initialize_quotients(const array<ring_elem> &I);
#endif

class PolynomialRing : public Ring
{
  friend class GBRingSkew;
  friend class FreeModule;
protected:
  const PolynomialRing * make_flattened_ring();
  void initialize_poly_ring(const Ring *K, const Monoid *M);
  virtual ~PolynomialRing();
  PolynomialRing() {}
public:
  static PolynomialRing *create(const Ring *K, const Monoid *MF);

protected:
  int _poly_size;

  GBRing *_gb_ring;
  const PolynomialRing *flattened_ring;

  // Quotient ring information
  const PolynomialRing *_base_ring; // == NULL iff this is not a quotient ring
  Computation *_quotient_gb;

#if 0
  QuotientInfo Quotient;

  array<ring_elem> _quotient_ideal;
  MonomialIdeal * _Rideal;	// This is used if the coeff ring is not ZZ.
  TermIdeal *_RidealZZ;		// This is used if the coeff ring is ZZ.
#endif

  bool _coefficients_are_ZZ;
  bool _isgraded;

  // Most skew-mult specific poly code is in skewpoly.{hpp,cpp}.  However, var, homogenize,
  //   and diff_term all have a small amount of skew commutative specific code.
  bool _is_skew;
  SkewMultiplication _skew; // Completely ignored if _is_skew is false.

  int *_EXP1, *_EXP2, *_EXP3;
public:
  static PolynomialRing *create_quotient_ring(Computation *G);

  virtual const PolynomialRing * cast_to_PolynomialRing()  const { return this; }
  virtual       PolynomialRing * cast_to_PolynomialRing()        { return this; }

  virtual bool is_basic_ring() const { return false; }
  GBRing *get_gb_ring() const { return _gb_ring; }
  const PolynomialRing * get_flattened_ring() const {  return flattened_ring; }

  // Queries for quotient ring
  bool        is_quotient_ring() const { return (_base_ring != NULL); }
  const PolynomialRing * get_base_poly_ring() const { return _base_ring; }
  

  bool is_skew_commutative() const { return _is_skew; }
  bool is_skew_var(int v) const { return _skew.is_skew_var(v); }

  virtual bool is_pid() const       { return (_nvars == 1 && K_->is_field())
				       || (_nvars == 0 && K_->is_pid()); }
  virtual bool has_gcd() const      { return (_nvars == 1 && K_->is_field())
				       || (_nvars == 0 && K_->has_gcd()); }
  virtual bool is_graded() const    { return _isgraded; } // MES: change this
  virtual bool is_expensive() const { return 1; }

  virtual bool is_poly_ring() const { return true; }
  virtual bool is_quotient_poly_ring() const { return _base_ring != NULL; }

  virtual void text_out(buffer &o) const;

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

protected:
  virtual ring_elem power2(const ring_elem f, mpz_t n) const;
  virtual ring_elem power2(const ring_elem f, int n) const;

  // Polynomial routines
  void make_Rideal(const array<ring_elem> &polys);
  void make_RidealZZ(const array<ring_elem> &polys);
public:

  virtual ring_elem mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;

  const int * lead_monomial(const ring_elem f) const;
  ring_elem lead_term(const ring_elem f) const; // copies the lead term
  int compare(const ring_elem f, const ring_elem g) const; // compares the lead terms

  virtual void make_monic(ring_elem &f) const;
  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const;

  void auto_reduce_to(ring_elem &f, ring_elem g) const;
  void subtract_multiple_to(ring_elem &f, 
			    ring_elem a, const int *m, const ring_elem g) const;
  ring_elem coeff_of(const ring_elem f, const int *m) const;

protected:
  ring_elem diff_term(const int *m, const int *n, 
		      int *resultmon,
		      int use_coeff) const;
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
  void imp_cancel_lead_term(ring_elem &f, 
			ring_elem g, 
			ring_elem &coeff, 
			int *monom) const;
  bool imp_attempt_to_cancel_lead_term(ring_elem &f, 
				      ring_elem g, 
				      ring_elem &coeff, 
				      int *monom) const;
  void cancel_lead_term(ring_elem &f, 
			ring_elem g, 
			ring_elem &coeff, 
			int *monom) const;

public:
  void normal_form(Nterm *&f) const;
  void apply_ring_elements(Nterm * &f, vec rsyz, const array<ring_elem> &elems) const;
  void normal_form_ZZ(Nterm *&f) const;
protected:
  ring_elem imp_skew_mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;
  virtual ring_elem imp_mult_by_term(const ring_elem f, 
			      const ring_elem c, const int *m) const;
  void imp_subtract_multiple_to(ring_elem &f, 
				ring_elem a, const int *m, const ring_elem g) const;

public:
  Nterm *resize(const PolynomialRing *R, Nterm *f) const;
  void sort(Nterm *&f) const;
  void debug_out(const ring_elem f) const;
  void debug_outt(const Nterm *f) const;

  ///////////////////////////////////////////////////////
  // Used in gbvector <--> vector/ringelem translation //
  ///////////////////////////////////////////////////////
  // These are only meant to be called by Ring's.
private:
  int *trans_EXP1; // Used by translate_* routines.
public:
  int *trans_MONOM1; // A monomial of Nmonoms()

  virtual ring_elem trans_to_ringelem(ring_elem coeff, 
				      const int *exp) const;
  virtual ring_elem trans_to_ringelem_denom(ring_elem coeff, 
					    ring_elem denom, 
					    int *exp) const;
  virtual void trans_from_ringelem(gbvectorHeap &H, 
				   ring_elem coeff, 
				   int comp, 
				   int *exp,
				   int firstvar) const;
  
  virtual trans_tag trans_type() const { return POLY; }
public:
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
