// Copyright 1995 Michael E. Stillman

#ifndef _polyring_hh_
#define _polyring_hh_

#include "ring.hpp"
#include "ringelem.hpp"
#include "monideal.hpp"

///// Ring Hierarchy ///////////////////////////////////

class PolynomialRing : public Ring
{
  friend class FreeModule;
protected:
  PolynomialRing(const Ring *K, const Monoid *MF);
  PolynomialRing(const PolynomialRing *R, const array<ring_elem> &I);
  virtual ~PolynomialRing();
protected:
  stash *pstash;

  // Quotient ring information
  const PolynomialRing *base_ring; // == NULL iff this is not a quotient ring
  array<ring_elem> quotient_ideal;
  MonomialIdeal Rideal;		// This is used if the coeff ring is not ZZ.
  TermIdeal *RidealZ;		// This is used if the coeff ring is ZZ.

  bool coefficients_are_ZZ;
  bool isgraded;

public:
  static PolynomialRing *create(const Ring *K, const Monoid *MF);
  static PolynomialRing *create(const PolynomialRing *R, const array<ring_elem> &I);
  
  class_identifier class_id() const { return CLASS_PolynomialRing; }

  // serialize
  virtual void write_object(object_writer &o) const;
  static PolynomialRing *read_object(object_reader &i);
  void write_element(object_writer &o, const ring_elem f) const;
  void read_element(object_reader &i, ring_elem &result) const;

  virtual const PolynomialRing * cast_to_PolynomialRing()  const { return this; }
  virtual       PolynomialRing * cast_to_PolynomialRing()        { return this; }

  // Queries for quotient ring
  bool        is_quotient_ring() const { return (base_ring != NULL); }
  const PolynomialRing * get_base_poly_ring() const { return base_ring; }
  MonomialIdeal  get_quotient_monomials() const { return Rideal; }
  const TermIdeal *get_quotient_monomials_ZZ() const { return RidealZ; }
  const FreeModule *get_Rsyz() const;

  Matrix     get_ideal() const;
  ring_elem get_quotient_elem(int i) const { return quotient_ideal[i]; }
  int        get_quotient_elem_length() const { return quotient_ideal.length(); }


  virtual bool is_pid() const       { return (nvars == 1 && K->is_field())
				       || (nvars == 0 && K->is_pid()); }
  virtual bool has_gcd() const      { return (nvars == 1 && K->is_field())
				       || (nvars == 0 && K->has_gcd()); }
  virtual bool is_Z() const         { return 0; }
  virtual bool is_poly_ring() const { return 1; }
  virtual bool is_quotient_poly_ring() const { return base_ring != NULL; }
  virtual bool is_graded() const    { return isgraded; } // MES: change this
  virtual bool is_expensive() const { return 1; }
  virtual bool is_commutative_ring() const { return !M->is_skew(); }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;

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
  virtual void elem_bin_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const;

protected:
  virtual ring_elem power2(const ring_elem f, mpz_t n) const;
  virtual ring_elem power2(const ring_elem f, int n) const;

  // Polynomial routines
  void make_Rideal(const array<ring_elem> &polys);
  void make_RidealZ(const array<ring_elem> &polys);
public:

  virtual ring_elem mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;

  virtual void make_monic(ring_elem &f) const;
  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const;

  void auto_reduce_to(ring_elem &f, ring_elem g) const;
  void subtract_multiple_to(ring_elem &f, 
			    ring_elem a, const int *m, const ring_elem g) const;
  ring_elem coeff_of(const ring_elem f, const int *m) const;

  ring_elem diff_by_term(const int *exp, const ring_elem f,
			 int use_coeff) const;

  // Routines special to polynomial rings
  // in_subring, div_degree, divide_by_var
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
  void term_degree(const Nterm *t, int *degt) const;
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
};

#endif
