// Copyright 2004 Michael E. Stillman

#ifndef _polyquotient_hpp_
#define _polyquotient_hpp_

#include "polyring.hpp"
#include "monideal.hpp"

class PolyRingQuotient : public PolynomialRing
{
  const PolyRing *R_;
  bool overZZ_;

  MonomialIdeal *Rideal_;
  MonomialTable *ringtable_;
  MonomialTableZZ *ringtableZZ_;

  bool is_ZZ_quotient_;		// true if this is a quotient of a polynomial ring over ZZ, AND
				// there is an integer in the factored ideal.
  ring_elem ZZ_quotient_value_;	// This is the integer in the factor ideal, if is_ZZ_quotient is set.

  int *MONOM1_;
  int *EXP1_, *EXP2_;
protected:
  void makeQuotientIdeal(const vector<Nterm *, gc_alloc> &quotients);
  void makeQuotientIdealZZ(const vector<Nterm *, gc_alloc> &quotients);

  bool reduce_lead_term_ZZ(Nterm * &f, const Nterm * g) const;
  void reduce_lead_term_basic_field(Nterm * &f, const Nterm * g) const;

  void normal_form(ring_elem &f) const;
  void normal_form_basic_field(ring_elem &f) const;
  void normal_form_ZZ(ring_elem &f) const;

  virtual ~PolyRingQuotient();
  PolyRingQuotient() {}

public:
  static PolyRingQuotient *create(const PolyRing *R, const Matrix *M);

  Matrix * getPresentation() const;

  virtual const Ring *getLogicalCoefficients() const { return R_->getLogicalCoefficients(); }
  // The logical coefficient ring of 'this'.  
  // This is either a non polynomial ring, or it is a PolyRing.

  virtual const Ring *getCoefficients() const { return R_->getCoefficients(); }
  // The implementation coeff ring of 'this'.  This is either a basic ring (field, ZZ), or
  // is another PolyRing.

  virtual const Monoid *getLogicalMonoid() const { return R_->getLogicalMonoid(); }
  // The logical monoid of this polynomial ring.

  virtual const Monoid *getMonoid() const { return R_->getMonoid(); }
  // The implementation monoid of this ring.

  virtual const PolyRing * getAmbientRing() const { return R_->getAmbientRing(); }
  // Yields the ambient PolyRing corresponding to this polynomial ring
  // This ring has no quotients, no fractions (not even QQ), but may have
  // skew, weyl, or solvable multiplication, or even (later) be an associative
  // free algebra.

  virtual const RingOrNull *getDenominatorRing() const { return 0; }
  // If this ring has no denominators, NULL is returned.  Otherwise the ring which
  // implements denominators is returned.  When one asks for a denominator for elements of
  // 'this', the result value is its ring.

  virtual GBRing *get_gb_ring() const { return R_->get_gb_ring(); }

  virtual const PolyRingQuotient * cast_to_PolyRingQuotient()  const { return this; }
  virtual       PolyRingQuotient * cast_to_PolyRingQuotient()        { return this; }

  // Quotient ring information
  virtual bool        is_quotient_ring() const { return true; }

  virtual bool is_quotient_poly_ring() const { return true; }

  virtual const MonomialIdeal *  get_quotient_monomials() const { return Rideal_; }
  // Each bag value is an "Nterm *".

  virtual const MonomialTableZZ * get_quotient_MonomialTableZZ() const { return ringtableZZ_; }
  // Each id is an index into quotient_ideal_

  virtual const MonomialTable * get_quotient_MonomialTable() const { return ringtable_; }
  // Each id is an index into quotient_ideal_


  
  virtual bool is_pid() const {
    return n_vars() == 1 && !getCoefficients()->is_ZZ() && n_quotients() == 1;
  }

  virtual bool has_gcd() const { return false; } // Is this correct??

  virtual void text_out(buffer &o) const;

  ////////////////////////
  // Arithmetic //////////
  ////////////////////////

  virtual ring_elem from_double(double n) const { return R_->from_double(n); }
  virtual ring_elem from_int(int n) const { return R_->from_int(n); }
  virtual ring_elem from_int(mpz_ptr n) const { return R_->from_int(n); }

  virtual ring_elem var(int v, int n) const { 
    ring_elem result = R_->var(v,n);
    normal_form(result);
    return result;
  }
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const { return R_->preferred_associate(f); }

  virtual bool is_unit(const ring_elem f) const; // TODO

  virtual bool is_zero(const ring_elem f) const { 
    return R_->PolyRing::is_zero(f);
  }

  virtual bool is_equal(const ring_elem f, const ring_elem g) const {
    return R_->PolyRing::is_equal(f,g); 
  }

  virtual ring_elem copy(const ring_elem f) const {
    return R_->PolyRing::copy(f);
  }
  virtual void remove(ring_elem &f) const {
    R_->PolyRing::remove(f);
  }

  virtual ring_elem negate(const ring_elem f) const {
    ring_elem result = R_->PolyRing::negate(f);
    if (overZZ_) normal_form(result);
    return result;
  }

  virtual ring_elem add(const ring_elem f, const ring_elem g) const {
    ring_elem result =  R_->PolyRing::add(f,g);
    if (overZZ_) normal_form(result);
    return result;
  }

  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const {
    ring_elem result = R_->PolyRing::subtract(f,g);
    if (overZZ_) normal_form(result);
    return result;
  }

  virtual ring_elem mult(const ring_elem f, const ring_elem g) const {
    ring_elem result = R_->PolyRing::mult(f,g);
    normal_form(result);
    return result;
  }

  virtual ring_elem power(const ring_elem f, mpz_t n) const;

  virtual ring_elem power(const ring_elem f, int n) const;

  virtual ring_elem invert(const ring_elem f) const;

  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;

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

  virtual ring_elem random(int homog, const int *deg) const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const {
    R_->PolyRing::elem_text_out(o,f);
  }
  
  virtual ring_elem eval(const RingMap *map, const ring_elem f) const;

  /////////////////////////
  // Polynomial routines //
  /////////////////////////
  virtual int index_of_var(const ring_elem a) const {
    return R_->PolyRing::index_of_var(a);
  }

  virtual M2_arrayint support(const ring_elem a) const {
    return R_->PolyRing::support(a);
  }

  virtual bool is_homogeneous(const ring_elem f) const {
    return is_graded() && R_->PolyRing::is_homogeneous(f);
  }

  virtual void degree(const ring_elem f, int *d) const {
    R_->PolyRing::degree(f,d);
  }

  virtual bool multi_degree(const ring_elem f, int *d) const {
    return R_->PolyRing::multi_degree(f,d);
  }

  virtual int primary_degree(const ring_elem f) const {
    return R_->PolyRing::primary_degree(f);
  }

  virtual void degree_weights(const ring_elem f, const M2_arrayint wts, 
			      int &lo, int &hi) const {
    return R_->PolyRing::degree_weights(f,wts,lo,hi);
  }

  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       const M2_arrayint wts) const {
    ring_elem result = R_->PolyRing::homogenize(f,v,deg,wts);
    normal_form(result);
    return result;
  }

  virtual ring_elem homogenize(const ring_elem f, int v, const M2_arrayint wts) const {
    ring_elem result = R_->PolyRing::homogenize(f,v,wts);
    normal_form(result);
    return result;
  }

  virtual ring_elem mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const {
    ring_elem result = R_->mult_by_term(f,c,m);
    normal_form(result);
    return result;
  }

  virtual int n_flat_terms(const ring_elem f) const {
    return R_->PolyRing::n_flat_terms(f);
  }

  virtual int n_logical_terms(const ring_elem f) const {
    return R_->PolyRing::n_logical_terms(f);
  }

  virtual ArrayPairOrNull list_form(const ring_elem f) const {
    return R_->PolyRing::list_form(f);
  }

  virtual ring_elem make_flat_term(const ring_elem a, const int *m) const {
    ring_elem result = R_->PolyRing::make_flat_term(a,m);
    normal_form(result);
    return result;
  }

  virtual ring_elem make_logical_term(const ring_elem a, const int *m) const {
    ring_elem result = R_->PolyRing::make_logical_term(a,m);
    normal_form(result);
    return result;
  }
  //  virtual ring_elem term(const ring_elem a, const int *m) const = 0;

  virtual ring_elem lead_flat_coeff(const ring_elem f) const {
    return R_->PolyRing::lead_flat_coeff(f);
  }

  virtual ring_elem lead_logical_coeff(const ring_elem f) const {
    return R_->PolyRing::lead_logical_coeff(f);
  }

  virtual ring_elem get_coeff(const ring_elem f, const int *vp) const {
    return R_->PolyRing::get_coeff(f,vp);
  }
  // vp is a varpower monomial, in the logical monoid.
  // The result will be an element in the logical coefficient ring.

  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const {
    return R_->PolyRing::get_terms(f,lo,hi);
  }
  // get the (logical) terms from lo to hi in f.  A negative value means count from
  // the end.  get_terms(f,0,0) is the logical lead term of f.

  virtual const int * lead_flat_monomial(const ring_elem f) const {
    return R_->PolyRing::lead_flat_monomial(f);
  }

  virtual const int * lead_logical_monomial(const ring_elem f) const {
    return R_->PolyRing::lead_logical_monomial(f);
  }

  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const {
    R_->PolyRing::mult_coeff_to(a,f);
    normal_form(f);
  }

  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const {
#warning "diff for quotient rings: should do what?"
    return R_->PolyRing::diff(a,b,use_coeff);
  }

  virtual bool in_subring(int n, const ring_elem a) const {
    return R_->PolyRing::in_subring(n,a);
  }

  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const {
    return R_->PolyRing::degree_of_var(n,a,lo,hi);
  }

  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const {
    return R_->PolyRing::divide_by_var(n,d,a);
  }

  virtual ring_elem divide_by_expvector(const int *exp, const ring_elem a) const {
    return R_->PolyRing::divide_by_expvector(exp,a);
  }

  virtual vec vec_lead_term(int nparts, vec v) const {
    return R_->PolyRing::vec_lead_term(nparts, v);
  }

  virtual gbvector * translate_gbvector_from_ringelem(ring_elem coeff) const {
    return R_->PolyRing::translate_gbvector_from_ringelem(coeff);
  }

  // result/denom == v.
  // result_denom will be an element in getDenominatorRing() (if non-NULL).
  virtual gbvector * translate_gbvector_from_vec(const FreeModule *F, 
						 const vec v, 
						 ring_elem &result_denominator) const {
    return R_->PolyRing::translate_gbvector_from_vec(F,v,result_denominator);
  }

  virtual vec translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const {
    return R_->PolyRing::translate_gbvector_to_vec(F,v);
  }
  

  // Translate v/denom to a vector in F.  denom does not need to be positive,
  // although it had better be non-zero.
  // denom should be an element of getDenominatorRing() (if non-NULL, otherwise 'denom'
  // is ignored).
  virtual vec translate_gbvector_to_vec_denom(const FreeModule *F, 
					      const gbvector *v,
					      const ring_elem denom) const {
    return R_->PolyRing::translate_gbvector_to_vec_denom(F,v,denom);
  }
};

#if 0
class QuotientInfo
{
public:
  bool is_quotient() const;
  vector<gbvector *,gc_alloc> elements;
  const PolyRing *ambient_ring;
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


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
