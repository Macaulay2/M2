// Copyright 2005 Michael E. Stillman

#ifndef _polyQQ_hpp_
#define _polyQQ_hpp_

#include "polyring.hpp"

class PolyQQ : public PolyFrac
{
  // Polynomial rings, possibly a quotient ring, which are 
  // f.g. over QQ.

  // Implementation: (Nterm *numer, mpz_ptr denom)

  friend class PolynomialRing;
  struct elem {
    Nterm *numer;
    mpz_t denom;
  };

  elem *new_elem() const;
  elem *make_fraction(Nterm *top) const;
  elem *make_fraction(Nterm *top, mpz_ptr bottom) const;

  mpz_ptr denom(ring_elem f) const;
  void simplify(elem *f) const;
  Nterm *mult_by_coeff(mpz_ptr c, Nterm *f) const;

protected:
  virtual ~PolyQQ();
  PolyQQ() {}
public:
  static PolyQQ *create(const PolyRing *P);

  virtual const PolyQQ * cast_to_PolyQQ()  const      { return this; }
  virtual       PolyQQ * cast_to_PolyQQ()             { return this; }

  virtual void text_out(buffer &o) const;

  virtual Nterm * numerator(ring_elem f) const;

  virtual ring_elem from_double(double n) const;
  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_rational(mpq_ptr n) const;

  virtual ring_elem var(int v) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;

  virtual bool is_unit(const ring_elem f) const;

  virtual bool is_zero(const ring_elem f) const;

  virtual bool is_equal(const ring_elem f, const ring_elem g) const;

  virtual int compare_elems(const ring_elem f, const ring_elem g) const;

  virtual ring_elem copy(const ring_elem f) const;

  virtual void remove(ring_elem &f) const;

  virtual ring_elem negate(const ring_elem f) const;

  virtual ring_elem add(const ring_elem f, const ring_elem g) const;

  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;

  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;

  virtual ring_elem power(const ring_elem f, mpz_t n) const;

  virtual ring_elem power(const ring_elem f, int n) const;

  virtual ring_elem invert(const ring_elem f) const;

  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;

  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;

  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
					 ring_elem &quot) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;
  
  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

  /////////////////////////
  // Polynomial routines //
  /////////////////////////
  virtual int index_of_var(const ring_elem a) const;

  virtual M2_arrayint support(const ring_elem a) const;

  virtual bool is_homogeneous(const ring_elem f) const;

  virtual void degree(const ring_elem f, int *d) const;

  virtual bool multi_degree(const ring_elem f, int *d) const;

  virtual void degree_weights(const ring_elem f, M2_arrayint wts, 
			      int &lo, int &hi) const;

  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       M2_arrayint wts) const;

  virtual ring_elem homogenize(const ring_elem f, int v, M2_arrayint wts) const;

  virtual ring_elem mult_by_term(const ring_elem f, 
				 const ring_elem c, const int *m) const;

  virtual int n_flat_terms(const ring_elem f) const;

  virtual int n_logical_terms(int nvars0,const ring_elem f) const;

  virtual ArrayPairOrNull list_form(const Ring *coeffR, const ring_elem f) const;

  virtual ring_elem make_flat_term(const ring_elem a, const int *m) const;

  virtual ring_elem make_logical_term(const Ring *coeffR, const ring_elem a, const int *exp) const;

  //  virtual ring_elem term(const ring_elem a, const int *m) const = 0;

  virtual ring_elem lead_flat_coeff(const ring_elem f) const;

  virtual ring_elem lead_logical_coeff(const Ring *coeffR, const ring_elem f) const;

  virtual ring_elem get_coeff(const Ring *coeffR, const ring_elem f, const int *vp) const;

  // vp is a varpower monomial, in the logical monoid.
  // The result will be an element in the logical coefficient ring.

  virtual ring_elem get_terms(int nvars0,const ring_elem f, int lo, int hi) const;

  // get the (logical) terms from lo to hi in f.  A negative value means count from
  // the end.  get_terms(--,f,0,0) is the logical lead term of f.

  virtual const int * lead_flat_monomial(const ring_elem f) const;

  virtual void lead_logical_exponents(int nvars0, const ring_elem f, int * result_exp) const;

  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const;

  virtual void monomial_divisor(const ring_elem a, int *exp) const;

  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const;

  virtual bool in_subring(int nslots, const ring_elem a) const;

  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const;

  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const;

  virtual ring_elem divide_by_expvector(const int *exp, const ring_elem a) const;

  const vecterm * vec_locate_lead_term(const FreeModule *F, vec v) const;

  virtual vec vec_lead_term(int nparts, const FreeModule *F, vec v) const;

  virtual vec vec_top_coefficient(const vec v, int &x, int &e) const;

  virtual gbvector * translate_gbvector_from_ringelem(ring_elem coeff) const;

  virtual gbvector * translate_gbvector_from_vec(const FreeModule *F, 
						 const vec v, 
						 ring_elem &result_denominator) const;

  virtual vec translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const;

  virtual vec translate_gbvector_to_vec_denom(const FreeModule *F, 
					      const gbvector *v,
					      const ring_elem denom) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
