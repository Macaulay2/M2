// Copyright 1996 Michael E. Stillman.
#ifndef _frac_hh_
#define _frac_hh_

#include "ring.hpp"

struct frac_elem
{
  ring_elem numer;
  ring_elem denom;
};

class FractionField : public Ring
{
  const Ring *R_;		// Base ring.  Assumed to be a domain.
  ring_elem _MINUS_ONE;		// this is -1 in the ring R.

  frac_elem *new_frac_elem() const;
  void simplify(frac_elem *f) const;
  frac_elem *make_elem(ring_elem a, ring_elem b) const;

protected:
  FractionField() {}
  virtual ~FractionField() {}
  bool initialize_frac(const Ring *R);
public:
  static FractionField * create(const Ring *R);

  FractionField * cast_to_FractionField() { return this; }
  const FractionField * cast_to_FractionField() const { return this; }

  const Ring *get_ring() const { return R_; }

  ring_elem numerator(ring_elem f) const;
  ring_elem denominator(ring_elem f) const;
  ring_elem fraction(const ring_elem top, const ring_elem bottom) const;

// The following are all the routines required by 'ring'
  virtual bool is_fraction_field() const { return true; }

  virtual bool is_pid() const       { return 1; }
  virtual bool has_gcd() const      { return 1; }
  virtual bool is_graded() const    { return R_->is_graded(); }
  virtual bool is_expensive() const { return 1; }

  virtual CoefficientType coefficient_type() const;
  virtual int n_fraction_vars() const;

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem var(int v, int n) const;

  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

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
  virtual ring_elem random(int homog, const int *deg) const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const;

  virtual int n_terms(const ring_elem f) const;
  virtual ring_elem term(const ring_elem a, const int *m) const;
  virtual ring_elem lead_coeff(const ring_elem f) const;
  virtual ring_elem get_coeff(const ring_elem f, const int *m) const;
  virtual ring_elem get_terms(const ring_elem f, int lo, int hi) const;

  ///////////////////////////////////////////////////////
  // Used in gbvector <--> vector/ringelem translation //
  ///////////////////////////////////////////////////////
protected:
  ring_elem trans_one; // 1 as an element of globalZZ.

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
  
  virtual trans_tag trans_type() const { return FRAC; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
