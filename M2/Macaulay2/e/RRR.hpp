// Copyright 1995 Michael E. Stillman.

// need to be careful that exponent does not overflow, according to GMP

#ifndef _RRR_hh_
#define _RRR_hh_

#include "aring-RRR.hpp"
#include "ZZ.hpp"

//class CoefficientRingRRR; // CoefficientRingRRR is replaced with Ring_RRR
/**
    @ingroup rings
*/
class RRR : public Ring
{
  unsigned long precision;
  int _elem_size;
  mpfr_ptr _zero_elem;

  Ring_RRR *coeffR;

  mpfr_ptr new_elem() const;

protected:
  RRR() {}
  virtual ~RRR() {}
  bool initialize_RRR(unsigned long prec);
public:
  static RRR * create(unsigned long prec);

  RRR * cast_to_RRR() { return this; }
  const RRR * cast_to_RRR() const { return this; }

  Ring_RRR *get_CoeffRing() const { return coeffR; }
  Ring_RRR *get_ARing() const { return coeffR; }  //TODO: MES: change to ARing type once implemented.
  
  unsigned long get_precision() const { return precision; }

  bool is_greater(const ring_elem a, const ring_elem b) const;
  bool is_less(const ring_elem a, const ring_elem b) const;
  ring_elem absolute(const ring_elem f) const;
  ring_elem sqrt(const ring_elem f) const;

  virtual mpfr_ptr to_BigReal(ring_elem f) const;

// The following are all the routines required by 'ring'
  virtual bool is_RRR() const         { return true; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_rational(mpq_ptr r) const;
  virtual bool from_BigReal(gmp_RR r, ring_elem &a) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;

  int is_positive(const ring_elem a) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem a, const ring_elem b) const;

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

  virtual void syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one=true,
                             bool p_plus=false,
                             bool p_parens=false) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

  virtual ring_elem zeroize_tiny(gmp_RR epsilon, const ring_elem f) const;
  virtual void increase_maxnorm(gmp_RR norm, const ring_elem f) const;
  // If any real number appearing in f has larger absolute value than norm, replace norm.

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
