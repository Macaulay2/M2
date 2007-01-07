// Copyright 1995 Michael E. Stillman.

#ifndef _ZZ_hh_
#define _ZZ_hh_

#include "ring.hpp"
#include <gmp.h>
class CoefficientRingZZ_NTL;

// #define MPZ_VAL(f) (reinterpret_cast<mpz_ptr>((f).poly_val))
// #define MPZ_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

#define MPQ_VAL(f) (reinterpret_cast<M2_Rational>((f).poly_val))
#define MPQ_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

#define CCELEM_VAL(f) (reinterpret_cast<M2_CC>((f).poly_val))
#define CC_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))
#define CC_IM(f) ((CCELEM_VAL(f))->im)
#define CC_RE(f) ((CCELEM_VAL(f))->re)
#define CC_NORM(f) (sqrt(CC_RE(f)*CC_RE(f) + CC_IM(f)*CC_IM(f)))

#define MPF_VAL(f) (reinterpret_cast<mpf_ptr>((f).poly_val))
#define MPF_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))

#define BIGCC_VAL(f) (reinterpret_cast<M2_CCC>((f).poly_val))
#define BIGCC_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))
#define BIGCC_RE(f) (&BIGCC_VAL(f)->re)  // returns actual value, not copy
#define BIGCC_IM(f) (&BIGCC_VAL(f)->im)


// The following lines are here only to remove complaints about old style casts from gmp
extern "C" inline int mask_mpz_cmp_si(mpz_t x, long int i) { return mpz_cmp_si(x,i); }
extern "C" inline int mask_mpq_cmp_si(mpq_t x, long int i, long int j) { return mpq_cmp_si(x,i,j); }

class RingZZ : public Ring
{
  friend class CoefficientRingZZ_NTL;

  int _elem_size;
  mpz_ptr _zero_elem;

  mpz_ptr new_elem() const;
  void remove_elem(mpz_ptr f) const;

  CoefficientRingZZ_NTL *coeffR;
protected:
  virtual ~RingZZ() {}

public:
  typedef mpz_ptr element_type;

  //////////////////////////////////////////////
  // Creation of globalZZ is done in PolyRing::make_trivial_ZZ_poly_ring
  // These two routines should not be called from elsewhere
  RingZZ() {}
  bool initialize_ZZ(const PolynomialRing *deg_ring);
  //////////////////////////////////////////////

  RingZZ * cast_to_RingZZ() { return this; }
  const RingZZ * cast_to_RingZZ() const { return this; }

  CoefficientRingZZ_NTL * get_CoeffRing() const { return coeffR; }

// The following are all the routines required by 'ring'
  virtual bool is_ZZ() const         { return true; }

  virtual CoefficientType coefficient_type() const { return COEFF_ZZ; }

  virtual void text_out(buffer &o) const;

  static unsigned int mod_ui(mpz_t n, unsigned int p);
  static bool get_ui(unsigned int &result, mpz_t n);
  static bool get_si(int &result, mpz_t n);

  // If the base ring of a is ZZ:
  // To get a bignum from a RingElement a, use: a.get_value().get_mpz()
  // To get a bignum from an ring_elem  a, use: a.get_mpz()

  virtual int coerce_to_int(ring_elem a) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_rational(mpq_ptr q) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;
  virtual bool lower_associate_divisor(ring_elem &f, ring_elem g) const;

  int is_positive(const ring_elem a) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;

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

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;
  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
					 ring_elem &quot) const;

  ring_elem gcd(const ring_elem f, const ring_elem g) const;
  ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
