// Copyright 2001 Michael E. Stillman.

#ifndef _RR_hh_
#define _RR_hh_

#include "ring.hpp"

class CoefficientRingRR;

class RingRR : public Ring
// Elements of this ring are real numbers: 'double's
{
  friend class CoefficientRingRR;
public:
  struct RRelem_rec {
    double val;
  };
  typedef RRelem_rec *RRelem;
private:
  double _epsilon;  // Elements closer than this are considered identical.

  RRelem new_elem() const;
  void remove_elem(RRelem f) const;

  bool is_zero_RR(double a) const;
  int compare_RR(double a, double b) const;

  CoefficientRingRR *coeffR;
protected:
  RingRR() {}
  virtual ~RingRR();
  bool initialize_RR(double epsilon);
public:
  static RingRR * create(double epsilon);

  RingRR * cast_to_RingRR() { return this; }
  const RingRR * cast_to_RingRR() const { return this; }

  CoefficientRingRR *get_CoeffRing() const { return coeffR; }

  double to_double(ring_elem a) const;

// The following are all the routines required by 'ring'
  virtual bool is_RR() const { return true; }

  virtual void text_out(buffer &o) const;

  virtual ring_elem from_int(int n) const;
  virtual ring_elem from_int(mpz_ptr n) const;
  virtual ring_elem from_double(double r) const;
  virtual ring_elem from_rational(mpq_ptr r) const;
  virtual bool from_BigReal(M2_RRR a, ring_elem &result) const;
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

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;
};

#define RRELEM_VAL(f) (reinterpret_cast<RingRR::RRelem>((f).poly_val))
#define RR_RINGELEM(a) (ring_elem(reinterpret_cast<Nterm *>(a)))
#define RR_VAL(f) ((RRELEM_VAL(f))->val)

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
