// Copyright 1995 Michael E. Stillman.

#ifndef _QQ_hh_
#define _QQ_hh_

#include "ZZ.hpp"

class QQ : public Ring
{
  int _elem_size;
  M2_Rational _zero_elem;

  M2_Rational new_elem() const;
  void remove_elem(M2_Rational f) const;

protected:
  QQ() {}
  virtual ~QQ() {}
  bool initialize_QQ();
public:
  static QQ * create();

  QQ * cast_to_QQ() { return this; }
  const QQ * cast_to_QQ() const { return this; }

  virtual bool is_QQ() const         { return true; }
  virtual bool is_basic_ring() const { return false; } 

  virtual const Ring *get_ambient_ring() const { return globalZZ; }
  virtual const Ring *get_denominator_ring() const { return globalZZ; }

  virtual CoefficientType coefficient_type() const { return COEFF_QQ; }

  virtual void text_out(buffer &o) const;

  virtual int coerce_to_int(ring_elem a) const;

  ring_elem numerator(ring_elem q) const;
  ring_elem denominator(ring_elem q) const;
  ring_elem fraction(ring_elem top, ring_elem bottom) const;

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
  virtual int compare_elems(const ring_elem a, const ring_elem b) const;


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

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const;

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

  ///////////////////////////////////////////////////////
  // Used in gbvector <--> vector/ringelem translation //
  ///////////////////////////////////////////////////////
#if 0
// protected:
//   ring_elem trans_one; // 1 as an element of globalZZ.
// 
//   virtual ring_elem trans_to_ringelem(ring_elem coeff, 
// 				      const int *exp) const;
//   virtual ring_elem trans_to_ringelem_denom(ring_elem coeff, 
// 					    ring_elem denom, 
// 					    int *exp) const;
//   virtual void trans_from_ringelem(gbvectorHeap &H, 
// 				   ring_elem coeff, 
// 				   int comp, 
// 				   int *exp,
// 				   int firstvar) const;
//   
//   virtual trans_tag trans_type() const { return FRAC_QQ; }
#endif
  
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
