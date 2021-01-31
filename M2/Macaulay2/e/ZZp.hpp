// Copyright 1995 Michael E. Stillman.
#ifndef _z_mod_p_hh_
#define _z_mod_p_hh_

#include "ring.hpp"
#include "coeffrings.hpp"
namespace M2 {
class ARingZZp;
};
/**
    @ingroup rings
*/
class Z_mod : public Ring
{
  // int P; // this is defined in class Ring
  int P;      // this class only allows char < 32767, so we stash characteristic
              // here
  int _P1;    // = P-1
  int _ZERO;  // = p-1, log of zero...

  int _prim_root;
  int _minus_one;
  int *_exp_table;
  int *_log_table;

  int int_to_exp(int a) const;

  CoefficientRingZZp *coeffR;
  M2::ARingZZp *aringZZp;

  int to_int(int a) const;

 protected:
  Z_mod() {}
  virtual ~Z_mod() {}
  bool initialize_Z_mod(int p);

 public:
  static Z_mod *create(int p);

  bool isFinitePrimeField() const { return true; }
  Z_mod *cast_to_Z_mod() { return this; }
  const Z_mod *cast_to_Z_mod() const { return this; }
  CoefficientRingZZp *get_CoeffRing() const { return coeffR; }
  M2::ARingZZp *get_ARing() const { return aringZZp; }
  virtual MutableMatrix *makeMutableMatrix(size_t nrows,
                                           size_t ncols,
                                           bool dense) const;

  virtual std::pair<bool, long> coerceToLongInteger(ring_elem a) const;

  long discreteLog(const ring_elem &a) const;  // returns -1 if a is 0

  // The following are all the routines required by 'ring'

  virtual void text_out(buffer &o) const;

  virtual unsigned int computeHashValue(const ring_elem a) const;

  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_srcptr n) const;
  virtual bool from_rational(mpq_srcptr q, ring_elem &result) const;

  virtual bool promote(const Ring *R,
                       const ring_elem f,
                       ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem a, const ring_elem b) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

  int internal_negate(int f) const;
  int internal_add(int f, int g) const;
  int internal_subtract(int f, int g) const;
  
  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;
  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;

  virtual void syzygy(const ring_elem a,
                      const ring_elem b,
                      ring_elem &x,
                      ring_elem &y) const;

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one = true,
                             bool p_plus = false,
                             bool p_parens = false) const;

  virtual ring_elem eval(const RingMap *map,
                         const ring_elem f,
                         int first_var) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
