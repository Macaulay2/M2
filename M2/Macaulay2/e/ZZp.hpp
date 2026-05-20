// Copyright 1995 Michael E. Stillman.
#ifndef _z_mod_p_hh_
#define _z_mod_p_hh_

#include "ring.hpp"
#include "coeffrings.hpp"
namespace M2 {
class ARingZZp;
};

/**
 * @file ZZp.hpp
 * @brief Legacy `Z_mod` --- a `Ring`-derived `Z/p` with log / exp tables.
 *
 * `Z_mod` is the original `Z/p` class, pre-dating the `aring`
 * framework. Elements are stored as log indices of a chosen
 * primitive root `alpha`, but the encoding differs from the
 * aring sibling: here the *high* index `P - 1` (stored as
 * `_ZERO`) represents the field zero, and `0 <= n <= P - 2`
 * represent `alpha^n mod p` (so `alpha^0 = 1` lives at index
 * `0`). Multiplication of non-zero values reduces to
 * `(a + b) mod (P - 1)` on the indices, and addition is
 * performed via parallel `_exp_table` / `_log_table` lookups
 * built at construction. The class supports characteristic
 * `< 32767`.
 *
 * `Z_mod` retains pointers to a `CoefficientRingZZp` and an
 * `M2::ARingZZp` for the same prime --- exposed via
 * `get_CoeffRing()` and `get_ARing()` --- so callers on either
 * side of the legacy / aring boundary can ask for the form they
 * need. `M2::ARingZZp` (`aring-zzp.hpp`) uses the inverted
 * convention (`0` is the field zero), and `M2::ARingZZpFlint`
 * (`aring-zzp-flint.hpp`) / `M2::ARingZZpFFPACK`
 * (`aring-zzp-ffpack.hpp`) are the FLINT- and FFLAS-backed
 * variants.
 *
 * @see aring-zzp.hpp
 * @see aring-zzp-flint.hpp
 * @see aring-zzp-ffpack.hpp
 * @see coeffrings.hpp
 */

/**
 * @brief Engine-side `Z/p` ring for small primes (`p < 32767`), using a
 * discrete-log (Zech) representation.
 *
 * @details Stores non-zero elements as their exponent indices relative to a
 * primitive root, with `_ZERO = p-1` reserved as the encoding of
 * 0. Holds two size-`P` tables (`_exp_table`, `_log_table`) for
 * hop-to-residue / hop-back conversions, and bundles a
 * `CoefficientRingZZp` and `ARingZZp` facade so the same ring can
 * appear under either coefficient interface. The `int_to_exp` /
 * `to_int` helpers convert between the internal index world and
 * the externally visible residue.
 *
 * @ingroup rings
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
