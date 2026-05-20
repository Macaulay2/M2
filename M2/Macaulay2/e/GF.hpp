// Copyright 1995 Michael E. Stillman.
#ifndef _GF_hh_
#define _GF_hh_

#include "relem.hpp"

/**
 * @file GF.hpp
 * @brief Legacy `Ring`-based Galois field with explicit Zech-style lookup tables.
 *
 * `GF` is the engine's original Galois-field implementation,
 * pre-dating the `aring` refactor. It inherits from `Ring`
 * directly (so arithmetic goes through virtual dispatch) and
 * stores two `int` tables indexed by powers of a chosen
 * primitive element: `_one_table[i]` is the log index of
 * `alpha^i + 1` (i.e. the Zech logarithm), and
 * `_from_int_table[a]` maps a `Z/p` residue `a` to its log
 * index. Multiplication of non-zero elements becomes index
 * addition mod `Q - 1`, addition reduces to one `_one_table`
 * lookup. Alongside the tables the class keeps a pointer to the
 * presenting polynomial ring `(Z/p)[t]/f(t)` and the primitive
 * `RingElement`, so M2 code that needs to introspect the
 * defining polynomial or round-trip through it can still do so
 * via `getMinimalPolynomial` / `get_rep`.
 *
 * Three modern siblings cover the same algebra with different
 * backing stores: `ARingGFM2` (native, CRTP-inlined via
 * `SimpleARing`), `ARingGFFlint` (FLINT Zech tables, small
 * `q`), and `ARingGFFlintBig` (FLINT `fq_nmod`, large `q`).
 * New engine code prefers those; the legacy class is reached
 * through the `rawGaloisField` entry point in
 * `interface/ring.cpp` (`GF::create(f)`).
 *
 * @see aring-m2-gf.hpp
 * @see aring-gf-flint.hpp
 * @see aring-gf-flint-big.hpp
 * @see ZZp.hpp
 */

/**
 * @brief Engine-side finite field `GF(p^n)` built on top of `(Z/p)[t] / f(t)`
 * for a primitive element of the resulting field.
 *
 * @details Represents non-zero elements by their discrete-logarithm index
 * relative to a chosen primitive `primitive_element`: index 0
 * encodes 0, `Q-1` encodes 1, and index `n` encodes
 * `primitive^n`. `_x_exponent` is the index of the original
 * generator `t`. Multiplication / division become mod-`Q1` add /
 * sub on indices; addition uses the precomputed `_one_table` to
 * resolve `1 + primitive^n`. `_from_int_table` maps small integers
 * back into indices. Falls back to `Ring` for cases not covered
 * by the `aring`-style `ARingGFM2` / `ARingGFFlint*`
 * specialisations.
 *
 * @ingroup rings
 */
class GF : public Ring
{
  // int P; // this is defined in class Ring
  const PolynomialRing *_originalR;  // This should be the ring ((Z/p)[t])/f(t).

  const RingElement *_primitive_element;  // An element of K
  int _x_exponent;  // (primitive_element)^(x_exponent) = x,
                    // the given generator of K.
  int Q_;           // this is GF(Q) = GF(P^Qexp)
  int Qexp_;        // P^Qexp = Q
  int Q1_;          // Q1 = Q-1
  int _ZERO;        // = 0   is our representation of 0.
  int _ONE;         // = Q-1 is our representation of 1.
  int _MINUS_ONE;   // = (Q-1)/2 if Q odd, = ONE if Q even.

  int *_one_table;  // Indexed from 0..Q1
  int *_from_int_table;

  //  GF(const RingElement *prim);
 protected:
  GF();
  virtual ~GF();
  bool initialize_GF(const RingElement *prim);

 public:
  const PolynomialRing *originalR() const { return _originalR; }
  static GF *create(const RingElement *prim);

  int extension_degree() const { return Qexp_; }
  GF *cast_to_GF() { return this; }
  const GF *cast_to_GF() const { return this; }
  virtual bool isGaloisField() const { return true; }
  const RingElement *getMinimalPolynomial() const;
  // returns the polynomial f(t) mentioned in the def of _originalR above.
  // this is the minimal polynomial of the given generator of this ring
  // (which is not necessarily the primitive element)
  virtual const RingElement *getGenerator() const;
  virtual const RingElement *getRepresentation(const ring_elem &a) const;

  ring_elem get_rep(ring_elem f) const;
  // takes an element of this ring, and returns an element of _originalR->XXX()

  int discrete_log(ring_elem a) const;

  // The following are all the routines required by 'ring'
  unsigned int computeHashValue(const ring_elem a) const { return a.get_int(); }
  virtual void text_out(buffer &o) const;

  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_srcptr n) const;
  virtual ring_elem var(int v) const;
  virtual bool from_rational(mpq_srcptr q, ring_elem &result) const;
  virtual bool promote(const Ring *R,
                       const ring_elem f,
                       ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

private:  
  int internal_negate(int f) const;
  int internal_add(int f, int g) const;
  int internal_subtract(int f, int g) const;

public:  
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
