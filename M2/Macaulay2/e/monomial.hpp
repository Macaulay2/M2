// (c) 1994 Michael E. Stillman

#ifndef _monomial_hh_
#define _monomial_hh_

/**
 * @file monomial.hpp
 * @brief `EngineMonomial` --- opaque single-monomial value type used at the engine boundary.
 *
 * Declares `EngineMonomial`, the `EngineObject` subclass that
 * stores one monomial as a varpower-encoded `gc_vector<int>` in
 * the standard `[2n + 1, v_1, e_1, ..., v_n, e_n]` layout
 * inherited from `ExponentList.hpp`. All five constructors
 * (no-arg, `(int, int)`, `(const int *vp)`, `(M2_arrayint)`,
 * `(const std::vector<int>&)`) are class-private; public
 * construction goes through the four static `make(...)` factory
 * overloads that mirror the parameterised constructors. Beyond
 * construction, the class exposes monomial arithmetic
 * (`operator*` / `operator/`, `power`, `lcm`, `gcd`, `radical`,
 * `erase`, `monsyz`), predicates (`is_one`, `is_equal`, `divides`,
 * `compare`, `simple_degree`), and serialisation (`text_out`,
 * `to_arrayint`, `ints()` for the raw buffer). The class is the
 * M2-level `Monomial` --- a different beast from the encoded
 * monomials packed into a `Monoid`'s inner-loop layout and from
 * the `ExponentVector` / `ExponentList` storage layer.
 *
 * The header's long-standing reversal note matters in the
 * non-commutative case: the M2 front-end reverses the
 * variable-power list before crossing the boundary, and engine
 * code consuming `EngineMonomial` in the NC setting must
 * un-reverse before computing word products.
 *
 * @see ExponentList.hpp
 * @see monoid.hpp
 */

#include <vector>

#include "ExponentList.hpp"
#include "hash.hpp"
#include "engine-includes.hpp"
#include "buffer.hpp"

/**
 * @brief Engine-side immutable monomial value type wrapping a varpower-
 * encoded exponent vector.
 *
 * @details Storage is `[2n+1, v_1, e_1, ..., v_n, e_n]` (per
 * `ExponentList.hpp`): leading length, then alternating variable
 * indices and exponents. Inherits from `EngineObject` so the
 * monomial can carry a stable content-based hash once exposed to
 * the front end. The `TODO` at the top of the file notes a planned
 * template-based unification with the lower-level varpower routines.
 */
// TODO: can this be combined with varpower using templates?
class EngineMonomial : public EngineObject
{
  // The format of a monomial is from ExponentList.hpp:
  // [2n+1, v1, e1, ..., vn, en]
  gc_vector<int> val;

  EngineMonomial();
  EngineMonomial(int v, int e);
  EngineMonomial(const int *vp);
  EngineMonomial(M2_arrayint a);
  EngineMonomial(const std::vector<int>& vp);

 protected:
  virtual unsigned int computeHashValue() const;

 public:
  static EngineMonomial *make(int v, int e);
  static EngineMonomial *make(M2_arrayint m);
  static EngineMonomial *make(const int *vp);
  static EngineMonomial *make(const std::vector<int>& vp);
  // format for this is that of a 'varpower' monomial:
  // [2n+1, v1, e1, v2, e2, ..., vn, en]
  // with each ei != 0.

  int * ints() { return val.data(); }
  const int * ints() const { return val.data(); }

  EngineMonomial *operator*(const EngineMonomial &b) const;
  EngineMonomial *operator/(const EngineMonomial &b) const;
  EngineMonomial *power(int n) const;
  void monsyz(const EngineMonomial &b, EngineMonomial *&sa, EngineMonomial *&sb) const;
  EngineMonomial *lcm(const EngineMonomial &b) const;
  EngineMonomial *gcd(const EngineMonomial &b) const;

  EngineMonomial *radical() const;
  EngineMonomial *erase(const EngineMonomial &b) const;

  bool is_one() const;
  bool is_equal(const EngineMonomial &b) const;
  bool divides(const Monoid *M, const EngineMonomial &b) const;
  int compare(const Monoid *M, const EngineMonomial &b) const;
  int simple_degree() const;

  void text_out(buffer &o) const { varpower::elem_text_out(o, val.data()); }
  M2_arrayint to_arrayint() const { return varpower::to_arrayint(val.data()); }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
