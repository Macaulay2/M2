#ifndef _m2_free_algebra_hpp_
#define _m2_free_algebra_hpp_

/**
 * @file M2FreeAlgebra.hpp
 * @brief `Ring`-shaped wrapper that exposes a non-commutative `FreeAlgebra` to the rest of the engine.
 *
 * The non-commutative implementation in `NCAlgebras/FreeAlgebra.hpp`
 * (`class FreeAlgebra : public our_new_delete`) deliberately does
 * not inherit from `Ring` --- it wants clean templates and no
 * virtual-dispatch overhead. But matrices, modules, resolutions,
 * `Computation`s, and `RingMap` construction all want a `Ring*`.
 * `M2FreeAlgebra` reconciles the two by owning a
 * `std::unique_ptr<FreeAlgebra>` and forwarding every `Ring`
 * virtual call to the wrapped instance, so the non-commutative
 * ring slots transparently into `Matrix` / `MutableMatrix` /
 * `RingElement` slots.
 *
 * The class hierarchy here is two-level: this file also declares
 * the abstract `M2FreeAlgebraOrQuotient : public Ring`, which
 * `M2FreeAlgebra` and `M2FreeAlgebraQuotient` both inherit from.
 * That intermediate fixes `is_commutative_ring()` to `false` and
 * pins the abstract API (`freeAlgebra()`, `n_vars()`,
 * `coefficientRing()`, `from_coefficient()`, `makeTerm()`,
 * `cast_to_M2FreeAlgebraOrQuotient()`) plus `toPoly` / `fromPoly`
 * / `appendFromModuleMonom` / `fromModuleMonom` helpers that
 * translate between `ring_elem` (carrying the value through the
 * `mPolyVal` slot) and the shared `Poly` from `Polynomial.hpp`.
 * This wrap-a-templated-implementation-in-a-`Ring` pattern
 * mirrors `aring-glue.hpp`'s `ConcreteRing<R>` for the aring
 * family.
 *
 * @see NCAlgebras/FreeAlgebra.hpp
 * @see Polynomial.hpp
 * @see aring-glue.hpp
 * @see M2FreeAlgebraQuotient.hpp
 */

#include <M2/math-include.h>
#include "engine-includes.hpp"

#include <memory>
#include <string>
#include <vector>

#include "NCAlgebras/FreeAlgebra.hpp"
#include "NCAlgebras/FreeMonoid.hpp"
#include "Polynomial.hpp"
#include "ring.hpp"
#include "ringelem.hpp"

class PolynomialRing;
class RingMap;
class buffer;
struct Monoid;

//struct CoefficientRingTypeExample
//{
//  typedef ring_elem ElementType;
//};

/**
 * @brief Abstract `Ring` subclass that lifts either a `FreeAlgebra` or a
 * `FreeAlgebraQuotient` into the engine's `Ring` hierarchy.
 *
 * @details The common base of `M2FreeAlgebra` (the unquotiented case) and
 * `M2FreeAlgebraQuotient`. Provides the `Ring` overrides that
 * dispatch to the underlying `freeAlgebra()` and the type-safe
 * cast helpers (`toPoly` / `fromPoly` / `appendFromModuleMonom`)
 * the rest of the engine uses to convert between opaque `ring_elem`
 * handles and concrete `Poly*` values.
 */
class M2FreeAlgebraOrQuotient : public Ring
{
public:
  const Poly* toPoly(const ring_elem f) const { return reinterpret_cast<const Poly*>(f.get_Poly()); }

  ring_elem fromPoly(Poly* f) const { return reinterpret_cast<Nterm*>(f); }

  void appendFromModuleMonom(Poly& f, const ModuleMonom& m) const;

  ring_elem fromModuleMonom(const ModuleMonom& m) const;

public:  
  virtual const FreeAlgebra& freeAlgebra() const = 0;

  virtual int n_vars() const = 0;

  virtual const Ring* coefficientRing() const = 0;

  virtual ring_elem from_coefficient(const ring_elem a) const = 0;

  virtual ring_elem makeTerm(const ring_elem a, const_varpower monom) const = 0;

  // casting functions
  virtual const M2FreeAlgebraOrQuotient * cast_to_M2FreeAlgebraOrQuotient()  const { return this; }
  virtual       M2FreeAlgebraOrQuotient * cast_to_M2FreeAlgebraOrQuotient()        { return this; }

  bool is_commutative_ring() const { return false; }

};

/**
 * @brief Concrete `Ring` wrapper around an owned `FreeAlgebra` (no quotient).
 *
 * @details Holds the wrapped algebra via `std::unique_ptr<FreeAlgebra>` and
 * delegates every `Ring` operation to it. `create()` is the factory
 * that builds the underlying `FreeAlgebra` from the user-visible
 * names / degrees / weight vectors / heft vector and hands the
 * resulting unique pointer in. The companion
 * `M2FreeAlgebraQuotient` plays the same role for a
 * `FreeAlgebraQuotient`.
 */
class M2FreeAlgebra : public M2FreeAlgebraOrQuotient
{
private:
  const std::unique_ptr<FreeAlgebra> mFreeAlgebra;

  M2FreeAlgebra(std::unique_ptr<FreeAlgebra> F);

public:
  static M2FreeAlgebra* create(const Ring* K,
                               const std::vector<std::string>& names,
                               const PolynomialRing* degreeRing,
                               const std::vector<int>& degrees,
                               const std::vector<int>& wtvecs,
                               const std::vector<int>& heftVector
                               );

  const FreeAlgebra& freeAlgebra() const { return *mFreeAlgebra; }
  const FreeMonoid& monoid() const { return freeAlgebra().monoid(); }
  const Monoid& degreeMonoid() const { return monoid().degreeMonoid(); }

  const PolynomialRing* degreeRing() const { return monoid().degreeRing(); }
  const Ring* coefficientRing() const { return freeAlgebra().coefficientRing(); }
  
  int numVars() const { return monoid().numVars(); }
  virtual int n_vars() const { return numVars(); }
  
  // these are all the functions from Ring that must exist for M2FreeAlgebra to be instantiated
  virtual int index_of_var(const ring_elem a) const;
  
  virtual void text_out(buffer &o) const;
  virtual unsigned int computeHashValue(const ring_elem a) const;
  virtual ring_elem from_coefficient(const ring_elem a) const;
  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_srcptr n) const;
  virtual bool from_rational(const mpq_srcptr q, ring_elem &result) const;

  virtual ring_elem var(int v) const;
  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;
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

  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;

  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;
  virtual void syzygy(const ring_elem a, const ring_elem b,
                      ring_elem &x, ring_elem &y) const;
  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one,
                             bool p_plus,
                             bool p_parens) const;
  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const;

  virtual engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                              const ring_elem f) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual bool multi_degree(const ring_elem f, monomial d) const;

  virtual SumCollector *make_SumCollector() const;

  long n_terms(const ring_elem f) const;

  bool is_homogeneous(const Poly* f) const;

  // returns true if f is homogeneous, and sets already_allocated_degree_vector
  // to be the LCM of the exponent vectors of the degrees of all terms in f.
  virtual bool multi_degree(const Poly* f, monomial already_allocated_degree_vector) const;
  
  // lead coefficient, monomials and terms.
  ring_elem lead_coefficient(const Ring* coeffRing, const Poly* f) const;
  ring_elem lead_coefficient(const Ring* coeffRing, const ring_elem f) const
  {
    return lead_coefficient(coeffRing, reinterpret_cast<const Poly*>(f.get_Poly()));
  }

  #if 0
  // lead_monomial: returns an allocated EngineMonomial meant for the front end of M2.
  const int* lead_monomial(const Poly* f) const;
  const int* lead_monomial(const ring_elem f) const { return lead_monomial reinterpret_cast<const Poly*>((f.get_Poly())); }
  #endif
  
  // lead terms, or get contiguous terms
  Poly* get_terms(const Poly* f, int lo, int hi) const;
  ring_elem get_terms(const ring_elem f, int lo, int hi) const
  {
    const Poly* result = get_terms(reinterpret_cast<const Poly*>(f.get_Poly()), lo, hi);
    return ring_elem(reinterpret_cast<const Poly*>(result));
  }

  // support functions
  virtual M2_arrayint support(const ring_elem a) const;

  // casting functions
  virtual const M2FreeAlgebra * cast_to_M2FreeAlgebra()  const { return this; }
  virtual       M2FreeAlgebra * cast_to_M2FreeAlgebra()        { return this; }

  void debug_display(const Poly* f) const;
  void debug_display(const ring_elem ff) const;

  ring_elem makeTerm(const ring_elem a, const_varpower monom) const;

  void makeTerm(Poly& result, const ring_elem a, const_varpower monom) const;
};

PolyList copyPolyVector(const M2FreeAlgebraOrQuotient* A,
                        const PolyList& polys);
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
