// Copyright 2005, Michael E. Stillman

#ifndef _qring_hpp_
#define _qring_hpp_

/**
 * @file qring.hpp
 * @brief `QRingInfo` family --- bookkeeping plus normal-form machinery attached to a `PolyRingQuotient` for `R / I` reductions.
 *
 * Declares `QRingInfo`, the helper that holds the defining
 * ideal of a polynomial-ring quotient in two redundant forms:
 * `VECTOR(Nterm*) quotient_ideal` for `ring_elem`-side
 * reduction, and `VECTOR(gbvector*) quotient_gbvectors` for the
 * GB-tuned reduction path through `gbring.hpp`. Keeping both
 * representations skips per-reduction conversion, and
 * `qring.cpp` is what keeps them in sync via
 * `appendQuotientElement`. The base class itself is a virtual
 * shell: its `normal_form` and `gbvector_normal_form` are no-ops
 * (the bodies cast their arguments to `(void)` and return), and
 * its `get_quotient_monomials` / `_MonomialTable` / `_MonomialTableZZ`
 * all return `nullptr` --- the real reduction lives in three
 * subclass branches.
 *
 * `QRingInfo_field` adds a `MonomialIdeal Rideal` and a
 * `MonomialTable ringtable` indexed by `quotient_ideal` slot;
 * `QRingInfo_field_basic` (basic field coefficients) supplies
 * `reduce_lead_term_basic_field` and the matching `normal_form`
 * / `gbvector_normal_form` overrides, while
 * `QRingInfo_field_QQ` does the same with a denominator-aware
 * `reduce_lead_term_QQ` plus the three-argument
 * `gbvector_normal_form(F, f, use_denom, denom)` overload.
 * `QRingInfo_ZZ` swaps in a `MonomialTableZZ` and tracks the
 * `is_ZZ_quotient_` / `ZZ_quotient_value_` pair so quotients
 * whose defining ideal contains a non-zero integer can short-
 * circuit coefficient reductions. `QRingInfo` is not itself a
 * `Ring` --- it is the data hung off `PolyRingQuotient` (a
 * `PolyRingFlat` subclass in `polyquotient.hpp`), which is the
 * `Ring` the rest of the engine sees.
 *
 * @see polyquotient.hpp
 * @see gbring.hpp
 * @see polyring.hpp
 */

#include "style.hpp"
#include "ringelem.hpp"
#include <vector>

class PolyRing;
class FreeModule;
class MonomialIdeal;
class MonomialTable;
class MonomialTableZZ;
class gbvector;
class GBRing;

/**
 * @brief Bookkeeping helper holding the defining ideal of a polynomial-ring
 * quotient `R / I` in the two representations the engine reduces against.
 *
 * @details Stores the quotient generators twice: as `Nterm*` for `ring_elem`
 * reduction (`normal_form`) and as `gbvector*` for GB-tuned
 * reduction (`gbvector_normal_form`), kept in sync by
 * `appendQuotientElement`. This base class is a virtual shell ---
 * its `normal_form` / `gbvector_normal_form` bodies are no-ops and
 * its `get_quotient_*` accessors return `nullptr`. Concrete reduction
 * lives in the three subclass branches `QRingInfo_field_basic`,
 * `QRingInfo_field_QQ`, and `QRingInfo_ZZ`. A `QRingInfo` is not
 * itself a `Ring`; it is the data hung off `PolyRingQuotient`.
 *
 * @ingroup ringinfo
 */
class QRingInfo : public our_new_delete
{
  VECTOR(Nterm *) quotient_ideal;
  VECTOR(gbvector *) quotient_gbvectors;

 protected:
  const PolyRing *R;
  bool overZZ_;  // really: base is basic, ZZ, or frac ring

  size_t exp_size;    // byte size of temp exponents_t
  size_t monom_size;  // and monomials, to be allocated on stack

  void appendQuotientElement(Nterm *f, gbvector *g);
  QRingInfo(const PolyRing *R);

 public:
  QRingInfo() : R(nullptr), overZZ_(false), exp_size(0), monom_size(0) {}
  virtual void destroy(GBRing *GR);
  virtual ~QRingInfo();

  int n_quotients() const { return INTSIZE(quotient_ideal); }
  Nterm *quotient_element(int i) const { return quotient_ideal[i]; }
  const gbvector *quotient_gbvector(int i) const
  {
    return quotient_gbvectors[i];
  }

  virtual void normal_form(ring_elem &f) const
    {
      (void) f;
    }
  virtual void gbvector_normal_form(const FreeModule *F, gbvector *&f) const
    {
      (void) F;
      (void) f;
    }
  virtual void gbvector_normal_form(const FreeModule *F,
                                    gbvector *&f,
                                    bool use_denom,
                                    ring_elem &denom) const
  {
    (void) use_denom;
    (void) denom;
    gbvector_normal_form(F, f);
  }

  virtual const MonomialIdeal *get_quotient_monomials() const { return nullptr; }
  // Each bag value is an "Nterm *".

  virtual MonomialTable *get_quotient_MonomialTable() const { return nullptr; }
  // Each id is an index into quotient_ideal_

  virtual const MonomialTableZZ *get_quotient_MonomialTableZZ() const
  {
    return nullptr;
  }
  // Each id is an index into quotient_ideal_
};

/**
 * @brief `QRingInfo` partial specialisation that adds the field-coefficient
 * reduction tables: a `MonomialIdeal` for divisibility queries and a
 * `MonomialTable` indexed by `quotient_ideal` slot.
 *
 * @details Exposes the tables through `get_quotient_monomials` /
 * `get_quotient_MonomialTable` but leaves `normal_form` itself
 * unimplemented --- that is provided by the two concrete subclasses
 * `QRingInfo_field_basic` and `QRingInfo_field_QQ`.
 *
 * @ingroup ringinfo
 */
class QRingInfo_field : public QRingInfo
{
 protected:
  MonomialIdeal *Rideal;
  MonomialTable *ringtable;

 public:
  QRingInfo_field(const PolyRing *ambientR, const VECTOR(Nterm *) & quotients);
  void destroy(GBRing *GR);
  ~QRingInfo_field();

  virtual const MonomialIdeal *get_quotient_monomials() const { return Rideal; }
  // Each bag value is an "Nterm *".

  virtual MonomialTable *get_quotient_MonomialTable() const
  {
    return ringtable;
  }
  // Each id is an index into quotient_ideal_
};

/**
 * @brief `QRingInfo_field` specialisation for basic-field coefficients
 * (everything except `QQ`).
 *
 * @details Implements `normal_form` and `gbvector_normal_form` via
 * `reduce_lead_term_basic_field`, which expects field arithmetic
 * with no denominator tracking.
 *
 * @ingroup ringinfo
 */
class QRingInfo_field_basic : public QRingInfo_field
{
  void reduce_lead_term_basic_field(Nterm *&f, const Nterm *g) const;

 public:
  QRingInfo_field_basic(const PolyRing *ambientR,
                        const VECTOR(Nterm *) & quotients);
  ~QRingInfo_field_basic();

  void normal_form(ring_elem &f) const;

  void gbvector_normal_form(const FreeModule *F, gbvector *&f) const;
};

/**
 * @brief `QRingInfo_field` specialisation for `QQ` coefficients, which need
 * denominator tracking through reductions.
 *
 * @details Implements `normal_form` and `gbvector_normal_form` plus the
 * three-argument `gbvector_normal_form(F, f, use_denom, denom)`
 * overload that accumulates the cleared denominator. The reduction
 * itself goes through `reduce_lead_term_QQ`.
 *
 * @ingroup ringinfo
 */
class QRingInfo_field_QQ : public QRingInfo_field
{
  void reduce_lead_term_QQ(Nterm *&f, const Nterm *g) const;

 public:
  QRingInfo_field_QQ(const PolyRing *ambientR,
                     const VECTOR(Nterm *) & quotients);
  ~QRingInfo_field_QQ();

  void normal_form(ring_elem &f) const;

  void gbvector_normal_form(const FreeModule *F, gbvector *&f) const;

  void gbvector_normal_form(const FreeModule *F,
                            gbvector *&f,
                            bool use_denom,
                            ring_elem &denom) const;
};

/**
 * @brief `QRingInfo` specialisation for quotients of polynomial rings over `ZZ`.
 *
 * @details Replaces the field-side `MonomialTable` with a `MonomialTableZZ`
 * and tracks the `is_ZZ_quotient_` / `ZZ_quotient_value_` pair so
 * that if the defining ideal contains a non-zero integer, coefficient
 * reductions can be short-circuited modulo that integer.
 * `reduce_lead_term_ZZ` drives both `normal_form` and
 * `gbvector_normal_form`.
 *
 * @ingroup ringinfo
 */
class QRingInfo_ZZ : public QRingInfo
{
  MonomialTableZZ *ringtableZZ;
  bool is_ZZ_quotient_;  // true if this is a quotient of a polynomial ring over
                         // ZZ, AND
                         // there is an integer in the factored ideal.
  ring_elem ZZ_quotient_value_;  // This is the integer in the factor ideal, if
                                 // is_ZZ_quotient is set.

  bool reduce_lead_term_ZZ(Nterm *&f, const Nterm *g) const;

 public:
  QRingInfo_ZZ(const PolyRing *ambientR, const VECTOR(Nterm *) & quotients);
  void destroy(GBRing *GR);
  ~QRingInfo_ZZ();

  bool is_ZZ_quotient() const { return is_ZZ_quotient_; }
  ring_elem ZZ_quotient_value() const { return ZZ_quotient_value_; }
  void normal_form(ring_elem &f) const;

  void gbvector_normal_form(const FreeModule *F, gbvector *&f) const;

  virtual const MonomialTableZZ *get_quotient_MonomialTableZZ() const
  {
    return ringtableZZ;
  }
  // Each id is an index into quotient_ideal_
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
