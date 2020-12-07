// Copyright 2004 Michael E. Stillman

#ifndef _polyring_hpp_
#define _polyring_hpp_

#include "ringelem.hpp"

#include <vector>

#include "interface/computation.h"
#include "skew.hpp"

class buffer;
class Monoid;
class Ring;
class MonomialIdeal;
class MonomialTable;
class MonomialTableZZ;

class PolynomialRing;
class PolyRingFlat;
class PolyRing;
class PolyRingSkew;
class PolyRingWeyl;
class PolyRingNC;
class PolyQuotient;

class GBRing;
class GBRingSkew;
class GBComputation;

#include "ring.hpp"
#include "qring.hpp"

/**
 * \ingroup polynomialrings
 */
class PolynomialRing : public Ring
{
  bool is_graded_;

 protected:
  // Most skew-mult specific poly code is in skewpoly.{hpp,cpp}.  However, var,
  // homogenize,
  //   and diff_term all have a small amount of skew commutative specific code.
  bool is_skew_;
  SkewMultiplication skew_;  // Completely ignored if is_skew_ is false.

  bool is_weyl_;      // true iff numerR_ is a Weyl algebra.
  bool is_solvable_;  // true iff numerR_ is a solvable algebra.

  Ring::CoefficientType coeff_type_;
  bool overZZ_;  // true iff this is a quotient over ZZ.
  QRingInfo *qinfo_;
  bool is_ZZ_quotient_;  // true if this is a quotient of a polynomial ring over
                         // ZZ, AND
                         // there is an integer in the factored ideal.
  ring_elem ZZ_quotient_value_;  // This is the integer in the factor ideal, if
                                 // is_ZZ_quotient is set.

  size_t poly_size_;

  int nvars_;
  int exp_size;  // in bytes, only used for local stack storage for exponent
                 // vectors
  const Ring *K_;
  const Monoid *M_;
  const PolyRing *numerR_;  // numerator ring, possibly 'this'
  // This is always a PolyRing, with no quotient elements
  // If ring is basic[M]/I, then numerR is basic[M]
  // If ring is QQ[M]/I, then numerR is ZZ[M].
  // skew and weyl multiplication is determined by this ring.
  // initialize_PolynomialRing will set these values,
  //   if numerR_ != this.
  const Ring *denomR_;  // denominator ring, or NULL
                        // If ring is basic[M]/I, this is NULL
                        // If ring is QQ[M]/I, this is ZZ
  const PolynomialRing
      *ambientR_;  // ambient ring (i.e. no quotients), possibly 'this'.
                   // If ring is basic[M]/I, then ambientR_ is same as numerR_
                   // If ring is QQ[M]/I, then ambientR_ is QQ[M].

  GBRing *gb_ring_;

  void setIsGraded(bool new_val) { is_graded_ = new_val; }
  void setQuotientInfo(QRingInfo *qinfo0);
  void initialize_PolynomialRing(const Ring *K,
                                 const Monoid *M,
                                 const PolyRing *numeratorR,
                                 const PolynomialRing *ambientR,
                                 const Ring *denomR);

  virtual ~PolynomialRing();
  PolynomialRing() : is_graded_(true), is_skew_(false), qinfo_(new QRingInfo) {}
  static PolynomialRing *create_quotient(const PolynomialRing *R,
                                         VECTOR(Nterm *) & elems);
  // Grabs 'elems'.  Each element of 'elems' should be in the ring R.
  // They should also form a GB.

 public:
  static PolynomialRing *create_quotient(const PolynomialRing *R,
                                         const Matrix *M);

  static PolynomialRing *create_quotient(const PolynomialRing *R,
                                         const PolynomialRing *B);
  // R should be an ambient poly ring
  // B should have: ambient of B is the logical coeff ring of R
  //   i.e. R = A[x], B = A/I
  // return A[x]/I.

  virtual void clear() { qinfo_->destroy(gb_ring_); }
  // Removes all space associated with 'this', at least the part
  // that is stashed: gb_ring_ (if it was created by this ring), qinfo_.

  Matrix *getPresentation() const;

  unsigned long get_precision() const { return K_->get_precision(); }
  bool is_basic_ring() const
  {
    return false;
  }  // The default is to be a basic ring.

  bool is_poly_ring() const { return true; }
  // Returns true if this is a polynomial ring, possibly with fractions
  // and possibly with quotient ideal, and possibly with non-commutative
  // multiplication.  Equivalent to (cast_to_PolynomialRing() != 0).

  bool is_graded() const { return is_graded_; }
  // Is this ring graded, with the given grading?
  // polynomial rings are graded
  // Weyl algebras can be graded or not
  // quotient polynomial rings can be graded or not.

  CoefficientType coefficient_type() const { return coeff_type_; }
  // What the ultimate coefficient type is.  ZZ, QQ, finite fields return these
  // three values.  Fraction fields return their ultimate value, as do poly
  // rings.

  int n_vars() const { return nvars_; }
  static PolynomialRing *create_quotient_ring(const Matrix *M);

  QRingInfo *get_quotient_info() const { return qinfo_; }
  const Ring *getCoefficientRing() const { return getCoefficients(); }
  // getCoefficientRing needs to be totally removed.

  // Quotient ring information
  MonomialTable *get_quotient_MonomialTable() const
  {
    return qinfo_->get_quotient_MonomialTable();
  }

  const MonomialIdeal *get_quotient_monomials() const
  {
    return qinfo_->get_quotient_monomials();
  }

  const MonomialTableZZ *get_quotient_MonomialTableZZ() const
  {
    return qinfo_->get_quotient_MonomialTableZZ();
  }

  int n_quotients() const { return qinfo_->n_quotients(); }
  Nterm *quotient_element(int i) const { return qinfo_->quotient_element(i); }
  const gbvector *quotient_gbvector(int i) const
  {
    return qinfo_->quotient_gbvector(i);
  }

  const MonomialIdeal *make_basis_MonomialIdeal() const
  {
    return get_quotient_monomials();
  }
  // Returns the monomial ideal consisting of monomials which are initial terms
  // in this quotient ring.  IE, the set of all monomials outside of this
  // ideal form a generating set for the ring as a
  // module over the ring getLogicalCoefficients().

  bool is_quotient_ring() const { return n_quotients() > 0; }
  // skew commutativity
  bool is_skew_commutative() const { return is_skew_; }
  int n_skew_commutative_vars() const { return skew_.n_skew_vars(); }
  int skew_variable(int i) const { return skew_.skew_variable(i); }
  bool is_skew_var(int v) const { return skew_.is_skew_var(v); }
  const SkewMultiplication &getSkewInfo() const { return skew_; }
  virtual bool is_commutative_ring() const
  {
    return !is_weyl_ && !is_skew_ && !is_solvable_;
  }
  // Returns true iff this is a commutative ring.

  virtual bool is_weyl_algebra() const { return is_weyl_; }
  // Returns true if this is a polynomial ring (possibly with quotient)
  // (possibly with ZZ fractions, or other commutative fractions)
  // but with Weyl algebra multiplication on some of the variables.

  virtual bool is_skew_commutative_ring() const { return is_skew_; }
  // Returns true if this is a polynomial ring (possibly with quotient)
  // (possibly with ZZ fractions, or other commutative fractions)
  // but with some variables anti-commuting.

  virtual bool is_solvable_algebra() const { return is_solvable_; }
  virtual const PolyRing *getNumeratorRing() const { return numerR_; }
  virtual const PolynomialRing *getAmbientRing() const { return ambientR_; }
  // Yields the ambient PolyRing corresponding to this polynomial ring
  // This ring has no quotients, no fractions (not even QQ), but may have
  // skew, weyl, or solvable multiplication, or even (later) be an associative
  // free algebra.

  virtual const Ring /* or null */ *getDenominatorRing() const
  {
    return denomR_;
  }
  // If this ring has no denominators, NULL is returned.  Otherwise the ring
  // which
  // implements denominators is returned.  When one asks for a denominator for
  // elements of
  // 'this', the result value is its ring.

  virtual GBRing *get_gb_ring() const { return gb_ring_; }
  virtual const Ring *getCoefficients() const { return K_; }
  // The implementation coeff ring of 'this'.  This is either a basic ring
  // (field, ZZ), or
  // is another PolyRing.

  virtual const Monoid *getMonoid() const { return M_; }
  // The implementation monoid of this ring.

  virtual bool is_fraction_poly_ring() const
  {
    return getDenominatorRing() != 0;
  }
  // returns true if this ring has fractions.  This includes
  // polynomial rings over QQ, polynomial rings over fraction fields,
  // fraction rings, and localizations.
  // If this returns true, then 'get_denominator_ring()' returns non-NULL value.
  //

  virtual int n_fraction_vars() const
  {
    const Ring *D = getDenominatorRing();
    if (D == 0) return 0;
    const PolynomialRing *DR = D->cast_to_PolynomialRing();
    if (DR == 0) return 0;
    return DR->n_vars();
  }

  virtual const PolynomialRing *cast_to_PolynomialRing() const { return this; }
  virtual PolynomialRing *cast_to_PolynomialRing() { return this; }
  SumCollector *make_SumCollector() const;

  ////////////////////////////////
  // To possibly be over-ridded //
  ////////////////////////////////

  virtual void text_out(buffer &o) const = 0;

  ////////////////////////
  // Arithmetic //////////
  ////////////////////////
  virtual unsigned int computeHashValue(const ring_elem a) const;

  virtual ring_elem var(int v) const = 0;

  /////////////////////////
  // Polynomial routines //
  /////////////////////////
  virtual int index_of_var(const ring_elem a) const = 0;
  virtual M2_arrayint support(const ring_elem a) const = 0;

  virtual bool is_homogeneous(const ring_elem f) const = 0;
  virtual void degree(const ring_elem f, int *d) const = 0;
  virtual bool multi_degree(const ring_elem f, int *d) const = 0;
  virtual void degree_weights(const ring_elem f,
                              M2_arrayint wts,
                              int &lo,
                              int &hi) const = 0;
  virtual ring_elem homogenize(const ring_elem f,
                               int v,
                               int deg,
                               M2_arrayint wts) const = 0;
  virtual ring_elem homogenize(const ring_elem f,
                               int v,
                               M2_arrayint wts) const = 0;

  virtual ring_elem mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const int *m) const = 0;

  virtual int n_flat_terms(const ring_elem f) const = 0;
  virtual int n_logical_terms(int nvars0, const ring_elem f) const = 0;

  virtual engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                              const ring_elem f) const = 0;
  virtual ring_elem *get_parts(const M2_arrayint wts,
                               const ring_elem f,
                               long &result_len) const = 0;
  virtual ring_elem get_part(const M2_arrayint wts,
                             const ring_elem f,
                             bool lobound_given,
                             bool hibound_given,
                             long lobound,
                             long hibound) const = 0;

  int n_terms(const ring_elem f) const { return n_flat_terms(f); }
  // This is here mainly because geopoly requires n_terms.

  virtual ring_elem make_flat_term(const ring_elem a, const int *m) const = 0;
  virtual ring_elem make_logical_term(const Ring *coeffR,
                                      const ring_elem a,
                                      const int *exp) const = 0;
  //  virtual ring_elem term(const ring_elem a, const int *m) const = 0;

  virtual ring_elem lead_flat_coeff(const ring_elem f) const = 0;
  virtual ring_elem lead_logical_coeff(const Ring *coeffR,
                                       const ring_elem f) const = 0;

  virtual ring_elem get_coeff(const Ring *coeffR,
                              const ring_elem f,
                              const int *vp) const = 0;
  // vp is a varpower monomial, in the logical monoid.
  // The result will be an element in the logical coefficient ring.

  virtual ring_elem get_terms(int nvars0,
                              const ring_elem f,
                              int lo,
                              int hi) const = 0;
  // get the (logical) terms from lo to hi in f.  A negative value means count
  // from
  // the end.  get_terms(--,f,0,0) is the logical lead term of f.

  virtual const int *lead_flat_monomial(const ring_elem f) const = 0;
  virtual void lead_logical_exponents(int nvars0,
                                      const ring_elem f,
                                      int *result_exp) const = 0;

  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const = 0;
  virtual void divide_coeff_to(ring_elem &f, ring_elem a) const = 0;

  virtual void monomial_divisor(const ring_elem a, int *exp) const = 0;
  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const = 0;
  virtual bool in_subring(int nslots, const ring_elem a) const = 0;
  virtual void degree_of_var(int n,
                             const ring_elem a,
                             int &lo,
                             int &hi) const = 0;
  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const = 0;
  virtual ring_elem divide_by_expvector(const int *exp,
                                        const ring_elem a) const = 0;

  virtual Nterm *numerator(ring_elem f) const = 0;

 protected:
  void sort(Nterm *&f) const;

 public:
  virtual const vecterm *vec_locate_lead_term(const FreeModule *F,
                                              vec v) const = 0;
  // Returns a pointer to the vector term of v which contains the lead term (of
  // the
  // numerator).
  // If result = R->vec_locate_lead_term(F,v), (if v is non-zero)
  // To get the lead coeff, use result->comp
  // To get the lead flat monomial (of numerator), use
  // R->lead_flat_monomial(result->coeff).

  virtual vec vec_lead_term(int nparts, const FreeModule *F, vec v) const = 0;

  virtual vec vec_top_coefficient(const vec v, int &var, int &exp) const = 0;

  virtual gbvector *translate_gbvector_from_ringelem(ring_elem coeff) const = 0;

  virtual gbvector *translate_gbvector_from_vec(
      const FreeModule *F,
      const vec v,
      ring_elem &result_denominator) const = 0;
  // result/denom == v.
  // result_denom will be an element in getDenominatorRing() (if non-NULL).

  virtual vec translate_gbvector_to_vec(const FreeModule *F,
                                        const gbvector *v) const = 0;

  virtual vec translate_gbvector_to_vec_denom(const FreeModule *F,
                                              const gbvector *v,
                                              const ring_elem denom) const = 0;
  // Translate v/denom to a vector in F.  denom does not need to be positive,
  // although it had better be non-zero.
  // denom should be an element of getDenominatorRing() (if non-NULL, otherwise
  // 'denom'
  // is ignored).
};

/**
 * \ingroup polynomialrings
 */
class PolyRingFlat : public PolynomialRing
// The class of polynomial rings implemented as a pointer (single value).
{
 public:
  virtual Nterm *numerator(ring_elem f) const { return f.poly_val; }
  virtual const PolyRingFlat *cast_to_PolyRingFlat() const { return this; }
  virtual PolyRingFlat *cast_to_PolyRingFlat() { return this; }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
