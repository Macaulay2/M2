// Copyright 1995 Michael E. Stillman

#ifndef _ring_hh_
#define _ring_hh_

#include <utility>           // for pair

#include "aring.hpp"         // for RingID, ring_old
#include "error.h"           // for ERROR
#include "exceptions.hpp"    // for engine_error
#include "hash.hpp"          // for MutableEngineObject
#include "newdelete.hpp"     // for our_new_delete
#include "ringelem.hpp"      // for ring_elem, vec, vecterm (ptr only), Nter...

class ARing;
class CCC;
class CoefficientRingR;
class FractionField;
class FreeModule;
class GF;
class LocalRing;
class Monoid;
class MutableMatrix;
class PolyQQ;
class PolyRing;
class PolyRingFlat;
class PolynomialRing;
class RRR;
class RingMap;
class RingZZ;
class SchurRing2;
class SchurRing;
class SchurSnRing;
class SkewPolynomialRing;
class SolvableAlgebra;
class SumCollector;
class Tower;
class WeylAlgebra;
class Z_mod;
class buffer;
struct Matrix;
struct RingElement;

/**
    @ingroup rings

    @brief xxx
    xxx
    xxx
*/
class Ring : public MutableEngineObject
{
 protected:
  const ARing *getARing() const { return AR; }
  long mCharacteristic;  // not all rings will have characteristic that fits in
                         // a long int
  // int P;
  const PolynomialRing *degree_ring;
  M2_arrayint heft_vector;
  // This vector, if NULL, and if there are any variables in the ring imply that
  // the heft vector should be taken as the default: the first degree should be
  // used
  // If non-NULL, this should dot to a positive value for every variable in the
  // ring.
  // Question: does this include coefficient variables in the ring?

  const ARing *AR;
  mutable const CoefficientRingR *cR;  // set to NULL.  If a ring does not have
                                       // a better "ARing" or "coeffring"
                                       // implementation,
  // then calling getCoefficientRingR() will set this, and return it.
  ring_elem _non_unit;
  int _isfield;  // 1: means yes, or declared yes.
                 // 0: means not declared to be so.
                 // -1: means a non unit was found, and that _non_unit contains
                 //    a non-zero non-unit.
                 // If a non unit is found, then _isfield is set to -1.

  ring_elem zeroV;  // Three generally useful values in a ring.
  ring_elem oneV;
  ring_elem minus_oneV;

  void initialize_ring(long charac,
                       const PolynomialRing *DR = 0,
                       const M2_arrayint heft_vec = 0);
  Ring() : heft_vector(0) {}
 public:
  virtual ~Ring();

  const CoefficientRingR *getCoefficientRingR() const;

  ////////////////////////
  // Ring informational //
  ////////////////////////

  long characteristic() const { return mCharacteristic; }
  const Monoid *degree_monoid() const;
  const PolynomialRing *get_degree_ring() const { return degree_ring; }
  M2_arrayint get_heft_vector() const
  {
    return heft_vector;
  }  // This CAN BE NULL

  virtual M2::RingID ringID() const { return M2::ring_old; }
  virtual bool is_basic_ring() const
  {
    return true;
  }  // The default is to be a basic ring.
  virtual bool isFinitePrimeField() const { return false; }
  virtual bool isGaloisField() const { return false; }
  virtual bool is_ZZ() const { return false; }
  virtual bool is_QQ() const { return false; }
  virtual bool is_RRR() const { return false; }
  virtual bool is_CCC() const { return false; }
  virtual bool is_fraction_field() const { return false; }
  virtual bool is_fraction_poly_ring() const { return false; }
  // returns true if this ring has fractions.  This includes
  // polynomial rings over QQ, polynomial rings over fraction fields,
  // fraction rings, and localizations.
  // If this returns true, then 'get_denominator_ring()' returns non-NULL value.
  //

  virtual bool is_poly_ring() const { return false; }
  // Returns true if this is a polynomial ring, possibly with fractions
  // and possibly with quotient ideal, and possibly with non-commutative
  // multiplication.  Equivalent to (cast_to_PolynomialRing() != 0).

  virtual bool is_commutative_ring() const { return true; }
  // Returns true iff this is a commutative ring.

  virtual bool is_quotient_ring() const { return false; }
  // Returns true if this is a polynomial ring, (possibly with fractions),
  // with a quotient ideal.  This could be a non-commutative ring
  // with skew-commutative, Weyl algebra, or other multiplication.

  virtual bool is_weyl_algebra() const { return false; }
  // Returns true if this is a polynomial ring (possibly with quotient)
  // (possibly with ZZ fractions, or other commutative fractions)
  // but with Weyl algebra multiplication on some of the variables.

  virtual bool is_skew_commutative_ring() const { return false; }
  // Returns true if this is a polynomial ring (possibly with quotient)
  // (possibly with ZZ fractions, or other commutative fractions)
  // but with some variables anti-commuting.

  virtual bool is_solvable_algebra() const { return false; }
  virtual bool is_graded() const { return true; }
  // Is this ring graded, with the given grading?
  // ZZ, QQ, ZZ/p, GF, RR, ... are all graded.
  // polynomial rings are graded
  // Weyl algebras can be graded or not
  // quotient polynomial rings can be graded or not.

  bool is_field() const;
  bool declare_field();  // if false is returned, then an ERROR has been set.

  ring_elem get_non_unit() const;
  void set_non_unit(
      ring_elem zero_div) const;  // not really const: sets "_non_unit"

  typedef enum { COEFF_ZZ, COEFF_QQ, COEFF_BASIC } CoefficientType;
  virtual CoefficientType coefficient_type() const { return COEFF_BASIC; }
  // What the ultimate coefficient type is.  ZZ, QQ, finite fields return these
  // three values.  Fraction fields return their ultimate value, as do poly
  // rings.

  virtual bool has_associate_divisors() const { return true; }
  // There are only a few rings which do not have such divisors: frac rings
  //   over quotients of poly rings.

  ///////////////////////////////////
  // Casting up the ring hierarchy //
  ///////////////////////////////////
  virtual const RingZZ *cast_to_RingZZ() const { return 0; }
  virtual RingZZ *cast_to_RingZZ() { return 0; }
  virtual const Z_mod *cast_to_Z_mod() const { return 0; }
  virtual Z_mod *cast_to_Z_mod() { return 0; }
  virtual const GF *cast_to_GF() const { return 0; }
  virtual GF *cast_to_GF() { return 0; }
  virtual const Tower *cast_to_Tower() const { return 0; }
  virtual Tower *cast_to_Tower() { return 0; }
  virtual const PolynomialRing *cast_to_PolynomialRing() const { return 0; }
  virtual PolynomialRing *cast_to_PolynomialRing() { return 0; }
  virtual const PolyRing *cast_to_PolyRing() const { return 0; }
  virtual PolyRing *cast_to_PolyRing() { return 0; }
  virtual const PolyQQ *cast_to_PolyQQ() const { return 0; }
  virtual PolyQQ *cast_to_PolyQQ() { return 0; }
  virtual const PolyRingFlat *cast_to_PolyRingFlat() const { return 0; }
  virtual PolyRingFlat *cast_to_PolyRingFlat() { return 0; }
  virtual const FractionField *cast_to_FractionField() const { return 0; }
  virtual FractionField *cast_to_FractionField() { return 0; }
  virtual const LocalRing *cast_to_LocalRing() const { return 0; }
  virtual LocalRing *cast_to_LocalRing() { return 0; }

  virtual const SchurRing *cast_to_SchurRing() const { return 0; }
  virtual SchurRing *cast_to_SchurRing() { return 0; }
  virtual const SchurRing2 *cast_to_SchurRing2() const { return 0; }
  virtual SchurRing2 *cast_to_SchurRing2() { return 0; }
  virtual const SchurSnRing *cast_to_SchurSnRing() const { return 0; }
  virtual SchurSnRing *cast_to_SchurSnRing() { return 0; }
  virtual const SkewPolynomialRing *cast_to_SkewPolynomialRing() const
  {
    return 0;
  }
  virtual SkewPolynomialRing *cast_to_SkewPolynomialRing() { return 0; }
  virtual const SolvableAlgebra *cast_to_SolvableAlgebra() const { return 0; }
  virtual SolvableAlgebra *cast_to_SolvableAlgebra() { return 0; }
  virtual const WeylAlgebra *cast_to_WeylAlgebra() const { return 0; }
  virtual RRR *cast_to_RRR() { return 0; }
  virtual const RRR *cast_to_RRR() const { return 0; }
  virtual CCC *cast_to_CCC() { return 0; }
  virtual const CCC *cast_to_CCC() const { return 0; }
  // Galois Field routines.  These three routines only return non-NULL values
  // if this was created as a Galois field, isom to A = kk[b]/(f(b)), kk = prime
  // field of char p.

  // Returns NULL if not a GF.  Returns f(b) in the ring kk[b].  (Variable name
  // might be different)
  virtual const RingElement *getMinimalPolynomial() const { return 0; }
  // Returns NULL if not a GF.  Returns an element of 'this', whose powers give
  // all non-zero elements
  // of the field.
  virtual const RingElement *getGenerator() const
  {
    ERROR("not implemented for this ring");
    return 0;
  }

  // For some finite fields, if a = (getGenerator())^r, return r.
  // If it is not implemented for this ring, an exception is thrown
  // If a is zero, then r is set to -1.
  virtual long discreteLog(const ring_elem &a) const
  {
    throw exc::engine_error("cannot compute discrete logarithm in this ring");
  }

  // Returns the element in the polynomial ring A corresponding to the element
  // a.
  // Returns NULL if not a GF field.
  // Essentially the same as 'lift', except that more information, not readily
  // available, is needed
  // for that call.
  virtual const RingElement *getRepresentation(const ring_elem &a) const
  {
    return 0;
  }

  virtual MutableMatrix *makeMutableMatrix(size_t nrows,
                                           size_t ncols,
                                           bool dense) const
  {
    return 0;
  }

  virtual FreeModule *make_FreeModule() const;
  virtual FreeModule *make_Schreyer_FreeModule() const;
  virtual FreeModule *make_FreeModule(int n) const;

  virtual SumCollector *make_SumCollector() const;

  virtual void text_out(buffer &o) const = 0;

  //////////////////////
  // Ring arithmetic ///
  //////////////////////
  virtual unsigned int computeHashValue(const ring_elem a) const = 0;

  virtual std::pair<bool, long> coerceToLongInteger(ring_elem a) const;

  ring_elem one() const { return oneV; }
  ring_elem minus_one() const { return minus_oneV; }
  ring_elem zero() const { return zeroV; }
  virtual ring_elem from_long(long n) const = 0;
  virtual ring_elem from_int(mpz_srcptr n) const = 0;

  // from_rational: if the rational q cannot be placed into this ring, false is
  // returned, and result is not touched.
  virtual bool from_rational(const mpq_srcptr q, ring_elem &result) const = 0;

  // The default version calls from_long(0) and returns false.
  virtual bool from_BigReal(gmp_RR a, ring_elem &result) const;
  // The default version calls from_long(0) and returns false.
  virtual bool from_BigComplex(gmp_CC z, ring_elem &result) const;
  // Returns false if this ring cannot coerce a double to an element in this
  // ring
  virtual bool from_double(double a, ring_elem &result) const;
  // Returns false if this ring cannot coerce a complex double (re+im*ii) to an
  // element in this ring
  virtual bool from_complex_double(double re,
                                   double im,
                                   ring_elem &result) const;

  virtual ring_elem var(int v) const;

  virtual ring_elem preferred_associate(ring_elem f) const;
  // Returns an invertible element c of the same ring such that c*f is the
  // preferred associate of the element f.
  // WARNING: The default implementation is for a field.

  virtual bool lower_associate_divisor(ring_elem &f, ring_elem g) const;
  // Replaces f with the unit c such that (fx+g)//c is the preferred associate
  //   of fx+g, in the ring A[x], where A is 'this'.
  // Returns false if f will never be changed after this
  // (This happens over ZZ if f is non-zero (therefore 1 or -1, over a finite
  // filed if f != 0,
  // but over QQ will never happen)
  // WARNING: The default implementation is for a field.

  virtual bool promote(const Ring *R,
                       const ring_elem f,
                       ring_elem &result) const = 0;
  virtual bool lift(const Ring *R,
                    const ring_elem f,
                    ring_elem &result) const = 0;

  virtual bool is_unit(const ring_elem f) const = 0;
  virtual bool is_zero(const ring_elem f) const = 0;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const = 0;

  virtual int compare_elems(const ring_elem f, const ring_elem g) const = 0;
  // Returns -1 if f < g, 0 if f and g are either equal, or considered equal,
  // and 1 if f > g.
  // The specific definition is ring dependent, but for numeric rings it
  // defaults to
  // the standard order.

  virtual ring_elem copy(const ring_elem f) const = 0;
  virtual void remove(ring_elem &f) const = 0;

  void negate_to(ring_elem &f) const;
  void add_to(ring_elem &f, ring_elem &g) const;
  void subtract_to(ring_elem &f, ring_elem &g) const;
  void mult_to(ring_elem &f, const ring_elem g) const;
  virtual ring_elem negate(const ring_elem f) const = 0;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const = 0;

  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;
  // These two power routines can be used for n >= 0.

  virtual ring_elem invert(const ring_elem f) const = 0;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const = 0;

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;
  virtual ring_elem remainderAndQuotient(const ring_elem f,
                                         const ring_elem g,
                                         ring_elem &quot) const;
  // The default version is for a field:
  //   f % 0 is f, otherwise f % g is 0.
  //   f // 0 is 0, otherwise f // g is f/g
  // These three routines: remainder, quotient and remainderAndQuotient
  // satisfy these properties:
  // If r = remainder(f,g), q = quotient(f,g), then
  // (1) f = q*g + r
  // (2) If f is in ideal(g), then r = 0.
  // (3) If g is invertible, then r = 0, and q = f * g^(-1).
  // (4) If the ring is ZZ, then the remainder is "balanced": -[g/2] < r <=
  // [g/2]
  // remainderAndQuotient combines remainder and quotient into one routine.

  virtual void syzygy(const ring_elem a,
                      const ring_elem b,
                      ring_elem &x,
                      ring_elem &y) const = 0;
  // Constructs elements x and y in the ring s.t. ax + by = 0.  This syzygy is
  // chosen as simply as possible.  For example, over QQ, x is chosen
  // to be positive.  The routine must handle the case when a=0, but can
  // ignore the case when b=0... (Really?)

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o,
                             const ring_elem f,
                             bool p_one = true,
                             bool p_plus = false,
                             bool p_parens = false) const = 0;

  virtual ring_elem eval(const RingMap *map,
                         const ring_elem f,
                         int first_var) const = 0;

  // Polynomial routines
  // The default implementation is for non-polynomial rings
  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  virtual void monomial_divisor(const ring_elem a, int *exp) const;
  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const;
  virtual bool in_subring(int nslots, const ring_elem a) const;
  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const;
  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const;
  virtual ring_elem divide_by_expvector(const int *exp,
                                        const ring_elem a) const;

  virtual ring_elem homogenize(const ring_elem f,
                               int v,
                               int deg,
                               M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, M2_arrayint wts) const;

  // Routines expecting a grading.  The default implementation
  // is that the only degree is 0.
  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;
  // returns true iff f is homogeneous
  virtual void degree_weights(const ring_elem f,
                              M2_arrayint wts,
                              int &lo,
                              int &hi) const;

  // antipode: for non skew commuting poly rings, this is the identity.
  // Otherwise, this changes the signs of the monomials, implementing the
  // (anti)-isomorphism
  // of the ring with its opposite ring.
  virtual ring_elem antipode(ring_elem f) const { return f; }
  //////////////////////////////////////////
  // Cleaning real and complex numbers /////
  //////////////////////////////////////////
  virtual unsigned long get_precision()
      const;  // if the ring is not over RRR or CCC, returns 0.
  virtual ring_elem zeroize_tiny(gmp_RR epsilon, const ring_elem f) const;
  // Default is to return f itself.
  virtual void increase_maxnorm(gmp_RRmutable norm, const ring_elem f) const;
  // If any real number appearing in f has larger absolute value than norm,
  // replace norm.
  // Default for rings not over RRR or CCC is to do nothing.
  vec vec_zeroize_tiny(gmp_RR epsilon, const vec f) const;
  // Default is to return f itself.
  void vec_increase_maxnorm(gmp_RRmutable norm, const vec f) const;
  // If any real number appearing in f has larger absolute value than norm,
  // replace norm.
  // Default for rings not over RRR or CCC is to do nothing.

  //////////////////////////////////////////
  /// vector operations ////////////////////
  //////////////////////////////////////////
  // These routines all act on linked lists
  // of vecterm's, sorted by descending component.
  // We always assume that ringelem's are immutable:
  // The same value might be shared in several vecterms.
  //
  // These routines are implemented in ring-vec.cpp
  //////////////////////////////////////////
 protected:
  vec new_vec() const;
  void remove_vec_node(vec n) const;

 public:
  void vec_sort(vecterm *&f) const;

  int compare_vecs(vec v, vec w) const;

  vec e_sub_i(int r) const;
  vec make_vec(int r, ring_elem a) const;
  vec make_vec_from_array(int len, Nterm **array)
      const;  // takes ownership of the Nterm's!!

  vec copy_vec(const vecterm *v) const;
  void remove_vec(vec v) const;

  bool is_equal(const vecterm *a, const vecterm *b) const;
  bool get_entry(const vecterm *v, int r, ring_elem &result) const;
  ring_elem get_entry(vec v, int r) const;
  vec sub_vector(const vecterm *v, M2_arrayint r) const;
  int n_nonzero_terms(const vecterm *v) const;
  void vec_text_out(buffer &o,
                    const vecterm *v,
                    bool p_one = true,
                    bool p_plus = false,
                    bool p_parens = false) const;
  vec vec_eval(const RingMap *map, const FreeModule *F, const vec v) const;

  virtual vec vec_lead_term(int nparts, const FreeModule *F, vec v) const;

  vec negate_vec(vec v) const;
  vec add_vec(vec v, vec w) const;
  vec subtract_vec(vec v, vec w) const;
  vec mult_vec(int n, vec v) const;
  vec mult_vec(const ring_elem f, const vec w) const;
  vec rightmult_vec(const vec w, const ring_elem f) const;

  void set_entry(vec &v, int i, ring_elem r) const;
  void mult_vec_to(vec &v,
                   const ring_elem r,
                   bool opposite_mult) const;  // multiplies v <- r * v or v * r
  void mult_row(vec &v, const ring_elem r, int i, bool opposite_mult) const;
  void negate_vec_to(vec &v) const;            // v <- -v.
  void add_vec_to(vec &v, vec &w) const;       // v <- v+w, w is set to 0.
  void subtract_vec_to(vec &v, vec &w) const;  // v <- v-w, w is set to 0.

  vec mult_vec_matrix(const Matrix *m, vec v, bool opposite_mult) const;

  vec component_shift(int n, vec v) const;

  vec tensor_shift(int n, int m, vec v) const;

  vec tensor(const FreeModule *F, vec v, const FreeModule *G, vec w) const;

  void divide_vec_to(vec &v, const ring_elem a) const;
  void divide_row(vec &v, int r, const ring_elem a) const;
  ring_elem dot_product(const vecterm *v, const vecterm *w) const;

  /* Polynomial routines.  These all set an error if the ring is not
     a polynomial ring.  OR, they will be moved to polyring.hpp  */
  vec vec_diff(vec v, int rankFw, vec w, int use_coeff) const;
  int vec_in_subring(int n, const vec v) const;
  void vec_degree_of_var(int n, const vec v, int &lo, int &hi) const;
  vec vec_divide_by_var(int n, int d, const vec v) const;
  vec vec_divide_by_expvector(const int *exp, const vec v) const;

  // Some divisibility routines
  bool vec_is_scalar_multiple(vec f, vec g)
      const;  // is cf = dg, some scalars c,d? (not both zero).
  vec vec_remove_monomial_factors(vec f, bool make_squarefree_only) const;

  bool vec_multi_degree(const FreeModule *F, const vec f, int *degf) const;
  // returns true iff f is homogeneous

  void vec_degree(const FreeModule *F, const vec f, int *d) const;
  void vec_degree_weights(const FreeModule *F,
                          const vec f,
                          M2_arrayint wts,
                          int &lo,
                          int &hi) const;
  bool vec_is_homogeneous(const FreeModule *F, const vec f) const;
  vec vec_homogenize(const FreeModule *F,
                     const vec f,
                     int v,
                     int deg,
                     M2_arrayint wts) const;
  vec vec_homogenize(const FreeModule *F,
                     const vec f,
                     int v,
                     M2_arrayint wts) const;

  // content of vectors and ring elements, default implementation is for basic
  // fields
  virtual void lower_content(ring_elem &c, ring_elem g)
      const;  // c is a content elem, g is in ring
  virtual ring_elem content(ring_elem f) const;
  virtual ring_elem content(ring_elem f, ring_elem g) const;
  virtual ring_elem divide_by_given_content(ring_elem f, ring_elem c) const;

  ring_elem divide_by_content(ring_elem f) const;
  ring_elem split_off_content(ring_elem f, ring_elem &result) const;
  ring_elem vec_content(vec f) const;
  vec vec_divide_by_given_content(vec f, ring_elem c) const;
  vec vec_divide_by_content(vec f) const;
  ring_elem vec_split_off_content(vec f, vec &result) const;
};

class SumCollector : public our_new_delete
{
 public:
  SumCollector() {}
  virtual ~SumCollector() {}
  virtual void add(ring_elem f) = 0;
  virtual ring_elem getValue() = 0;
};

#define ZERO_RINGELEM (ring_elem(static_cast<Nterm *>(0)))

#include "ZZ.hpp"
extern RingZZ *globalZZ;
extern RingZZ *makeIntegerRing();

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e ring.o "
// indent-tabs-mode: nil
// End:
