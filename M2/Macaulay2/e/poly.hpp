// Copyright 1995 Michael E. Stillman

#ifndef _poly_hpp_
#define _poly_hpp_

#include "ring.hpp"
#include "ringelem.hpp"
#include "skew.hpp"

///// Ring Hierarchy ///////////////////////////////////

class TermIdeal;
class Matrix;
class GBRing;
class GBRingSkew;
class GBComputation;
class ChineseRemainder;
#include "polyring.hpp"

class PolyRing : public PolyRingFlat
{
  friend class GBRingSkew;
  friend class FreeModule;
  friend class ChineseRemainder;

  friend class MatrixStream;  // for new_term()
  void initialize_poly_ring(const Ring *K,
                            const Monoid *M,
                            const PolynomialRing *deg_ring);
  // Only to be called from initialize_poly_ring and make_trivial_ZZ_poly_ring

 protected:
  void initialize_poly_ring(const Ring *K, const Monoid *M);
  // Called by subclasses (e.g. skew comm, Weyl, solvable.
  // After this, set the information for the special multiplication.
  // Then set the gb_ring

  virtual ~PolyRing();
  PolyRing() {}
  static PolyRing *trivial_poly_ring;
  static void make_trivial_ZZ_poly_ring();

 public:
  static const PolyRing *create(const Ring *K, const Monoid *M);

 public:
  virtual void clear();

  static const PolyRing *get_trivial_poly_ring();

  virtual const PolyRing *cast_to_PolyRing() const { return this; }
  virtual PolyRing *cast_to_PolyRing() { return this; }
  virtual void text_out(buffer &o) const;

  /////////////////////////
  // Arithmetic ///////////
  /////////////////////////

  ring_elem fromCoefficient(ring_elem &coeff) const;

  virtual ring_elem from_long(long n) const;
  virtual ring_elem from_int(mpz_srcptr n) const;
  virtual bool from_rational(mpq_srcptr q, ring_elem &result) const;

  virtual bool from_BigComplex(gmp_CC z, ring_elem &result) const;
  virtual bool from_BigReal(gmp_RR z, ring_elem &result) const;
  virtual bool from_Interval(gmp_RRi z, ring_elem &result) const;
  virtual bool from_double(double a, ring_elem &result) const;
  virtual bool from_complex_double(double re,
                                   double im,
                                   ring_elem &result) const;

  virtual ring_elem var(int v) const;

  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  virtual bool promote(const Ring *R,
                       const ring_elem f,
                       ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const;
  ring_elem preferred_associate_divisor(ring_elem ff) const;
  // ff is an element of 'this'.
  // result is in the coefficient ring
  // If the coefficient ring of this is
  //   ZZ -- gcd of all, same sign as lead coeff
  //   QQ -- gcd(numerators)/lcm(denominators)
  //   basic field -- lead coeff
  //   frac(poly ring) -- gcd(numerators)/lcm(denominators)
  //   frac(quotient of a poly ring) -- error

  virtual bool is_unit(const ring_elem f) const;
  virtual bool is_zero(const ring_elem f) const;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const;
  virtual int compare_elems(const ring_elem f, const ring_elem g) const;

  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;
  virtual void degree_weights(const ring_elem f,
                              M2_arrayint wts,
                              int &lo,
                              int &hi) const;
  virtual ring_elem homogenize(const ring_elem f,
                               int v,
                               int deg,
                               M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, M2_arrayint wts) const;

  virtual ring_elem copy(const ring_elem f) const;
  virtual void remove(ring_elem &f) const;

  void internal_negate_to(ring_elem &f) const;
  void internal_add_to(ring_elem &f, ring_elem &g) const;
  void internal_subtract_to(ring_elem &f, ring_elem &g) const;

  virtual ring_elem negate(const ring_elem f) const;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const;
  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;
  virtual ring_elem power(const ring_elem f, int n) const;
  virtual ring_elem invert(const ring_elem f) const;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;
  ring_elem gcd(const ring_elem f, const ring_elem g) const;
  ring_elem gcd_extended(const ring_elem f,
                         const ring_elem g,
                         ring_elem &u,
                         ring_elem &v) const;

 protected:
  void minimal_monomial(ring_elem f, int *&monom) const;
  Nterm *division_algorithm(Nterm *f, Nterm *g, Nterm *&quot) const;
  Nterm *division_algorithm(Nterm *f, Nterm *g) const;
  Nterm *powerseries_division_algorithm(Nterm *f, Nterm *g, Nterm *&quot) const;
  std::vector<int> setNegativeExponentMonomial(Nterm* f) const;
  Nterm *division_algorithm_with_laurent_variables(Nterm *f, Nterm *g, Nterm *&quot) const;

 public:
  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;
  virtual ring_elem remainderAndQuotient(const ring_elem f,
                                         const ring_elem g,
                                         ring_elem &quot) const;

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

  virtual ring_elem mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const int *m) const;

  virtual int n_flat_terms(const ring_elem f) const;
  virtual int n_logical_terms(int nvars0, const ring_elem f) const;

  virtual ring_elem get_coeff(const Ring *coeffR,
                              const ring_elem f,
                              const int *vp) const;
  virtual ring_elem get_terms(int nvars0,
                              const ring_elem f,
                              int lo,
                              int hi) const;

  virtual ring_elem make_flat_term(const ring_elem a, const int *m) const;
  virtual ring_elem make_logical_term(const Ring *coeffR,
                                      const ring_elem a,
                                      const int *exp) const;

  virtual ring_elem lead_flat_coeff(const ring_elem f) const;
  virtual ring_elem lead_logical_coeff(const Ring *coeffR,
                                       const ring_elem f) const;

  virtual const int *lead_flat_monomial(const ring_elem f) const;
  virtual void lead_logical_exponents(int nvars0,
                                      const ring_elem f,
                                      int *result_exp) const;

  ring_elem lead_term(const ring_elem f) const;  // copies the lead term

  virtual engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                              const ring_elem f) const;
  virtual ring_elem *get_parts(const M2_arrayint wts,
                               const ring_elem f,
                               long &result_len) const;
  virtual ring_elem get_part(const M2_arrayint wts,
                             const ring_elem f,
                             bool lobound_given,
                             bool hibound_given,
                             long lobound,
                             long hibound) const;

  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const;
  virtual void divide_coeff_to(ring_elem &f, ring_elem a) const;

  virtual ring_elem lead_term(int nparts, const ring_elem f) const;

 public:
  ///////////////////////////////////////
  // Univariate polynomial translation //
  ///////////////////////////////////////
  ring_elem fromSmallIntegerCoefficients(const std::vector<long> &coeffs,
                                         int var) const;

 public:
  /////////////////////////
  // RRR and CCC support //
  /////////////////////////
  virtual ring_elem zeroize_tiny(gmp_RR epsilon, const ring_elem f) const;
  virtual void increase_maxnorm(gmp_RRmutable norm, const ring_elem f) const;
  // If any real number appearing in f has larger absolute value than norm,
  // replace norm.

 public:
  virtual vec vec_lead_term(int nparts, const FreeModule *F, vec v) const;

  virtual vec vec_top_coefficient(const vec v, int &var, int &exp) const;

  const vecterm *vec_locate_lead_term(const FreeModule *F, vec v) const;
  // Returns a pointer to the lead vector of v.
  // This works if F has a Schreyer order, or an up/down order.

 protected:
  vec vec_coefficient_of_var(vec v, int var, int exp) const;

  ring_elem diff_term(const int *m,
                      const int *n,
                      int *resultmon,
                      int use_coeff) const;
  ring_elem power_direct(const ring_elem f, int n) const;

  ring_elem get_logical_coeff(const Ring *coeffR, const Nterm *&f) const;
  // Given an Nterm f, return the coeff of its logical monomial, in the
  // polynomial ring coeffR.  f is modified, in that it is replaced by
  // the pointer to the first term of f not used (possibly 0).

 public:
  virtual void monomial_divisor(const ring_elem a, int *exp) const; // not used

  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const;
  virtual bool in_subring(int nslots, const ring_elem a) const;
  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const;
  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const;
  virtual ring_elem divide_by_expvector(const int *exp,
                                        const ring_elem a) const;

  virtual void lower_content(ring_elem &cont, ring_elem new_coeff) const;
  virtual ring_elem content(ring_elem f) const;
  virtual ring_elem content(ring_elem f, ring_elem g) const;
  virtual ring_elem divide_by_given_content(ring_elem f, ring_elem c) const;

  // Routines special to polynomial rings
  // possibly others?
  // Rideal, exterior_vars.
  // nbits
  // heap merge of elements...?

  // Routines special to fields (anything else?)
 protected:
  Nterm *new_term() const;
  Nterm *copy_term(const Nterm *t) const;

  bool imp_attempt_to_cancel_lead_term(ring_elem &f,
                                       ring_elem g,
                                       ring_elem &coeff,
                                       int *monom) const;

 protected:
  ring_elem imp_skew_mult_by_term(const ring_elem f,
                                  const ring_elem c,
                                  const int *m) const;
  void imp_subtract_multiple_to(ring_elem &f,
                                ring_elem a,
                                const int *m,
                                const ring_elem g) const;

 public:
  void sort(Nterm *&f) const;

  ///////////////////////////////////////////////////////
  // Used in gbvector <--> vector/ringelem translation //
  ///////////////////////////////////////////////////////
  // These are only meant to be called by Ring's.
 public:
  void determine_common_denominator_QQ(ring_elem f, mpz_ptr denom_so_far) const;

  ring_elem get_denominator_QQ(ring_elem f) const;

  ring_elem vec_get_denominator_QQ(vec f) const;

  gbvector *translate_gbvector_from_vec_QQ(const FreeModule *F,
                                           const vec v,
                                           ring_elem &result_denominator) const;

  vec translate_gbvector_to_vec_QQ(const FreeModule *F,
                                   const gbvector *v,
                                   const ring_elem denom) const;

  gbvector *translate_gbvector_from_ringelem_QQ(ring_elem coeff) const;

  gbvector *translate_gbvector_from_ringelem(ring_elem coeff) const;

  gbvector *translate_gbvector_from_vec(const FreeModule *F,
                                        const vec v,
                                        ring_elem &result_denominator) const;

  vec translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const;

  vec translate_gbvector_to_vec_denom(const FreeModule *F,
                                      const gbvector *v,
                                      const ring_elem denom) const;
  // Translate v/denom to a vector in F.  denom does not need to be positive,
  // although it had better be non-zero.
};

// Returns a PolyRing iff R = ZZ/p[x], for some variable x, and some prime p.
// Otherwise returns null.
const PolyRing * /* or null */ isUnivariateOverPrimeField(const Ring *R);

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
