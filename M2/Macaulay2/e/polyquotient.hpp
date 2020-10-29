// Copyright 2004 Michael E. Stillman

#ifndef _polyquotient_hpp_
#define _polyquotient_hpp_

#include "engine-includes.hpp"

#include "poly.hpp"
#include "polyring.hpp"
#include "qring.hpp"
#include "ringelem.hpp"

class FreeModule;
class GBComputation;
class Ring;
class buffer;
class gbvector;
struct RingMap;

/**
 * \ingroup polynomialrings
 */
class PolyRingQuotient : public PolyRingFlat
{
  friend class PolynomialRing;

 protected:
  void normal_form(ring_elem &f) const { return qinfo_->normal_form(f); }
  virtual ~PolyRingQuotient();
  PolyRingQuotient() {}
  GBComputation *make_gb(const ring_elem g) const;
  ring_elem ann(const ring_elem a, const ring_elem b) const;
  // return an element h such that h*a is in (b).

 public:
  virtual const PolyRingQuotient *cast_to_PolyRingQuotient() const
  {
    return this;
  }
  virtual PolyRingQuotient *cast_to_PolyRingQuotient() { return this; }
  virtual void text_out(buffer &o) const;

  ////////////////////////
  // Arithmetic //////////
  ////////////////////////

  virtual ring_elem from_long(long n) const
  {
    ring_elem result = numerR_->from_long(n);
    normal_form(result);
    return result;
  }
  virtual ring_elem from_int(mpz_srcptr n) const
  {
    ring_elem result = numerR_->from_int(n);
    normal_form(result);
    return result;
  }
  virtual bool from_rational(mpq_srcptr q, ring_elem &result) const
  {
    bool ok = numerR_->from_rational(q, result);
    if (not ok) return false;
    normal_form(result);
    return true;
  }
  virtual bool from_BigReal(gmp_RR a, ring_elem &result) const
  {
    bool ret = numerR_->from_BigReal(a, result);
    normal_form(result);
    return ret;
  }
  virtual bool from_BigComplex(gmp_CC z, ring_elem &result) const
  {
    bool ret = numerR_->from_BigComplex(z, result);
    normal_form(result);
    return ret;
  }
  virtual bool from_double(double a, ring_elem &result) const
  {
    bool ret = numerR_->from_double(a, result);
    normal_form(result);
    return ret;
  }
  virtual bool from_complex_double(double re,
                                   double im,
                                   ring_elem &result) const
  {
    bool ret = numerR_->from_complex_double(re, im, result);
    normal_form(result);
    return ret;
  }

  virtual ring_elem var(int v) const
  {
    ring_elem result = numerR_->var(v);
    normal_form(result);
    return result;
  }
  virtual bool promote(const Ring *R,
                       const ring_elem f,
                       ring_elem &result) const;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const;

  virtual ring_elem preferred_associate(ring_elem f) const
  {
    return numerR_->preferred_associate(f);
  }

  virtual void lower_content(ring_elem &c, ring_elem g) const
  {
    numerR_->lower_content(c, g);
  }
  virtual ring_elem content(ring_elem f) const { return numerR_->content(f); }
  virtual ring_elem divide_by_given_content(ring_elem f, ring_elem c) const
  {
    return numerR_->divide_by_given_content(f, c);
  }

  virtual bool is_unit(const ring_elem f) const;  // TODO

  virtual bool is_zero(const ring_elem f) const
  {
    return numerR_->PolyRing::is_zero(f);
  }

  virtual bool is_equal(const ring_elem f, const ring_elem g) const
  {
    return numerR_->PolyRing::is_equal(f, g);
  }

  virtual int compare_elems(const ring_elem f, const ring_elem g) const
  {
    return numerR_->PolyRing::compare_elems(f, g);
  }

  virtual ring_elem copy(const ring_elem f) const
  {
    return numerR_->PolyRing::copy(f);
  }
  virtual void remove(ring_elem &f) const { numerR_->PolyRing::remove(f); }
  virtual ring_elem negate(const ring_elem f) const
  {
    ring_elem result = numerR_->PolyRing::negate(f);
    if (overZZ_) normal_form(result);
    return result;
  }

  virtual ring_elem add(const ring_elem f, const ring_elem g) const
  {
    ring_elem result = numerR_->PolyRing::add(f, g);
    if (overZZ_) normal_form(result);
    return result;
  }

  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const
  {
    ring_elem result = numerR_->PolyRing::subtract(f, g);
    if (overZZ_) normal_form(result);
    return result;
  }

  virtual ring_elem mult(const ring_elem f, const ring_elem g) const
  {
    ring_elem result = numerR_->PolyRing::mult(f, g);
    normal_form(result);
    return result;
  }

  virtual ring_elem power(const ring_elem f, mpz_srcptr n) const;

  virtual ring_elem power(const ring_elem f, int n) const;

  virtual ring_elem invert(const ring_elem f) const;

  virtual ring_elem divide(const ring_elem f, const ring_elem g) const;

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
                             bool p_parens = false) const
  {
    numerR_->PolyRing::elem_text_out(o, f, p_one, p_plus, p_parens);
  }

  virtual ring_elem eval(const RingMap *map,
                         const ring_elem f,
                         int first_var) const;

  /////////////////////////
  // Polynomial routines //
  /////////////////////////
  virtual int index_of_var(const ring_elem a) const
  {
    return numerR_->PolyRing::index_of_var(a);
  }

  virtual M2_arrayint support(const ring_elem a) const
  {
    return numerR_->PolyRing::support(a);
  }

  virtual bool is_homogeneous(const ring_elem f) const
  {
    return is_graded() && numerR_->PolyRing::is_homogeneous(f);
  }

  virtual void degree(const ring_elem f, int *d) const
  {
    numerR_->PolyRing::degree(f, d);
  }

  virtual bool multi_degree(const ring_elem f, int *d) const
  {
    return numerR_->PolyRing::multi_degree(f, d);
  }

  virtual void degree_weights(const ring_elem f,
                              M2_arrayint wts,
                              int &lo,
                              int &hi) const
  {
    return numerR_->PolyRing::degree_weights(f, wts, lo, hi);
  }

  virtual ring_elem homogenize(const ring_elem f,
                               int v,
                               int deg,
                               M2_arrayint wts) const
  {
    ring_elem result = numerR_->PolyRing::homogenize(f, v, deg, wts);
    normal_form(result);
    return result;
  }

  virtual ring_elem homogenize(const ring_elem f, int v, M2_arrayint wts) const
  {
    ring_elem result = numerR_->PolyRing::homogenize(f, v, wts);
    normal_form(result);
    return result;
  }

  virtual ring_elem mult_by_term(const ring_elem f,
                                 const ring_elem c,
                                 const int *m) const
  {
    ring_elem result = numerR_->mult_by_term(f, c, m);
    normal_form(result);
    return result;
  }

  virtual int n_flat_terms(const ring_elem f) const
  {
    return numerR_->PolyRing::n_flat_terms(f);
  }

  virtual int n_logical_terms(int nvars0, const ring_elem f) const
  {
    return numerR_->PolyRing::n_logical_terms(nvars0, f);
  }

  virtual engine_RawArrayPairOrNull list_form(const Ring *coeffR,
                                              const ring_elem f) const
  {
    return numerR_->PolyRing::list_form(coeffR, f);
  }

  virtual ring_elem *get_parts(const M2_arrayint wts,
                               const ring_elem f,
                               long &result_len) const
  {
    return numerR_->PolyRing::get_parts(wts, f, result_len);
  }

  virtual ring_elem get_part(const M2_arrayint wts,
                             const ring_elem f,
                             bool lobound_given,
                             bool hibound_given,
                             long lobound,
                             long hibound) const
  {
    return numerR_->PolyRing::get_part(
        wts, f, lobound_given, hibound_given, lobound, hibound);
  }

  virtual ring_elem make_flat_term(const ring_elem a, const int *m) const
  {
    ring_elem result = numerR_->PolyRing::make_flat_term(a, m);
    normal_form(result);
    return result;
  }

  virtual ring_elem make_logical_term(const Ring *coeffR,
                                      const ring_elem a,
                                      const int *exp) const
  {
    ring_elem result = numerR_->PolyRing::make_logical_term(coeffR, a, exp);
    normal_form(result);
    return result;
  }
  //  virtual ring_elem term(const ring_elem a, const int *m) const = 0;

  virtual ring_elem lead_flat_coeff(const ring_elem f) const
  {
    return numerR_->PolyRing::lead_flat_coeff(f);
  }

  virtual ring_elem lead_logical_coeff(const Ring *coeffR,
                                       const ring_elem f) const
  {
    return numerR_->PolyRing::lead_logical_coeff(coeffR, f);
  }

  virtual ring_elem get_coeff(const Ring *coeffR,
                              const ring_elem f,
                              const int *vp) const
  {
    return numerR_->PolyRing::get_coeff(coeffR, f, vp);
  }
  // vp is a varpower monomial, in the logical monoid.
  // The result will be an element in the logical coefficient ring.

  virtual ring_elem get_terms(int nvars0,
                              const ring_elem f,
                              int lo,
                              int hi) const
  {
    return numerR_->PolyRing::get_terms(nvars0, f, lo, hi);
  }
  // get the (logical) terms from lo to hi in f.  A negative value means count
  // from
  // the end.  get_terms(--,f,0,0) is the logical lead term of f.

  virtual const int *lead_flat_monomial(const ring_elem f) const
  {
    return numerR_->PolyRing::lead_flat_monomial(f);
  }

  virtual void lead_logical_exponents(int nvars0,
                                      const ring_elem f,
                                      int *result_exp) const
  {
    numerR_->PolyRing::lead_logical_exponents(nvars0, f, result_exp);
  }

  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const
  {
    numerR_->PolyRing::mult_coeff_to(a, f);
    normal_form(f);
  }
  virtual void divide_coeff_to(ring_elem &f, ring_elem a) const
  {
    numerR_->PolyRing::divide_coeff_to(f, a);
    // I dont believe that a normal form is required here (MES)
  }

  virtual void monomial_divisor(const ring_elem a, int *exp) const
  {
    return numerR_->PolyRing::monomial_divisor(a, exp);
  }

  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const
  {
#ifdef DEVELOPMENT
#warning "diff for quotient rings: should do what?"
#endif
    return numerR_->PolyRing::diff(a, b, use_coeff);
  }

  virtual bool in_subring(int nslots, const ring_elem a) const
  {
    return numerR_->PolyRing::in_subring(nslots, a);
  }

  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const
  {
    return numerR_->PolyRing::degree_of_var(n, a, lo, hi);
  }

  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const
  {
    return numerR_->PolyRing::divide_by_var(n, d, a);
  }

  virtual ring_elem divide_by_expvector(const int *exp, const ring_elem a) const
  {
    return numerR_->PolyRing::divide_by_expvector(exp, a);
  }

  const vecterm *vec_locate_lead_term(const FreeModule *F, vec v) const
  {
    // Returns a pointer to the lead vector of v.
    // This works if F has a Schreyer order, or an up/down order.
    return numerR_->PolyRing::vec_locate_lead_term(F, v);
  }

  virtual vec vec_lead_term(int nparts, const FreeModule *F, vec v) const
  {
    return numerR_->PolyRing::vec_lead_term(nparts, F, v);
  }

  virtual vec vec_top_coefficient(const vec v, int &x, int &e) const
  {
    return numerR_->vec_top_coefficient(v, x, e);
  }

  virtual gbvector *translate_gbvector_from_ringelem(ring_elem coeff) const
  {
    return numerR_->PolyRing::translate_gbvector_from_ringelem(coeff);
  }

  // result/denom == v.
  // result_denom will be an element in getDenominatorRing() (if non-NULL).
  virtual gbvector *translate_gbvector_from_vec(
      const FreeModule *F,
      const vec v,
      ring_elem &result_denominator) const
  {
    return numerR_->PolyRing::translate_gbvector_from_vec(
        F, v, result_denominator);
  }

  virtual vec translate_gbvector_to_vec(const FreeModule *F,
                                        const gbvector *v) const
  {
    return numerR_->PolyRing::translate_gbvector_to_vec(F, v);
  }

  // Translate v/denom to a vector in F.  denom does not need to be positive,
  // although it had better be non-zero.
  // denom should be an element of getDenominatorRing() (if non-NULL, otherwise
  // 'denom'
  // is ignored).
  virtual vec translate_gbvector_to_vec_denom(const FreeModule *F,
                                              const gbvector *v,
                                              const ring_elem denom) const
  {
    return numerR_->PolyRing::translate_gbvector_to_vec_denom(F, v, denom);
  }
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
