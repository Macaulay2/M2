#ifndef __gbring_hpp_
#define __gbring_hpp_

// Problems to solve:
//  a. F,Fsyz
//  b. hide Schreyer order completely
//  c. Make sure enough is here to do gbZZ
//  d. ditto for GB.c
//  e. heap: needs to be able to handle F,Fsyz both
//     and needs to be able to handle multiplication by a constant
//
// Schreyer order: probably don't keep polynomials in this form?
//  or in any case we should be able to change the representation easily
// In any case, it seems like there could have been bugs before...
//
// Implementations of GBRing:
//   Schreyer encoded, Schreyer order, no Schreyer
//   KK or ZZ
//   [polyring,skew,weyl,solvable]
//   quotient ideal

#include <M2/math-include.h>
#include "engine-includes.hpp"

#include <iostream>
#include <string>

#include "buffer.hpp"
#include "monoid.hpp"
#include "newdelete.hpp"
#include "ringelem.hpp"
#include "skew.hpp"
#include "style.hpp"

class CoefficientRingZZp;
class FreeModule;
class Ring;
class SolvableAlgebra;
class WeylAlgebra;
class gbvectorHeap;
class stash;

struct gbvector
{
  gbvector *next;
  ring_elem coeff;
  int comp;
  int monom[1];
};

struct POLY
{
  gbvector *f;
  gbvector *fsyz;
};

typedef int *monomial;

class GBRing : public our_new_delete
{
  friend class GBKernelComputation;
  friend class WeylAlgebra;
  friend class SkewPolynomialRing;

  // The FreeModule is used for the following:
  //  (a) degree of an element
  //  (b) monomial order comparing monomials with different lead components
  //  (c) Schreyer monomials, if any.
  // Using the original free module doesn't quite work for Schreyer orders,
  // unless we 'flatten' the Schreyer monomials, AND if we have a flag
  // for the monomial order...
  // If not Schreyer order, then
  //  i. WHEN order is considered
  // ii. is it UP or DOWN
 protected:
  bool _schreyer_encoded;
  const Monoid *M;  // flattened monoid
  const Ring *K;    // flattened coefficients

  bool _coeffs_ZZ;          // is K == globalZZ?
  CoefficientRingZZp *zzp;  // Only set to non-null if coeff ring is ZZ/p

  size_t gbvector_size;
  stash *mem;

  int _nvars;

  bool _up_order;  // is the free module order up or down?

  bool _is_skew;
  SkewMultiplication _skew;
  int *const
      *_skew_monoms;  // array 0.._skew.n_skew_vars()-1 of elements of monoid

  // Weyl algebra information
  // Private data goes into the subclass
  bool is_weyl;  // true if this is either a Weyl or homog Weyl algebra
  const WeylAlgebra *weyl;

  bool is_solvable;
  const SolvableAlgebra *solvable;

 protected:
  ring_elem _one;

  //////////////////////////
  // Pre-allocated values //
  //////////////////////////
  size_t exp_size;    // byte size of exponent vectors
  size_t monom_size;  // and monomials

  //////////////////////////////
  // Private support routines //
  //////////////////////////////
  gbvector *new_raw_term();
  void gbvector_remove_term(gbvector *f);
  gbvector *gbvector_copy_term(const gbvector *t);
  void divide_exponents(const int *exp1, const int *exp2, int *result) const;
  // result = exp1 - exp2;  No error checking is done.

  void exponent_syzygy(const int *exp1, const int *exp2, int *exp3, int *exp4);

  virtual gbvector *mult_by_term1(const FreeModule *F,
                                  const gbvector *f,
                                  ring_elem u,
                                  const int *monom,
                                  int comp) = 0;

  gbvector *mult_by_term(const FreeModule *F,
                         const gbvector *f,
                         ring_elem u,
                         const int *monom,
                         int comp);

  int skew_mult_sign(int *exp1, int *exp2) const;
  // returns -1 if exp1 * exp2 = - sort(exp1,exp2).
  // returns 0 if exp1, exp2 are not disjoint for skew comm variables
  // returns 1 if exp1 * exp2 = sort(exp1,exp2).

  void divide_coeff_exact_to_ZZ(gbvector *f, gmp_ZZ u) const;

  void lower_content_ZZ(gbvector *f, mpz_ptr content) const;

  void gbvector_remove_content_ZZ(gbvector *f,
                                  gbvector *fsyz,
                                  bool use_denom,
                                  ring_elem &denom) const;

  const gbvector *find_coeff(const FreeModule *F,
                             const gbvector *f,
                             const gbvector *g) const;

  GBRing(const Ring *K0, const Monoid *M0);

 public:
  // Each of these handles quotients as well
  static GBRing *create_PolynomialRing(const Ring *K, const Monoid *M);
  static GBRing *create_SkewPolynomialRing(const Ring *K0,
                                           const Monoid *M0,
                                           SkewMultiplication skew0);
  static GBRing *create_WeylAlgebra(const Ring *K0,
                                    const Monoid *M0,
                                    const WeylAlgebra *W0);
  static GBRing *create_SolvableAlgebra(const Ring *K0,
                                        const Monoid *M0,
                                        const SolvableAlgebra *R);

  virtual ~GBRing();

  const Monoid *get_flattened_monoid() const { return M; }
  const Ring *get_flattened_coefficients() const { return K; }
  int n_vars() const { return _nvars; }
  void memstats();

  //////////////////////
  // Ring information //
  //////////////////////

  // skew commutativity
  bool is_skew_commutative() const { return _skew.n_skew_vars() > 0; }
  int n_skew_commutative_vars() const { return _skew.n_skew_vars(); }
  int skew_variable(int i) const { return _skew.skew_variable(i); }
  const int *skew_monomial_var(int i) const { return _skew_monoms[i]; }
  // Weyl algebra
  bool is_weyl_algebra() const { return is_weyl; }
  // returns true if this is a Weyl algebra OR a homog Weyl algebra

  // Schreyer order information
  bool is_schreyer_encoded() const { return _schreyer_encoded; }
  //////////////////////
  // exponents support //
  //////////////////////

  exponents exponents_make();

  void exponents_delete(exponents e);

  size_t exponent_byte_size() const { return exp_size; }
  // use ALLOCATE_EXPONENTS(R->exponent_byte_size())
  // to allocate on the stack an uninitialized exponent vector (#ints = nvars+2)
  // it will be deallocated at the end of that function

  //////////////////////
  // gbvector support //
  //////////////////////

  const ring_elem one() { return _one; }  // the element '1' in the base K.
  void gbvector_remove(gbvector *f);

  gbvector *gbvector_raw_term(ring_elem coeff, const int *monom, int comp);
  // Returns coeff*monom*e_sub_i in a free module.  If the order is a Schreyer
  // order, the 'monom' should already be encoded.

  gbvector *gbvector_term(const FreeModule *F, ring_elem coeff, int comp);
  // Returns coeff*e_sub_i in F, the monomial is set to 1.
  // If comp==0, F is never considered (so it can be NULL)

  gbvector *gbvector_term(const FreeModule *F,
                          ring_elem coeff,
                          const int *monom,
                          int comp);
  // Returns coeff*mon*e_comp in F.  If comp==0, F is never considered (so it
  // can be NULL)

  gbvector *gbvector_term_exponents(const FreeModule *F,
                                    ring_elem coeff,
                                    const int *exp,
                                    int comp);
  // Returns coeff*exp*e_sub_i in F, where exp is an exponent vector.
  // If comp==0, F is never considered (so it can be NULL)

  gbvector *gbvector_zero() const { return 0; }
  void gbvector_sort(const FreeModule *F,
                     gbvector *&f);  // TO BE USED CAREFULLY: gbvector's should
  // mostly be kept in monomial order.  This is here when the construction
  // doesn't satisfy this property.

  bool gbvector_is_zero(const gbvector *f) const { return f == 0; }
  bool gbvector_is_equal(const gbvector *f, const gbvector *g) const;
  // f,g can be both be in F, or both in Fsyz

  int gbvector_n_terms(const gbvector *f) const;

#if 0
//   // Degrees, using the weight vector _degrees.
//   int exponents_weight(const int *e) const;
//
//   int gbvector_term_weight(const FreeModule *F,
//                         const gbvector *f);
//
//   void gbvector_weight(const FreeModule *F, const gbvector *f,
//                     int &result_lead,
//                     int &result_lo,
//                     int &result_hi);
//
//   int gbvector_degree(const FreeModule *F,
//                    const gbvector *f);
#endif

  void gbvector_multidegree(const FreeModule *F,
                            const gbvector *f,
                            int *&result_degree);
  // Places the multidegree of the first term of the non-zero poly f into
  // result_degree.

  int gbvector_compare(const FreeModule *F,
                       const gbvector *f,
                       const gbvector *g) const;

  gbvector *gbvector_lead_term(int n, const FreeModule *F, const gbvector *f);

  gbvector *gbvector_parallel_lead_terms(M2_arrayint w,
                                         const FreeModule *F,
                                         const gbvector *leadv,
                                         const gbvector *v);

  void gbvector_get_lead_monomial(const FreeModule *F,
                                  const gbvector *f,
                                  int *result);
  // This copies the monomial to result.  If a Schreyer order,
  // the result will NOT be the total monomial.

  void gbvector_get_lead_exponents(const FreeModule *F,
                                   const gbvector *f,
                                   int *result);
  // result[0]..result[nvars-1] are set

  int gbvector_lead_component(const gbvector *f) { return f->comp; }
  void gbvector_mult_by_coeff_to(gbvector *f, ring_elem u);
  // We assume that u is non-zero, and that for each coeff c of f, u*c is
  // non-zero

  gbvector *gbvector_mult_by_coeff(const gbvector *f, ring_elem u);
  // We assume that u is non-zero, and that for each coeff c of f, u*c is
  // non-zero

  void gbvector_add_to_zzp(const FreeModule *F, gbvector *&f, gbvector *&g);

  void gbvector_add_to(const FreeModule *F, gbvector *&f, gbvector *&g);

  void gbvector_negate_to(gbvector *f) const;

  gbvector *gbvector_copy(const gbvector *f);

  ////////////////
  // Arithmetic //
  ////////////////
  void find_reduction_coeffs(const FreeModule *F,
                             const gbvector *f,
                             const gbvector *g,
                             ring_elem &u,
                             ring_elem &v);
  bool find_reduction_coeffs_ZZ(const FreeModule *F,
                                const gbvector *f,
                                const gbvector *g,
                                ring_elem &v);
  void find_reduction_monomial(const FreeModule *F,
                               const gbvector *f,
                               const gbvector *g,
                               int &comp,
                               int *&monom);  // there must be enough space here

  void gbvector_mult_by_term(const FreeModule *F,
                             const FreeModule *Fsyz,
                             ring_elem a,
                             const int *m,  // element of M, a monomial
                             const gbvector *f,
                             const gbvector *fsyz,
                             gbvector *&result,
                             gbvector *&esult_syz);
  // Optionally, this reduces wrt to the defining ideal:
  //  result_syz (possibly multiplying result by a constant)
  // or bith result,result_syz.
  // If over a quotient ring, this might reduce result_syz wrt
  //  to the quotient ideal.  This might multiply result by a scalar.

  void gbvector_reduce_lead_term(const FreeModule *F,
                                 const FreeModule *Fsyz,
                                 gbvector *flead,
                                 gbvector *&f,
                                 gbvector *&fsyz,
                                 const gbvector *g,
                                 const gbvector *gsyz,
                                 bool use_denom,
                                 ring_elem &denom);
  // Reduce f wrt g, where leadmonom(g) divides leadmonom(f)
  // If u leadmonom(f) = v x^A leadmonom(g) (as monomials, ignoring lower
  // terms),
  // then: flead := u * flead
  //       f := u*f - v*x^A*g
  //       fsyz := u*fsyz - v*x^A*gsyz
  // If use_denom is true, then
  // denom is set to u*denom.

  void gbvector_reduce_lead_term(const FreeModule *F,
                                 const FreeModule *Fsyz,
                                 gbvector *flead,
                                 gbvector *&f,
                                 gbvector *&fsyz,
                                 const gbvector *g,
                                 const gbvector *gsyz);
  // Same as calling gbvector_reduce_lead_term with use_denom=false.

  void gbvector_reduce_with_marked_lead_term(const FreeModule *F,
                                             const FreeModule *Fsyz,
                                             gbvector *flead,
                                             gbvector *&f,
                                             gbvector *&fsyz,
                                             const gbvector *ginitial,
                                             const gbvector *g,
                                             const gbvector *gsyz,
                                             bool use_denom,
                                             ring_elem &denom);

  bool gbvector_reduce_lead_term_ZZ(const FreeModule *F,
                                    const FreeModule *Fsyz,
                                    gbvector *&f,
                                    gbvector *&fsyz,
                                    const gbvector *g,
                                    const gbvector *gsyz);
  // Never multiplies f by anything.  IE before(f), after(f) are equiv. mod g.
  // this should ONLY be used if K is globalZZ.
  // Sets f := f - v*m*g, where the resulting lead coeff of in(before(f)) is
  // either 0
  // or is the balanced remainder of leadcoeff(f) by leadcoeff(g).
  // Returns true iff this remainder is 0.

  void gbvector_cancel_lead_terms(const FreeModule *F,
                                  const FreeModule *Fsyz,
                                  const gbvector *f,
                                  const gbvector *fsyz,
                                  const gbvector *g,
                                  const gbvector *gsyz,
                                  gbvector *&result,
                                  gbvector *&result_syz);

  void gbvector_replace_2by2_ZZ(const FreeModule *F,
                                const FreeModule *Fsyz,
                                gbvector *&f,
                                gbvector *&fsyz,
                                gbvector *&g,
                                gbvector *&gsyz);

  void gbvector_combine_lead_terms_ZZ(const FreeModule *F,
                                      const FreeModule *Fsyz,
                                      const gbvector *f,
                                      const gbvector *fsyz,
                                      const gbvector *g,
                                      const gbvector *gsyz,
                                      gbvector *&result,
                                      gbvector *&result_syz);
  // If u*x^A*leadmonom(f) + v*x^B*leadmonom(g) = gcd(u,v)*monom (mod lower
  // terms),
  // set result := u*x^A*f + v*x^B*g
  //     resultsyz := u*x^A*fsyz + v*x^B*gyz
  // To keep in mind:
  //  (a) Schreyer orders
  //  (b) Quotient ideal
  // Currently: this does nothing with the quotient ring

  void reduce_lead_term_heap(
      const FreeModule *F,
      const FreeModule *Fsyz,
      const gbvector *fcurrent_lead,
      const int *exponents,  // exponents of fcurrent_lead
      gbvector *flead,
      gbvectorHeap &f,
      gbvectorHeap &fsyz,
      const gbvector *g,
      const gbvector *gsyz);

  void reduce_marked_lead_term_heap(
      const FreeModule *F,
      const FreeModule *Fsyz,
      const gbvector *fcurrent_lead,
      const int *exponents,  // exponents of fcurrent_lead
      gbvector *flead,
      gbvectorHeap &f,
      gbvectorHeap &fsyz,
      const gbvector *marked_in_g,
      const gbvector *g,
      const gbvector *gsyz);

  void gbvector_remove_content(gbvector *f,
                               gbvector *fsyz,
                               bool use_denom,
                               ring_elem &denom);

  // if c = content(f,fsyz), then
  //  f = f//c
  //  fsyz = fsyz//c
  //  denom = denom*c
  // CAUTION: denom needs to be a valid element of the
  //          coefficient ring.
  // If coeff ring is not ZZ, but is a field, c is chosen so that
  // f is monic (if not 0, else fsyz will be monic).

  void gbvector_remove_content(gbvector *f, gbvector *fsyz);
  // Same as calling gbvector_remove_content with use_denom=false.

  void gbvector_auto_reduce(const FreeModule *F,
                            const FreeModule *Fsyz,
                            gbvector *&f,
                            gbvector *&fsyz,
                            const gbvector *g,
                            const gbvector *gsyz);

  void gbvector_auto_reduce_ZZ(const FreeModule *F,
                               const FreeModule *Fsyz,
                               gbvector *&f,
                               gbvector *&fsyz,
                               const gbvector *g,
                               const gbvector *gsyz);
  // If g = a*x^A*ei + lower terms
  // and if f = ... + b*x^A*ei + ...
  // and if v*a + b is the balanced remainder of b by a
  // then set f := f + v*g, fsyz := fsyz + v*gsyz
  // No content is removed.

  void gbvector_text_out(buffer &o,
                         const FreeModule *F,
                         const gbvector *f,
                         int nterms = -1) const;

  void gbvector_apply(const FreeModule *F,
                      const FreeModule *Fsyz,
                      gbvector *&f,
                      gbvector *&fsyz,
                      const gbvector *gsyz,
                      const gbvector **elems,
                      const gbvector **elems_syz,
                      const gbvector **quotients);
  // gsyz is allowed to have negative elements.  These refer to
  // quotient ring elements.  In this case, the component that
  // is used is the lead component of f. (i.e. this is designed for
  // cancelling lead terms).
  // [combines: freemod::apply_quotient_ring_elements,
  // GBZZ_comp::apply_gb_elements]
};

class GBRingPoly : public GBRing
{
 protected:
  friend class GBRing;
  GBRingPoly(const Ring *K0, const Monoid *M0) : GBRing(K0, M0) {}
 public:
  virtual gbvector *mult_by_term1(const FreeModule *F,
                                  const gbvector *f,
                                  ring_elem u,
                                  const int *monom,
                                  int comp);
  virtual ~GBRingPoly();
};

class GBRingWeyl : public GBRing
{
 protected:
  friend class GBRing;
  GBRingWeyl(const Ring *K0, const Monoid *M0, const WeylAlgebra *R0);

 public:
  virtual gbvector *mult_by_term1(const FreeModule *F,
                                  const gbvector *f,
                                  ring_elem u,
                                  const int *monom,
                                  int comp);
  virtual ~GBRingWeyl();
};

class GBRingWeylZZ : public GBRingWeyl
{
 protected:
  friend class GBRing;
  GBRingWeylZZ(const Ring *K0, const Monoid *M0, const WeylAlgebra *R0);

 public:
  virtual gbvector *mult_by_term1(const FreeModule *F,
                                  const gbvector *f,
                                  ring_elem u,
                                  const int *monom,
                                  int comp);
  virtual ~GBRingWeylZZ();
};

class GBRingSkew : public GBRing
{
 protected:
  friend class GBRing;
  GBRingSkew(const Ring *K0, const Monoid *M0, SkewMultiplication skew0);

 public:
  virtual gbvector *mult_by_term1(const FreeModule *F,
                                  const gbvector *f,
                                  ring_elem u,
                                  const int *monom,
                                  int comp);
  virtual ~GBRingSkew();
};

class GBRingSolvable : public GBRing
{
 protected:
  friend class GBRing;
  GBRingSolvable(const Ring *K0, const Monoid *M0, const SolvableAlgebra *R0);

 public:
  virtual gbvector *mult_by_term1(const FreeModule *F,
                                  const gbvector *f,
                                  ring_elem u,
                                  const int *monom,
                                  int comp);
  virtual ~GBRingSolvable();
};

///////////////////
// Heap routines //
///////////////////
class gbvectorHeap
{
  GBRing *GR;
  const FreeModule *F;
  const Ring *K;  // The coefficient ring
  gbvector *heap[GEOHEAP_SIZE];
  ring_elem heap_coeff[GEOHEAP_SIZE];
  int top_of_heap;
  int mLead;  // set after a call to get_lead_term.
              // set negative after each call to add,
              // or remove_lead_term
 public:
  gbvectorHeap(GBRing *GR, const FreeModule *F);
  ~gbvectorHeap();

  GBRing *get_gb_ring() { return GR; }
  const FreeModule *get_freemodule() { return F; }
  void add(gbvector *p);
  void mult_by_coeff(ring_elem a);

  const gbvector *get_lead_term();  // Returns NULL if none.
  gbvector *remove_lead_term();     // Returns NULL if none.

  gbvector *value();
  // Returns the linearized value, and resets the gbvectorHeap.

  gbvector *debug_list(int i) { return heap[i]; }
  // DO NOT USE, except for debugging purposes!

  gbvector *current_value() const;
  // Adds up all the elements and returns this value
  // Mainly used for debugging.

  void show() const;
  // Displays the current values at each part of the heap
  // to stdout
};

template <typename container, typename fcn>
// void displayElements(GBRing* R, const FreeModule* F, iter a, iter b, fcn f)
void displayElements(std::string header, GBRing *R, container a, fcn f)
{
  std::cout << header << std::endl;
  long count = 0;
  //  for (auto c = a; c != b; ++c)
  for (auto c : a)
    {
      buffer o;
      const gbvector *g = f(c);
      o << "[" << count << "] = ";
      R->gbvector_text_out(o, nullptr, g, 3);
      o << newline;
      std::cout << o.str();
      ++count;
    }
}

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
