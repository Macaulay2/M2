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

#include "ring.hpp"
#include "skew.hpp"
#include "ntuple.hpp"

struct gbvector {
  gbvector * next;
  ring_elem coeff;
  int comp;
  int monom[1];
};

class TermIdeal;
typedef int * monomial;

class gbvectorHeap;

class GBRing
{
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
  const PolynomialRing *originalR;
  const PolynomialRing *R; // flattended poly ring: is this a quotient ring?
  const Monoid *M; // flattened monoid
  const Ring *K; // flattened coefficients
  const GRType *_GT;
  bool _coeffs_ZZ; 

  int gbvector_size;

  int _nvars;
  int *_degrees;

  bool _up_order; // is the free module order up or down?

  int _nquotients;
  gbvector **_quotients;
  MonomialIdeal *_Rideal;
  TermIdeal *_RidealZZ;

  SkewMultiplication _skew;

  // Weyl algebra information
  // Private data goes into the subclass
  bool _is_weyl_algebra; // true if this is either a Weyl or homog Weyl algebra
  const WeylAlgebra * _W;

  virtual ~GBRing();
protected:
  ring_elem _one;

  //////////////////////////
  // Pre-allocated values //
  //////////////////////////
  int * _EXP1, *_EXP2, *_EXP3, *_EXP4;
  int * _SKEW1, *_SKEW2;
  int * _MONOM1, *_MONOM2;


  //////////////////////////////
  // Private support routines //
  //////////////////////////////
  gbvector * new_raw_term();
  void gbvector_remove_term(gbvector *f);
  gbvector * gbvector_copy_term(const gbvector *t);
  void divide_exponents(const int *exp1,
			const int *exp2,
			int *result) const;
  // result = exp1 - exp2;  No error checking is done.


  void exponent_syzygy(const int *exp1,
		       const int *exp2,
		       int *exp3,
		       int *exp4);

  virtual gbvector *mult_by_term(const FreeModule *F,
				 const gbvector *f,
				 ring_elem u,
				 const int *monom,
				 int comp) = 0;
  
  int skew_mult_sign(int *exp1, int *exp2) const;
  // returns -1 if exp1 * exp2 = - sort(exp1,exp2).
  // returns 0 if exp1, exp2 are not disjoint for skew comm variables
  // returns 1 if exp1 * exp2 = sort(exp1,exp2).

  void divide_coeff_exact_to_ZZ(gbvector * f, M2_Integer u) const;

  void lower_content_ZZ(gbvector *f, M2_Integer content) const;

  void gbvector_remove_content_ZZ(gbvector *f, 
				  gbvector *fsyz,
				  mpz_t denom) const;

  const gbvector *find_coeff(const FreeModule *F,
			     const gbvector *f, const gbvector *g) const;

  GBRing(const PolynomialRing *origR);

  void initialize_quotients();

public:
  // Each of these handles quotients as well
  static GBRing * create_PolynomialRing(const PolynomialRing *R);
  static GBRing * create_SkewPolynomialRing(const SkewPolynomialRing *R);
  static GBRing * create_WeylAlgebra(const WeylAlgebra *R);
  static GBRing * create_SolvableAlgebra(const SolvableAlgebra *R);
  

  const PolynomialRing * get_flattened_ring() const { return R; }
  const Monoid * get_flattened_monoid() const { return M; }
  const Ring * get_flattened_coefficients() const { return K; }
  int n_vars() const { return _nvars; }

  //////////////////////
  // Ring information //
  //////////////////////

  // array of quotient elements (all component 0).
  bool is_quotient_ring() const { return _nquotients > 0; }
  MonomialIdeal * get_quotient_monomials() const { return _Rideal; }
  TermIdeal * get_quotient_monomials_ZZ() const { return _RidealZZ; }
  int n_quotients() const { return _nquotients; }
  const gbvector * quotient_element(int i) const { return _quotients[i]; }

  // skew commutativity
  bool is_skew_commutative() const { return _skew.n_skew_vars() > 0; }
  int n_skew_commutative_vars() const { return _skew.n_skew_vars(); }
  int skew_variable(int i) const { return _skew.skew_variable(i); }
  gbvector * skew_poly(int i) const; // TODO

  // Weyl algebra
  bool is_weyl_algebra() const { return _is_weyl_algebra; }
  // returns true if this is a Weyl algebra OR a homog Weyl algebra
  
  /////////////////
  // Translation //
  /////////////////
  gbvector * gbvector_from_vec(const FreeModule *F, 
			       const vec v, 
			       ring_elem &result_denominator);

  vec gbvector_to_vec(const FreeModule *F, const gbvector *v) const;

  vec gbvector_to_vec_denom(const FreeModule *F, 
			    const gbvector *v,
			    const ring_elem denom) const;
  // Translate v/denom to a vector in F.  denom does not need to be positive,
  // although it had better be non-zero.


  //////////////////////
  // exponents support //
  //////////////////////

  exponents exponents_make();

  void exponents_delete(exponents e);

  //////////////////////
  // gbvector support //
  //////////////////////

  const ring_elem one() { return _one; }  // the element '1' in the base K.

  void gbvector_remove(gbvector *f);

  gbvector * gbvector_term(const FreeModule *F, ring_elem coeff, int comp);
  // Returns coeff*e_sub_i in F, the monomial is set to 1.

  gbvector * gbvector_zero() const { return 0; }

  bool gbvector_is_zero(const gbvector *f) const { return f == 0; }

  bool gbvector_is_equal(const gbvector *f,
			 const gbvector *g) const;
  // f,g can be both be in F, or both in Fsyz

  int gbvector_n_terms(const gbvector *f) const;

  // Degrees, using the weight vector _degrees.
  int exponents_weight(const int *e) const;

  int gbvector_term_weight(const FreeModule *F, 
			   const gbvector *f);

  void gbvector_weight(const FreeModule *F, const gbvector *f,
		       int &result_lead,
		       int &result_lo,
		       int &result_hi);

  int gbvector_degree(const FreeModule *F, 
		      const gbvector *f);

  int gbvector_compare(const FreeModule *F,
		       const gbvector *f,
		       const gbvector *g) const;


  gbvector * gbvector_lead_term(int n, 
				const FreeModule *F, 
				const gbvector *f);

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
  // We assume that u is non-zero, and that for each coeff c of f, u*c is non-zero

  gbvector *gbvector_mult_by_coeff(const gbvector * f, ring_elem u);
  // We assume that u is non-zero, and that for each coeff c of f, u*c is non-zero

  void gbvector_add_to(const FreeModule *F,
		       gbvector * &f, gbvector * &g);

  gbvector * gbvector_copy(const gbvector *f);


  ////////////////
  // Arithmetic //
  ////////////////
  void gbvector_mult_by_term(const FreeModule *F,
			     const FreeModule *Fsyz,
			     ring_elem a, 
			     const int *m, // element of M, a monomial
			     const gbvector *f,
			     const gbvector *fsyz,
			     gbvector * &result,
			     gbvector * &esult_syz);
  // Optionally, this reduces wrt to the defining ideal:
  //  result_syz (possibly multiplying result by a constant)
  // or bith result,result_syz.
  // If over a quotient ring, this might reduce result_syz wrt 
  //  to the quotient ideal.  This might multiply result by a scalar.
  
  void gbvector_reduce_lead_term(const FreeModule *F,
				 const FreeModule *Fsyz,
				 gbvector * flead,
				 gbvector * &f,
				 gbvector * &fsyz,
				 const gbvector *g,
				 const gbvector *gsyz);
  // Reduce f wrt g, where leadmonom(g) divides leadmonom(f)
  // If u leadmonom(f) = v x^A leadmonom(g) (as monomials, ignoring lower terms),
  // then: flead := u * flead
  //       f := u*f - v*x^A*g
  //       fsyz := u*fsyz - v*x^A*gsyz

  void gbvector_reduce_lead_term_coeff(const FreeModule *F,
				       const FreeModule *Fsyz,
				       gbvector * flead,
				       gbvector * &f,
				       gbvector * &fsyz,
				       const gbvector *g,
				       const gbvector *gsyz,
				       ring_elem &denom);
  // Same as gbvector_reduce_lead_term, except that 
  // denom is set to u*denom.


  void gbvector_cancel_lead_terms(
				  const FreeModule *F,
				  const FreeModule *Fsyz,
				  const gbvector *f,
				  const gbvector *fsyz,
				  const gbvector *g,
				  const gbvector *gsyz,
				  gbvector *&result,
				  gbvector *&result_syz);
  
  void reduce_lead_term_heap(const FreeModule *F,
			     const FreeModule *Fsyz,
			     const gbvector *fcurrent_lead,
			     const int *exponents,// exponents of fcurrent_lead
			     gbvector * flead,
			     gbvectorHeap &f,
			     gbvectorHeap &fsyz,
			     const gbvector *g,
			     const gbvector *gsyz);
  
  void gbvector_remove_content(gbvector *f, 
			       gbvector *fsyz,
			       ring_elem &denom);
  // if c = content(f,fsyz), then 
  //  f = f//c
  //  fsyz = fsyz//c
  //  denom = denom*c
  // CAUTION: denom needs to be a valid element of the 
  //          coefficient ring.
  // If coeff ring is not ZZ, but is a field, c is chosen so that
  // f is monic (if not 0, else fsyz will be monic).


  void gbvector_remove_content(gbvector *f, 
			       gbvector *fsyz);
  // let c = gcd(content(f),content(fsyz)).
  // set f := f/c,  fsyz := fsyz/c.

  void gbvector_auto_reduce(const FreeModule *F,
			    const FreeModule *Fsyz,
			    gbvector * &f, 
			    gbvector * &fsyz,
			    const gbvector *g, 
			    const gbvector *gsyz);

  void gbvector_text_out(buffer &o,
			 const FreeModule *F,
			 const gbvector *f) const;

  void gbvector_apply(const FreeModule *F,
		      const FreeModule *Fsyz,
		      gbvector * & f, gbvector * & fsyz,
		      const gbvector * gsyz,
		      const gbvector **elems,
		      const gbvector **elems_syz);
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
  GBRingPoly(const PolynomialRing *P);
public:
  virtual gbvector *mult_by_term(const FreeModule *F,
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
  GBRingWeyl(const WeylAlgebra *W);
public:
  virtual gbvector *mult_by_term(const FreeModule *F,
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
  GBRingWeylZZ(const WeylAlgebra *W);
public:
  virtual gbvector *mult_by_term(const FreeModule *F,
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
  GBRingSkew(const SkewPolynomialRing *P);
public:
  virtual gbvector *mult_by_term(const FreeModule *F,
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
  GBRingSolvable(const SolvableAlgebra *P);
public:
  virtual gbvector *mult_by_term(const FreeModule *F,
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
  const Ring *K;		// The coefficient ring
  gbvector * heap[GEOHEAP_SIZE];
  ring_elem heap_coeff[GEOHEAP_SIZE];
  int top_of_heap;
  int mLead;			// set after a call to get_lead_term.
				// set negative after each call to add, 
				// or remove_lead_term
public:
  gbvectorHeap(GBRing *GR, const FreeModule *F);
  ~gbvectorHeap();

  GBRing * get_gb_ring() { return GR; }
  const FreeModule * get_freemodule() { return F; }

  void add(gbvector * p);
  void mult_by_coeff(ring_elem a);
  
  const gbvector * get_lead_term(); // Returns NULL if none.
  gbvector * remove_lead_term(); // Returns NULL if none.
  
  gbvector * value();
  // Returns the linearized value, and resets the gbvectorHeap.
  
  gbvector * debug_list(int i) { return heap[i]; } 
  // DO NOT USE, except for debugging purposes!
  
  gbvector * current_value() const; 
  // Adds up all the elements and returns this value
  // Mainly used for debugging.
};

class GRType {
protected:
  typedef enum { BASE, FRAC_QQ, FRAC, POLY } tag;
  const GRType *next_;
  GRType(const GRType *next) : next_(next) {}
public:
  virtual ring_elem to_ringelem(ring_elem coeff, 
				const int *exp) const = 0;
  virtual ring_elem to_ringelem_denom(ring_elem coeff, 
				      ring_elem denom, 
				      int *exp) const = 0;
  virtual void from_ringelem(gbvectorHeap &H, 
			     ring_elem coeff, 
			     int comp, 
			     int *exp,
			     int firstvar) const = 0;

  virtual tag type() const = 0;

  static const GRType *make_BASE(const Ring *R);
  static const GRType *make_FRAC(const FractionField *R);
  static const GRType *make_QQ(const QQ *Q);
  static const GRType *make_POLY(const PolynomialRing *R);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
// End:
