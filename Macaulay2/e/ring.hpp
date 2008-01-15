// Copyright 1995 Michael E. Stillman

#ifndef _ring_hh_
#define _ring_hh_

#include "hash.hpp"
#include "ringelem.hpp"
#include "monoid.hpp"

///// Ring Hierarchy ///////////////////////////////////

class RingZZ;
class QQ;
class RRR;
class CCC;
class Z_mod;
class GF;
class FractionField;
class PolynomialRing;
class PolyRing;
class PolyRingFlat;
class PolyQQ;
class SkewPolynomialRing;
class SchurRing;
class WeylAlgebra;
class SolvableAlgebra;

class FreeModule;
class RingMap;

class gbvectorHeap;
class gbvector;
class buffer;

class Ring : public mutable_object
{
protected:
  int P;
  const PolynomialRing *degree_ring;

  ring_elem _non_unit;
  int _isfield;		// 1: means yes, or declared yes.
                        // 0: means not declared to be so.
                        // -1: means a non unit was found, and that _non_unit contains
                        //    a non-zero non-unit.
                        // If a non unit is found, then _isfield is set to -1.

  ring_elem zeroV;              // Three generally useful values in a ring.
  ring_elem oneV;
  ring_elem minus_oneV;

  void initialize_ring(int charac, 
		       const PolynomialRing *DR = 0);
  Ring() {}
public:
  virtual ~Ring();

  ////////////////////////
  // Ring informational //
  ////////////////////////

  int charac() const { return P; }

  const Monoid * degree_monoid() const;
  const PolynomialRing *get_degree_ring() const { return degree_ring; }

  virtual bool is_basic_ring() const { return true; } // The default is to be a basic ring.
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
  bool declare_field(); // if false is returned, then an ERROR has been set.

  ring_elem get_non_unit() const;
  void set_non_unit(ring_elem zero_div) const; // not really const: sets "_non_unit"

  typedef enum {COEFF_ZZ, COEFF_QQ, COEFF_BASIC} CoefficientType;
  virtual  CoefficientType coefficient_type() const { return COEFF_BASIC; }
  // What the ultimate coefficient type is.  ZZ, QQ, finite fields return these 
  // three values.  Fraction fields return their ultimate value, as do poly rings.

  virtual bool has_associate_divisors() const { return true; }
  // There are only a few rings which do not have such divisors: frac rings
  //   over quotients of poly rings.

  ///////////////////////////////////
  // Casting up the ring hierarchy //
  ///////////////////////////////////
  virtual const RingZZ * cast_to_RingZZ() const         { return 0; }
  virtual       RingZZ * cast_to_RingZZ()               { return 0; }
  virtual const QQ * cast_to_QQ() const         { return 0; }
  virtual       QQ * cast_to_QQ()               { return 0; }
  virtual const Z_mod * cast_to_Z_mod() const         { return 0; }
  virtual       Z_mod * cast_to_Z_mod()               { return 0; }
  virtual const GF * cast_to_GF() const         { return 0; }
  virtual       GF * cast_to_GF()               { return 0; }
  virtual const PolynomialRing * cast_to_PolynomialRing()  const      { return 0; }
  virtual       PolynomialRing * cast_to_PolynomialRing()             { return 0; }

  virtual const PolyRing * cast_to_PolyRing()  const      { return 0; }
  virtual       PolyRing * cast_to_PolyRing()             { return 0; }

  virtual const PolyQQ * cast_to_PolyQQ()  const      { return 0; }
  virtual       PolyQQ * cast_to_PolyQQ()             { return 0; }

  virtual const PolyRingFlat * cast_to_PolyRingFlat()  const { return 0; }
  virtual       PolyRingFlat * cast_to_PolyRingFlat()        { return 0; }

  virtual const FractionField * cast_to_FractionField() const    { return 0; }
  virtual       FractionField * cast_to_FractionField()          { return 0; }
  virtual const SchurRing * cast_to_SchurRing() const { return 0; }
  virtual       SchurRing * cast_to_SchurRing()       { return 0; }
  virtual const SkewPolynomialRing * cast_to_SkewPolynomialRing()  const      { return 0; }
  virtual       SkewPolynomialRing * cast_to_SkewPolynomialRing()             { return 0; }
  virtual const SolvableAlgebra * cast_to_SolvableAlgebra()  const      { return 0; }
  virtual       SolvableAlgebra * cast_to_SolvableAlgebra()             { return 0; }
  virtual const WeylAlgebra *cast_to_WeylAlgebra() const { return 0; }

  virtual RRR * cast_to_RRR() { return 0; }
  virtual const RRR * cast_to_RRR() const { return 0; }
  virtual CCC * cast_to_CCC() { return 0; }
  virtual const CCC * cast_to_CCC() const { return 0; }


  virtual FreeModule *make_FreeModule() const;
  virtual FreeModule *make_Schreyer_FreeModule() const;
  virtual FreeModule *make_FreeModule(int n) const;

  virtual void text_out(buffer &o) const = 0;

  //////////////////////
  // Ring arithmetic ///
  //////////////////////
  virtual int coerce_to_int(ring_elem a) const;

  ring_elem one() const { return oneV; }
  ring_elem minus_one() const { return minus_oneV; }
  ring_elem zero() const { return zeroV; }

  virtual ring_elem from_int(int n) const = 0;
  virtual ring_elem from_int(mpz_ptr n) const = 0;

  virtual ring_elem from_rational(mpq_ptr q) const = 0;  
  // The default version calls from_int(0). Change it?
  virtual bool from_BigReal(M2_RRR a, ring_elem &result) const;  
  // The default version calls from_int(0) and returns false.
  virtual bool from_BigComplex(M2_CCC z, ring_elem &result) const;  
  // The default version calls from_int(0) and returns false.

  virtual ring_elem var(int v) const;

  virtual ring_elem preferred_associate(ring_elem f) const;
  // Returns an invertible element c of the same ring such that c*f is the
  // preferred associate of the element f.
  // WARNING: The default implementation is for a field.

  virtual bool lower_associate_divisor(ring_elem &f, ring_elem g) const;
  // Replaces f with the unit c such that (fx+g)//c is the preferred associate
  //   of fx+g, in the ring A[x], where A is 'this'.
  // Returns an invertible element c of the same ring such that c*f is the
  // preferred associate of the element f.
  // WARNING: The default implementation is for a field.

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const = 0;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const = 0;

  virtual bool is_unit(const ring_elem f) const = 0;
  virtual bool is_zero(const ring_elem f) const = 0;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const = 0;

  virtual int compare_elems(const ring_elem f, const ring_elem g) const = 0;
  // Returns -1 if f < g, 0 if f and g are either equal, or considered equal, and 1 if f > g.
  // The specific definition is ring dependent, but for numeric rings it defaults to
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

  virtual ring_elem power(const ring_elem f, mpz_t n) const;
  virtual ring_elem power(const ring_elem f, int n) const;
  // These two power routines can be used for n >= 0.

  virtual ring_elem invert(const ring_elem f) const = 0;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const = 0;

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const;
  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
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
  // (4) If the ring is ZZ, then the remainder is "balanced": -[g/2] < r <= [g/2]
  // remainderAndQuotient combines remainder and quotient into one routine.

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const = 0;
  // Constructs elements x and y in the ring s.t. ax + by = 0.  This syzygy is
  // chosen as simply as possible.  For example, over QQ, x is chosen 
  // to be positive.  The routine must handle the case when a=0, but can
  // ignore the case when b=0... (Really?)

  virtual ring_elem random() const;

  virtual void elem_text_out(buffer &o, const ring_elem f) const = 0;

  virtual ring_elem eval(const RingMap *map, const ring_elem f, int first_var) const = 0;

  // Polynomial routines
  // The default implementation is for non-polynomial rings
  virtual int index_of_var(const ring_elem a) const;
  virtual M2_arrayint support(const ring_elem a) const;

  virtual void monomial_divisor(const ring_elem a, int *exp) const;
  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const;
  virtual bool in_subring(int nslots, const ring_elem a) const;
  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const;
  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const;
  virtual ring_elem divide_by_expvector(const int *exp, const ring_elem a) const;

  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       M2_arrayint wts) const;
  virtual ring_elem homogenize(const ring_elem f, int v, M2_arrayint wts) const;

  // Routines expecting a grading.  The default implementation
  // is that the only degree is 0.
  virtual bool is_homogeneous(const ring_elem f) const;
  virtual void degree(const ring_elem f, int *d) const;
  virtual bool multi_degree(const ring_elem f, int *d) const;
    // returns true iff f is homogeneous
  virtual void degree_weights(const ring_elem f, M2_arrayint wts, int &lo, int &hi) const;

  //////////////////////////////////////////
  // Cleaning real and complex numbers /////
  //////////////////////////////////////////
  virtual unsigned long get_precision() const;  // if the ring is not over RRR or CCC, returns 0.
  virtual ring_elem zeroize_tiny(M2_RRR epsilon, const ring_elem f) const;
  // Default is to return f itself.
  virtual void increase_maxnorm(M2_RRR norm, const ring_elem f) const;
  // If any real number appearing in f has larger absolute value than norm, replace norm.
  // Default for rings not over RRR or CCC is to do nothing.
  vec vec_zeroize_tiny(M2_RRR epsilon, const vec f) const;
  // Default is to return f itself.
  void vec_increase_maxnorm(M2_RRR norm, const vec f) const;
  // If any real number appearing in f has larger absolute value than norm, replace norm.
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
  vec copy_vec(const vecterm * v) const;
  void remove_vec(vec v) const;

  bool is_equal(const vecterm * a, const vecterm * b) const;
  bool get_entry(const vecterm * v, int r, ring_elem &result) const;
  ring_elem get_entry(vec v, int r) const;
  vec sub_vector(const vecterm * v, M2_arrayint r) const;
  int n_nonzero_terms(const vecterm * v) const;
  void vec_text_out(buffer &o, const vecterm * v) const;
  vec vec_eval(const RingMap *map, const FreeModule *F,	const vec v) const;

  virtual vec vec_lead_term(int nparts, const FreeModule *F, vec v) const;

  vec negate_vec(vec v) const;
  vec add_vec(vec v, vec w) const;
  vec subtract_vec(vec v, vec w) const;
  vec mult_vec(int n, vec v) const;
  vec mult_vec(const ring_elem f, const vec w) const;
  vec rightmult_vec(const vec w, const ring_elem f) const;

  void set_entry(vec &v, int i, ring_elem r) const;
  void mult_vec_to(vec &v, const ring_elem r, bool opposite_mult) const; // multiplies v <- r * v or v * r
  void mult_row(vec &v, const ring_elem r, int i, bool opposite_mult) const;
  void negate_vec_to(vec &v) const; // v <- -v.
  void add_vec_to(vec &v, vec &w) const; // v <- v+w, w is set to 0.
  void subtract_vec_to(vec &v, vec &w) const; // v <- v-w, w is set to 0.
  void interchange_rows(vec &v, int i, int j) const;
  void vec_row_op(vec &v, int i, ring_elem r, int j, bool opposite_mult) const;

  vec mult_vec_matrix(const Matrix *m,
		      vec v,
		      bool opposite_mult) const;

  vec component_shift(int n, vec v) const;

  vec tensor_shift(int n, int m, vec v) const;

  vec tensor(const FreeModule *F, vec v, 
	     const FreeModule *G, vec w) const;


  void row2by2(vec &, int r1, int r2,
	       ring_elem a1, ring_elem a2,
	       ring_elem b1, ring_elem b2) const;

  void divide_vec_to(vec &v, const ring_elem a) const;
  void divide_row(vec &v, int r, const ring_elem a) const;
  ring_elem dot_product(const vecterm *v, const vecterm *w) const;

  /* Polynomial routines.  These all set an error if the ring is not
     a polynomial ring.  OR, they will be moved to poly.hpp  */
  vec vec_diff(vec v, int rankFw, vec w, int use_coeff) const;
  int vec_in_subring(int n, const vec v) const;
  void vec_degree_of_var(int n, const vec v, int &lo, int &hi) const;
  vec vec_divide_by_var(int n, int d, const vec v) const;
  vec vec_divide_by_expvector(const int *exp, const vec v) const;

  // Some divisibility routines
  bool vec_is_scalar_multiple(vec f, vec g) const;// is cf = dg, some scalars c,d? (not both zero).
  vec vec_remove_monomial_factors(vec f, bool make_squarefree_only) const;

  bool vec_multi_degree(const FreeModule *F, const vec f, int *degf) const;
  // returns true iff f is homogeneous

  void vec_degree(const FreeModule *F, const vec f, int *d) const;
  void vec_degree_weights(const FreeModule *F, 
			  const vec f, 
			  M2_arrayint wts, 
			  int &lo, 
			  int &hi) const;
  bool vec_is_homogeneous (const FreeModule *F, const vec f) const;
  vec vec_homogenize(const FreeModule *F, 
		     const vec f, 
		     int v, 
		     int deg, 
		     M2_arrayint wts) const;
  vec vec_homogenize(const FreeModule *F, 
		     const vec f, 
		     int v, 
		     M2_arrayint wts) const;
};

#define ZERO_RINGELEM (ring_elem(reinterpret_cast<Nterm *>(0)))

#include "ZZ.hpp"
extern RingZZ *globalZZ;
extern QQ *globalQQ;
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
