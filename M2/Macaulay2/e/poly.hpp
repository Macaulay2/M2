// Copyright 2004 Michael E. Stillman

#ifndef _poly_hpp_
#define _poly_hpp_

#include "ringelem.hpp"
#include "engine.h"
#include "skew.hpp"
#include <vector>

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
class PolyFrac;
class PolyRingNC;
class PolyQuotient;

class GBRing;
class GBRingSkew;
class GBComputation;

#include "ring.hpp"
#include "qring.hpp"

class PolynomialRing : public Ring
{
  bool is_graded_;

protected:
  // Most skew-mult specific poly code is in skewpoly.{hpp,cpp}.  However, var, homogenize,
  //   and diff_term all have a small amount of skew commutative specific code.
  bool is_skew_;
  SkewMultiplication skew_; // Completely ignored if is_skew_ is false.

  bool is_weyl_; // true iff numerR_ is a Weyl algebra.
  bool is_solvable_; // true iff numerR_ is a solvable algebra.

  Ring::CoefficientType coeff_type_;
  bool overZZ_; // true iff this is a quotient over ZZ.
  QRingInfo *qinfo_;
  bool is_ZZ_quotient_;		// true if this is a quotient of a polynomial ring over ZZ, AND
				// there is an integer in the factored ideal.
  ring_elem ZZ_quotient_value_;	// This is the integer in the factor ideal, if is_ZZ_quotient is set.

  int poly_size_;

  int nvars_;
  const Ring *K_;
  const Monoid *M_;
  const PolyRing *numerR_; // numerator ring, possibly 'this'
                           // This is always a PolyRing, with no quotient elements
                           // If ring is basic[M]/I, then numerR is basic[M]
                           // If ring is QQ[M]/I, then numerR is ZZ[M].
                           // skew and weyl multiplication is determined by this ring.
                           // initialize_PolynomialRing will set these values,
                           //   if numerR_ != this.
  const Ring *denomR_; // denominator ring, or NULL
                       // If ring is basic[M]/I, this is NULL
                       // If ring is QQ[M]/I, this is ZZ
  const PolynomialRing *ambientR_; // ambient ring (i.e. no quotients), possibly 'this'.
                       // If ring is basic[M]/I, then ambientR_ is same as numerR_
                       // If ring is QQ[M]/I, then ambientR_ is QQ[M].

  GBRing * gb_ring_;

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
					 std::vector<Nterm *, gc_allocator<Nterm *> > &elems);
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

  Matrix * getPresentation() const;




  bool is_basic_ring() const { return false; } // The default is to be a basic ring.

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
  // three values.  Fraction fields return their ultimate value, as do poly rings.

  int n_vars() const { return nvars_; }

  static PolynomialRing *create_quotient_ring(const Matrix *M);

  QRingInfo *get_quotient_info() const { return qinfo_; }

  const Ring *  Ncoeffs() const { return getCoefficients(); }
  const Monoid * Nmonoms() const { return getMonoid(); }
  // MY BAD: sometimes means flat coeffs, sometimes logical coeffs
  // Both Ncoeffs and Nmonoms need to be totally removed.

  // Quotient ring information
  const MonomialTable * get_quotient_MonomialTable() const 
  { return qinfo_->get_quotient_MonomialTable(); }
  
  const MonomialIdeal *  get_quotient_monomials() const 
  { return qinfo_->get_quotient_monomials(); }

  const MonomialTableZZ * get_quotient_MonomialTableZZ() const
  { return qinfo_->get_quotient_MonomialTableZZ(); }

  int n_quotients() const { return qinfo_->n_quotients(); }

  Nterm * quotient_element(int i) const { return qinfo_->quotient_element(i); }

  const gbvector * quotient_gbvector(int i) const { return qinfo_->quotient_gbvector(i); }

  const MonomialIdeal * make_basis_MonomialIdeal() const 
  { return  get_quotient_monomials(); }
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
  const SkewMultiplication & getSkewInfo() const { return skew_; }

  virtual bool is_commutative_ring() const { return !is_weyl_ && !is_skew_ && !is_solvable_; }
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

  virtual const PolyRing * getNumeratorRing() const { return numerR_; }

  virtual const PolynomialRing * getAmbientRing() const { return ambientR_; }
  // Yields the ambient PolyRing corresponding to this polynomial ring
  // This ring has no quotients, no fractions (not even QQ), but may have
  // skew, weyl, or solvable multiplication, or even (later) be an associative
  // free algebra.

  virtual const RingOrNull *getDenominatorRing() const { return denomR_; }
  // If this ring has no denominators, NULL is returned.  Otherwise the ring which
  // implements denominators is returned.  When one asks for a denominator for elements of
  // 'this', the result value is its ring.

  virtual GBRing *get_gb_ring() const { return gb_ring_; }

  virtual const Ring *getCoefficients() const { return K_; }
  // The implementation coeff ring of 'this'.  This is either a basic ring (field, ZZ), or
  // is another PolyRing.

  virtual const Monoid *getMonoid() const { return M_; }
  // The implementation monoid of this ring.

  virtual bool is_pid() const       { return (nvars_ == 1 && K_->is_field())
				       || (nvars_ == 0 && K_->is_pid()); }
  virtual bool has_gcd() const      { return (nvars_ == 1 && K_->is_field())
				       || (nvars_ == 0 && K_->has_gcd()); }

  virtual bool is_fraction_poly_ring() const { return getDenominatorRing() != 0; }
  // returns true if this ring has fractions.  This includes
  // polynomial rings over QQ, polynomial rings over fraction fields,
  // fraction rings, and localizations.
  // If this returns true, then 'get_denominator_ring()' returns non-NULL value.
  // 

  virtual int n_fraction_vars() const {
    const Ring *D = getDenominatorRing();
    if (D == 0) return 0;
    const PolynomialRing *DR = D->cast_to_PolynomialRing();
    if (DR == 0) return 0;
    return DR->n_vars();
  }

  virtual const PolynomialRing * cast_to_PolynomialRing()  const { return this; }
  virtual       PolynomialRing * cast_to_PolynomialRing()        { return this; }

  ////////////////////////////////
  // To possibly be over-ridded //
  ////////////////////////////////

  virtual void text_out(buffer &o) const = 0;

  ////////////////////////
  // Arithmetic //////////
  ////////////////////////
  virtual ring_elem var(int v) const = 0;

#if 0
  virtual ring_elem from_double(double n) const = 0;
  virtual ring_elem from_int(int n) const = 0;
  virtual ring_elem from_int(mpz_ptr n) const = 0;

  virtual bool promote(const Ring *R, const ring_elem f, ring_elem &result) const = 0;
  virtual bool lift(const Ring *R, const ring_elem f, ring_elem &result) const = 0;

  virtual ring_elem preferred_associate(ring_elem f) const = 0;

  virtual bool is_unit(const ring_elem f) const = 0;
  virtual bool is_zero(const ring_elem f) const = 0;
  virtual bool is_equal(const ring_elem f, const ring_elem g) const = 0;

  virtual ring_elem copy(const ring_elem f) const = 0;
  virtual void remove(ring_elem &f) const = 0;

  virtual ring_elem negate(const ring_elem f) const = 0;
  virtual ring_elem add(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem subtract(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem mult(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem power(const ring_elem f, mpz_t n) const = 0;
  virtual ring_elem power(const ring_elem f, int n) const = 0;
  virtual ring_elem invert(const ring_elem f) const = 0;
  virtual ring_elem divide(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem gcd(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem gcd_extended(const ring_elem f, const ring_elem g, 
				  ring_elem &u, ring_elem &v) const = 0;

  virtual ring_elem remainder(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem quotient(const ring_elem f, const ring_elem g) const = 0;
  virtual ring_elem remainderAndQuotient(const ring_elem f, const ring_elem g, 
					 ring_elem &quot) const = 0;

  virtual void syzygy(const ring_elem a, const ring_elem b,
		      ring_elem &x, ring_elem &y) const = 0;

  virtual ring_elem random() const = 0;

  virtual void elem_text_out(buffer &o, const ring_elem f) const = 0;

  virtual ring_elem eval(const RingMap *map, const ring_elem f) const = 0;
#endif
  /////////////////////////
  // Polynomial routines //
  /////////////////////////
  virtual int index_of_var(const ring_elem a) const = 0;
  virtual M2_arrayint support(const ring_elem a) const = 0;

  virtual bool is_homogeneous(const ring_elem f) const = 0;
  virtual void degree(const ring_elem f, int *d) const = 0;
  virtual bool multi_degree(const ring_elem f, int *d) const = 0;
  virtual int primary_degree(const ring_elem f) const = 0;
  virtual void degree_weights(const ring_elem f, const M2_arrayint wts, 
			      int &lo, int &hi) const = 0;
  virtual ring_elem homogenize(const ring_elem f, int v, int deg, 
			       const M2_arrayint wts) const = 0;
  virtual ring_elem homogenize(const ring_elem f, int v, const M2_arrayint wts) const = 0;


  virtual ring_elem mult_by_term(const ring_elem f, 
				  const ring_elem c, const int *m) const = 0;

  virtual int n_flat_terms(const ring_elem f) const = 0;
  virtual int n_logical_terms(int nvars0, const ring_elem f) const = 0;

  virtual ArrayPairOrNull list_form(const Ring *coeffR, const ring_elem f) const = 0;

  int n_terms(const ring_elem f) const { return n_flat_terms(f); }
  // This is here mainly because geopoly requires n_terms.

  virtual ring_elem make_flat_term(const ring_elem a, const int *m) const = 0;
  virtual ring_elem make_logical_term(const Ring *coeffR, const ring_elem a, const int *exp) const = 0;
  //  virtual ring_elem term(const ring_elem a, const int *m) const = 0;

  virtual ring_elem lead_flat_coeff(const ring_elem f) const = 0;
  virtual ring_elem lead_logical_coeff(const Ring *coeffR, const ring_elem f) const = 0;

  virtual ring_elem get_coeff(const Ring *coeffR, const ring_elem f, const int *vp) const = 0;
  // vp is a varpower monomial, in the logical monoid.
  // The result will be an element in the logical coefficient ring.

  virtual ring_elem get_terms(int nvars0, const ring_elem f, int lo, int hi) const = 0;
  // get the (logical) terms from lo to hi in f.  A negative value means count from
  // the end.  get_terms(--,f,0,0) is the logical lead term of f.

  virtual const int * lead_flat_monomial(const ring_elem f) const = 0;
  virtual void lead_logical_exponents(int nvars0, const ring_elem f, int * result_exp) const = 0;

  virtual void mult_coeff_to(ring_elem a, ring_elem &f) const = 0;

  virtual void monomial_divisor(const ring_elem a, int *exp) const = 0;
  virtual ring_elem diff(ring_elem a, ring_elem b, int use_coeff) const = 0;
  virtual bool in_subring(int nslots, const ring_elem a) const = 0;
  virtual void degree_of_var(int n, const ring_elem a, int &lo, int &hi) const = 0;
  virtual ring_elem divide_by_var(int n, int d, const ring_elem a) const = 0;
  virtual ring_elem divide_by_expvector(const int *exp, const ring_elem a) const = 0;

  virtual Nterm * numerator(ring_elem f) const = 0;
protected:
  void sort(Nterm *&f) const;
public:
  virtual const vecterm * vec_locate_lead_term(const FreeModule *F, vec v) const = 0;
  // Returns a pointer to the vector term of v which contains the lead term (of the
  // numerator). 
  // If result = R->vec_locate_lead_term(F,v), (if v is non-zero)
  // To get the lead coeff, use result->comp
  // To get the lead flat monomial (of numerator), use R->lead_flat_monomial(result->coeff).

  virtual vec vec_lead_term(int nparts, const FreeModule *F, vec v) const = 0;

  virtual vec vec_top_coefficient(const vec v, int &var, int &exp) const = 0;

  virtual gbvector * translate_gbvector_from_ringelem(ring_elem coeff) const = 0;

  virtual gbvector * translate_gbvector_from_vec(const FreeModule *F, 
						 const vec v, 
						 ring_elem &result_denominator) const = 0;
  // result/denom == v.
  // result_denom will be an element in getDenominatorRing() (if non-NULL).
  
  virtual vec translate_gbvector_to_vec(const FreeModule *F, const gbvector *v) const = 0;
  
  virtual vec translate_gbvector_to_vec_denom(const FreeModule *F, 
					      const gbvector *v,
					      const ring_elem denom) const = 0;
  // Translate v/denom to a vector in F.  denom does not need to be positive,
  // although it had better be non-zero.
  // denom should be an element of getDenominatorRing() (if non-NULL, otherwise 'denom'
  // is ignored).
};

class PolyFrac : public PolynomialRing
// The class of polynomial rings implemented as (numer,denom)
{
};

class PolyRingFlat : public PolynomialRing
// The class of polynomial rings implemented as a pointer (single value).
{
public:
  virtual Nterm * numerator(ring_elem f) const { return f.poly_val; }

  virtual const PolyRingFlat * cast_to_PolyRingFlat()  const { return this; }
  virtual       PolyRingFlat * cast_to_PolyRingFlat()        { return this; }
};







///////////////////////////////////////////////////////
///////////////////////////////////////////////////////
#if 0
class RRing
{
public:
  virtual const PPolynomialRing *cast_to_PPolynomialRing() const { return 0; }
  virtual const PolyRing *cast_to_PolyRing() const { return 0; }
};

class PPolynomialRing : public RRing
{
  int element_size_; // Size of ring_elem for ring elements for this ring.

  bool is_graded_;   // Is this a graded ring?

protected:
  int getElementSize();
  int setElementSize();

  void setGraded(bool isgraded);
public:
  const PolyRing *getAmbientRing() const;
  // Every polynomial ring has an ambient ring.
  // R is an ambient ring if R == R->getAmbientRing().
  // An ambient ring has no quotient, no fractions, not even QQ.

  const RRing * getDenominatorRing() const;

  void text_out(buffer &o) const; // Display the ring in debugging form

  ////////////////
  // casting up //
  ////////////////
  virtual const PolyRing     *cast_to_PolyRing() const { return 0; }
  virtual const PolyRingSkew *cast_to_PolyRingSkew() const { return 0; }
  virtual const PolyRingWeyl *cast_to_PolyRingWeyl() const { return 0; }

  virtual const PolyQuotient *cast_to_PolyQuotient() const { return 0; }

  virtual const PolyFrac     *cast_to_PolyFrac() const { return 0; }

  virtual const PolyRingNC   *cast_to_PolyRingNC() const { return 0; }

  ////////////////////////
  // Ring informational //
  ////////////////////////
  bool isGraded() const;

  bool isSkewCommutative() const;
  bool isWeylAlgebra() const;
  bool isGradedWeylAlgebra() const;

  //////////////////////////////
  // Ring creation interface ///
  //////////////////////////////

  static PPolynomialRing *createAmbientRing(const Ring *K, const Monoid *M);

  const PPolynomialRing *create_Quotient(const Matrix *quotients) const;
  // Creates a clone of this, but mod out by the GB in quotients.
  // 'this' may already be a quotient ring.
  
  const PPolynomialRing *create_Quotient(const PPolynomialRing *B) const;
  // Creates a clone of this, if this = A[M], then
  // and B = A/I, then create A[M]/I.
  // One possible restriction is that A is an ambient poly ring.
  // Assumption: A is the ambient polynomial ring of B.  If not, NULL
  // is returned, and an error is flagged.

  const PPolynomialRing *create_Fractions(const Matrix *Prime) const;
  // Create a fraction ring, where fractions can be any element not in P.
  // P should be a GB (in the ambient ring)  of a prime ideal.
  // P can be a list of length 0, in which case this is a fraction field.

  const PPolynomialRing *create_Fractions(const Ring *A) const;
  // Create a polynomial ring over a fraction ring.  The ring A
  // should have its ambient ring == a coefficient ring of the ambient ring
  // of A, and A should be a fraction ring.

  const RRing *findCoefficientRing(const RRing *A) const;
  // If A is a coefficient ring of this, or could be, return the coefficient
  // ring of this which corresponds.  NULL means no such coefficient ring
  // could be found.

  ////////////////////////
  // Arithmetic //////////
  ////////////////////////

  ///////////////////////////
  // Parts of a polynomial //
  ///////////////////////////
};

///////////////////////////////////////////////////
///////////////////////////////////////////////////
class PolyRing : public PPolynomialRing
{
  // The class of polynomial rings, possibly non-commutative
  // with no fractions (including no QQ coefficients).
  // HOWEVER: they may have quotient ideals.
protected:
  static M2_arrayint addScalar(M2_arrayint a, int n); // helper routine

  void initialize(const RRing *coeff_ring,
		  const Monoid *monoid,
		  const RRing *flat_coeff_ring,
		  const Monoid *flat_monoid);

  virtual const PolyRing *createPolyRing(const Monoid *monoid) const;
  // Create a polynomial ring over this, of the same type (e.g. weyl, skew, comm) 
  // as this.
public:
  static const PolyRing * getTrivialPolyRing();

  static const PolyRing * create(const RRing *coeff_ring,
				  const Monoid *monoid);
  // Creates an ambient poly ring.  If coeff_ring is
  // skew or weyl, so is the result.

  virtual const PolyRing *cast_to_PolyRing() const { return this; }

  const RRing * getCoefficients() const;
  const RRing * getFlatCoefficients() const;
  const Monoid * getMonoid() const; // What about NC polys??
  const Monoid * getFlatMonoid() const;
};
///////////////////////////////////////////////////
///////////////////////////////////////////////////
class PolyRingSkew : public PolyRing
{
  bool is_skew_;
  SkewMultiplication skew_;

  void setSkewInfo(const M2_arrayint skew_vars);

  virtual const PolyRing *createPolyRing(const Monoid *monoid) const;
  // Create a polynomial ring over this, of the same type (e.g. weyl, skew, comm) 
  // as this.

public:
  virtual const PolyRingSkew *cast_to_PolyRingSkew() const { return this; }

  virtual ~PolyRingSkew();
  void getSkewInfo(M2_arrayint &result_skewvars);
  // result_skewvarsw is a CONSTANT return value.  DO NOT
  // change its value!!

  static const PolyRingSkew *create(const PolyRing *P, M2_arrayint skewvars);
  // Creates a clone of P, with skew symmetric multiplication.
};
///////////////////////////////////////////////////
///////////////////////////////////////////////////
class PolyRingWeyl : public PolyRing
{
  void setWeylInfo(M2_arrayint result_comm, 
		   M2_arrayint result_deriv, 
		   int homog_var);

  virtual const PolyRing *createPolyRing(const Monoid *monoid) const;
  // Create a polynomial ring over this, of the same type (e.g. weyl, skew, comm) 
  // as this.
public:
  virtual const PolyRingWeyl *cast_to_PolyRingWeyl() const { return this; }

  virtual ~PolyRingWeyl();
  void getWeylInfo(M2_arrayint &result_comm, 
		   M2_arrayint &result_deriv, 
		   int &homog_var) const;
  // result_comm and result_deriv are CONSTANT return values.  DO NOT
  // change their values!!

  const PolyRingWeyl *create(const PolyRing *P,
			     M2_arrayint derivatives,
			     M2_arrayint commutatives,
			     int homog_var);
  // Creates a clone of P, with Weyl algebra multiplication.
};
///////////////////////////////////////////////////
///////////////////////////////////////////////////
class PolyQuotient : public PPolynomialRing
{
  // This class handles quotients of poly rings, skew comm, and Weyl algebras
  // If there are fractions (even QQ), this is the wrong class to use?

  std::vector<ring_elem, gc_allocator<ring_elem> > quotients_;

  MonomialIdeal *Rideal_; // contains the lead monomials (in flatMonoid)
  MonomialTable *quotient_table_;
  MonomialTableZZ *quotient_table_ZZ_;
  bool is_ZZ_quotient_;
  ring_elem ZZ_quotient_value_;

  void setQuotientInfo(std::vector<ring_elem, gc_allocator<ring_elem> > &quotients);

  // Passes off most routines to its ambient ring, which is a PolyRing.
  // exceptions: promote, (maybe lift), mult, term.  What else?
  // from_int, from_double, var,
  // promote, is_unit, homogenize
  // mult_by_term, term, 
public:
  virtual ~PolyQuotient();

  static const PolyQuotient * create(const Matrix *m);
};
///////////////////////////////////////////////////
///////////////////////////////////////////////////
class PolyRingNC : public PolyRing
{
public:
  virtual ~PolyRingNC();
};
///////////////////////////////////////////////////
///////////////////////////////////////////////////
class PolyFrac : public PPolynomialRing
{
public:
  virtual ~PolyFrac();
};
///////////////////////////////////////////////////
///////////////////////////////////////////////////
class PolyFracQuotient : public PolyFrac
{
  PolyFrac *R; // A fraction ring
  PolyQuotient *S; // This is a quotient 
  // This ring is R/(ideal(S))
public:
  virtual ~PolyFracQuotient();

  static const PolyFracQuotient * create(const Matrix *m);
};

///////////////////////////////////////////////////
///////////////////////////////////////////////////
///// Fraction Info ///////////////////////////////
///////////////////////////////////////////////////
///////////////////////////////////////////////////

struct MultClosedSet
{
  MultClosedSet *next;

  virtual bool is_invertible(Nterm *f);
};
struct LocalSet : public MultClosedSet
{
  GBComputation *G;

  bool is_invertible(Nterm *f)
  {
    if (in_subring(nvars, f))
      {
	G->reduce(f);
	return f != 0;
      }
  }
};

class Fractions
{
  const PolyRing *R;

public:
  bool is_invertible(Nterm *f);
};

class MultClosedSet_local
{
  const PolyRing *R;
};

class MultClosedSet_semilocal
{
  const PolyRing *R;
};

class MultClosedSet_powers
{
  const PolyRing *R;
};

class MultClosedSet_powers
{
  const PolyRing *R;
};
///////////////////////////////////////////////////
///////////////////////////////////////////////////
///// Quotient Info ///////////////////////////////
///////////////////////////////////////////////////
///////////////////////////////////////////////////

class Quotient
{
public:
  virtual void normal_form(Nterm *&f, ring_elem &g);
  virtual void normal_form(Nterm *&f); // Throws away any denominator used.

  // Obtaining quotient elements
  int n_quotients() const;
  ring_elem get_quotient_element(int i) const; // elements in the PolyRing R.

  // Obtaining monomial ideal
  const MonomialIdeal * get_quotient_monomials() const;
  const MonomialTable * get_quotient_montable() const;

  // Obtaining monomial ideal
  const MonomialTableZZ * get_quotient_montableZZ() const;
};

class QuotientBasic : public Quotient
{
  const PolyRing *R; // This is the ambient ring of the quotient
  
public:
  QuotientBasic(const Matrix *m);
  // The ring of m should be a PolyRing, and m should be a monic GB
  // of this ring.

  QuotientBasic(const Matrix *m, const Matrix *n);
  // The rings of m and n should be the same, a PolyRing, and the union
  // of the columns should be a (possibly non-minimal) GB, where every 
  // column of n is minimal.

  ~QuotientBasic();

  void normal_form(Nterm *&f);

  void normal_form(const FreeModule *F, gbvector *&f);
};

class QuotientZZ : public Quotient
{
  const PolyRing *R; // This is the ambient ring of the quotient
  // whose flat coeff ring is ZZ.
  
public:
  QuotientZZ(const Matrix *m);
  // The ring of m should be a PolyRing, and m should be a monic GB
  // of this ring.

  QuotientZZ(const Matrix *m, const Matrix *n);
  // The rings of m and n should be the same, a PolyRing, and the union
  // of the columns should be a (possibly non-minimal) GB, where every 
  // column of n is minimal.

  ~QuotientZZ();

  void normal_form(Nterm *&f);

  void normal_form(const FreeModule *F, gbvector *&f);
};

class QuotientFrac : public Quotient
{
  const PolyRing *R; // This is the ambient ring of the quotient

  const RRing *denomR; // These are the allowed denominators
  // This will be globalZZ, if the ring is QQ[...].
public:
  QuotientFrac(const RRing *denomR, const Matrix *m);
  // The ring of m should be a PolyRing, and m should be a monic GB
  // of this ring.

  QuotientFrac(const RRing *denomR, const Matrix *m, const Matrix *n);
  // The rings of m and n should be the same, a PolyRing, and the union
  // of the columns should be a (possibly non-minimal) GB, where every 
  // column of n is minimal.

  ~QuotientFrac();

  void normal_form(Nterm *&f, ring_elem &denom);

  void normal_form(const FreeModule *F, gbvector *&f, ring_elem &denom);

};
#endif
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End: 
