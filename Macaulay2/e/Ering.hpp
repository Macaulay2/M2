// Copyright 1998 by Michael Stillman
#ifndef __ring_hpp_
#define __ring_hpp_

#include "EZZp.hpp"
#include "Emonoid.hpp"

class MonomialDivisionTable;
class Ideal;

class ERing;
class EMonoid;
class ECommMonoid;
class ENCMonoid;
class EFreeModule;

class monomial;
class EVector;

class EPolynomialRing;
class ECommPolynomialRing;
class EWeylAlgebra;
class ESkewCommPolynomialRing;
class ENCPolynomialRing;

class EPolynomialRing : public ERing
{
  friend class ECommPolynomialRing;
  friend class EFreeModule;
  friend class EVector;
  friend class EMatrix;
private:
#if 0
  EVector mult1(const EFreeModule *resultF,
		bool component_from_f,
		const evec *f, 
		const EVector &g) const;
#endif
protected:
  stash *epoly_stash;
  
  // Quotient information
  // We will put:
  //   bool is_quotient;
  //   EPolynomialRing *cover; or a function...
  //   an array of the set of elements
  //   a GB of these elements
  //   a monomial lookup table of these elements.

  EPolynomialRing() {}
  void initialize(const ERing *K, 
             int nvars,  // only the variables from the monoid, not from K.
             const EPolynomialRing *ZZDD,
             int * _degs  // grabbed
             );
  virtual ~EPolynomialRing();
public:
  // Parts of the ring
  const virtual EMonoid *getMonoid() const = 0;

  int characteristic() const 
    { return K->characteristic(); }
  
  // Quotient ring stuff
  bool isQuotient() const { return false; }
  
protected:
  // These are used for low-level creation/removal of polynomial and vector  nodes
  void remove_poly_term(epoly *t) const;
  epoly *new_poly_term() const;
  evec *vec_copy_term(const evec *t) const;
  epoly *copy_term(const epoly *t) const;
  bool terms_equal(const epoly *s, const epoly *t) const;
  bool vec_terms_equal(const evec *s, const evec *t) const;
  
public:
  // Free modules
  EFreeModule *makeSchreyerFreeModule(int rank, 
                                      const monomial **degrees,  // grabbed
                                      const monomial **ordering, // grabbed
                                      int *tiebreaks) const;     // grabbed
  EFreeModule *makeSchreyerFreeModule(const EMatrix *m) const;
    // Return 'source m' but with a Schreyer order coming from the
    // lead terms of the matrix, and with the tiebreaks coming from
    // the components of the lead monomials.
  
  // Display
  virtual void text_out(buffer &o) const = 0;
  virtual void bin_out(buffer &) const {}

  //static EPolynomialRing *binary_in(istream &i);

  // Query routines about what kind of ring we have

  virtual EPolynomialRing * cast_to_EPolynomialRing() { return this; }
  virtual const EPolynomialRing * cast_to_EPolynomialRing() const { return this; }

  virtual const EPolynomialRing *toPolynomialRing() const { return this; }

  virtual ERingElement vec_term_to_ring(const evec *v) const;
  virtual int vec_compare_terms(const evec *v, const evec *w) const;
  virtual ERingElement vec_component(const EVector &v, int x) const;
  void vec_add_to(EVector &v, EVector &w) const;
  virtual EVector vec_make(const EFreeModule *F, const ERingElement a, int x) const;
  EVector vec_term(const EFreeModule *F, 
                      const ERingElement c, 
                      const monomial *m,
                      int x) const;
  virtual void vec_degree_lohi(const evec *v, int nwts, const int *wts,
                               int &lo, int &hi) const;
  virtual void vec_text_out(buffer &o, const EVector &v) const;
  virtual void vec_bin_out(buffer &o, const EVector &v) const;

  virtual EVector vec_left_mult(const ERingElement a, const EVector &b) const;
  virtual EVector vec_right_mult(const EVector &a, const ERingElement b) const;
  virtual EVector vec_evaluate(const ERingMap *map, const EFreeModule *Ftarget, const EVector &v) const;

  virtual bool promote(const ERing *Rf, const ERingElement f, ERingElement &result) const;
  virtual bool lift(const ERing *Rg, const ERingElement f, ERingElement &result) const;
  virtual bool vec_promote(const EVector &v, const EFreeModule *resultF, EVector &result) const;
  virtual bool vec_lift(const EVector &v, const EFreeModule *resultF, EVector &result) const;

protected:
  virtual epoly *mult1(const epoly *a, // single term
		       const epoly *b  // entire polynomial
		       ) const;
  virtual EVector mult1(
    const EFreeModule *resultF,
    const epoly *f,    // single term
    const EVector &g   // entire vector
    ) const;
  virtual EVector mult1(
    const EFreeModule *resultF,
    const evec *f,   // single term
    const epoly *g   // entire polynomial
    ) const;

  void add_to0(EVector &v, EVector &w) const;
  void add_to_schreyer(EVector &v, EVector &w) const;

///////////////////////////////////////////////////////////////////////////////
// Non-virtual functions which operate directly on elements of type 'epoly *' // 
///////////////////////////////////////////////////////////////////////////////
public:   
  int n_terms(const epoly *f) const;

  epoly *_make_term(const ERingElement a, const monomial *m) const;

  void remove(epoly *a) const;
     
  epoly *clone(const epoly *a) const;

  epoly *_from_int(int n) const;

  epoly *_zero() const
    { return 0; }
    
  epoly *_one() const
    { return _from_int(1); }

  void negate_to(const epoly *a) const;

  epoly *negate(const epoly *a) const;
    
  void add_to(epoly *&a, epoly *&b) const;
    // a := a+b.
    
  epoly *add(const epoly *a, const epoly *b) const;

  epoly *subtract(const epoly *a, const epoly *b) const;
    
  epoly *mult_by_ZZ(const epoly *a, int n) const;

  virtual epoly *mult(const epoly *a, const epoly *b) const;

  epoly *divide(const epoly *a, const epoly *b) const; /*TODO*/
    
  epoly *power(const epoly *a, int n) const;

  bool is_zero(const epoly *a) const
    { return a == 0; }
    
  bool is_equal(const epoly *a, const epoly *b) const;

  void elem_text_out(buffer &o, const epoly *a) const;
  void elem_bin_out(buffer &o, const epoly *a) const;/*TODO*/
  epoly *elem_binary_in(istream &i) const;
    
  
/////////////////////////////
// Virtual Ring Operations //
/////////////////////////////

  virtual ERingElement from_int(int n) const 
    { return POLY_TO_ERingElement(_from_int(n)); }
    
  virtual ERingElement zero() const
    { return POLY_TO_ERingElement(_zero()); }
    
  virtual ERingElement one() const
    { return POLY_TO_ERingElement(_one()); }

  virtual void remove(ERingElement a) const
    { remove(a.poly_val); }

  virtual ERingElement clone(ERingElement a)  const
    { return POLY_TO_ERingElement(clone(a.poly_val)); }

  virtual ERingElement negate(ERingElement a) const 
    { return POLY_TO_ERingElement(negate(POLYVAL(a))); }

  virtual void negate_to(ERingElement &a) const
    { negate_to(a.poly_val); }

  virtual ERingElement add(ERingElement a, ERingElement b) const
    { return POLY_TO_ERingElement(add(POLYVAL(a),POLYVAL(b))); }

  virtual ERingElement subtract(ERingElement a, ERingElement b) const
    { return POLY_TO_ERingElement(subtract(POLYVAL(a),POLYVAL(b))); }

  virtual void add_to(ERingElement &a, ERingElement &b) const
    { add_to(a.poly_val, b.poly_val); }
    
  virtual ERingElement mult_by_ZZ(ERingElement a, int n) const
    { return POLY_TO_ERingElement(mult_by_ZZ(POLYVAL(a),n)); }

  virtual ERingElement mult(ERingElement a, ERingElement b) const
    { return POLY_TO_ERingElement(mult(POLYVAL(a),POLYVAL(b))); }

  virtual ERingElement divide(ERingElement a, ERingElement b) const
    { return POLY_TO_ERingElement(divide(POLYVAL(a),POLYVAL(b))); }

  virtual ERingElement power(ERingElement a, int n) const
    { return POLY_TO_ERingElement(power(POLYVAL(a),n)); }

  virtual bool is_zero(ERingElement a) const
    { return is_zero(POLYVAL(a)); }

  virtual bool is_equal(ERingElement a, ERingElement b) const
    { return is_equal(POLYVAL(a),POLYVAL(b)); }

  virtual void elem_text_out(buffer &o, ERingElement a) const
    { elem_text_out(o, POLYVAL(a)); }

  virtual void elem_bin_out(buffer &o, ERingElement a) const
    { elem_bin_out(o, POLYVAL(a)); }

  virtual ERingElement evaluate(const ERingMap *map, const ERingElement r) const;

  ERingElement make_term(ERingElement a, const monomial *m) const
    { return POLY_TO_ERingElement(_make_term(a,m)); }
  ERingElement make_term(const ERingElement c, const intarray &term) const;
  ERingElement make_ring_variable(int v, int exponent) const;

  virtual void degreeWeightsLoHi(ERingElement f, int nwts, const int *wts,
                          int &lo,
                          int &hi) const;

  // Not virtual:
  const monomial *leadMonomial(const ERingElement f) const;
  ERingElement leadCoefficient(const ERingElement f) const;
  ERingElement leadTerm(const ERingElement f, int n) const;
  ERingElement getTerms(const ERingElement f, int lo, int hi) const;

  /////////////////////////////////////////////
  // Iterator over elements of a polynomial ///
  /////////////////////////////////////////////
  class iterator {
    const epoly *p;
  public:
    iterator(const ERingElement a) : p(POLYVAL(a)) {}
    iterator(const epoly *a) : p(a) {}
    iterator() {}

    bool valid() { return p != 0; }
    void operator++() { p = p->next; }

    epoly * operator*() { return (epoly *)p ; }
    epoly * operator->() { return (epoly *)p; }
  };

  /////////////////////////////////////
  // Heap class for faster additions //
  /////////////////////////////////////
  class heap {
    const EPolynomialRing *R;
    const EMonoid *M;
    const ERing *K;		// The coefficient ring
    epoly * theHeap[GEOHEAP_SIZE];
    int len[GEOHEAP_SIZE];
    int top_of_heap;
    int n_terms(const epoly *g) const;
  public:
    heap(const EPolynomialRing *R);
    ~heap();
    void reset();
  
    void add(ERingElement f);  // grabs f.
    void add(epoly * f);  // grabs f.
    bool getLeadTerm(ERingElement &coeff, const monomial *&monom); // returns false if no more.
				// Does NOT remove this monomial from the heap.
    ERingElement value();
    epoly *poly_value();
  };

  //////////////////////////////
  // Creation of a polynomial //
  //////////////////////////////
  class collector {
    const EPolynomialRing *R;
    epoly head;
    epoly *last;
  public:
    collector(const EPolynomialRing *R) : R(R), last(&head) {}
    ~collector() {}
    
    void append(epoly *a) { 
      last->next = a; 
      last = a; 
    }

    void append_final_terms(epoly *a) { 
      last->next = a; 
      last = 0; 
    }
    epoly *poly_value() {
      if (last != 0) last->next = 0; 
      return head.next;
    }

    ERingElement value() { 
      return POLY_TO_ERingElement(poly_value());
    }
  };
};

//////////////////////////////////////////////////////////////////////////
class ECommPolynomialRing : public EPolynomialRing
{
protected:
  const ECommMonoid *M;
  static ECommPolynomialRing *_trivial;
  ECommPolynomialRing() {}
  void initialize(
      const ERing *KK,
      const ECommMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs);  // grabbed, length not checked
  virtual ~ECommPolynomialRing();
  static void makeTrivialRing();  // ZZ as a polynomial ring, trivial degree ring.
public:
  virtual const EMonoid *getMonoid() const
    { return M; }

  static ECommPolynomialRing *make(const ERing *KK, 
                                   const ECommMonoid *MM,
                                   const EPolynomialRing *ZZDD,
                                   int *degrees  // Grabbed: this is an array of length MM->n_vars*ZZDD->n_vars;
                                   );

  static const ECommPolynomialRing *getTrivialRing();
                          
  static ECommPolynomialRing *binary_in(istream &i);

  virtual const ECommPolynomialRing *toCommPolynomialRing() const { return this; }
  virtual const EWeylAlgebra *toEWeylAlgebra() const { return 0; }
  virtual const ESkewCommPolynomialRing *toESkewCommPolynomialRing() const { return 0; }

  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;

  class_identifier class_id() const { return CLASS_ECommPolynomialRing; }
};
//////////////////////////////////////////////////////////////////////////
class EWeylAlgebra : public ECommPolynomialRing
{
private:
protected:
  int nderivatives;
  bool homogeneous_weyl_algebra;
  int homog_var;		// Only used if 'homogeneous_weyl_algebra' is true.
  int *derivative;		// a value _derivative[i] = r >= 0 means that i is diff(r).
				// If < 0 : the variable i does not have a diff op.
  int *commutative;		// Same as above, but in opposite direction.
  
  static int binomtop;
  static int diffcoeffstop;
  static int **binomtable;
  static int **diffcoeffstable;

  EWeylAlgebra() {}
  virtual ~EWeylAlgebra();
  void initialize(const ERing *KK, 
               const ECommMonoid *MM, 
               const EPolynomialRing *ZD,
               int *degs,  // grabbed, length not checked
               int npairs, 
	       const int *derivative, const int *commutative,
	       bool is_homog, int homog_var);
  void initialize1();
  
  void extractDerivativePart(const int *exponents, int *result) const;
  void extractCommutativePart(const int *exponents, int *result) const;
  ERingElement binomial(int top, int bottom) const;
  ERingElement multinomial(const ERingElement a, const int *exptop, const int *expbottom) const;
  bool increment(int *current_derivative, const int *top_derivative) const;

  bool divides(const int *expbottom, const int *exptop) const;
  ERingElement diff_coefficients(const ERingElement c, const int *derivatives, const int *exponents) const;

  epoly * weyl_diff(
	  const ERingElement c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const epoly *g) const;  // An entire polynomial
  EVector weyl_diff(
	  const ERingElement c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const EVector &g) const;  // An entire polynomial
  EVector weyl_diff(
	  const EFreeModule *resultF,
	  const ERingElement c,
	  const int *expf,  // The exponent vector of f
	  int component,
	  const int *derivatives, 
	  const epoly *g) const;  // An entire polynomial

  virtual epoly * mult1(
    const epoly *f, // single term
    const epoly *g  // entire polynomial
    ) const;
  virtual EVector mult1(
    const EFreeModule *resultF,
    const epoly *f,    // single term
    const EVector &g   // entire vector
    ) const;
  virtual EVector mult1(
    const EFreeModule *resultF,
    const evec *f,   // single term
    const epoly *g   // entire polynomial
    ) const;

public:
  static EWeylAlgebra *make(const ERing *KK, 
                            const ECommMonoid *MM, 
                            const EPolynomialRing *ZD,
                            int *degs,  // grabbed, length not checked
			    int npairs,
			    const int *derivative, const int *commutative);

  static EWeylAlgebra *make(const ERing *KK, 
                            const ECommMonoid *MM, 
                            const EPolynomialRing *ZD,
                            int *degs,  // grabbed, length not checked
			    int npairs,
			    const int *derivative, const int *commutative,
			    int homog_var);

  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;
  static EWeylAlgebra *binary_in(istream &i);

  virtual const EWeylAlgebra *toEWeylAlgebra() const { return this; }

  class_identifier class_id() const { return CLASS_EWeylAlgebra; }
};
//////////////////////////////////////////////////////////////////////////
class ESkewCommPolynomialRing : public ECommPolynomialRing
{
private:
  int exp_skew_vars(const int *exp, int *result) const;
  int skew_vars(const monomial *m, int *result) const;
  int skew_mult_sign(const monomial *m, const monomial *n) const;
#if 0
  EVector skew_mult1(
    const EFreeModule *resultF,
    bool component_from_f,
    const evec *f,			// A single term, either in the ring, or in resultF.
    const EVector &g) const;
#endif
  int *skew_mvars;		// These are here to cut down on alloc/dealloc
  int *skew_nvars;		// They are used exclusively in skew_mult_sign.
protected:
  // Variables for implementing skew multiplication
  int nskew;			// Number of skew commutative variables
  bool *skewvars;		// 0..nvars-1: skewvars[v] = 1 iff v skew commutes with other variables
  int *skewlist;		// 0..nskew-1: skewlist[i] = (skew var in range 0..nvars-1)

  ESkewCommPolynomialRing() {}
  void initialize(const ERing *KK, 
             const ECommMonoid *MM, 
  	     const EPolynomialRing *ZD,
	     int *degs, // grabbed
             int nskew, 
             int *skew);
  virtual ~ESkewCommPolynomialRing();

  virtual epoly *mult1(const epoly *a, // single term
		       const epoly *b  // entire polynomial
		       ) const;
  virtual EVector mult1(
    const EFreeModule *resultF,
    const epoly *f,    // single term
    const EVector &g   // entire vector
    ) const;
  virtual EVector mult1(
    const EFreeModule *resultF,
    const evec *f,   // single term
    const epoly *g   // entire polynomial
    ) const;
public:
  static ESkewCommPolynomialRing *
     make(const ERing *KK, 
                          const ECommMonoid *MM, 
  	                  const EPolynomialRing *ZD,
	                  int *degs, // grabbed
                          int nskew, 
                          int *skew);

  virtual EVector vec_term(const EFreeModule *F, const ERingElement a, const monomial *m, int x) const;
  //  virtual EVector vec_mult(const EVector &f, const EVector &g, bool component_from_f) const;

  virtual void text_out(buffer &o) const;

  virtual const ESkewCommPolynomialRing *toESkewCommPolynomialRing() const { return this; }

  class_identifier class_id() const { return CLASS_ESkewCommPolynomialRing; }
};
//////////////////////////////////////////////////////////////////////////
class ENCPolynomialRing : public EPolynomialRing
{
private:
protected:
  const ENCMonoid *M;
  ENCPolynomialRing() {}
  void initialize(const ERing *KK, 
             const ENCMonoid *MM,
             const EPolynomialRing *ZD,
             int *degs  // grabbed, length not checked
             );
  virtual ~ENCPolynomialRing();
public:
  static ENCPolynomialRing *make(const ERing *KK, 
                    const ENCMonoid *MM,
                    const EPolynomialRing *ZD,
                    int *degs  // grabbed, length not checked
                    );

  virtual const EMonoid *getMonoid() const { return M; }
  virtual void text_out(buffer &o) const;

  virtual const ENCPolynomialRing *toENCPolynomialRing() const { return this; }

  class_identifier class_id() const { return CLASS_ENCPolynomialRing; }
};
#if 0
//////////////////////////////////////////////////////////////////////////
class QuotientPolynomialRing : public ECommPolynomialRing
{
private:
  QuotientPolynomialRing(const ECommPolynomialRing *RR, const Ideal *II);
protected:
  const ECommPolynomialRing *coverR;
  int nideal;			// # generators in GB of quotient ideal
  EVector **gens;			// these generators
  MonomialDivisionTable *lookup;
public:
  static ECommPolynomialRing *make(const ECommPolynomialRing *RR, const Ideal *II);
};
//////////////////////////////////////////////////////////////////////////
class QuotientWeylAlgebra : public EWeylAlgebra
{
private:
  QuotientWeylAlgebra(const EWeylAlgebra *RR, const Ideal *II);
public:
  static QuotientWeylAlgebra *make(const EWeylAlgebra *RR, const Ideal *II);
};
//////////////////////////////////////////////////////////////////////////
class QuotientSkewCommutativePolynomialRing : public ESkewCommPolynomialRing
{
private:
  QuotientSkewCommutativePolynomialRing(const ESkewCommPolynomialRing *RR,
					const Ideal *II);
public:
  static QuotientSkewCommutativePolynomialRing *make(const ESkewCommPolynomialRing *RR,
						     const Ideal *II);
};
//////////////////////////////////////////////////////////////////////////
class QuotientNCPolynomialRing : public ENCPolynomialRing
{
private:
  QuotientNCPolynomialRing(const ENCPolynomialRing *RR,
			   const Ideal *II);

public:
  static QuotientNCPolynomialRing *make(const ENCPolynomialRing *RR,
					const Ideal *II);
};
//////////////////////////////////////////////////////////////////////////
#endif
#endif
