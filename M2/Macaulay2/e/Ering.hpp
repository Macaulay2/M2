// Copyright 1998 by Michael Stillman
#ifndef __ring_hpp_
#define __ring_hpp_

#include "EZZp.hpp"
#include "Emonoid.hpp"

#define ERingElement EVector

struct poly
{
  poly *     next;
  const monomial * monom;
  int        component;
  field      coeff;
};

const int N_POLYS_PER_BLOCK = 1000;

struct block_polys
{
  block_polys *next;
  poly blocks[N_POLYS_PER_BLOCK];
};

class MonomialDivisionTable;
class Ideal;

class ECoefficientRing;
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

class EPolynomialRing : public type
{
  friend class ECommPolynomialRing;
  friend class EFreeModule;
  friend class EVector;
  friend class EMatrix;
private:
  EVector *mult1(const EFreeModule *resultF,
		bool component_from_f,
		const poly *f, 
		const poly *g) const;
protected:
  static EPolynomialRing *_trivial;
  const ECoefficientRing *K;

  // All free modules use the same storage for poly nodes.
  block_polys *polyblocks;
  poly *freelist;
  poly *new_block_terms() const;

  // Degree information
  const EMonoid *D;		// Degree Monoid
  const EPolynomialRing *ZD;	// Usually the ring ZZ[D], although a different base may be used.
  const int *_degrees;
  
  // Quotient information
  // We will put:
  //   bool is_quotient;
  //   EPolynomialRing *cover; or a function...
  //   an array of the set of elements
  //   a GB of these elements
  //   a monomial lookup table of these elements.

  EFreeModule *R1;

  EPolynomialRing();  // Only used for the trivial ring.
  EPolynomialRing(const ECoefficientRing *KK);
  EPolynomialRing(const ECoefficientRing *KK, 
                  const EPolynomialRing *ZZDD,
                  int * _degs  // grabbed
                  );
  virtual ~EPolynomialRing();
public:
  // Parts of the ring
  const ECoefficientRing *getCoefficientRing() const 
    { return K; }
  const virtual EMonoid *getMonoid() const = 0;
  const EFreeModule * getRingFreeModule() const 
    { return R1; }
  const EMonoid *getDegreeMonoid() const
    { return D; }
  
  int n_vars() const
    { return getMonoid()->n_vars(); }
  
  int n_degrees() const
    { return getDegreeMonoid()->n_vars(); }

  int characteristic() const 
    { return K->characteristic(); }

  const int *getDegreeVector(int i) const;
    
  static const EPolynomialRing *getTrivialRing();  // ZZ as a polynomial ring, trivial degree ring.
  
  // Quotient ring stuff
  bool isQuotient() const { return false; }
  
protected:
  // These are used for low-level creation/removal of polynomial nodes
  poly *newBlockOfTerms() const;
  void deleteTerm(poly *t) const;
  void delete_terms(poly *t) const;
  poly *newTerm() const;
  poly *copy_term(const poly *t) const;
  
  int n_terms(const poly *f) const;
  int add_to(poly *&f, poly *&g) const;
  
public:
  void addTo(EVector *&v, EVector *&w) const;
  EVector *multiply(const EVector *f, const EVector *v) const;
  EVector *rightMultiply(const EVector *v, const EVector *f) const;

  // Virtual operations for arithmetic
  virtual EVector *makeTerm(const EFreeModule *F, const field a, const monomial *m, int x) const;
  virtual EVector *mult(const EVector *f, const EVector *g, bool component_from_f) const;

  // Free modules
  virtual const EPolynomialRing *getCover() const;
  virtual EFreeModule *makeFreeModule(int rank) const; // Make a free module over this ring.
  virtual EFreeModule *makeFreeModule(int rank, const monomial **degrees) const; // Make a free module over this ring.
  virtual EFreeModule *makeFreeModule(int rank, 
                                      const monomial **degrees,  // grabbed
                                      const monomial **ordering, // grabbed
                                      int *tiebreaks) const;     // grabbed
  virtual EFreeModule *makeFreeModule(const EMatrix *m) const;
    // Return 'source m' but with a Schreyer order coming from the
    // lead terms of the matrix, and with the tiebreaks coming from
    // the components of the lead monomials.
  
  // Display
  virtual void elem_text_out(buffer &o, const ERingElement *f) const;
  virtual void text_out(ostream &o) const = 0;
  virtual void text_out(buffer &o) const = 0;
  virtual void binary_out(ostream &) const {}

  //static EPolynomialRing *binary_in(istream &i);

  // Query routines about what kind of ring we have

  // Casting down the heierarchy
  virtual const ENCPolynomialRing *toENCPolynomialRing() const;
  virtual const ECommPolynomialRing *toPolynomialRing() const { return 0; }
  virtual const EWeylAlgebra *toEWeylAlgebra() const { return 0; }
  virtual const ESkewCommPolynomialRing *toESkewCommPolynomialRing() const { return 0; }

  virtual EPolynomialRing * cast_to_EPolynomialRing() { return this; }
  virtual const EPolynomialRing * cast_to_EPolynomialRing() const { return this; }

  type_identifier  type_id () const { return TY_ERing; }
  const char * type_name   () const { return "ERing"; }
};

//////////////////////////////////////////////////////////////////////////
class ECommPolynomialRing : public EPolynomialRing
{
protected:
  const ECommMonoid *M;
  ECommPolynomialRing();  // Only used to create the trivial ring.
  ECommPolynomialRing(
      const ECoefficientRing *KK,
      const ECommMonoid *MM,
      const EPolynomialRing *ZD,
      int *degs);  // grabbed, length not checked
  virtual ~ECommPolynomialRing();
public:
  virtual const EMonoid *getMonoid() const
    { return M; }

  static ECommPolynomialRing *make(const ECoefficientRing *KK, 
                                   const ECommMonoid *MM,
                                   const EPolynomialRing *ZZDD,
                                   int *degrees  // Grabbed: this is an array of length MM->n_vars*ZZDD->n_vars;
                                   );

  static void initializeTrivialRing();
                          
  static ECommPolynomialRing *binary_in(istream &i);

  virtual const ECommPolynomialRing *toPolynomialRing() const { return this; }
  virtual const EWeylAlgebra *toEWeylAlgebra() const { return 0; }
  virtual const ESkewCommPolynomialRing *toESkewCommPolynomialRing() const { return 0; }

  virtual void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;
  virtual void binary_out(ostream &o) const;

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

  EWeylAlgebra(const ECoefficientRing *KK, 
               const ECommMonoid *MM, 
               const EPolynomialRing *ZD,
               int *degs,  // grabbed, length not checked
               int npairs, 
	       const int *derivative, const int *commutative,
	       bool is_homog, int homog_var);
  virtual ~EWeylAlgebra();

  void initialize();

  void extractDerivativePart(const int *exponents, int *result) const;
  void extractCommutativePart(const int *exponents, int *result) const;
  field binomial(int top, int bottom) const;
  field multinomial(const field a, const int *exptop, const int *expbottom) const;
  bool increment(int *current_derivative, const int *top_derivative) const;

  bool divides(const int *expbottom, const int *exptop) const;
  field diff_coefficients(const field c, const int *derivatives, const int *exponents) const;

  EVector *weyl_mult1(
    const EFreeModule *resultF,
    bool component_from_f,
    const poly *f,			// A single term, either in the ring, or in resultF.
    const poly *g) const;

  EVector *diff(const EFreeModule *resultF,
		bool component_from_f,
		const field c,
		const poly *f, // A single term
		const int *derivatives, 
		const poly *v) const;

public:
  static EWeylAlgebra *make(const ECoefficientRing *KK, 
                            const ECommMonoid *MM, 
                            const EPolynomialRing *ZD,
                            int *degs,  // grabbed, length not checked
			    int npairs,
			    const int *derivative, const int *commutative);

  static EWeylAlgebra *make(const ECoefficientRing *KK, 
                            const ECommMonoid *MM, 
                            const EPolynomialRing *ZD,
                            int *degs,  // grabbed, length not checked
			    int npairs,
			    const int *derivative, const int *commutative,
			    int homog_var);

  virtual EVector *mult(const EVector *f, const EVector *g, bool component_from_f) const;

  virtual void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;
  virtual void binary_out(ostream &o) const;
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

  EVector *skew_mult1(
    const EFreeModule *resultF,
    bool component_from_f,
    const poly *f,			// A single term, either in the ring, or in resultF.
    const poly *g) const;

  int *skew_mvars;		// These are here to cut down on alloc/dealloc
  int *skew_nvars;		// They are used exclusively in skew_mult_sign.
protected:
  // Variables for implementing skew multiplication
  int nskew;			// Number of skew commutative variables
  bool *skewvars;		// 0..nvars-1: skewvars[v] = 1 iff v skew commutes with other variables
  int *skewlist;		// 0..nskew-1: skewlist[i] = (skew var in range 0..nvars-1)

  ESkewCommPolynomialRing(const ECoefficientRing *KK, 
                          const ECommMonoid *MM, 
  	                  const EPolynomialRing *ZD,
	                  int *degs, // grabbed
                          int nskew, 
                          int *skew);
  virtual ~ESkewCommPolynomialRing();
public:
  static ESkewCommPolynomialRing *
     make(const ECoefficientRing *KK, 
                          const ECommMonoid *MM, 
  	                  const EPolynomialRing *ZD,
	                  int *degs, // grabbed
                          int nskew, 
                          int *skew);

  virtual EVector *makeTerm(const EFreeModule *F, const field a, const monomial *m, int x) const;
  virtual EVector *mult(const EVector *f, const EVector *g, bool component_from_f) const;

  virtual void text_out(ostream &o) const;
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
  ENCPolynomialRing(const ECoefficientRing *KK, 
                    const ENCMonoid *MM,
                    const EPolynomialRing *ZD,
                    int *degs  // grabbed, length not checked
                    );
  virtual ~ENCPolynomialRing();
public:
  virtual const EMonoid *getMonoid() const { return M; }
  static ENCPolynomialRing *make(const ECoefficientRing *KK, 
                    const ENCMonoid *MM,
                    const EPolynomialRing *ZD,
                    int *degs  // grabbed, length not checked
                    );

  virtual void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;

  virtual const ENCPolynomialRing *toENCPolynomialRing() const { return this; }

  class_identifier class_id() const { return CLASS_ENCPolynomialRing; }
};
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
