// Copyright 1998 by Michael Stillman
#ifndef __Evector_hpp_
#define __Evector_hpp_

#include "Ering.hpp"
#include "Efreemod.hpp"
class EVectorHeap;
const int GEOHEAP_SIZE = 15;

#define ERingElement EVector

class EVector
{
  friend class EPolynomialRing;
  friend class EWeylAlgebra;
  friend class ESkewCommPolynomialRing;

  friend class EFreeModule;
  friend class EVectorHeap;
  friend class EMatrix;
  
  const EFreeModule *F;
  int len;
  poly *elems;
protected:
  void prepend_term(poly *t);
  EVector *diff(const int *exponents, bool use_coeffs) const;/*TODO*/
public:
  void sort();

public:
  EVector *clone() const;
  ~EVector();

  const EFreeModule *getFreeModule() const { return F; }

  static EVector *zero(const EFreeModule *F);
  field leadCoefficient() const
    {if (len == 0) return F->getCoefficientRing()->zero(); else return elems->coeff;}

  const monomial *leadMonomial() const
    {if (len == 0) return 0; else return elems->monom;}

  int leadComponent() const
    {if (len == 0) return 0; else return elems->component; }

  bool isZero() const 
    {return len == 0;}

  bool isEqual(const EVector *w) const;
  bool entriesEqual(const EVector *w) const;/*TODO*/

  void addTo(EVector *&w);
  void subtractTo(EVector *&w);
  EVector *multiply_by_ZZ(int a) const;
  EVector *multiply(const EVector *a) const;
  EVector *rightMultiply(const EVector *a) const;

  EVector *negate() const;
  EVector *add(EVector *g) const;
  EVector *subtract(EVector *g) const; // return this - g

  /////////////////////////////////////
  // Obtaining parts of a vector //////
  /////////////////////////////////////

  int nTerms() const {return len;}
    // Number of monomials in the vector.
    
  EVector *leadTerm(int n=-1,bool only_same_component=true) const;
    // Yields the sum of the terms which are maximal using the first n parts
    // of the monomial order (-1 means use the entire order).
    // if only_same_component is true, returns the sum of the first terms
    // with the same component as the lead term.
    
  ERingElement *getComponent(int x) const;
    // Yields the sum of all terms with component x.
    // Each of these terms is given the component 0.
  
  EVector *getTerms(int lo, int hi) const;
    // Yields the (sum of the) monomials in the range lo..hi of this.
    // 0 refers to the first term.  Negative values count from the end:
    // -1 is the last monomial, -2 the second to last, etc.  0..-1 refers
    // to the entire vector.
  
  ERingElement *coefficientOf(monomial *m,  int x) const;/*TODO*/
    // Yields (a copy of) the coefficient of m*e_x in this.
  
  ERingElement *coefficientOf(int var, int exponent) const;/*TODO*/
    // Yields the coefficient of (var)^exponent
    // in 'this'.  Mostly useful in the commutative case.
  
  ERingElement *leadCoefficientOf() const;/*TODO*/
    // Yields the coefficient of (lead variable)^(lead exp).  

  int degreeInVariable(int var) const;/*TODO*/
    // Yields the degree of this in the variable var'.
    
  EVector *differentiate(const int *exponents) const
    {return diff(exponents,true);}

  EVector *contract(const int *exponents) const
    {return diff(exponents,false);}

  bool inSubring(int n=1) const;
    // Yields true iff the lead monomial has value 'zero' for the
    // first 'n' parts of the monomial order.
    
  EVector *divideByVariables(int *vars, int *&monom, const int *maxd=0) const;
    // Divide the vector by the variables v s.t. vars[v] > 0, by at most
    // the exponent maxd[v].  Place the amount removed in the exponent vector
    // 'monom'.  Each of these three vectors should be indexed 0..nvars-1.
    // If maxd is not given, then divide by the variables as much as possible.
    // WHAT ABOUT NON-COMMUTATIVE CASE??

  // Degrees
  bool isGraded(monomial *&result_degree) const;
  monomial *degree() const;
  void degreeLoHi(monomial *&lo, monomial *&hi) const;
  void degreeWeightsLoHi(int i, int &lo, int &hi) const;
  void degreeWeightsLoHi(const int *wts, 
                         const int * componentdegs, 
                         int &lo, int &hi) const;
  void degreeWeightsLoHi(const int *wts, 
                         int &lo, int &hi) const;

  EVector *homogenize(int v, int d, const int *wts) const;
  EVector *homogenize(int v, const int *wts) const;
  
  // I/O
  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;
  ostream &binary_out(ostream &o) const;

  static EVector *elem_binary_in(istream &i);
};

class EVectorHeap
{
  const EFreeModule *F;	// Our elements will be vectors in here
  const EPolynomialRing *R;
  const EMonoid *M;
  const ECoefficientRing *K;		// The coefficient ring
  EVector *heap[GEOHEAP_SIZE];
  int top_of_heap;
public:
  EVectorHeap(const EFreeModule *F);
  ~EVectorHeap();
  void reset();
  
  void add(EVector *G);
  bool getLeadTerm(const field &coeff, const monomial *&monom); // returns false if no more.
				// Does NOT remove this monomial from the heap.
  EVector *value();
};

class EVectorCollector
{
public:
  EVectorCollector(EFreeModule *F);
  ~EVectorCollector();

  void append(field a, monomial *m); // Eats a, m.
  void prepend(field a, monomial *m); // Eats a, m.
  EVector rawValue();
  EVector value();  // Sorts and combines the monomials, does normal form at the end.
};

class EVectorIterator
{
public:
  EVectorIterator(EVector v);
  ~EVectorIterator();

  bool valid();
  operator++();

  const ECoefficientRing *getCoefficient() const;
  const monomial *getMonomial() const;
  int getComponent() const;
};

class EVectorOperations
{
  
};

class object_EVector : public object_element
{
  EVector *val;
public:
  object_EVector(EVector *v) : val(v) { bump_up(val->getFreeModule()); }
  ~object_EVector() { 
    const EFreeModule *F = val->getFreeModule();
    delete val;
    bump_down(F);
  }
  
  EVector *getValue() { return val; }
  int length_of() const { return val->nTerms(); }
  
  class_identifier class_id() const { return CLASS_EVector; }
  type_identifier  type_id () const { return TY_EVector; }
  const char * type_name   () const { return "EVector"; }

  void text_out(buffer &o) const {  val->text_out(o); }
  void bin_out(buffer &o) const {  val->bin_out(o); }

  EVector * cast_to_EVector() { return val; }
  const EVector * cast_to_EVector() const { return val; }
};

#endif
