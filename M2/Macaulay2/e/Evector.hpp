// Copyright 1998 by Michael Stillman
#ifndef __Evector_hpp_
#define __Evector_hpp_

#include "Ering.hpp"
#include "Efreemod.hpp"

class EVectorHeap;
class ESortColumnAlgorithm;
class object_EVector;

// EVector -- the class of vectors, over a ring, or also a polynomial ring.
// Routines used from the ring class:
//   vec_new_term: create a new term
//   vec_remove_term
//   vec_copy_term
//   vec_terms_equal
//   vec_terms_compare
//   vec_make(F,a,x)
//   vec_term(F,a,m,x) -- only for vectors over polynomial rings
// and also
//   vec_add_to
//   vec_left_mult
//   vec_right_mult

class EVector
{
  class iterator;
  friend class object_EVector;
  friend class iterator;
  friend class EVectorHeap;
  friend class ESortColumnAlgorithm;
  friend class ERing;
  friend class EPolynomialRing;
  friend class EWeylAlgebra;
  friend class ESkewCommPolynomialRing;

  friend class EFreeModule;
  friend class ERingMap;

  const EFreeModule *F;
  int len;
  evec *elems;
protected:
  EVector diff(const int *exponents, bool use_coeffs) const;/*TODO*/
public:
  // These two routines should be used with care...
  void prepend_term(evec *t);
  void sort();

public:
  EVector();
  EVector(const EFreeModule *F, int len, evec *elems);
  EVector(const EFreeModule *F, evec *elems);
  EVector &operator=(const EVector &v);
    // Destructively copy v to this: the value of 'this' is not considered.
    // And 'v' is set to 0.  This routine has been made constant since one wants
    // to be able to do, e.g. v = w.add(w2).
    // DO NOT use 'v' after such an operation.  Use 'clone' instead.
  EVector(const EVector &v) { *this = v; }  // NOT const at all!!!

  EVector clone() const;
  ~EVector();
    // Reset the vector to the zero vector (i.e. free 'elems', but don't do anything 
    // to the free module).
  void reset();
    // Sets the vector to zero: frees 'elems'.
    
  const EFreeModule *getFreeModule() const { return F; }

  const ERing *getRing() const { return F->getRing(); }
  
  bool leadCoefficient(ERingElement &a) const
    // If non-zero, return true and set a; otherwise return false
    {if (len == 0) return false; else {a = elems->coeff; return true;}}

  const monomial *leadMonomial() const
    {if (len == 0) return 0; else return elems->monom;}

  int leadComponent() const
    {if (len == 0) return 0; else return elems->component; }

  bool isZero() const 
    {return len == 0;}

  bool isEqual(const EVector &w) const;
  bool entriesEqual(const EVector &w) const;/*TODO*/

  void negateTo();
  void addTo(EVector &w);
  void subtractTo(EVector &w);
  EVector multiply_by_ZZ(int a) const;
  EVector leftMultiply(const ERingElement a) const;
  EVector rightMultiply(const ERingElement a) const;

  EVector negate() const;
  EVector add(const EVector &g) const;
  EVector subtract(const EVector &g) const; // return this - g

  /////////////////////////////////////
  // Obtaining parts of a vector //////
  /////////////////////////////////////

  int nTerms() const {return len;}
    // Number of monomials in the vector.
    
  EVector leadTerm(int n=-1,bool only_same_component=true) const;
    // Yields the sum of the terms which are maximal using the first n parts
    // of the monomial order (-1 means use the entire order).
    // if only_same_component is true, returns the sum of the first terms
    // with the same component as the lead term.
    
  ERingElement getComponent(int x) const;
    // Yields the sum of all terms with component x.
  
  EVector getTerms(int lo, int hi) const;
    // Yields the (sum of the) monomials in the range lo..hi of this.
    // 0 refers to the first term.  Negative values count from the end:
    // -1 is the last monomial, -2 the second to last, etc.  0..-1 refers
    // to the entire vector.
  

  ERingElement diff_term(const monomial *d, 
			 const monomial *m, 
			 const monomial *& result, 
			 bool use_coeff) const;
  EVector diff_by_term(const EFreeModule *resultF,
		       const evec *p,
		       bool use_coeffs) const;
  EVector diff(const EFreeModule *resultF, const EVector &w, bool use_coeffs) const;

  bool inSubring(int n=1) const;
    // Yields true iff the lead monomial has value 'zero' for the
    // first 'n' parts of the monomial order.
    

  // Degrees
  bool isGraded(monomial *&result_degree) const;
  const monomial *degree() const;
  void degreeLoHi(const monomial *&lo, const monomial *&hi) const;
  void degreeWeightsLoHi(int i, int &lo, int &hi) const;
  void degreeWeightsLoHi(int nwts, const int *wts, 
                         const int * componentdegs, 
                         int &lo, int &hi) const;
  void degreeWeightsLoHi(int nwts, const int *wts, 
                         int &lo, int &hi) const;

  bool homogenize(int v, int d, int nwts, const int *wts, EVector &result) const;
  bool homogenize(int v, int nwts, const int *wts, EVector &result) const;
  
  // Routines for vectors over polynomial rings
  ERingElement coefficientOf(monomial *m,  int x) const;/*TODO*/
    // Yields (a copy of) the coefficient of m*e_x in this.
  
  EVector coefficientOf(int var, int exponent) const;/*TODO*/
    // Yields the coefficient of (var)^exponent
    // in 'this'.  Mostly useful in the commutative case.
  
  ERingElement leadCoefficientOf() const;/*TODO*/
    // Yields the coefficient of (lead variable)^(lead exp).  

  int degreeInVariable(int var) const;/*TODO*/
    // Yields the degree of this in the variable var'.

  EVector divideByVariables(int *vars, int *&monom, const int *maxd=0) const;
    // Divide the vector by the variables v s.t. vars[v] > 0, by at most
    // the exponent maxd[v].  Place the amount removed in the exponent vector
    // 'monom'.  Each of these three vectors should be indexed 0..nvars-1.
    // If maxd is not given, then divide by the variables as much as possible.
    // WHAT ABOUT NON-COMMUTATIVE CASE??


  ////////////////////////////////
  // Routines which MODIFY this //
  ////////////////////////////////

  EVector strip_vector(const bool *vars, 
		       const EFreeModule *Fmonom, 
		       EVector &vmonom);

    // Only valid for commutative polynomial rings.  'vars' is an array 0..nvars-1.
    // The vector 'this' has that part of itself which agrees in the 'vars' variables
    // with the lead term of 'this', split off: the monomial vmonom is set, and the 
    // coefficient of this monomial in the vector is returned as the result.

  ///////////////////////////////////
  // Routines useful with matrices //
  ///////////////////////////////////

  EVector translate(const EFreeModule *newF) const;
  EVector componentShift(const EFreeModule *newF, int r) const;
  EVector tensorShift(const EFreeModule *newF, int n, int m) const;
  EVector tensor(const EFreeModule *newF, const EVector &w) const;
  EVector subvector(const EFreeModule *newF, const intarray &r) const;

  EVector translate(const EFreeModule *newF, int newcomponent) const;  // Set all components to 'newcomponent'

  // I/O
  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;
  ostream &binary_out(ostream &o) const;

  static EVector elem_binary_in(istream &i);

  /////////////////////////////////////////
  // Iterator over elements of a vector ///
  /////////////////////////////////////////
  class iterator {
    evec *p;
  public:
    iterator(const EVector &v) : p(v.elems) {}
    iterator() {}

    bool valid() { return p != 0; }
    void operator++() { p = p->next; }

    evec * operator*() { return p ; }
    evec * operator->() { return p; }
#if 0    
    const ERingElement &getCoefficient() const { return p->coeff; }
    int getComponent() const { return p->component; }

    const monomial *getMonomial() const { return p->monom; }
      // Do not access the monomial if this is not a polynomial ring.
#endif
  };


  //////////////////////////
  // Creation of a vector //
  //////////////////////////
  class collector {
    const EFreeModule *F;
    int len;
    evec head;
    evec *last;
  public:
    collector(const EFreeModule *F) : F(F), len(0), last(&head) {}
    ~collector() {}
    
    void append(evec *a) { 
      last->next = a; 
      len++; 
      last = a; 
    }
    void append_final_terms(evec *a) {
      last->next = a; 
      last = 0; 
      for ( ; a != 0; a=a->next) len++;
    }

    EVector value() { 
      if (last != 0) last->next = 0; 
      evec *elems = head.next; 
      head.next = 0;
      return EVector(F,len,elems);
    }
    
    EVector sortedValue() {
      EVector result = value();
      result.sort();  // Also apply normal form??
      return result;
    }
  };

};

  /////////////////////////////////////
  // Heap class for faster additions //
  /////////////////////////////////////
  class EVectorHeap {
    const EFreeModule *F;	// Our elements will be vectors in here
    EVector heap[GEOHEAP_SIZE];
    int top_of_heap;
  public:
    EVectorHeap(const EFreeModule *F);
    ~EVectorHeap();
    void reset();
  
    void add(EVector &G);
    void add(evec *tm);
    bool getLeadTerm(const ERingElement &coeff, const monomial *&monom); // returns false if no more.
				// Does NOT remove this monomial from the heap.
    EVector value();
  };

class object_EVector : public object_element
{
  EVector *val;
public:
  object_EVector(EVector &v) { 
    val = new EVector(v); 
    v.elems = 0;
    v.len = 0;
    bump_up(val->getFreeModule()); 
  }
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
#if 0
class EMutableVector : public EVector
{
  EVector freeze() { EVector result = *this; elems=0; len=0; F=0; return result; }
  
  void prepend_term(evec *a);
  void sort();
};

#endif
#endif
