// Copyright 1998 by Michael Stillman
#ifndef __EZZp_hpp_
#define __EZZp_hpp_

#include "Edefs.hpp"
#include "Emonoid.hpp"
class EZZ;
class EZZp;

class monomial;
typedef long ZZ;
struct epoly;
union ERingElement
{
  int int_val;
  epoly * poly_val;
  ZZ ZZ_val;
};

struct epoly
{
  epoly *          next;
  ERingElement     coeff;
  const monomial * monom;
};

struct evec0
{
  evec0 *          next;
  int              component;
  ERingElement     coeff;
};
struct evec
{
  evec *           next;
  int              component;
  ERingElement     coeff;
  const monomial * monom;
};


inline ZZ ZZVAL(ERingElement a) { return a.ZZ_val; }
inline ERingElement ZZ_TO_ERingElement(ZZ a)
  { ERingElement result; result.ZZ_val = a; return result; }

inline int ZZPVAL(ERingElement a) { return a.int_val; }
inline ERingElement ZZP_TO_ERingElement(int a)
  { ERingElement result; result.int_val = a; return result; }

inline epoly *POLYVAL(ERingElement a) { return a.poly_val; }
inline ERingElement POLY_TO_ERingElement(epoly *a)
  { ERingElement result; result.poly_val = a; return result; }


extern const EZZ * EZ;
class ERing : public type
{
protected:
  static stash *vec_stash;  // Set to the desired size...
  static stash *vecpoly_stash;
  static ECommPolynomialRing *_trivial;
protected:
  const ERing *K;    // Either 'this' or the coefficient ring.
                     // Ths is the ring type stored in each term of a vector.
                     // Typically, this will be set in 'make'.
  int nvars;
  int totalvars;

  stash *evec_stash;  // One of vec_stash, vecpoly_stash.

  // Degree information
  const ECommMonoid *D;		// Degree Monoid
  const EPolynomialRing *ZD;	// Usually the ring ZZ[D], although a different base may be used.
  int *_degrees;
  
  // GB information
  const ERing *GBring;  // For polynomial rings over ZZ,ZZp, GBring is 'this'
                        // Otherwise, it is a 'flattened' ring.
                        // Probably NEED ringmaps to/from as well.
  const EPolynomialRing *_cover;
    // If this is a polynomial ring, and a quotient, 
    // _cover is this polynomial ring (with no quotient).
    // Otherwise, it is 0.

public:
  ERing() {}
  void initialize(int nvars, 
        int totalvars, 
        const ERing *K,
        const EPolynomialRing *ZZDD, // if 0, set to the trivial poly ring ZZ[].
        int *_degs   // grabbed
        );
  void setGBring(const EPolynomialRing *GBR);
  virtual ~ERing();

  // Each ring type should have the following routines defined:
  // Only constructor is the default one: do nothing.
  // void initialize(...) <-- NOT VIRTUAL!!
  // static T *make(...);

  const EPolynomialRing *getCover() const
    { return _cover; }  

  EFreeModule *makeFreeModule(int rank) const; // Make a free module over this ring.
  EFreeModule *makeFreeModule(int rank, const monomial **degrees) const; // Make a free module over this ring.

  const ERing *getCoefficientRing() const 
    { return K; }

  const EMonoid *getDegreeMonoid() const
    { return D; }
  
  int n_degrees() const
    { return getDegreeMonoid()->n_vars(); }

  const int *getDegreeVector(int i) const;

  int n_vars() const
    { return nvars; }
  
  int total_n_vars() const
    { return totalvars; }

  const ERing * getVectorCoefficientRing() const 
    { return K; }
      
  virtual void text_out(buffer &o) const = 0;
  virtual void bin_out(buffer &o) const = 0;
  virtual int characteristic() const = 0;

  virtual ERingElement from_int(int n) const = 0;
  virtual ERingElement zero() const = 0;
  virtual ERingElement one() const = 0;

  virtual void remove(ERingElement a) const = 0;
  virtual ERingElement clone(ERingElement a) const = 0;
  virtual ERingElement negate(ERingElement a) const = 0;
  virtual void negate_to(ERingElement &a) const = 0;
  virtual void add_to(ERingElement &a, ERingElement &b) const = 0;
  virtual ERingElement add(ERingElement a, ERingElement b) const = 0;
  virtual ERingElement subtract(ERingElement a, ERingElement b) const = 0;
  virtual ERingElement mult_by_ZZ(ERingElement a, int n) const = 0;
  virtual ERingElement mult(ERingElement a, ERingElement b) const = 0;
  virtual ERingElement divide(ERingElement a, ERingElement b) const = 0;
  virtual ERingElement power(ERingElement a, int n) const = 0;
  virtual bool is_zero(ERingElement a) const = 0;
  virtual bool is_equal(ERingElement a, ERingElement b) const = 0;
  virtual void elem_text_out(buffer &o, ERingElement a) const = 0;
  virtual void elem_bin_out(buffer &o, ERingElement a) const = 0;

  virtual void degreeWeightsLoHi(ERingElement f, int nwts, const int *wts,
                          int &lo,
                          int &hi) const;
  void degreeLoHi(ERingElement f, const monomial *&lo, const monomial *&hi) const;
  const monomial * degree(ERingElement f) const;
  bool isGraded(ERingElement f, const monomial *&deg) const;
  virtual ERingElement evaluate(const ERingMap *map, const ERingElement r) const = 0;

  // In each of the four routines below, if the two rings are identical, 'false' is returned.

  virtual bool promote(const ERing *Rf, const ERingElement f, ERingElement &result) const = 0;
  // If there is a 'natural' map Rf --> A=this, and f in Rf; then place its image in A 
  // into 'result', and return true.  Otherwise return false.
  virtual bool lift(const ERing *Rg, const ERingElement f, ERingElement &result) const = 0;
  // If there is a 'natural' map Rg --> A=this, and f is in A; place a (hopefully natural)
  // inverse image (element of Rg) into 'result', and return true.  Otherwise return false.
  virtual bool vec_promote(const EVector &v, const EFreeModule *resultF, EVector &result) const = 0;
  // If there is a 'natural' map Rf --> A=this, and v is a vector over Rf, return its image in 
  // 'resultF', which should be a free module of the same rank as v's, and whose base ring is A.
  // Return true in this case, otherwise return false.
  virtual bool vec_lift(const EVector &v, const EFreeModule *resultF, EVector &result) const = 0;
  // If there is a 'natural' map Rg --> A=this, and v is a vector over A, return a preimage
  // of v in the free module 'resultF', which should be a freemodule over Rg, of the same rank as
  // the free module of v.

  // Vector operations
  evec *vec_new_term() const;  // Virtual not needed here
  void vec_remove_term(evec *t) const;
  virtual evec *vec_copy_term(const evec *t) const;
  virtual bool vec_terms_equal(const evec *s, const evec *t) const;
  virtual ERingElement vec_term_to_ring(const evec *v) const;
  virtual int vec_compare_terms(const evec *v, const evec *w) const;
  virtual ERingElement vec_component(const EVector &v, int x) const;
  virtual void vec_degree_lohi(const evec *v, int nwts, const int *wts,
                               int &lo, int &hi) const;
  
  virtual EVector vec_make(const EFreeModule *F, ERingElement c, int x) const;
  virtual void vec_add_to(EVector &a, EVector &b) const;
  virtual EVector vec_left_mult(const ERingElement a, const EVector &b) const;
  virtual EVector vec_right_mult(const EVector &a, const ERingElement b) const;
  virtual void vec_text_out(buffer &o, const EVector &v) const;
  virtual void vec_bin_out(buffer &o, const EVector &v) const;

  virtual EVector vec_evaluate(const ERingMap *map, const EFreeModule *Ftarget, const EVector &v) const;
#if 0
  int n_terms(const evec *v) const;
  EVector clone(const EVector &a) const;
  void remove(EVector &a) const;
  bool is_zero(const EVector &a) const;
  bool is_equal(const EVector &a, const EVector &b) const;
  void negate_to(const EVector &a) const;
  EVector negate(const EVector &a) const;
  EVector add(const EVector &a, const EVector &b) const;
  EVector subtract(const EVector &a, const EVector &b) const;
  EVector zero_vector(const EFreeModule *F) const;
  EVector basis_element(const EFreeModule *F, int x) const;

  virtual void vector_text_out(buffer &o, const EVector &a) const;
  virtual void vector_bin_out(buffer &o, const EVector &a) const;
  virtual EVector make_vector(const EFreeModule *F, const EVector * &vecs) const = 0;
  virtual EVector make_sparse_matrix(const EFreeModule *F,
                                     const EVector * & elems,
                                     const intarray &rows) const = 0;

  virtual EVector make_vector(const EFreeModule *F, const vector<EVector> &vecs) const = 0;
  virtual EVector make_sparse_matrix(const EFreeModule *F,
                                     const vector<EVector> & elems,
                                     const vector<int> &rows) const = 0;
    // In above two routines,
    // Should this be const EVector * ??  In other words, do we grab the EVector?
                                     
  virtual EVector random(const EFreeModule *F) const = 0;
  
  virtual EVector divide(const EVector &a, const ERingElement b) const = 0;
  
  virtual int n_terms(const EVector &a) const = 0;
  virtual ERingElement get_component(const EVector &a, int x) const = 0;
  virtual EVector lead_term(const EVector &a, int n, bool same_component_only=true) const = 0;
  virtual EVector get_terms(const EVector &a, int lo, int hi) const = 0;

  virtual bool isGraded(const EVector &a, monomial *&result_degree) const;
  virtual monomial *degree(const EVector &a) const;
  virtual void degreeLoHi(const EVector &a, monomial *&lo, monomial *&hi) const;
  virtual void degreeWeightsLoHi(const EVector &a, int i, int &lo, int &hi) const;
  virtual void degreeWeightsLoHi(const EVector &a,
                         const int *wts, 
                         const int * componentdegs, 
                         int &lo, int &hi) const;
  virtual void degreeWeightsLoHi(const EVector &a,
                         const int *wts, 
                         int &lo, int &hi) const;

  virtual EVector homogenize(const EVector &a, int v, int d, const int *wts) const;
  virtual EVector homogenize(const EVector &a, int v, const int *wts) const;

  virtual EVector eval(const ERingMap *map, const EFreeModule *Fnew, const EVector &v) const;
  virtual bool promote(const EFreeModule *Fnew, const EVector &v, EVector &result) const;
  virtual bool lift(const EFreeModule *Fnew, const EVector &v, EVector &result) const;
  
  // Vector operations for matrices
  virtual EVector subvector(const EFreeModule *newF, 
                            const EVector &a,
                            const intarray &r) const = 0;
  virtual EVector translate(const EFreeModule *newF, const EVector &a) const = 0;
  virtual EVector component_shift(const EFreeModule *newF,
                                  const EVector &a,
                                  int r) const = 0;
  virtual EVector tensor_shift(const EFreeModule *newF,
                               const EVector &a,
                               int n, int m) const = 0;
  virtual EVector tensor(const EFreeModule *newF,
                         const EVector &a,
                         const EVector &b) const = 0;
  virtual EVector *transpose(const EFreeModule *newTarget,
                             const vector<EVector> &columns) const = 0;
  virtual EVector multiplyByMatrix(const vector<EVector> &columns, const EVector &a) const = 0;
                        
  // Vector polynomial operations
  virtual monomial * lead_monomial(const EVector &a) const = 0;
  virtual bool coefficient_of(const EVector &a, 
                              const monomial *m, 
                              int x,
                              ERingElement &result) const = 0;

  virtual EVector diff(const int *exponents, 
                       const EVector &a, 
                       bool do_contract=false) const = 0;                            
  
  // Missing so far:
  // matrix routines: koszul, coeffs (maybe more?)
  
  // Mutable Vector operations
  virtual void sort(EVector &a) const = 0;
  virtual void prepend_term(EVector &a, 
                            ERingElement c, 
                            int x) const = 0;
  virtual void prepend_term(EVector &a, 
                            ERingElement c,
                            const monomial *m, 
                            int x) const = 0;
#endif  

  virtual ERing * cast_to_ERing() { return this; }
  virtual const ERing * cast_to_ERing() const { return this; }
  type_identifier  type_id () const { return TY_ERing; }
  const char * type_name   () const { return "ERing"; }
  
  // Here we put cast routines for each kind of ring.
  // These may be used to determine whether, e.g. a ring is a polynomial ring.
  virtual EZZ *to_EZZ() { return 0; }
  virtual const EZZ *to_EZZ() const { return 0; }
  
  virtual EZZp *to_EZZp() { return 0; }
  virtual const EZZp *to_EZZp() const { return 0; }

  virtual const ENCPolynomialRing *toENCPolynomialRing() const { return 0; }
  virtual const EPolynomialRing *toPolynomialRing() const { return 0; }
  virtual const ECommPolynomialRing *toCommPolynomialRing() const { return 0; }
  virtual const EWeylAlgebra *toEWeylAlgebra() const { return 0; }
  virtual const ESkewCommPolynomialRing *toESkewCommPolynomialRing() const { return 0; }

  virtual EPolynomialRing * cast_to_EPolynomialRing() { return 0; }
  virtual const EPolynomialRing * cast_to_EPolynomialRing() const { return 0; }
};

class EZZ : public ERing
{
  static EZZ *_ZZ;
public:
  EZZ() {}
  void initialize() { ERing::initialize(0,0,this,0,0); }
  virtual ~EZZ() {}

  static const EZZ *make() { 
    if (_ZZ == 0) {
      _ZZ = new EZZ; 
      _ZZ->initialize();
    }
    return _ZZ; 
  }
  
  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;
  // static EZZ *binary_in(istream &i);

  virtual int characteristic() const
    { return 0; }

  virtual class_identifier class_id() const { return CLASS_EZZ; }
  virtual EZZ *to_EZZ() { return this; }
  virtual const EZZ *to_EZZ() const { return this; }

/////////////////////////////////////////////////////////////////////////
// Non-virtual functions which operate directly on elements of type ZZ // 
/////////////////////////////////////////////////////////////////////////
   
  void remove(ZZ a) const
    { }
     
  ZZ clone(ZZ a) const
    { return a; }

  ZZ _zero() const
    { return 0; }
    
  ZZ _one() const
    { return 1; }
    
  ZZ _from_int(int n) const
    { return n; }

  ZZ negate(ZZ a) const
    { return -a; }
    
  ZZ add(ZZ a, ZZ b) const
    { return a+b; }
    
  ZZ subtract(ZZ a, ZZ b) const
    { return a-b; }
    
  ZZ mult_by_ZZ(ZZ a, int n) const
    { return n*a; }
    
  ZZ mult(ZZ a, ZZ b) const
    { return a*b; }

  ZZ divide(ZZ a, ZZ b) const
    { return a/b; }
    
  ZZ power(ZZ a, int n) const
    { ZZ result = 1; for (int i=0; i<n; i++) result *= a; return result;}

  bool is_zero(ZZ a) const
    { return a == 0; }
    
  bool is_equal(ZZ a, ZZ b) const
    { return a == b; }

  void elem_text_out(buffer &o, ZZ a) const;
  void elem_bin_out(buffer &o, ZZ a) const;
  // ZZ elem_binary_in(istream &i) const;

  int compare(ZZ a, ZZ b) const
    { if (a>b) return GT; if (a<b) return LT; return EQ; }

/////////////////////////////
// Virtual Ring Operations //
/////////////////////////////
  virtual ERingElement from_int(int n) const 
    { return ZZ_TO_ERingElement(n); }
    
  virtual ERingElement zero() const
    { return ZZ_TO_ERingElement(0); }
    
  virtual ERingElement one() const
    { return ZZ_TO_ERingElement(1); }

  virtual void remove(ERingElement a) const
    { }

  virtual ERingElement clone(ERingElement a)  const
    { return a; }

  virtual ERingElement negate(ERingElement a) const 
    { return ZZ_TO_ERingElement(negate(ZZVAL(a))); }

  virtual void negate_to(ERingElement &a) const
    { a.ZZ_val = -a.ZZ_val; }

  virtual ERingElement add(ERingElement a, ERingElement b) const
    { return ZZ_TO_ERingElement(add(ZZVAL(a),ZZVAL(b))); }

  virtual void add_to(ERingElement &a, ERingElement &b) const
    { a.ZZ_val = a.ZZ_val + b.ZZ_val; }
    
  virtual ERingElement subtract(ERingElement a, ERingElement b) const
    { return ZZ_TO_ERingElement(subtract(ZZVAL(a),ZZVAL(b))); }

  virtual ERingElement mult_by_ZZ(ERingElement a, int n) const
    { return ZZ_TO_ERingElement(mult_by_ZZ(ZZVAL(a),n)); }

  virtual ERingElement mult(ERingElement a, ERingElement b) const
    { return ZZ_TO_ERingElement(mult(ZZVAL(a),ZZVAL(b))); }

  virtual ERingElement divide(ERingElement a, ERingElement b) const
    { return ZZ_TO_ERingElement(divide(ZZVAL(a),ZZVAL(b))); }

  virtual ERingElement power(ERingElement a, int n) const
    { return ZZ_TO_ERingElement(power(ZZVAL(a),n)); }

  virtual bool is_zero(ERingElement a) const
    { return is_zero(ZZVAL(a)); }

  virtual bool is_equal(ERingElement a, ERingElement b) const
    { return is_equal(ZZVAL(a),ZZVAL(b)); }

  virtual void elem_text_out(buffer &o, ERingElement a) const
    { elem_text_out(o, ZZVAL(a)); }

  virtual void elem_bin_out(buffer &o, ERingElement a) const
    { elem_bin_out(o, ZZVAL(a)); }

  virtual ERingElement evaluate(const ERingMap *map, const ERingElement r) const;
  virtual bool promote(const ERing *Rf, const ERingElement f, ERingElement &result) const;
  virtual bool lift(const ERing *Rg, const ERingElement g, ERingElement &result) const;
  virtual bool vec_promote(const EVector &v, const EFreeModule *resultF, EVector &result) const;
  virtual bool vec_lift(const EVector &v, const EFreeModule *resultF, EVector &result) const;
};

class EZZp : public ERing
{
  int P;
public:
  EZZp() {}
  void initialize(int p) { 
    ERing::initialize(0,0,this,0,0);
    P = p;
  }
  virtual ~EZZp() {}

  static EZZp *make(int p) { 
    EZZp *R = new EZZp();
    R->initialize(p);
    return R;
  }

  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;
  // static EZZp *binary_in(istream &i);

  virtual int characteristic() const { return P; }
  virtual class_identifier class_id() const { return CLASS_EZZp; }
  virtual EZZp *to_EZZp() { return this; }
  virtual const EZZp *to_EZZp() const { return this; }
  
//////////////////////////////////////////////////////////////////////////
// Non-virtual functions which operate directly on elements of type int // 
//////////////////////////////////////////////////////////////////////////

  void gcd_extended(int a, int b, int &u, int &v, int &g) const;

  int invert(int a) const
    { int u,v,g; gcd_extended(a,P,u,v,g); return u; }

  void remove(int) const 
    { }

  int clone(int a) const 
    { return a; }

  int _zero() const
    { return 0; }
    
  int _one() const
    { return 1; }
    
  int _from_int(int n) const
    { int c = (n % P); if (c < 0) c += P; return c; }

  int negate(int a) const
    { return P - a; }

  int add(int a, int b) const 
    { int c = a+b; if (c >= P) c -= P; return c; }

  int subtract(int a, int b) const
    { int c = a-b; if (c < 0) c += P; return c; }
    
  int mult_by_ZZ(int a, int n) const
    { return _from_int(n*a); }

  int mult(int a, int b) const 
    { return _from_int(a*b); }

  int divide(int a, int b) const
    { int c = invert(b); return mult(a,c); }

  // MES: REWRITE TO USE repeated squaring
  int power(int a, int n) const
    { int result = 1; 
      for (int i=0; i<n; i++) 
        result = mult(result,a); 
      return result;
    }

  bool is_zero(int a) const
    { return a == 0; }
    
  bool is_equal(int a, int b) const
    { return a == b; }

  void elem_text_out(buffer &o, int a) const;
  void elem_bin_out(buffer &o, int a) const;
  // int elem_binary_in(istream &i) const;

/////////////////////////////
// Virtual Ring Operations //
/////////////////////////////
  virtual ERingElement from_int(int n) const 
    { return ZZP_TO_ERingElement(_from_int(n)); }
    
  virtual ERingElement zero() const
    { return ZZP_TO_ERingElement(0); }
    
  virtual ERingElement one() const
    { return ZZP_TO_ERingElement(1); }

  virtual void remove(ERingElement a) const
    { }

  virtual ERingElement clone(ERingElement a)  const
    { return a; }

  virtual ERingElement negate(ERingElement a) const 
    { return ZZP_TO_ERingElement(negate(ZZPVAL(a))); }

  virtual void negate_to(ERingElement &a) const
    { a.int_val = P - a.int_val; }

  virtual void add_to(ERingElement &a, ERingElement &b) const
    { a.int_val = add(a.int_val, b.int_val); }
    
  virtual ERingElement add(ERingElement a, ERingElement b) const
    { return ZZP_TO_ERingElement(add(ZZPVAL(a),ZZPVAL(b))); }

  virtual ERingElement subtract(ERingElement a, ERingElement b) const
    { return ZZ_TO_ERingElement(subtract(ZZVAL(a),ZZVAL(b))); }

  virtual ERingElement mult_by_ZZ(ERingElement a, int n) const
    { return ZZ_TO_ERingElement(mult_by_ZZ(ZZVAL(a),n)); }

  virtual ERingElement mult(ERingElement a, ERingElement b) const
    { return ZZP_TO_ERingElement(mult(ZZPVAL(a),ZZPVAL(b))); }

  virtual ERingElement divide(ERingElement a, ERingElement b) const
    { return ZZP_TO_ERingElement(divide(ZZPVAL(a),ZZPVAL(b))); }

  virtual ERingElement power(ERingElement a, int n) const
    { return ZZP_TO_ERingElement(power(ZZPVAL(a),n)); }

  virtual bool is_zero(ERingElement a) const
    { return is_zero(ZZPVAL(a)); }

  virtual bool is_equal(ERingElement a, ERingElement b) const
    { return is_equal(ZZPVAL(a),ZZPVAL(b)); }

  virtual void elem_text_out(buffer &o, ERingElement a) const
    { elem_text_out(o, ZZPVAL(a)); }

  virtual void elem_bin_out(buffer &o, ERingElement a) const
    { elem_bin_out(o, ZZPVAL(a)); }

  virtual ERingElement evaluate(const ERingMap *map, const ERingElement r) const;

  virtual bool promote(const ERing *Rf, const ERingElement f, ERingElement &result) const;
  virtual bool lift(const ERing *Rg, const ERingElement g, ERingElement &result) const;
  virtual bool vec_promote(const EVector &v, const EFreeModule *resultF, EVector &result) const;
  virtual bool vec_lift(const EVector &v, const EFreeModule *resultF, EVector &result) const;
};

class object_ERingElement : public object_element
{
  const ERing *R;
  ERingElement val;
public:
  object_ERingElement(const ERing *R, ERingElement a) 
    : R(R), val(a) {
    bump_up(R);
  }
  ~object_ERingElement() { 
    R->remove(val);
    bump_down(R);
  }
  
  ERingElement getValue() const { return val; }
  const ERing *getRing() const { return R; }
  int length_of() const;
  
  operator ERingElement() { return val; }
  
  class_identifier class_id() const { return CLASS_ERingElement; }
  type_identifier  type_id () const { return TY_ERingElement; }
  const char * type_name   () const { return "ERingElement"; }

  void text_out(buffer &o) const {  R->elem_text_out(o,val); }
  void bin_out(buffer &o) const {  R->elem_bin_out(o,val); }

  object_ERingElement *cast_to_ERingElement() { return this; }
  const object_ERingElement * cast_to_ERingElement() const { return this; }
};

#endif
