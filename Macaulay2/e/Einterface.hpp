// Copyright 1999  Michael E. Stillman
#ifndef _Einterface_hh_
#define _Einterface_hh_

#include "polyring.hpp"
#include "matrix.hpp"
#include "geovec.hpp"

extern "C" char system_interrupted;
extern int comp_printlevel;
extern "C" void system_spincursor(void);

// The following two are actually pointers to arrays of integers
#define monomial int
#define exponent_vector int

#define ringelement ring_elem
#define polynomial ring_elem
typedef vec term;
typedef vecterm * const const_term;
typedef vecHeap vector_heap;
//typedef vecterm * vector;
typedef const vecterm * const_vector;
typedef const FreeModule *freemodule;

#if 0
class vector_collector
{
  const freemodule F;
  vecterm head;
  vecterm *last;
public:
  vector_collector(const freemodule &F) : F(F)
    { head.next = 0; last = &head;
      printf("&head = %x, last = %x\n",&head,last); }
  void append(term &t)  // eats 't'
    { last->next = t; last = t;
      printf("&head = %x, head.next = %x, last = %x\n", &head, head.next, last);
     }
  vec value()
    { last->next = 0; vec result = head.next; head.next = last = 0; return result; }
};
#endif
class vector_collector
{
  const freemodule F;
  vecterm *first;
  vecterm *last;
public:
  vector_collector(const freemodule &F) : F(F)
    { first = last = 0; }
  void append(term &t)  // eats 't'
    { if (last == 0) { first = t; last = first; }
      else { last->next = t; last = t; }
     }
  vec value()
    { if (last == 0) return 0;
       last->next = 0;
       return first;
    }
};

class EInterface
{
  const PolynomialRing *R;
  const Ring *K;
  const Monoid *M;

  const PolynomialRing *Rquotient;
  const WeylAlgebra *RWeyl;

  ringelement one_;
  ringelement minus_one_;

  int nvars_;

public:
  void initialize(const Ring *RR);
  EInterface(const Ring *RR);
  ~EInterface() {}

  // Quotient ring operations
  bool ring_is_quotient() const;
  int n_quotients() const;
  polynomial get_quotient_element(int i) const;
  
  // Weyl algebra operations
  bool ring_is_weyl_algebra() const;

  /////////////////////////////////
  // Skew commutative operations //
  /////////////////////////////////
  bool ring_is_skew_commutative() const;

  int exp_skew_vars(const exponent_vector *exp, int *result) const;
      // The number s of skew variables in 'exp' is returned, and their
      // indices are placed in result[0], ..., result[s-1].

  int skew_divide_monomial(const monomial *m, const monomial *n,
			   monomial *result) const;

  int skew_monomial_syzygy(const exponent_vector *m, const exponent_vector *n,
			   exponent_vector *m1, exponent_vector *n1) const;

  int skew_mult_sign(const monomial *m, const monomial *n) const;

  ///////////////////////////////
  // Coefficient ring elements //
  ///////////////////////////////
  const ringelement &one() const;
  const ringelement &minus_one() const;
  ringelement copy_coefficient(const ringelement &a) const;
  void remove_coefficient(ringelement &a) const;
  ringelement negate_coefficient(const ringelement &a) const;
  void coefficient_syzygy(const ringelement &a,
			  const ringelement &b,
			  ringelement &a1,
			  ringelement &b1) const;
  ///////////////
  // monomials //
  ///////////////
  int n_vars() const;
  int compare_monomials(const monomial *a, const monomial *b) const;
  void to_exponents(const monomial *m, exponent_vector *&result) const;
  void divide_exponents(const exponent_vector *exp1,
			const exponent_vector *exp2,
			exponent_vector *result,
			int &sign) const;
  void exponent_syzygy(const exponent_vector *exp1,
		       const exponent_vector *exp2,
		       exponent_vector *result1,
		       exponent_vector *result2,
		       int &sign) const;

  void display_exponents(buffer &o, const exponent_vector *exp) const;

  /////////////////
  // polynomials //
  /////////////////
  const monomial *lead_monomial_of_polynomial(const polynomial &f) const;
  const ringelement &lead_coefficient_of_polynomial(const polynomial &f) const;

  ///////////
  // terms //
  ///////////
  const term &lead_term(const vec &v) const;
  const ringelement &term_coefficient(const term &t) const;
  const monomial *term_monomial(const term &t) const;
  int term_component(const term &t) const;

  /////////////
  // vectors //
  /////////////
  const monomial *lead_monomial(const vec &v) const;
  const ringelement &lead_coefficient(const vec &v) const;
  int lead_component(const vec &v) const;

  void remove_vector(const freemodule &F, vec &v) const;
  vec copy_vector(const freemodule &F, const vec &v) const;
  vec zero_vector(const freemodule &F) const;
  vec e_sub_i(const freemodule &F, int i) const;
  bool is_zero_vector(const freemodule &F, const vec &v) const;

  vec mult_by_term(const freemodule &F, 
		   const ringelement &coeff,
		   const exponent_vector *exp,
		   const vec &g) const;

  vec ring_mult_by_term(const freemodule &F, 
		   const ringelement &coeff,
		   const exponent_vector *exp,
		   int x,
		   const polynomial &g) const;

  void make_monic(const freemodule &F, const freemodule &Fsyz, 
		  vec &f, vec &fsyz) const;

  void auto_reduce(const freemodule &F,
		   const freemodule &Fsyz,
		   vec &f,
		   vec &fsyz,
		   const vec &g,
		   const vec &gsyz) const;

  void display_vector(buffer &o, const freemodule &F, const vec &f) const;

  ///////////////////////
  // Vector collecting //
  ///////////////////////
  // These collect terms IN DESCENDING MONOMIAL ORDER ONLY
  vector_collector start_collection(const freemodule &F) const;
  void append_to_collection(vector_collector &a, term &t) const;
  vec end_collection(vector_collector &a) const;

  /////////////////
  // Vector heap //
  /////////////////
  void start_heap(const freemodule &F, vector_heap &heap) const;
  vec end_heap(vector_heap &heap) const;
  void add_to_heap(vector_heap &heap, vec &f) const;
  bool get_lead_term_from_heap(vector_heap &h,
			       ringelement &coeff,
			       const monomial *&monom,
			       int component) const;
  bool remove_lead_term_from_heap(vector_heap &h, term &t) const;

  void add_multiple_to(vector_heap &h, 
		       const ringelement &a, 
		       const exponent_vector *m,
		       const vec &v) const;

  void add_ring_multiple_to(vector_heap &h, 
			    const ringelement &a, 
			    const exponent_vector *m,
			    int x,
			    const polynomial &r) const;

  void cancel_lead_terms(vector_heap &h, vector_heap &hsyz,
			 const ringelement &hcoefficient, 
			 const exponent_vector *hexponents,
			 const exponent_vector *gexponents,
			 const vec &g,
			 const vec &gsyz) const;
  // h -= c*hcoefficient*t*g, hsyz -= c*hcoefficient*t*gsyz, where
  // where leadterm(h) = hcoefficient*hlead*hcomponent, and 
  // leadterm(g) = gexponents*hcomponent, and
  // c in K, t is a monomial, such that in(h) = in(c*hcoefficient*t*g).

  void ring_cancel_lead_terms(vector_heap &h,
			      const ringelement &hcoefficient, 
			      const exponent_vector *hexponents,
			      int hcomponent,
			      const exponent_vector *gexponents,
			      const polynomial &g) const;
  // Set h -= c*hcoefficient*t*g*hcomponent  (IS THIS WHAT WE WANT IN GENERAL??)
  // where c in K, t a monomial, are chosen so that
  // in(h) = in(c*hcoefficient*t*g*hcomponent).
			       
  // Creation of an engine matrix
  Matrix make_matrix(freemodule F, array< vec > &columns) const;

};
// The following type contains all of the interface elements needed to
// the rest of the system, including polynomial/monoid/coefficient arithmetic
#if 0
class EGBinterface
{
public:

  // Ring informational
  bool is_weyl_algebra();
  bool is_quotient_ring();
  bool is_skew_commutative();
  bool ring_is_graded();
  bool coefficients_have_denominators();

  // Operations on ringelement's
  ringelement syz_coefficient(const ringelement a, const ringelement b, 
			      ringelement &a1,
			      ringelement &b1) const;
  bool is_zero_coefficient(ringelement a) const;
  bool is_equal_coefficient(ringelement a, ringelement b) const;

  // Operations on monomials
  int nvars;
  const int *heuristic;  // Array 0..nvars-1 of positive integers giving a
			 // "standard" degree for monomials.
  const int *standard;   // Array 0..nvars-1 of all 1's.
  int degree_monomial(const int *wts, const monomial m) const;
  monomial one_monomial;
  
  monomial lcm_monomial(const monomial a, const monomial b) const;
  monomial gcd_monomial(const monomial a, const monomial b) const;
  monomial syz_monomial(const monomial a, const monomial b,
			monomial &a1, monomial &b1) const; // returns the lcm

  bool is_one_monomial(const monomial a) const;
  bool is_equal_monomial(const monomial a, const monomial b) const;
  int compare_monomial(const monomial a, const monomial b) const;
    // returns EQ, LT, or GT
  monomial copy_monomial(const monomial a) const;
  void remove_monomial(monomial &a) const;

  monomial monomial_from_exponents(const int *exponents) const;
  monomial monomial_from_pairs(int npairs, const int *pairs) const;
  void copy_exponents_of_monomial(const monomial m, int *exponents) const;

  // Special operations for skew commuting variables
  
  // Operations on exponent vectors (of length 'nvars')...

  // Operations on terms
  term make_term(ringelement c, monomial m, int x) const;
  void remove_term(term &t) const;

  // Operations on vectors
  void remove_vector(const freemodule F, vector &a) const;
  vector copy_vector(const freemodule F, const vector a) const;
  vector e_sub_i(const freemodule F, int i) const;
  bool is_zero_vector(const vector a) const;
  bool is_equal_vector(const vector a, const vector b) const;

  const ringelement lead_coefficient(const vector a) const;
  const monomial lead_monomial(const vector a) const;
  int lead_component(const vector a) const;
  int size(const vector a) const;
  const term lead_term(const vector a) const;

  void negate_to(vector &a) const;
  vector multiply_by_term(const ringelement a, const monomial m, 
			  const vector v) const;
  vector multiply_by_term(const polynomial f, 
			  const ringelement a, const monomial m, int x) const;
  void normal_form(const freemodule F, vector &a);

  ringelement remove_content(vector &a) const;
  void divide_to(vector &a, const ringelement c) const;


  // vector heap operations
  vector_heap start_heap(const freemodule F) const;
  void add_to_heap(vector_heap &h, vector &v) const;
  const term get_lead_term(vector_heap &h) const;
  term remove_lead_term(vector_heap &h) const;
  vector heap_to_vector(vector_heap &h) const;
  
  // free module operations
  int degree(const freemodule F, int i) const;
  int rank(const freemodule F) const;
  
  // matrix creation operations
};
#endif

inline
bool EInterface::ring_is_quotient() const
{
  return Rquotient != 0;
}
inline
int EInterface::n_quotients() const
{
  return Rquotient->get_quotient_elem_length();
}
inline
ringelement EInterface::get_quotient_element(int i) const
{
  return Rquotient->get_quotient_elem(i);
}

inline
bool EInterface::ring_is_weyl_algebra() const
{
  return RWeyl != 0;
}

/////////////////////////////////
// Skew commutative operations //
/////////////////////////////////
inline
bool EInterface::ring_is_skew_commutative() const
{ 
  return M->is_skew();
}

inline
int EInterface::exp_skew_vars(const exponent_vector *exp, int *result) const
{
  return M->exp_skew_vars(exp,result);
}

inline
int EInterface::skew_divide_monomial(const monomial *m, const monomial *n,
				     monomial *result) const
{ 
  return M->skew_divide(m,n,result);
}

inline
int EInterface::skew_mult_sign(const monomial *m, const monomial *n) const
{ 
  return M->skew_mult_sign(m,n); 
}

inline
int EInterface::skew_monomial_syzygy(const exponent_vector *m, const exponent_vector *n,
				     exponent_vector *m1, exponent_vector *n1) const
{
  return 0;  // MES
}

inline
const ringelement &EInterface::one() const
{
  return one_;
}

inline
const ringelement &EInterface::minus_one() const
{
  return minus_one_;
}

inline
ringelement EInterface::copy_coefficient(const ringelement &a) const
{
  return K->copy(a);
}

inline
void EInterface::remove_coefficient(ringelement &a) const
{
  K->remove(a);
}

inline
ringelement EInterface::negate_coefficient(const ringelement &a) const
{
  return K->negate(a);
}

inline
void EInterface::coefficient_syzygy(const ringelement &a,
				    const ringelement &b,
				    ringelement &a1,
				    ringelement &b1) const
{
  // FAILS IF elements are NOT MONIC
  a1 = K->from_int(1);
  b1 = K->from_int(1);
}

///////////////
// monomials //
///////////////
inline
int EInterface::n_vars() const
{
  return nvars_;
}

inline
int EInterface::compare_monomials(const monomial *a, const monomial *b) const
{
  return M->compare(a,b);
}

inline
void EInterface::to_exponents(const monomial *m, 
			      exponent_vector *&result) const
{
  M->to_expvector(m,result);
}

///////////
// terms //
///////////

inline
const term &EInterface::lead_term(const vec &v) const
{
  return v;
}

inline
const ringelement &EInterface::term_coefficient(const term &t) const
{
  return t->coeff;
}

inline
const monomial *EInterface::term_monomial(const term &t) const
{
  return t->monom;
}

inline
int EInterface::term_component(const term &t) const
{
  return t->comp;
}

/////////////////
// polynomials //
/////////////////
inline
const monomial *EInterface::lead_monomial_of_polynomial(const polynomial &f) const
{
  return ((const Nterm *)f)->monom;
}

inline
const ringelement &EInterface::lead_coefficient_of_polynomial(const polynomial &f) const
{
  return ((const Nterm *)f)->coeff;
}

/////////////
// vectors //
/////////////
inline
const monomial *EInterface::lead_monomial(const vec &v) const
{
  // Assumes that v is not 0.
  return v->monom;
}

inline
const ringelement &EInterface::lead_coefficient(const vec &v) const
{
  return v->coeff;
}

inline
int EInterface::lead_component(const vec &v) const
{
  return v->comp;
}

inline
void EInterface::remove_vector(const freemodule &F, vec &v) const
{
  F->remove(v);
}

inline
vec EInterface::copy_vector(const freemodule &F, const vec &v) const
{
  return F->copy(v);
}

inline
vec EInterface::zero_vector(const freemodule &F) const
{
  return F->zero();
}

inline
vec EInterface::e_sub_i(const freemodule &F, int i) const
{
  return F->e_sub_i(i);
}

inline
bool EInterface::is_zero_vector(const freemodule &, const vec &v) const
{
  return v == 0;
}

inline
vec EInterface::mult_by_term(const freemodule &F, 
			     const ringelement &coeff,
			     const exponent_vector *exp,
			     const vec &g) const
{
  int *m = M->make_one();
  M->from_expvector(exp,m);
  vec result = F->mult_by_term(coeff,m,g);
  M->remove(m);
  return result;
}

inline
vec EInterface::ring_mult_by_term(const freemodule &F, 
			     const ringelement &coeff,
			     const exponent_vector *exp,
			     int x,
			     const polynomial &g) const
{
  int *m = M->make_one();
  M->from_expvector(exp,m);
  vec result = F->imp_ring_mult_by_term(g,coeff,m,x);
  if (ring_is_quotient())
    F->normal_form(result);
  M->remove(m);
  return result;
}

inline
void EInterface::make_monic(const freemodule &F, const freemodule &Fsyz, 
			    vec &f, vec &fsyz) const
{
  F->make_monic(f,fsyz);
}

inline
void EInterface::auto_reduce(const freemodule &F,
			     const freemodule &Fsyz,
			     vec &f,
			     vec &fsyz,
			     const vec &g,
			     const vec &gsyz) const
{
  F->auto_reduce(Fsyz,f,fsyz,g,gsyz);
}

///////////////////////
// Vector collecting //
///////////////////////

inline
vector_collector EInterface::start_collection(const freemodule &F) const
{
  return vector_collector(F);
}

inline
void EInterface::append_to_collection(vector_collector &a, term &t) const
{
  a.append(t);
}

inline
vec EInterface::end_collection(vector_collector &a) const
{
  return a.value();
}

/////////////////
// Vector heap //
/////////////////

inline
void EInterface::start_heap(const freemodule &F, vector_heap &heap) const
{
  heap = vector_heap(F);  
}

inline
vec EInterface::end_heap(vector_heap &heap) const
{
  return heap.value();
}

inline
void EInterface::add_to_heap(vector_heap &heap, vec &f) const
{
  heap.add(f);
}

inline
bool EInterface::get_lead_term_from_heap(vector_heap &h,
			       ringelement &coeff,
			       const monomial *&monom,
			       int component) const
{
  const vecterm *t = h.get_lead_term();
  if (t == 0) return false;
  coeff = t->coeff;
  monom = t->monom;
  component = t->comp;
  return true;
}

inline
bool EInterface::remove_lead_term_from_heap(vector_heap &h, term &t) const
{
  vecterm *tmp = h.remove_lead_term();
  if (tmp == 0) return false;
  t = tmp;
  return true;
}


#endif
