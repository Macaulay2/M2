// Copyright 1995  Michael E. Stillman

#ifndef _FreeModule_hh_
#define _FreeModule_hh_

#include "ring.hpp"
#include "schorder.hpp"

class Matrix;

class FreeModule : public immutable_object
{
protected:

  // free module part
  array<int *> components; // Degrees of each component
  SchreyerOrder *schreyer; // NULL, if not a Schreyer order...

  const Ring *R;

  // stash used for vecterm's: R->vecstash
  
protected:
  // Do we need all these routines?
  vec new_term() const;

//////////////////////////////////////////////
//  Free module routines /////////////////////
//////////////////////////////////////////////

protected:
  void initialize(const Ring *RR);
  void symm1(int lastn, int pow) const;   // privately called by 'symm'.
public:
  FreeModule(const Ring *R);
  FreeModule(const Ring *R, int n);
  FreeModule(const Ring *RR, const FreeModule *F);

  static FreeModule *make_schreyer(const Matrix *m);
  Matrix * get_induced_order() const;

  virtual ~FreeModule();

  virtual FreeModule *new_free() const;

  void append(const int *d); // append a new row to a FREE or FREE_POLY

  const Ring *  get_ring()      const { return R; }
  const Monoid * degree_monoid() const { return R->degree_monoid(); }
  const SchreyerOrder * get_schreyer_order() const { return schreyer; }

  const int *          degree(int i)    const { return components[i]; }
#if 0
  const int *          base_monom(int i)const { return schorder->base_monom(i); }
        int            compare_num(int i)const { return schorder->compare_num(i); }
#endif

  int                  rank()           const { return components.length(); }

  int                  primary_degree(int i) const;

  // WARNING: change_degree modifies the degree, and should only be used during
  // the construction of a free module (or matrix).
  void change_degree(int i, const int *deg);

  bool is_equal(const FreeModule *F) const;
  bool is_zero() const;

  FreeModule * sub_space   (int n)                const;
  FreeModule * sub_space   (const M2_arrayint a)  const;
  FreeModule * transpose   ()                     const;
  FreeModule * direct_sum  (const FreeModule *G)  const;
  FreeModule * shift       (const int *d)         const;
  FreeModule * tensor      (const FreeModule *G)  const;
  FreeModule * schur       (const int *m)         const;
  FreeModule * exterior    (int p)                const;
  FreeModule * symm        (int p)                const;

  void direct_sum_to(const FreeModule *G);
  int lowest_primary_degree() const;
  int highest_primary_degree() const;

//////////////////////////////////////////////
//  Vector operations ////////////////////////
//////////////////////////////////////////////

public:
#if 0
  void to_exponents(const int *m, int component, int *result_exponents) const;
  void from_exponents(const int *exponents, int component, int *result_monomial) const;
#endif

  vec zero() const { return NULL; }
  vec e_sub_i(int i) const;
  vec raw_term(ring_elem a, int r) const;

  bool is_equal(vec v, vec w) const;
  bool is_zero(vec v) const  { return v == NULL; }

  vec copy_term(vec v) const;

  vec copy(vec v) const;
  void remove(vec &v) const;

  void negate_to(vec &v) const;
  void add_to(vec &v, vec &w) const;
  void subtract_to(vec &v, vec &w) const;

  vec negate(vec v) const;
  vec add(vec v, vec w) const;
  vec subtract(vec v, vec w) const;

  int n_terms(vec v) const; // ??
  ring_elem get_coefficient(vec v, int r) const;
  vec get_terms(vec v, int lo, int hi) const; // ??

  // These four require some (small) computation
  int lead_component(vec v) const;
#if 0
  vec lead_term(vec v) const;
  vec lead_term(int n, vec v) const;
#endif
  ring_elem lead_coefficient(vec v) const;

#if 0
  vec lead_var_coefficient(vec &v, int &var, int &exp) const;
  vec coefficient_of_var(vec v, int var, int exp) const;
#endif

#if 0
  // Polynomial routines
  vec from_varpower(const int *vp, int x) const;
  void lead_varpower(const vec v, intarray &vp) const;
  vec term(int r, ring_elem a, const int *m) const;


#endif
  int compare(const vecterm *f, const vecterm *g) const;

  // Some divisibility routines
  int is_scalar_multiple(vec f, vec g) const;// is cf = dg, some scalars c,d? (not both zero).

//////////////////////////////////////////////
//  Groebner basis support routines //////////
//////////////////////////////////////////////
#if 0
  vec mult_by_monomial(const int *m, vec v) const;

  ring_elem coeff_of(const vec v, const int *m, int x) const;

  void auto_reduce(const FreeModule *Fsyz, 
		   vec &f, vec &fsyz, 
		   vec g, vec gsyz) const;

  void auto_reduce_coeffs(const FreeModule *Fsyz, 
		   vec &f, vec &fsyz, 
		   vec g, vec gsyz) const;

  void make_monic(vec &v, vec &vsyz) const;

  vec strip(vec v) const;
  const vec component_occurs_in(int x, vec v) const;
#endif
//////////////////////////////////////////////
//  Multiplication ///////////////////////////
//////////////////////////////////////////////

protected:
#if 0
  // These routines do straight multiplication with no normal forms.

  vec imp_mult_by_coeff       (const ring_elem c, vec v) const;
  virtual vec imp_mult_by_term        (const ring_elem c, const int *m, const vec v) const;
  void    imp_subtract_multiple_to(vec &v, ring_elem c, const int *m, 
				   const vec w) const;

  //MES: vec imp_mult_by_monomial    (const int *m, vec v) const;

  vec imp_skew_mult_by_term(const ring_elem c,
			    const int *m, vec v) const; // return c*m*v
  void imp_cancel_lead_term(vec &f, 
			    vec g, 
			    ring_elem &coeff, 
			    int *monom) const;
  void imp_ring_cancel_lead_term(vec &f, 
				 ring_elem gg, 
				 ring_elem &coeff, 
				 int *monom) const;
  vec imp_skew_ring_mult_by_term(
				 const ring_elem f,
				 const ring_elem c,
				 const int *m, 
				 int x) const;   // return c*m*f*e_x

#endif


public:
          vec mult         (int n, vec v)        const;
  virtual vec mult         (const ring_elem f, const vec v) const;
  virtual vec rightmult    (const vec v, const ring_elem f) const;
#if 0
  virtual vec mult_by_coeff(const ring_elem c, vec v) const;
  virtual vec mult_by_term (const ring_elem c, const int *m, const vec v) const;
  virtual vec right_mult_by_term (vec v, ring_elem c, const int *m) const;

  void subtract_multiple_to(vec &v, ring_elem c, 
			    const int *m, const vec w) const;
#endif
//////////////////////////////////////////////
//  Normal forms /////////////////////////////
//////////////////////////////////////////////

#if 0
protected:
  vec imp_ring_mult_by_term(const ring_elem f, 
				const ring_elem c, 
				const int *m, int x) const;
  void imp_subtract_ring_multiple_to(vec &f, 
				     ring_elem a, 
				     const int *m, 
				     const Nterm *g) const;
public:
  void apply_quotient_ring_elements(vec &f, int x, vec rsyz) const;
  void apply_map(vec &f, vec gsyz, const array<vec> &vecs) const;
  
  virtual void normal_form(vec &v) const;
  virtual void normal_form_ZZ(vec &v) const;

  void normal_form(vec &v, 
		   const array<MonomialIdeal *> &mis, 
		   const array<vec> &vecs) const;

  void normal_form_ZZ(vec &f,
		      const array<TermIdeal *> &termideals,
		      const FreeModule *Gsyz,
		      const array<vec> &vecs) const;

#endif
//////////////////////////////////////////////
//  Matrix routines //////////////////////////
//////////////////////////////////////////////

  void reshape          (const Matrix &m, Matrix &result) const;
  void transpose_matrix (const Matrix &m, Matrix &result) const;

  vec sub_vector(const FreeModule *F, vec v, 
		     const M2_arrayint r) const;

  vec component_shift(int n, const FreeModule *F, 
			  vec v) const;

  vec tensor_shift(int n, int m, 
		       const FreeModule *F,
		       vec v) const;

  vec mult_by_matrix(const Matrix *m,
			 const FreeModule *F, 
			 vec v) const;

//////////////////////////////////////////////
//  Misc routines  ///////////////////////////
//////////////////////////////////////////////

  vec eval(const RingMap *map, const FreeModule *F,
	       const vec v) const;

  vec tensor(const FreeModule *F, vec v, 
		 const FreeModule *G, vec w) const;

#if 0
  void auto_reduce(array<vec> &vecs) const;
protected:
  void auto_reduce_ZZ(array<vec> &vecs) const;
#endif
public:
  vec random() const;  // Produces a random vector of coefficients (no monomials)
protected:
  int sort_compare(int i, int j) const;
  int sort_partition(int lo, int hi) const;
  void sort_range(int lo, int hi) const;

public:
  M2_arrayint sort(const array<vec> &vecs, 
		   const M2_arrayint degrees, // only needed if degorder!=0
		   int degorder, // -1=descending, 0=don't use, 1=ascending
		   int monorder // -1=descending, 1=ascending.
		   ) const;


public:
  vec diff(const FreeModule *F, vec v, 
	       const FreeModule *G, vec w,
	       int use_coeff) const;


  void monomial_divisor(vec f, int *exp) const;
  vec monomial_squarefree(vec f) const;
  vec remove_monomial_divisors(vec f) const;

  int in_subring(int n, const vec v) const;
  void degree_of_var(int n, const vec v, int &lo, int &hi) const;
  vec divide_by_var(int n, int d, const vec v) const;
  vec divide_by_expvector(const int *exp, const vec v) const;
  
//////////////////////////////////////////////
//  Homogeniety and the grading //////////////
//////////////////////////////////////////////

public:
  bool multi_degree(const vec f, int *degf) const;
  // returns true iff f is homogeneous

  void    degree         (const vec f, int *d)         const;
  void    degree_weights (const vec f, const M2_arrayint wts, int &lo, int &hi) const;
  int     primary_degree (const vec f)                 const;
  bool    is_homogeneous (const vec f)                 const;
  vec homogenize     (const vec f, int v, int deg, const M2_arrayint wts) const;
  vec homogenize     (const vec f, int v, const M2_arrayint wts)          const;

//////////////////////////////////////////////
//  Translation and sorting routines /////////
//////////////////////////////////////////////

public:
  vec resize    (const FreeModule *oldF, vec v) const;
  void    sort      (vecterm *&f)                       const;
  vec translate (const FreeModule *F, vec v)    const;  // NOT NEEDED ANYMORE...

//////////////////////////////////////////////
//  Input, output, infrastructure ////////////
//////////////////////////////////////////////

  void text_out(buffer &o) const;

  void elem_text_out(buffer &o, const vec a) const;
};

#endif
