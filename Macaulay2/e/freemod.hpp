// Copyright 1995  Michael E. Stillman

#ifndef _FreeModule_hh_
#define _FreeModule_hh_

#include "ring.hpp"
#include "monideal.hpp"

struct index_type
{
  int       compare_num;
  int *     base_monom;
  int *     deg;

  index_type(int i) : compare_num(i) {}
};

class FreeModule : public type
{
  friend class NGB_comp;
  friend class GB_comp;
  friend class GBinhom_comp;
  friend class gb2_comp;
protected:

  // free module part
  array<index_type *> components;

  // stash used for vecterm's: R->vecstash
  
  const Ring *K;		// If R is a poly ring, K is the coeff ring, otherwise
  const Ring *R;		// K = R.
  const Monoid *M;	// Resizing is possible.  This will 'change' R, M.
  int is_quotient_ring;		// MES: this needs to be set!

  enum {FREE, FREE_POLY, FREE_SCHREYER} ty;
      // A free module will be of type FREE if the base ring is not a polynomial ring
      // Otherwise, the free module starts off as a FREE_POLY, and is changed to a
      // FREE_SCHREYER if a generator with base != one is appended.

  intarray nf_exp_a;
  int *nf_1, *nf_exp, *mon_1;
protected:
  // Do we need all these routines?
  vec new_term() const;
  vec copy_term(vec v) const;
  vec new_term(int e, ring_elem a, const int *m) const;

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
  ~FreeModule();

  virtual FreeModule *new_free() const;

  void append(const int *d); // append a new row to a FREE or FREE_POLY
  void append(const int *d, const int *basemonom); // append to a FREE_SCHREYER
  void append(const int *d, const int *basemonom, int comparenum); // append to a FREE_SCHREYER

  const Ring *  Ring_of()      const { return R; }
  const Ring *  Ncoeffs()       const { return K; }
  const Monoid * Nmonoms()       const { return M; }
  const Monoid * degree_monoid() const { return R->degree_monoid(); }

  const index_type  *  component(int i) const { return components[i]; }
  const int *          degree(int i)    const { return components[i]->deg; }
  const int *          base_monom(int i)const { return components[i]->base_monom; }
        int            compare_num(int i)const { return components[i]->compare_num; }
  int                  rank()           const { return components.length(); }

  int                  primary_degree(int i) const;

  // WARNING: change_degree modifies the degree, and should only be used during
  // the construction of a free module (or matrix).
  void change_degree(int i, const int *deg);

  bool is_equal(const FreeModule *F) const;
  bool is_zero() const;

  FreeModule * sub_space   (int n)                const;
  FreeModule * sub_space   (const intarray &a)    const;
  FreeModule * transpose   ()                     const;
  FreeModule * direct_sum  (const FreeModule *G) const;
  FreeModule * shift       (const int *d)         const;
  FreeModule * tensor      (const FreeModule *G) const;
  FreeModule * schur       (const int *m)         const;
  FreeModule * exterior    (int p)                const;
  FreeModule * symm        (int p)                const;

  void direct_sum_to(const FreeModule *G);
  void gcd(intarray &lo_deg) const;
  void lcm(intarray &hi_deg) const;
  int lowest_primary_degree() const;
  int highest_primary_degree() const;

//////////////////////////////////////////////
//  Vector operations ////////////////////////
//////////////////////////////////////////////

public:
  vec zero() const { return NULL; }
  vec e_sub_i(int i) const;
  vec term(int r, ring_elem a) const;

  bool is_equal(vec v, vec w) const;
  bool is_zero(vec v) const  { return v == NULL; }

  vec copy(vec v) const;
  void remove(vec &v) const;

  void negate_to(vec &v) const;
  void add_to(vec &v, vec &w) const;
  void subtract_to(vec &v, vec &w) const;

  vec negate(vec v) const;
  vec add(vec v, vec w) const;
  vec subtract(vec v, vec w) const;


  int n_terms(vec v) const;
  ring_elem get_coefficient(vec v, int r) const;
  vec get_terms(vec v, int lo, int hi) const;

  int lead_component(vec v) const;
  vec lead_term(vec v) const;
  vec lead_term(int n, vec v) const;
  ring_elem lead_coefficient(vec v) const;

  vec lead_var_coefficient(vec &v, int &var, int &exp) const;
  vec coefficient_of_var(vec v, int var, int exp) const;

  // Polynomial routines
  vec from_varpower(const int *vp, int x) const;
  void lead_varpower(const vec v, intarray &vp) const;
  vec term(int r, ring_elem a, const int *m) const;

  int compare(const vecterm *f, const vecterm *g) const;

  // Some divisibility routines
  int is_scalar_multiple(vec f, vec g) const;// is cf = dg, some scalars c,d? (not both zero).

//////////////////////////////////////////////
//  Groebner basis support routines //////////
//////////////////////////////////////////////

  vec mult_by_monomial(const int *m, vec v) const;

  ring_elem coeff_of(const vec v, const int *m, int x) const;

  void auto_reduce(const FreeModule *Fsyz, 
		   vec &f, vec &fsyz, 
		   vec g, vec gsyz) const;

  void make_monic(vec &v, vec &vsyz) const;

  vec strip(vec v) const;
  const vec component_occurs_in(int x, vec v) const;

//////////////////////////////////////////////
//  Multiplication ///////////////////////////
//////////////////////////////////////////////

protected:
  // These routines do straight multiplication with no normal forms.

  vec imp_mult_by_coeff       (const ring_elem c, vec v) const;
  vec imp_mult_by_term        (const ring_elem c, const int *m, const vec v) const;
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




public:
          vec mult         (int n, vec v)        const;
  virtual vec mult         (const ring_elem f, const vec v) const;
  virtual vec rightmult    (const vec v, const ring_elem f) const;
  virtual vec mult_by_coeff(const ring_elem c, vec v) const;
  virtual vec mult_by_term (const ring_elem c, const int *m, const vec v) const;
  virtual vec right_mult_by_term (vec v, ring_elem c, const int *m) const;

  void subtract_multiple_to(vec &v, ring_elem c, 
			    const int *m, const vec w) const;

//////////////////////////////////////////////
//  Normal forms /////////////////////////////
//////////////////////////////////////////////

protected:
  vec imp_ring_mult_by_term(const ring_elem f, 
				const ring_elem c, 
				const int *m, int x) const;
  void imp_subtract_ring_multiple_to(vec &f, 
				     ring_elem a, 
				     const int *m, 
				     const Nterm *g) const;
public:
  virtual void normal_form(vec &v) const;

  void normal_form(vec &v, 
		   const array<MonomialIdeal> &mis, 
		   const array<vec> &vecs) const;

//////////////////////////////////////////////
//  Matrix routines //////////////////////////
//////////////////////////////////////////////

  void reshape          (const Matrix &m, Matrix &result) const;
  void transpose_matrix (const Matrix &m, Matrix &result) const;

  vec sub_vector(const FreeModule *F, vec v, 
		     const intarray &r) const;

  vec component_shift(int n, const FreeModule *F, 
			  vec v) const;

  vec tensor_shift(int n, int m, 
		       const FreeModule *F,
		       vec v) const;

  vec mult_by_matrix(const Matrix &m,
			 const FreeModule *F, 
			 vec v) const;

//////////////////////////////////////////////
//  Misc routines  ///////////////////////////
//////////////////////////////////////////////

  vec eval(const RingMap &map, const FreeModule *F,
	       const vec v) const;

  vec tensor(const FreeModule *F, vec v, 
		 const FreeModule *G, vec w) const;

  void auto_reduce(array<vec> &vecs) const;

private:
  int sort_compare(int i, int j) const;
  int sort_partition(int lo, int hi) const;
  void sort_range(int lo, int hi) const;

public:
  void sort(const array<vec> &vecs, 
	    const intarray &degrees, // only needed if degorder!=0
	    int degorder, // -1=descending, 0=don't use, 1=ascending
	    int monorder, // -1=descending, 1=ascending.
	    intarray &result) const;
  
private:  // Used as local routine to 'diff'
  ring_elem diff_term(const int *m, const int *n, 
		       int *resultmon,
		       int use_coeff) const;
public:
  vec diff(const FreeModule *F, vec v, 
	       const FreeModule *G, vec w,
	       int use_coeff) const;

  int in_subring(int n, const vec v) const;
  int degree_of_var(int n, const vec v) const;
  vec divide_by_var(int n, int d, const vec v) const;
  
//////////////////////////////////////////////
//  Homogeniety and the grading //////////////
//////////////////////////////////////////////

protected:
  void    term_degree    (const vecterm *t, int *degt)    const;
public:
  void    degree         (const vec f, int *d)         const;
  void    degree_weights (const vec f, const int *wts, int &lo, int &hi) const;
  int     primary_degree (const vec f)                 const;
  bool    is_homogeneous (const vec f)                 const;
  vec homogenize     (const vec f, int v, int deg, const int *wts) const;
  vec homogenize     (const vec f, int v, const int *wts)          const;

//////////////////////////////////////////////
//  Translation and sorting routines /////////
//////////////////////////////////////////////

public:
  vec resize    (const FreeModule *oldF, vec v) const;
  void    sort      (vecterm *&f)                       const;
  vec translate (const FreeModule *F, vec v)    const;

//////////////////////////////////////////////
//  Input, output, infrastructure ////////////
//////////////////////////////////////////////

  void text_out(ostream &o) const;
  void bin_out(ostream &o) const;

  void elem_text_out(ostream &o, const vec a) const;
  void elem_bin_out(ostream &o, const vec a) const;

  int                 length_of()           const { return rank(); }
  FreeModule *       cast_to_FreeModule()       { return this; }
  const FreeModule * cast_to_FreeModule() const { return this; }
  object_types        type_of()             const { return TY_FREEMODULE; }
  const char *        type_name()           const { return "FreeModule"; }

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }
};

#endif
