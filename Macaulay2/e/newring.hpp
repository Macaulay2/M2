// Copyright 1997 Michael E. Stillman

// The following could be a template definition...
#define BASERING Ring

class ncPolynomialRing : public type
{
public:
  ncPolynomialRing(const BASERING *K, 
		   int nvars, 
		                   /* names of variables */
		   const int *degs  /* degrees */
		   );

  ~ncPolynomialRing();

  //////////////////////////////////////
  // virtual ring element functions ////
  //////////////////////////////////////
  virtual ring_elem ring_add(const ring_elem &a, const ring_elem &b) const;
  virtual ring_elem ring_subtract(const ring_elem &a, const ring_elem &b) const;
  virtual ring_elem ring_mult(const ring_elem &a, const ring_elem &b) const;
  virtual ring_elem ring_divide(const ring_elem &a, const ring_elem &b) const;

  virtual ring_elem ring_negate(const ring_elem &a) const;
  virtual ring_elem ring_invert(const ring_elem &a) const;

  //////////////////////////////////////
  // virtual vector functions //////////
  //////////////////////////////////////
  virtual vec e_sub_i(int i) const;
  virtual vec make_vector(const ring_elem &a, int i) const;

  virtual ring_elem get_entry(const vec &a, int r) const;
  virtual int n_terms(const vec &a) const;
  virtual vec get_terms(const vec &a, int lo, int hi) const;

  virtual vec vec_copy_term(const vec &a) const;
  virtual vec vec_copy(const vec &a) const;
  virtual void vec_remove(vec &a) const;

  virtual bool is_equal(const vec &a, const vec &b) const;
  virtual bool is_zero(const vec &a) const;
  
  virtual vec vec_negate(const vec &a) const;
  virtual vec vec_add(const vec &a, const vec &b) const;
  virtual vec vec_subtract(const vec &a, const vec &b) const;
  virtual vec vec_mult(const ring_elem &a, const vec &b) const;
  virtual vec vec_mult(const vec &a, const ring_elem &b) const;

  // multiplication routines [need both left/right multiplication]
  vec mult_by_coefficient(const ring_elem c, const vec v) const;
  vec mult_by_monomial(const int *m, const vec v) const;
  vec mult_by_term(const ring_elem c, const int *m, const vec v) const;
  vec cancel_lead_terms(vec &f, vec g, ring_elem &coeff, int *monom) const;
  vec ring_cancel_lead_terms(vec &f, ring_elem g, ring_elem &coeff, int *monom) const;

  // faster, non-virtual functions

  //////////////////////////////////////////////
  //  Homogeniety and the grading //////////////
  //////////////////////////////////////////////
  void    degree         (const vec f, int *d)         const;
  void    degree_weights (const vec f, const int *wts, int &lo, int &hi) const;
  int     primary_degree (const vec f)                 const;
  bool    is_homogeneous (const vec f)                 const;
  vec homogenize     (const vec f, int v, int deg, const int *wts) const;
  vec homogenize     (const vec f, int v, const int *wts)          const;

};


/*
R->base_poly_ring()->mult(f,g)  // doesn't reduce mod the ideal.
R->mult(f,g) // does reduce
// If this is a skew polynomial ring, then 
R->mult(f,g) // does the skew bit..., and 
R->base_poly_ring // is a skew commutative polynomial ring.


 */


// Routines needed for Groebner basis computations
class VectorFunctions
{
  vec copy(const vec f) const;
  void remove(vec &f) const;
  bool is_zero(const vec f) const;  // Not necessarily the NULL element...
  bool is_equal(const vec f, const vec g) const;
  vec e_sub_i(int i) const;
  vec zero() const;
  int simple_degree(vec f) const;  // Possibly also return lo,hi values...

  // reduction step, arithmetic:
  void add_to(vec &f, vec &g) const;
  

  // heap arithmetic
  class heap {
  public:
    heap();
    ~heap();

    vec value();
    void add_to(vec &f);
    remove_lead_term();
  };
};
