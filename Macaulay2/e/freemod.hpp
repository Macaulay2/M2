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

  virtual FreeModule *new_free() const;
public:
  FreeModule(const Ring *R, int n, bool has_schreyer_order);
  // if n>0, and has_schreyer_order is true, then all base monomials are set to 0.
#if 0
  FreeModule(const Ring *RR, const FreeModule *F);
#endif
  static FreeModule *make_schreyer(const Matrix *m);
  Matrix * get_induced_order() const;

  virtual ~FreeModule();


public:
  void append(const int *d);
  void append_schreyer(const int *d, const int *monom, int compare_num); // append to a Schreyer order.

public:
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

  // These four require some (small) computation

  //  ring_elem lead_coefficient(vec v) const;


//////////////////////////////////////////////
//  Misc routines  ///////////////////////////
//////////////////////////////////////////////

public:
  //  vec random() const;  // Produces a random vector of coefficients (no monomials)
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


  // Some divisibility routines
  int is_scalar_multiple(vec f, vec g) const;// is cf = dg, some scalars c,d? (not both zero).
  void monomial_divisor(vec f, int *exp) const;
  vec monomial_squarefree(vec f) const;
  vec remove_monomial_divisors(vec f) const;

  
//////////////////////////////////////////////
//  Homogeniety and the grading //////////////
//////////////////////////////////////////////

public:

//////////////////////////////////////////////
//  Input, output, infrastructure ////////////
//////////////////////////////////////////////

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
