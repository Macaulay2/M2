// Copyright 1998 by Michael Stillman
#ifndef __Efreemod_hpp_
#define __Efreemod_hpp_

#include "Edefs.hpp"

#include "Emonoid.hpp"
#include "EZZp.hpp"
#include "Ering.hpp"

class EFreeModule : public type
{
  const ERing *R;

  int               _rank;
  const monomial ** _degrees;

  bool              _induced_order;
  const monomial ** _orderings;
  const int *       _tiebreaks;
  
  const EFreeModule *_cover;  // if not a quotient ring, then 'this'
public:
  EFreeModule(const ERing *RR,int rank);
  EFreeModule(const ERing *RR,int rank,const monomial **degrees);
  EFreeModule(const ERing *RR,
              int rank, 
              const monomial **degrees,  // grabbed
              const monomial **ordering, // grabbed
              int *tiebreaks);     // grabbed
  EFreeModule(const ERing *RR, const EFreeModule *F);
       // EFreeModule(RR,F): constructs the free module F over a quotient ring.

  virtual ~EFreeModule();

  int hash() const { return _rank; }
  
  virtual void text_out(buffer &o) const;
  virtual void bin_out(buffer &o) const;
  static EFreeModule *binary_in(istream &i);

  const ERing *getCoefficientRing() const
    { return R->getCoefficientRing(); }

  const ERing *getRing() const
    { return R; }

  const EMonoid *getDegreeMonoid() const
    { return R->getDegreeMonoid(); }
    
  const monomial *getDegree(int i) const  // CHECK THE BOUND IN THE ROUTINE!!!
    { return _degrees[i]; }

  const int getPrimaryDegree(int i) const
    { if (R->n_degrees()  == 0) return 0; return getDegreeMonoid()->to_exponents(_degrees[i])[0]; }

  int rank() const
    { return _rank; }

  bool hasInducedOrder() const
    { return _induced_order; }
  
  const EFreeModule * getCover() const { return _cover; }
  
  virtual EFreeModule * cast_to_EFreeModule() { return this; }
  virtual const EFreeModule * cast_to_EFreeModule() const { return this; }

  class_identifier class_id() const { return CLASS_EFreeModule; }
  type_identifier  type_id () const { return TY_EFreeModule; }
  const char * type_name   () const { return "EFreeModule"; }
public:
  void getDegrees(intarray &result) const;  // Mostly used by the front end...
  EMatrix * getInducedOrder() const;
  
  bool isEqual(const EFreeModule *F) const;
  bool contentIsEqual(const EFreeModule *F) const;  // Only used by Ehashtab

  static EFreeModule *makeFreeModuleFromDegrees(const ERing *R, int ncols, EVector *cols);

  EFreeModule * subSpace(int n) const;
  EFreeModule * subSpace(const intarray &a) const;
  EFreeModule * dual() const;
  EFreeModule * directSum(const EFreeModule *G) const;
  EFreeModule * shift(const monomial *d) const;
  EFreeModule * tensor(const EFreeModule *G) const;
  EFreeModule * exterior(int p) const;
  EFreeModule * symm(int p) const;/*TODO*/

public:
  void setCover(EFreeModule *F)  // To be used ONLY by makeFreeModule...
    { _cover = F; if (_cover != this) bump_up(_cover); }

public:
  /////////////////////
  // Vector routines //
  /////////////////////
  EVector zero() const;
  EVector basisElement(int x) const;
  EVector random() const;
  EVector makeVector(ERingElement g,  // grabbed
                      int r) const;

  EVector makeVector(const ERingElement *elems) const;
  EVector makeSparseVector(const ERingElement *elems, const intarray &rows) const;

};
#if 0
class EFreeModuleNoPoly : public EFreeModule
{
  // Ring elements are kept in each slot.  The ring maintains the
  // freelist for nodes.
  // nodes for this: new_term, copy_term, remove_term
  ERing *R;
  
  evec *new_term() const;
  evec *copy_term() const;
  void remove_term(evec *&t) const;
  int n_terms(evec *a) const;
  void sort(evec *&a) const;
    
  evec *copy(evec *a) const;
  void remove(evec *&a) const;
  
  evec *basis_element(int x) const;
  evec *make_term(ERingElement a, int r) const;  
    // a*e_r.  a is grabbed.
  
  bool is_equal(evec *a, evec *b) const;
  
  evec *lead_term(int n, evec *a) const;

  ERingElement get_component(evec *a, int r) const;
    // Copies the coefficient of e_r in the vector a.
    // The result is a ring element.
  evec *get_terms(evec *a, int lo, int hi) const;
  
  int add_to(evec *&a, evec *&b) const;
    // Returns the number of terms in the result: a += b, b=0.
  void negate_to(evec *&a) const;
  int subtract_to(evec *&a, evec *&b) const;
    // Returns the number of terms in the result: a -= b, b=0.
    
  evec *add(evec *a, evec *b) const;
  evec *subtract(evec *a, evec *b) const;
  evec *mult(ERingElement a, evec *b) const;
  evec *mult(evec *a, ERingElement *b) const;

  evec *random() const;
  evec *eval(const ERingMap *map, const EFreeModule *oldF, const evec *a) const;
  
  // Funny ones:
  is_scalar_multiple
  // Funny, poly ones:
  monomial_divisor
  monomial_squarefree
  remove_monomial_divisors
  coefficient_of_var
  lead_var_coefficient
  degree_of_var
  divide_by_var
  divide_by_expvector
  
  // Normal form routines
  apply_quotient_ring_elements
  apply_map
  normal_form_ZZ  (2 forms)
  normal_form (2 forms)
  
  // Matrix routines
  component_shift
  tensor_shift
  sub_vector
  tensor

  sort_vectors

  reshape
  transpose_matrix
  mult_by_matrix
  
};

class EFreeModulePoly : public EFreeModule
{
  // Grabs the coefficient c: These call routines in ring class.
  poly *make_term(ERingElement c, monomial *m, int r) const;
  poly *make_term(ERingElement c, const intarray  &varexp, int r) const;
  
  poly *term(const ERingElement c, monomial *m, int r) const;
    // Copies the coefficient c.

  monomial *lead_monomial(poly *a) const;
    // Needed?
    
  degree
  is_graded
  homogenize
  
  diff
  contract
  
  // GB useful:
  bool coeff_of(poly *a, const monomial *m, int r, ERingElement &result);
  auto_reduce
  auto_reduce_coeffs
  auto-reduce_ZZ
  make_monic
  
  in_subring
};
class EFreeModuleSchreyer : public EFreeModulePoly
{
  // Redefines the add_to routine, and the leadTerm(n,v) routine.
};
#endif
#if 0
class EMutableFreeModule : public EFreeModule
{
public:
  // make a mutable free module from a non-mutable one (clone)
  // forget the mutable-ness.
  // Append a new element, after a given one.
  // Delete an element
  // Swap two elements
  // Replace an element
};

public:
public:

  EVector translate(const EVector &v) const;
  EVector translate(const EVector &v, int newcomponent) const;  // Set all components to 'newcomponent'
  EVector componentShift(int r, const EVector &v) const;
  EVector tensorShift(int n, int m, const EVector &v) const;
  EVector tensor(const EVector &v, const EVector &w) const;
  EVector getComponent(const EVector &v, int comp, int newcomp) const;
  EVector subvector(const EVector &v, const intarray &r) const;


#endif
#endif
