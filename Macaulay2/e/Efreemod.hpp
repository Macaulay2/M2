// Copyright 1998 by Michael Stillman
#ifndef __Efreemod_hpp_
#define __Efreemod_hpp_

#include "Edefs.hpp"

#include "Emonoid.hpp"
#include "EZZp.hpp"
#include "Ering.hpp"

class EFreeModule : public type
{
  const EPolynomialRing *R;

  int               _rank;
  const monomial ** _degrees;

  bool              _induced_order;
  const monomial ** _orderings;
  const int *       _tiebreaks;
  
  const EFreeModule *_cover;  // if not a quotient ring, then 'this'
public:
  EFreeModule(const EPolynomialRing *RR,int rank);
  EFreeModule(const EPolynomialRing *RR,int rank,const monomial **degrees);
  EFreeModule(const EPolynomialRing *RR,
              int rank, 
              const monomial **degrees,  // grabbed
              const monomial **ordering, // grabbed
              int *tiebreaks);     // grabbed
  EFreeModule(const EPolynomialRing *RR, const EFreeModule *F);
       // EFreeModule(RR,F): constructs the free module F over a quotient ring.

  virtual ~EFreeModule();

  int hash() const { return _rank; }
  
  void text_out(ostream &o) const;
  virtual void text_out(buffer &o) const;
  void binary_out(ostream &o) const;
  static EFreeModule *binary_in(istream &i);

  const ECoefficientRing *getCoefficientRing() const
    { return R->getCoefficientRing(); }

  const EMonoid *getMonoid() const
    { return R->getMonoid(); }

  const EPolynomialRing *getRing() const
    { return R; }

  const EMonoid *getDegreeMonoid() const
    { return R->getDegreeMonoid(); }
    
  const monomial *getDegree(int i) const  // CHECK THE BOUND IN THE ROUTINE!!!
    { return _degrees[i]; }
    
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

  EFreeModule * subSpace(int n) const;
  EFreeModule * subSpace(const intarray &a) const;
  EFreeModule * dual() const;
  EFreeModule * directSum(const EFreeModule *G) const;
  EFreeModule * shift(const monomial *d) const;
  EFreeModule * tensor(const EFreeModule *G) const;
  EFreeModule * exterior(int p) const;
  EFreeModule * symm(int p) const;/*TODO*/

public:
  EVector *buildVector(poly *f, int len) const;

  EVector *makeTerm(const field a, const monomial *m, int x) const;

  EVector *basisElement(int x) const;

  EVector *zero() const;
  
  void sort(poly *&f) const;

  EVector *makeVector(const EVector **elems) const;
  EVector *makeSparseVector(const EVector **elems, const intarray &rows) const;
  EVector *getComponent(const EVector *v, int comp, int newcomp) const;
  EVector *subvector(const EVector *v, const intarray &r) const;

public:
  EVector *random() const;
  EVector *translate(const EVector *v) const;
  EVector *translate(const EVector *v, int newcomponent) const;  // Set all components to 'newcomponent'
  EVector *componentShift(int r, const EVector *v) const;
  EVector *tensorShift(int n, int m, const EVector *v) const;
  EVector *tensor(const EVector *v, const EVector *w) const;
public:
  void setCover(EFreeModule *F)  // To be used ONLY by makeFreeModule...
    { _cover = F; if (_cover != this) bump_up(_cover); }
};

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
#endif
#endif
