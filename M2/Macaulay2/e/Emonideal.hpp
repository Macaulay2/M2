// Copyright 1999  Michael E. Stillman
#ifndef _Emonideal_hh_
#define _Emonideal_hh_

#include "Emonlookup.hpp"
#include "Ering.hpp"

class EMonomialIdeal : public object_element
{

  const EPolynomialRing *R;
  EMonomialLookupTable *mi;
public:

  EMonomialIdeal(const EPolynomialRing *RR) : 
    object_element(), 
    R(RR), 
    mi(new EMonomialLookupTable)
    { bump_up(R); }

  EMonomialIdeal(const EPolynomialRing *RR, EMonomialLookupTable *mmi) : 
    object_element(), 
    R(RR), 
    mi(mmi)
    { bump_up(R); }

  EMonomialIdeal(const EMatrix *m, int r);

  virtual ~EMonomialIdeal() { delete mi; bump_down(R); }

  // Infrastructure
  class_identifier class_id() const { return CLASS_EMonomialIdeal; }
  type_identifier  type_id () const { return TY_EMonomialIdeal; }
  const char * type_name   () const { return "EMonomialIdeal"; }

  EMonomialIdeal * cast_to_EMonomialIdeal() { return this; }
  const EMonomialIdeal * cast_to_EMonomialIdeal() const { return this; }

  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;

  int length_of() const { return mi->length(); }

  const ERing *getRing() const { return R; }
  const EMonoid *getDegreeMonoid() const { return R->getDegreeMonoid(); }

  friend void i_Ecommands();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

public:
  EMonomialIdeal(const ERing *R, queue<Bag *> &elems);
  EMonomialIdeal(const ERing *R, queue<Bag *> &elems, queue<Bag *> &rejects);

  EMatrix *toMatrix() const;

  EMonomialIdeal *copy() const;

  bool is_equal(const EMonomialIdeal *mi) const;

  EMonomialIdeal *intersect(const monomial *m) const;
  EMonomialIdeal *intersect(const EMonomialIdeal *J) const;
  EMonomialIdeal *quotient(const monomial *m) const;
  EMonomialIdeal *quotient(const EMonomialIdeal *J) const;
  EMonomialIdeal *erase(const monomial *m) const; // m is a varpower monomial
  EMonomialIdeal *saturate(const EMonomialIdeal *J) const;

  EMonomialIdeal *radical() const;

  EMonomialIdeal *borel() const;
  int is_borel() const;
  
  EMonomialIdeal *operator+(const EMonomialIdeal *F) const;
  EMonomialIdeal *operator-(const EMonomialIdeal *F) const;
  EMonomialIdeal *operator*(const EMonomialIdeal *F) const;
};

#endif


