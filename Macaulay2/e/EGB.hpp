// Copyright 1998  Michael E. Stillman
#ifndef _EGB_hh_
#define _EGB_hh_

class EGB : public type
{
public:
  void bin_out(buffer &) const;
  void text_out(buffer &o) const;

  class_identifier class_id() const { return CLASS_EGB; }
  type_identifier  type_id () const { return TY_EGB; }
  const char * type_name   () const { return "EGB"; }

  EGB * cast_to_EGB() { return this; }

public:
  virtual void moreRelations(int lo, int hi, const EMatrix *m);
  virtual void moreGenerators(int lo, int hi, const EMatrix *m);
  virtual void extendRing(const EPolynomialRing *newR);
  virtual void informHilbertSeries(const EVector *hf);
  
  virtual void setEndConditions();
  virtual int calc(int d);
  
  virtual EMatrix *reduce(const EMatrix *m, EMatrix *&lift);
  virtual EVector *reduce(const EVector *v, EVector *&lift);
  virtual int contains(const EMatrix *m);
  
  // NULL is returned if the specific computation doesn't support
  // the given operation.
  virtual EMatrix *getMinimalGenerators();
  virtual EMatrix *getGB();
  virtual EMatrix *getChangeOfBasis();
  virtual EMatrix *getSyzygies();
  virtual EMatrix *getLeadTerms(int n=-1);
  virtual EMatrix *getSubring(int n=-1);
  virtual EMatrix *getSubringGB(int n=-1);
};

#if 0
GB of a module:
(generators:EMatrix, relations:EMatrix)
GB of: relations
GB of generators + relations
partial syzygies
mingens of the module.

moreGenerators
moreRelations
syzygies or not?

want to be able to declare a GB.
want to be able to say that s-pairs will reduce to zero
  among a set of elements. (Really?)
  
want to be able to form a GB from, say, subring elements.

GB should, by default, come out unique: auto-reduced, and 
sorted in some order.
#endif
#endif