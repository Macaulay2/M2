// Copyright 1996  Michael E. Stillman
#ifndef _ringmap_hh_
#define _ringmap_hh_

#include "ring.hpp"

class RingMap_rec : public object_element
{
  const Ring *R;		// This is the target ring.
  array <ring_elem> map;

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  friend class RingMap;
  
  RingMap_rec(const Matrix &m);
  ~RingMap_rec();

  object_types type_of         () const { return TY_RING_MAP; }
  const char * type_name       () const { return "ring map"; }
  RingMap cast_to_RingMap();
  
  int length_of() const { return map.length(); }

  void bin_out(ostream &o) const;
  void text_out(ostream &o) const;
};

class RingMap
{
  POINTER(RingMap, RingMap_rec)

public:
  RingMap(const Matrix &m) : obj(new RingMap_rec(m)) {}

  const ring_elem elem(int i) const { return obj->map[i]; }
  int length() const { return obj->map.length(); }

  ring_elem eval_term(const Ring *coeff_ring, const ring_elem coeff, 
		      const int *vp) const;

  RingElement eval(const RingElement &r) const;
  Vector eval(const FreeModule *resultfree, const Vector &v) const;
  Matrix eval(const FreeModule *newrows, const Matrix &m) const;

  const Ring *Ring_of() const { return obj->R; }
  const Monoid *degree_monoid() const { return Ring_of()->degree_monoid(); }

  void text_out(ostream &o) const;
};

inline RingMap RingMap_rec::cast_to_RingMap() 
{ return RingMap(this,caster); }

#endif
