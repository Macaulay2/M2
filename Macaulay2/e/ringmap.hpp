// Copyright 1996  Michael E. Stillman
#ifndef _ringmap_hh_
#define _ringmap_hh_

#include "ring.hpp"
#if 0
class RingMap : public type
{
  const Ring *R;		// This is the target ring.
  array <ring_elem> map;
public:
  RingMap(const Matrix &m);
  ~RingMap();

  const Ring *Ring_of() const { return R; }
  const ring_elem elem(int i) const { return map[i]; }

  ring_elem eval_term(const Ring *coeff_ring, const ring_elem coeff, 
		      const int *vp) const;

  RingElement eval(const RingElement &r) const;
  Vector eval(const FreeModule *resultfree, const Vector &v) const;
  Matrix eval(const FreeModule *newrows, const Matrix &m) const;


  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  class_identifier class_id() const { return CLASS_RingMap; }
  type_identifier  type_id () const { return TY_RING_MAP; }
  const char * type_name   () const { return "RingMap"; }

  RingMap *cast_to_RingMap() { return this; }
  const RingMap *cast_to_RingMap() const{ return this; }
  
  int length_of() const { return map.length(); }

  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;
};
#endif
class RingMap : public type
{
  struct var {
    bool is_zero;		// Does this variable map to 0?

    bool coeff_is_one;
    bool monom_is_one;
    bool bigelem_is_one;

    ring_elem coeff;		// this variable maps to coeff*monom*bigelem
				// where coeff is 1 if isone is true.
				// and   monom is 1 if mon_isone is true,
				// and   bigelem is 1 if bigone_isone is true.
				// coeff is an element of type K.
    int *monom;			// This is an exponent vector in R.
    ring_elem bigelem;
  };

  const Ring *R;		// This is the target ring.
  const Ring *K;
  const Monoid *M;

  bool is_monomial;		// True, if each term maps to a term in the
				// target ring.

  int nvars;			// Number of variables in the source ring.
  var *_elem;			// elem[i] is the structure representing the image of
				// the i th variable.
public:
  RingMap(const Matrix &m);
  ~RingMap();

  const Ring *Ring_of() const { return R; }
  const ring_elem elem(int i) const { return _elem[i].bigelem; }
  
  ring_elem eval_term(const Ring *coeff_ring, const ring_elem coeff, 
		      const int *vp) const;

  RingElement eval(const RingElement &r) const;
  Vector eval(const FreeModule *resultfree, const Vector &v) const;
  Matrix eval(const FreeModule *newrows, const Matrix &m) const;

  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;

  ////////////////////
  // Infrastructure //
  ////////////////////

  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  class_identifier class_id() const { return CLASS_RingMap; }
  type_identifier  type_id () const { return TY_RING_MAP; }
  const char * type_name   () const { return "RingMap"; }

  RingMap *cast_to_RingMap() { return this; }
  const RingMap *cast_to_RingMap() const{ return this; }
  
  int length_of() const { return nvars; }

};

#endif
