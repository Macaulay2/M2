// Copyright 1995 Michael E. Stillman

#ifndef _vector_hh_
#define _vector_hh_

#include "relem.hpp"
#include "freemod.hpp"

class Vector
{
  const FreeModule *F;
  vec val;

  Vector(const FreeModule *F);
  Vector(const FreeModule *F, const RingElement *r, int n);
  Vector(const FreeModule *F, const vec v);

public:
  static Vector *zero(const FreeModule *F);
  static Vector *make_raw(const FreeModule *F, vec v);
  static Vector *make(const FreeModule *F, const RingElement *r, int n);
  static Vector *make(const FreeModule *F, RingElement_array *fs);
  static Vector *e_sub_i(const FreeModule *F, int i);

  vec get_value() { return val; }
  const vec get_value() const { return val; }

  const Ring    * get_ring()  const  { return F->get_ring(); }
  const FreeModule *free_of() const { return F; }
  const Monoid *degree_monoid() const { return get_ring()->degree_monoid(); }

  bool is_zero() const { return F->is_zero(val); }
  bool is_equal(const Vector *b) const;

  Vector *operator-() const;
  Vector *operator+(const Vector *b) const;
  Vector *operator-(const Vector *b) const;
  Vector *operator*(const RingElement *b) const; // returns this * b
  Vector *operator*(int n) const;
  Vector *left_mult(const RingElement *b) const; // returns b * this

  RingElement *get_coefficient(int i) const;
  RingElement_array *get_all_coeffs() const;

  bool     is_homogeneous() const;
  M2_arrayint  degree() const;

  Vector *get_terms(int lo, int hi) const;
  int n_terms() const;
  int lead_component() const;
  Vector *lead_term() const;
  RingElement *lead_coefficient() const;
  Vector   *homogenize(int v, const M2_arrayint wts) const;
  Vector   *homogenize(int v, int deg, const M2_arrayint wts) const;

  void text_out(buffer &o) const;
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
