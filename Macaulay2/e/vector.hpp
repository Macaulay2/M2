// Copyright 1995 Michael E. Stillman

#ifndef _vector_hh_
#define _vector_hh_

#include "relem.hpp"
#include "freemod.hpp"

class Vector_rec : public object_element
{
  friend void i_stashes();
  static stash *mystash;
  void *operator new(size_t) { return mystash->new_elem(); }
  void operator delete(void *p) { mystash->delete_elem(p); }

  friend class Vector;

  const FreeModule *F;
  vec val;

  Vector_rec  (const FreeModule *FF, vec val) 
    : F(FF), val(val) { bump_up((FreeModule *) F); }
  ~Vector_rec () { F->remove(val); bump_down((FreeModule *) F); }

  // Infrastructure
  class_identifier class_id() const { return CLASS_Vector; }
  type_identifier  type_id () const { return TY_VECTOR; }
  const char * type_name   () const { return "Vector"; }

  Vector       cast_to_Vector   ();

  int          length_of        () const;

  void         text_out (buffer &o) const;
  void         bin_out  (buffer &o) const;
};

class Vector
{
  POINTER(Vector, Vector_rec)
public:
  Vector(const FreeModule *F);
  Vector(const FreeModule *F, int n);
  Vector(const FreeModule *F, const RingElement r, int n);
  Vector(const FreeModule *F, const vec v);

  vec get_value() { return obj->val; }
  const vec get_value() const { return obj->val; }
  const Ring    * Ring_of()  const  { return obj->F->Ring_of(); }
  const FreeModule *free_of() const { return obj->F; }

  const Monoid *degree_monoid() const { return Ring_of()->degree_monoid(); }

  // vector arithmetic

  bool is_zero() const;
  bool is_equal(const Vector &b) const;

  Vector operator-() const;
  Vector operator+(const Vector &b) const;
  Vector operator-(const Vector &b) const;
  Vector operator*(const RingElement &b) const;
  Vector operator*(int n) const;

//  RingElement elem(int i) const;
  RingElement get_coefficient(int i) const;
  int lead_component() const;
  Vector lead_term() const;
  RingElement lead_coefficient() const;

  bool     is_homogeneous() const;
  Vector   homogenize(int v, const int *wts) const;
  Vector   homogenize(int v, int deg, const int *wts) const;
  intarray  degree() const;

  Vector sub_vector(const FreeModule *F, const intarray &r) const;
//  Vector mult_by_matrix(const matrix &M) const;

//  Vector component_shift(int n) const; // Shift each component by n
//  Vector tensor_shift(int n, int m) const; // component i --> n*i+m
//  Vector tensor(const Vector &v, int m) const;
//  Vector diff(const Vector &v, int m, int use_coef) const;

  Vector get_terms(int lo, int hi) const;

  void bin_out(buffer &o) const;
  void text_out(buffer &o) const;
};


//-----------------------------------------------------------------
inline Vector Vector_rec :: cast_to_Vector() 
{ return Vector(this,caster); }
//-----------------------------------------------------------------

inline Vector::Vector(const FreeModule *F) : 
  obj(new Vector_rec(F, NULL))
{  
}

inline Vector::Vector(const FreeModule *F, int n) : 
  obj(new Vector_rec(F, 
		      F->term(n, F->Ring_of()->from_int(1))))
{
}

inline Vector::Vector(const FreeModule *F, const RingElement r, int n) : 
  obj(new Vector_rec(F, F->term(n, r.get_value())))
{
}

inline bool Vector::is_zero() const
{
  return free_of()->is_zero(obj->val);
}

inline Vector::Vector(const FreeModule *F, const vec v) : 
  obj(new Vector_rec(F, v))
{
}

inline bool Vector::is_equal(const Vector &b) const
{
  if (this == &b) return true;
  if (free_of() != b.free_of()) return false;
  return free_of()->is_equal(obj->val, b.obj->val);
}

#endif
