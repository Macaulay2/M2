// Copyright 1995 Michael E. Stillman

#include "vector.hpp"

stash *Vector_rec::mystash;

bool Vector_rec::equals(const object_element *o) const
{
  const Vector_rec *b = dynamic_cast<const Vector_rec *>(o);
  if (F != b->F) return false;
  return F->is_equal(val,b->val);
}

void Vector_rec::text_out(buffer &o) const
{
  F->elem_text_out(o, val);
}

void Vector_rec::bin_out(buffer &o) const
{
  F->elem_bin_out(o, val);
}

void Vector::text_out(buffer &o) const
{
  obj->text_out(o);
}

void Vector::bin_out(buffer &o) const
{
  obj->bin_out(o);
}

int Vector_rec::length_of() const
{
  return F->n_terms(val);
}

Vector Vector::get_terms(int lo, int hi) const
{
  Vector result(free_of(), free_of()->get_terms(obj->val, lo, hi));
  return result;
}

Vector Vector::operator-() const
{
  Vector result(free_of(), free_of()->negate(obj->val));
  return result;
}

Vector Vector::operator+(const Vector &b) const
{
  Vector result(free_of());
  if (free_of() != b.free_of())
    gError << "vector addition requires both elements to have the same "
      << "ambient module";
  else 
    {
      vec f = obj->val;
      vec g = b.obj->val;
      result.obj->val = free_of()->add(f, g);
    }
  return result;
}
Vector Vector::operator-(const Vector &b) const
{
  Vector result(free_of());
  if (free_of() != b.free_of())
    gError << "vector subtraction requires both elements to have the same "
      << "ambient module";
  else 
    {
      vec f = obj->val;
      vec g = b.obj->val;
      result.obj->val = free_of()->subtract(f, g);
    }
  return result;
}

Vector Vector::operator*(const RingElement &b) const
{
  Vector result(free_of());
  if (get_ring() != b.get_ring())
    gError << "scalar multiplication requires both elements to have the same "
      << "base ring";
  else 
     result.obj->val = free_of()->rightmult(obj->val, b.get_value());
  return result;
}

Vector Vector::operator*(int n) const
{
  Vector result(free_of(), free_of()->mult(n, obj->val));
  return result;
}

RingElement Vector::get_coefficient(int i) const
{
  RingElement result(get_ring(), free_of()->get_coefficient(obj->val, i));
  return result;
}

int Vector::lead_component() const
{
  return free_of()->lead_component(obj->val);
}

Vector Vector::lead_term() const
{
  Vector result(free_of(), free_of()->lead_term(obj->val));
  return result;
}

RingElement Vector::lead_coefficient() const
{
  RingElement result(get_ring()->Ncoeffs(), free_of()->lead_coefficient(obj->val));
  return result;
}

bool Vector::is_homogeneous() const
{
  return free_of()->is_homogeneous(get_value());
}

intarray Vector::degree() const
{
  const Ring *R = get_ring();
  const FreeModule *F = free_of();
  intarray result;

  int *mon = new int[R->degree_monoid()->monomial_size()];
  int *d = result.alloc(R->degree_monoid()->n_vars());

  if (is_zero()) 
    gError << "the zero element has no degree";
  else
    {
      F->degree(get_value(), mon);
      R->degree_monoid()->to_expvector(mon, d);
    }

  delete [] mon;
  return result;
}

Vector Vector::homogenize(int v, const int *wts) const
{
  const FreeModule *F = free_of();
  Vector result(F, F->homogenize(get_value(), v, wts));
  return result;
}

Vector Vector::homogenize(int v, int deg, const int *wts) const
{
  const FreeModule *F = free_of();
  Vector result(F, F->homogenize(get_value(), v, deg, wts));
  return result;
}

Vector Vector::sub_vector(const FreeModule * /*F*/, const intarray & /*r*/) const
{
#if 0
  Vector result(F);
  if (!is_zero())
    vinfo()->sub_vector(r, ints(), result.get_intarray());
  return result;
#endif
  return Vector();
}

#if 0
Vector Vector::mult_by_matrix(const matrix &M) const
{
  Vector result(free_of());
  if (!is_zero())
    vinfo()->mult_by_matrix(M, ints(), result.get_intarray());
  return result;
}

Vector Vector::component_shift(int n) const
    // Shift each component by n
{
  Vector result(free_of());
  if (!is_zero())
    vinfo()->component_shift(ints(), n, result.get_intarray());
  return result;
}

Vector Vector::tensor_shift(int n, int m) const
    // component i --> n*i+m
{
  Vector result(free_of());
  if (!is_zero())
    vinfo()->tensor_shift(ints(), n, m, result.get_intarray());
  return result;
}

Vector Vector::tensor(const Vector &v, int m) const
    // return the tensor product of 'this' and 'v', where 'm' is the
    // rank of the ambient freemodule to 'v'.
{
  Vector result(free_of());
  if (!is_zero() && !v.is_zero())
    vinfo()->tensor(ints(), v.ints(), m, result.get_intarray());
  return result;
}

Vector Vector::diff(const Vector &v, int m, int use_coef) const
    // return the result (as a Vector of the same shape as for 'tensor'
    // of differentiating v by 'this', where 'm' is the
    // rank of the ambient freemodule to 'v'.  'use_coef' == 0 means
    // use contraction, not differentiation.
{
  Vector result(free_of());
  if (!is_zero() && !v.is_zero())
    vinfo()->diff(ints(), v.ints(), m, use_coef, result.get_intarray());
  return result;
}
#endif
