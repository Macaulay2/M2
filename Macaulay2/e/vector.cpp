// Copyright 1995 Michael E. Stillman

#include "vector.hpp"

inline Vector::Vector(const FreeModule *F) : 
  F(F), val(0)
{
}
inline Vector::Vector(const FreeModule *F, const RingElement *r, int i) : 
  F(F), val(F->raw_term(F->get_ring()->copy(r->get_value()),i))
{
}
inline Vector::Vector(const FreeModule *F, const vec v) : 
  F(F), val(v)
{
}

bool Vector::is_equal(const Vector *b) const
{
  if (this == b) return true;
  if (free_of() != b->free_of()) return false;
  return free_of()->is_equal(val, b->val);
}

Vector *Vector::make_raw(const FreeModule *F, vec v)
  // This function GRABS the v, so be careful...
{
  return new Vector(F,v);
}

Vector *Vector::zero(const FreeModule *F)
{
  return Vector::make_raw(F,0);
}

Vector *Vector::e_sub_i(const FreeModule *F, int i)
{
  if (i < 0 || i >= F->rank())
    {
      ERROR("index out of range");
      return 0;
    }
  return Vector::make_raw(F,F->e_sub_i(i));
}

Vector *Vector::make(const FreeModule *F, const RingElement *r, int n)
{
  if (F->get_ring() != r->get_ring()) 
    {
      ERROR("expected same ring");
      return 0;
    }
  if (n < 0 || n >= F->rank())
    {
      ERROR("index out of range");
      return 0;
    }
  vec v = F->raw_term(F->get_ring()->copy(r->get_value()), n);
  return Vector::make_raw(F,v);
}

Vector *Vector::make(const FreeModule *F, RingElement_array *fs)
{
  unsigned int n = F->rank();
  if (n != fs->len)
    {
      ERROR("wrong length array for vector creation");
      return 0;
    }
  const Ring *R = F->get_ring();
  for (unsigned int i=0; i<n; i++)
    {
      if (R != fs->array[i]->get_ring())
	{
	  ERROR("expected same ring");
	  return 0;
	}
    }
  Vector *result = new Vector(F);
  for (unsigned int i=0; i<n; i++)
    result = (*result) + new Vector(F, fs->array[i], i);

  return result;
}

RingElement_array * Vector::get_all_coeffs() const
{
  RingElement_array *result = (RingElement_array *) getmem(sizeofarray(result,F->rank()));
  result->len = F->rank();

  for (int i=0; i<F->rank(); i++)
    result->array[i] = RingElement::make_raw(get_ring(), F->get_coefficient(val, i));

  return result;
}

void Vector::text_out(buffer &o) const
{
  F->elem_text_out(o, val);
}

Vector *Vector::get_terms(int lo, int hi) const
{
  return new Vector(free_of(), free_of()->get_terms(val, lo, hi));
}

Vector *Vector::operator-() const
{
  return new Vector(free_of(), free_of()->negate(val));
}

Vector *Vector::operator+(const Vector *b) const
{
  if (F != b->free_of())
    {
      ERROR("vector addition requires both elements to have the same ambient module");
      return 0;
    }

  return new Vector(F, F->add(val, b->val));
}

Vector *Vector::operator-(const Vector *b) const
{
  if (F != b->free_of())
    {
      ERROR("vector subtraction requires both elements to have the same ambient module");
      return 0;
    }

  return new Vector(F, F->subtract(val, b->val));
}

Vector *Vector::operator*(const RingElement *b) const
{
  if (get_ring() != b->get_ring())
    {
      ERROR("scalar multiplication requires both elements to have the same base ring");
      return 0;
    }

  return new Vector(F, F->rightmult(val, b->get_value()));
}

Vector *Vector::operator*(int n) const
{
  return new Vector(F, F->mult(n, val));
}

RingElement *Vector::get_coefficient(int i) const
{
  if (i < 0 || i >= F->rank())
    {
      ERROR("index out of range");
      return 0;
    }
  return RingElement::make_raw(get_ring(), F->get_coefficient(val, i));
}

int Vector::lead_component() const
{
  // returns -1 if applied to the zero vector
  return free_of()->lead_component(val);
}

Vector *Vector::lead_term() const
{
#warning "implement Vector::lead_term()"
#if 0
  // MES aug 2002
  return new Vector(free_of(), free_of()->lead_term(val));
#endif
  return 0;
}

RingElement *Vector::lead_coefficient() const
{
  if (val == 0) return 0;
  return RingElement::make_raw(get_ring(), free_of()->lead_coefficient(val));
}

bool Vector::is_homogeneous() const
{
  return free_of()->is_homogeneous(get_value());
}

M2_arrayint Vector::degree() const
{
  if (is_zero()) 
    {
      ERROR("the zero element has no degree");
      return 0;
    }

  const FreeModule *F = free_of();
  const Ring *R = get_ring();
  const Monoid *D = R->degree_monoid();

  M2_arrayint result = makearrayint(D->n_vars());

  int *mon = new int[D->monomial_size()];

  F->degree(get_value(), mon);
  D->to_expvector(mon, result->array);

  delete [] mon;
  return result;
}

Vector *Vector::homogenize(int v, const M2_arrayint wts) const
{
  if (v < 0 || v >= get_ring()->n_vars())
    {
      ERROR("homogenization: improper ring variable");
      return 0;
    }
  if (wts == NULL || wts->len != (unsigned)(get_ring()->n_vars()))
    {
      ERROR("homogenization: improper weight function");
      return 0;
    }
  if (wts->array[v] == 0)
    {
      ERROR("homogenization: variable weight is zero");
      return 0;
    }

  return Vector::make_raw(F, F->homogenize(get_value(), v, wts));
}

Vector *Vector::homogenize(int v, int deg, const M2_arrayint wts) const
{
  if (v < 0 || v >= get_ring()->n_vars())
    {
      ERROR("homogenization: improper ring variable");
      return 0;
    }
  if (wts == NULL || wts->len != (unsigned)(get_ring()->n_vars()))
    {
      ERROR("homogenization: improper weight function");
      return 0;
    }
  if (wts->array[v] == 0)
    {
      ERROR("homogenization: variable weight is zero");
      return 0;
    }

  return Vector::make_raw(F, F->homogenize(get_value(), v, deg, wts));
}

int Vector::n_terms() const
{
  int result = 0;
  for (vec a = get_value(); a != NULL; a = a->next)
    result++;
  return result;
}
