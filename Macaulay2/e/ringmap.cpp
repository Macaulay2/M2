// Copyright 1996  Michael E. Stillman

#include "ringmap.hpp"
#include "matrix.hpp"
#include "bin_io.hpp"

stash *RingMap_rec::mystash;

RingMap_rec::RingMap_rec(const Matrix &m)
: R(m.Ring_of())
{
  bump_up((Ring *) R);
  // possibly expand out elements here, into: coef, mon, poly?
  for (int i=0; i<m.n_cols(); i++)
    map.append(m.elem(0,i));
}

RingMap_rec::~RingMap_rec()
{
  for (int i=0; i<map.length(); i++)
    R->remove(map[i]);
  bump_down((Ring *) R);
}

ring_elem RingMap::eval_term(const Ring *K,
			      const ring_elem a, const int *vp) const
{
  int i;
  int first_var = K->total_n_vars();
  int npairs = *vp++ - 1;
  for (i=0; i<npairs; i++)
    {
      int v = first_var + varpower::var(vp[i]);
      if (v >= length() || Ring_of()->is_zero(elem(v)))
	return Ring_of()->from_int(0);	// This is zero.
    }

  ring_elem result = K->eval(*this, a);
  if (Ring_of()->is_zero(result)) return result;

  for (i=0; i<npairs; i++)
    {
      int v = varpower::var(vp[i]);
      int e = varpower::exponent(vp[i]);
      ring_elem thispart = Ring_of()->power(elem(first_var + v), e);
      Ring_of()->mult_to(result, thispart);
      Ring_of()->remove(thispart);
      if (Ring_of()->is_zero(result)) break;
    }
  return result;
}



RingElement RingMap::eval(const RingElement &r) const
{
  RingElement result(Ring_of(), r.Ring_of()->eval(*this, r.get_value()));
  return result;
}

Vector RingMap::eval(const FreeModule *F, const Vector &v) const
{
  Vector result(F, v.free_of()->eval(*this, F, v.get_value()));
  return result;
}

Matrix RingMap::eval(const FreeModule *F, const Matrix &m) const
{
  Matrix result(F);
  for (int i=0; i<m.n_cols(); i++)
    result.append(m.rows()->eval(*this, F, m[i]));
  return result;
}

void RingMap_rec::bin_out(buffer &o) const
{
  bin_int_out(o, map.length());
  for (int i=0; i<map.length(); i++)
    R->elem_text_out(o, map[i]);
}

void RingMap_rec::text_out(buffer &o) const
{
  o << "(";
  for (int i=0; i<map.length(); i++)
    {
      if (i > 0) o << ", ";
      R->elem_text_out(o, map[i]);
    }
  o << ")";
}

void RingMap::text_out(buffer &o) const
{
  obj->text_out(o);
}
