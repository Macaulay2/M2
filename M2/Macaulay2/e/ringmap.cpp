// Copyright 1996  Michael E. Stillman

#include "ringmap.hpp"
#include "matrix.hpp"
#include "bin_io.hpp"

stash *RingMap::mystash;


RingMap::RingMap(const Matrix &m)
: R(m.get_ring())
{
  bump_up(R);
  M = R->Nmonoms();
  K = R->Ncoeffs();

  nvars = m.n_cols();
  is_monomial = true;

  ring_elem one = K->from_int(1);

  // Allocate space for the ring map elements
  _elem = new var[nvars];

  for (int i=0; i<nvars; i++)
    {
      // First initialize these fields:
      _elem[i].is_zero = false;
      _elem[i].coeff_is_one = true;
      _elem[i].monom_is_one = true;
      _elem[i].bigelem_is_one = true;
      _elem[i].coeff = (Nterm *) NULL;
      _elem[i].monom = NULL;

      vec v = m[i];
      _elem[i].bigelem = m.elem(0,i);  // This does a copy.

      // We determine the parts of elem[i] using v.

      if (v == NULL) 
	_elem[i].is_zero = true;
      else if (v->next == NULL)
	{
	  // This is a single term
	  if (!K->is_equal(v->coeff, one))
	    {
	      _elem[i].coeff_is_one = false;
	      _elem[i].coeff = K->copy(v->coeff);
	    }

	  if (!M->is_one(v->monom))  // should handle M->n_vars() == 0 case correctly.
	    {
	      _elem[i].monom_is_one = false;
	      _elem[i].monom = M->make_new(v->monom);
	    }
	}
      else
	{
	  // This is a bigterm
	  is_monomial = false;
	  _elem[i].bigelem_is_one = false;
	}
    }
  K->remove(one);
}

RingMap::~RingMap()
{
  for (int i=0; i<nvars; i++)
    {
      if (!_elem[i].coeff_is_one) K->remove(_elem[i].coeff);
      if (!_elem[i].monom_is_one) M->remove(_elem[i].monom);
      R->remove(_elem[i].bigelem);
    }
  delete [] _elem;
  K = NULL;
  M = NULL;
  bump_down(R);
}

void RingMap::bin_out(buffer &o) const
{
  bin_int_out(o, nvars);
  for (int i=0; i<nvars; i++)
    R->elem_text_out(o, _elem[i].bigelem);
}

void RingMap::text_out(buffer &o) const
{
  o << "(";
  for (int i=0; i<nvars; i++)
    {
      if (i > 0) o << ", ";
      R->elem_text_out(o, _elem[i].bigelem);
    }
  o << ")";
}

ring_elem RingMap::eval_term(const Ring *sourceK,
			      const ring_elem a, const int *vp) const
{
  int i;
  assert(sourceK->total_n_vars() <= nvars);
  int first_var = sourceK->total_n_vars();
  int npairs = *vp++ - 1;
  for (i=0; i<npairs; i++)
    {
      int v = first_var + varpower::var(vp[i]);
      if (v >= nvars || _elem[v].is_zero)
	return R->from_int(0);	// The result is zero.
    }

  // If K is a coeff ring of R, AND map is an identity on K,
  // then don't recurse: use this value directly.
  // Otherwise, we must recurse, I guess.
  ring_elem result = sourceK->eval(this, a);
  if (R->is_zero(result)) return result;

  ring_elem result_coeff = R->Ncoeffs()->from_int(1);
  int *result_monom = R->Nmonoms()->make_one();
  int *temp_monom = R->Nmonoms()->make_one();

  if (!R->is_commutative_ring())
    {
      // This is the only non-commutative case so far
      for (i=0; i<npairs; i++)
	{
	  int v = first_var + varpower::var(vp[i]);
	  int e = varpower::exponent(vp[i]);
	  for (int j=0; j<e; j++)
	    {
	      assert(v < nvars);
	      ring_elem tmp = R->mult(_elem[v].bigelem,result);
	      R->remove(result);
	      result = tmp;
	    }
	}
    }
  else {
    for (i=0; i<npairs; i++)
      {
	int v = first_var + varpower::var(vp[i]);
	int e = varpower::exponent(vp[i]);
	assert(v < nvars);
	if (_elem[v].bigelem_is_one)
	  {
	    if (!_elem[v].coeff_is_one)
	      {
		ring_elem tmp = R->Ncoeffs()->power(_elem[v].coeff, e);
		R->Ncoeffs()->mult_to(result_coeff, tmp);
		R->Ncoeffs()->remove(tmp);
	      }
	    if (!_elem[v].monom_is_one)
	      {
		R->Nmonoms()->power(_elem[v].monom, e, temp_monom);
		R->Nmonoms()->mult(result_monom, temp_monom, result_monom);
	      }
	  }
	else
	  {
	    ring_elem thispart = R->power(_elem[v].bigelem, e);
	    R->mult_to(result, thispart);
	    R->remove(thispart);
	    if (R->is_zero(result)) break;
	  }
      }
    ring_elem temp = R->term(result_coeff, result_monom);
    R->Ncoeffs()->remove(result_coeff);
    R->Nmonoms()->remove(result_monom);
    R->Nmonoms()->remove(temp_monom);
    R->mult_to(result,temp);
    R->remove(temp);
  }
  return result;
}

RingElement RingMap::eval(const RingElement &r) const
{
  RingElement result(get_ring(), r.get_ring()->eval(this, r.get_value()));
  return result;
}

Vector RingMap::eval(const FreeModule *F, const Vector &v) const
{
  Vector result(F, v.free_of()->eval(this, F, v.get_value()));
  return result;
}

Matrix RingMap::eval(const FreeModule *F, const Matrix &m) const
{
  Matrix result(F);
  for (int i=0; i<m.n_cols(); i++)
    result.append(m.rows()->eval(this, F, m[i]));
  return result;
}


