// Copyright 1998 by Michael Stillman

#include "Eringmap.hpp"
#include "Ematrix.hpp"

ERingMap::ERingMap(const EMatrix *m)
: R(m->getTarget()->getRing())
{
  bump_up(R);
  const EPolynomialRing *RP = R->toPolynomialRing();
  ispolyring = (RP != 0);
  if (ispolyring)
    {
      M = RP->getMonoid();
      K = RP->getCoefficientRing();
    }
  else
    {
      M = 0;
      K = R;
    }

  nvars = m->n_cols();

  ERingElement one = K->from_int(1);

  // Allocate space for the ring map elements
  _elem = new var[nvars];

  for (int i=0; i<nvars; i++)
    {
      // First initialize these fields:
      _elem[i].is_zero = false;
      _elem[i].coeff_is_one = true;
      _elem[i].monom_is_one = true;
      _elem[i].bigelem_is_one = true;
      _elem[i].coeff = K->zero();
      _elem[i].monom = 0;

      const EVector &v = m->column(i);
      _elem[i].bigelem = m->entry(0,i);  // This does a copy.

      // We determine the parts of elem[i] using v.

      evec *w = v.elems;
      if (w == 0) 
	_elem[i].is_zero = true;
      else if (ispolyring && w->next == 0)
	{
	  // This is a single term
	  if (!K->is_equal(w->coeff, one))
	    {
	      _elem[i].coeff_is_one = false;
	      _elem[i].coeff = K->clone(w->coeff);
	    }

	  if (!M->is_one(w->monom))  // should handle M->n_vars() == 0 case correctly.
	    {
	      _elem[i].monom_is_one = false;
	      _elem[i].monom = w->monom;
	    }
	}
      else
	{
	  // This is a bigterm
	  _elem[i].bigelem_is_one = false;
	}
    }
  K->remove(one);
}

ERingMap::~ERingMap()
{
  for (int i=0; i<nvars; i++)
    {
      if (!_elem[i].coeff_is_one) K->remove(_elem[i].coeff);
      R->remove(_elem[i].bigelem);
    }
  delete [] _elem;
  K = 0;
  M = 0;
  bump_down(R);
}


ERingElement ERingMap::evaluateTerm(
    const ERing *sourceK,
    const ERingElement a, 
    const intarray &vp) const
{
  // The target ring should be a polynomial ring here always
  int i;
  int first_var = sourceK->total_n_vars();
  int vplen = vp.length();  // Will be 2*(#terms).
  for (i=vplen-1; i>=0; i -= 2)
    {
      int v = first_var + vp[i-1];
      if (v >= nvars || _elem[v].is_zero)
	return R->from_int(0);	// The result is zero.
    }

  // If K is a coeff ring of R, AND map is an identity on K,
  // then don't recurse: use this value directly.
  // Otherwise, we must recurse, I guess.
  ERingElement result = sourceK->evaluate(this, a);
  if (R->is_zero(result)) return result;

  ERingElement result_coeff; // element of K, if target is a polynomial ring.
  const monomial *result_monom = NULL; // element of M, if target is a polynomial ring.
  if (ispolyring)
    {
      result_coeff = K->from_int(1);
      result_monom = M->one();
    }

  for (i=vplen-1; i>=0; i -= 2)
    {
      int v = first_var + vp[i-1];
      int e = vp[i];
      if (_elem[v].bigelem_is_one)
	{
	  if (!_elem[v].coeff_is_one)
	    {
	      ERingElement tmp = K->power(_elem[v].coeff, e);
	      ERingElement tmp2 = K->mult(result_coeff, tmp);
	      K->remove(tmp);
	      K->remove(result_coeff);
	      result_coeff = tmp2;
	    }
	  if (!_elem[v].monom_is_one)
	    {
	      const monomial *tmp = M->power(_elem[v].monom, e);
	      result_monom = M->mult(result_monom, tmp);
	    }
	}
      else
	{
	  ERingElement thispart = R->power(_elem[v].bigelem, e);
	  ERingElement tmp = R->mult(thispart,result);
	  R->remove(thispart);
	  R->remove(result);
	  result = tmp;
	  if (R->is_zero(result)) break;
	}
    }
  if (ispolyring)
    {
      ERingElement tmp = R->toPolynomialRing()->make_term(result_coeff, result_monom);
      ERingElement tmp2 = R->mult(tmp,result);
      K->remove(result_coeff);
      R->remove(result);
      R->remove(tmp);
      result = tmp2;
    }
  return result;
}

ERingElement ERingMap::evaluate(const ERing *source, const ERingElement r) const
{
  return source->evaluate(this, r);
}

EVector ERingMap::evaluate(const EFreeModule *Ftarget, const EVector &v) const
{
  return v.getRing()->vec_evaluate(this, Ftarget, v);
}

EMatrix *ERingMap::evaluate(const EFreeModule *Ftarget, const EMatrix *m) const
{
  return m->evaluate(this,Ftarget);
}

///////////////////////////////////////////////
// Evaluation routines for our various rings //
///////////////////////////////////////////////

ERingElement EZZ::evaluate(const ERingMap *map, const ERingElement r) const
{
  return map->getTarget()->from_int(ZZVAL(r));
}
ERingElement EZZp::evaluate(const ERingMap *map, const ERingElement r) const
{
  return map->getTarget()->from_int(ZZPVAL(r));
}
ERingElement EPolynomialRing::evaluate(const ERingMap *map, const ERingElement r) const
{
  // The way we collect the result depends on whether the target ring
  // is a polynomial ring: if so, use a heap structure.  If not, just add to the result.

  const ERing *target = map->getTarget();
  const EPolynomialRing *R = target->toPolynomialRing();
  if (R != 0)
    {
      intarray vp;
      heap H(R);

      for (iterator t = r; t.valid(); ++t)
	{
	  vp.shrink(0);
	  getMonoid()->to_variable_exponent_pairs(t->monom, vp);
	  ERingElement g = map->evaluateTerm(K, t->coeff, vp);
	  H.add(g);
	}
      return H.value();
    }
  else 
    {
      ERingElement result = target->zero();
      intarray vp;

      for (iterator t = r; t.valid(); ++t)      
	{
	  vp.shrink(0);
	  getMonoid()->to_variable_exponent_pairs(t->monom, vp);
	  ERingElement g = map->evaluateTerm(K, t->coeff, vp);
	  target->add_to(result, g);
	}
      return result;
    }
}
// other rings here...

EVector ERing::vec_evaluate(const ERingMap *map, 
			    const EFreeModule *Ftarget,
			    const EVector &v) const
{
  // This one is pretty easy: loop through each term of v,
  // evaluate, and collect the results in a vector.

  intarray vp;
  EVector::collector H(Ftarget);
  for (EVector::iterator t = v; t.valid(); ++t)
    {
      evec *tm = vec_new_term();
      tm->component = t->component;
      tm->coeff = evaluate(map, t->coeff);
      if (is_zero(tm->coeff))
	{
	  vec_remove_term(tm);
	  continue;
	}
      H.append(tm);
    }
  return H.value();
}
EVector EPolynomialRing::vec_evaluate(const ERingMap *map, 
			    const EFreeModule *Ftarget,
			    const EVector &v) const
{
  // This one is pretty easy: loop through each term of v,
  // evaluate, and collect the results in a vector heap.

  intarray vp;
  EVectorHeap H(Ftarget);

  for (EVector::iterator t = v; t.valid(); ++t)
    {
      vp.shrink(0);
      getMonoid()->to_variable_exponent_pairs(t->monom, vp);
      ERingElement r = map->evaluateTerm(K, t->coeff, vp);

      EVector g = vec_make(Ftarget,r,t->component);
      H.add(g);
      remove(r);
    }
  return H.value();
}

///////////////////
// promote, lift //
///////////////////

///////////////////
// Target is EZZ //
///////////////////
bool EZZ::promote(const ERing *Rf, const ERingElement f, ERingElement &result) const
{
  return false;
}

bool EZZ::lift(const ERing *Rg, const ERingElement f, ERingElement &result) const
{
  return false;
}

bool EZZ::vec_promote(const EVector &v, const EFreeModule *resultF, EVector &result) const
{
  return false;
}

bool EZZ::vec_lift(const EVector &v, const EFreeModule *resultF, EVector &result) const
{
  return false;
}

////////////////////
// Target is EZZp //
////////////////////
bool EZZp::promote(const ERing *Rf, const ERingElement f, ERingElement &result) const
  // Rf --> A = this, f in Rf.
{
  if (Rf == EZ)
    {
      result = from_int(ZZVAL(f));
      return true;
    }
  return false;
}

bool EZZp::lift(const ERing *Rg, const ERingElement f, ERingElement &result) const
  // Rg --> A = this, f in A.
{
  if (Rg == EZ)
    {
      // map is ZZ --> ZZ/p.  Lift to an integer
      int a = ZZPVAL(f);
      if (a >= P/2) a -= P;
      result = Rg->from_int(a);
      return true;
    }
  return false;
}

bool EZZp::vec_promote(const EVector &v, const EFreeModule *resultF, EVector &result) const
  // Rf = ring(v) --> A=this=ZZ/p, resultF over A.
{
  if (v.getRing() == EZ)
    {
      // loop through the vector, promoting each element:
      EVector::collector H(resultF);
      for (EVector::iterator t = v; t.valid(); ++t)
	{
	  ERingElement a = from_int(ZZVAL(t->coeff));
	  if (is_zero(a)) continue;
	  evec * tm = vec_new_term();
	  tm->coeff = a;
	  tm->component = t->component;
	  H.append(tm);
	}
      result = H.value();
      return true;
    }
  return false;
}

bool EZZp::vec_lift(const EVector &v, const EFreeModule *resultF, EVector &result) const
{
  if (resultF->getRing() == EZ)
    {
      // loop through the vector, promoting each element:
      EVector::collector H(resultF);
      for (EVector::iterator t = v; t.valid(); ++t)
	{
	  ERingElement a;
	  lift(EZ, t->coeff, a);
	  evec * tm = vec_new_term();
	  tm->coeff = a;
	  tm->component = t->component;
	  H.append(tm);
	}
      result = H.value();
      return true;
    }
  return false;
}

/////////////////////////////////
// Target is a polynomial ring //
/////////////////////////////////
// Here the possible maps are
// Rf --> A = Rf[x]/I
// Rf[x]/J --> A = Rf[x]/I
// In either of these cases, A may be non-commutative.
  // case 1:  Rf = A[x]/J ---> A[x]/I  is one of the 'base_ring's of 'this'.
  // case 2:  Rf = A      ---> A[x]/I  is the ring of scalars

bool EPolynomialRing::promote(const ERing *Rf, const ERingElement f, ERingElement &result) const
  // Rf --> A = this
{
  //if (getCover() == Rf->getCover())
  //{
  //result = getCover()->clone(f);
  ////normal_form(result);  // In the ring A.  MES: put this in when quotients are ready.
  //return true;
  //}
  if (K == Rf)
    {
      const monomial *m = getMonoid()->one();
      result = make_term(f, m);
      return true;
    }
  return false;
}

bool EPolynomialRing::lift(const ERing *Rg, const ERingElement f, ERingElement &result) const
  // Rg --> A = this
{
  //if (getCover() == Rg->getCover())
  //{
  //result = getCover()->clone(f);
  //// Rg->normal_form(result);  // In the ring Rg  MES: put this in when quotients are ready.
  //return true;
  //}
  if (K == Rg)
    {
      epoly *f1 = POLYVAL(f);
      if (f1 == 0)
	{
	  result = K->from_int(0);
	  return true;
	}
      if (f1->next != 0) return false;
      if (!getMonoid()->is_one(f1->monom))
	return false;
      result = K->clone(f1->coeff);
      return true;
    }
  return false;
}

bool EPolynomialRing::vec_promote(const EVector &v, const EFreeModule *resultF, EVector &result) const
  // Rf --> A = this
  // ring(v) = Rf, ring(resultF)=A.
{
  //if (getCover() == v.getRing()->getCover())
  //{
  //result = v.translate(resultF);
  ////resultF->normal_form(result);  MES: once quotients are ready.
  //return true;
  //}
  if (K == v.getRing())
    {
      const monomial *m = getMonoid()->one();
      EVectorHeap H(resultF);
      for (EVector::iterator p = v; p.valid(); ++p)
	{
	  evec *tm = vec_new_term();
	  tm->coeff = K->vec_term_to_ring(*p);
	  tm->monom = m;
	  tm->component = p->component;
	  tm->next = 0;
	  H.add(tm);
	}
      result = H.value();
      //result.normal_form();
      return true;
    }
  return false;
}

bool EPolynomialRing::vec_lift(const EVector &v, const EFreeModule *resultF, EVector &result) const
  // Rg --> A = this
  // Rg = ring(resultF), A = ring(v).
{
  //  if (getCover() == resultF->getRing()->getCover())
  //{
  //  result = v.translate(resultF);
  ////resultF->normal_form(result);  MES: once quotients are ready.
  //return true;
  //}
  if (K == resultF->getRing())
    {
      // Lift iff each term has monomial = 1.
      EVectorHeap H(resultF);
      for (EVector::iterator p = v; p.valid(); ++p)
	{
	  if (!getMonoid()->is_one(p->monom))
	    {
	      return false;  // H is automatically freed...
	    }
	  EVector tm = K->vec_make(resultF, K->clone(p->coeff), p->component);
	  H.add(tm);
	}
      result = H.value();
      return true;
    }
  return false;
}
