// Copyright 1997 Michael E. Stillman

#include "weylalg.hpp"

#include "geopoly.hpp"
#include "text_io.hpp"

#include "weylfree.hpp"

WeylAlgebra::WeylAlgebra(const Ring *K, 
			 const Monoid *MF, 
			 const intarray &a)
  : PolynomialRing(K,MF)
{
  _derivative = new int[nvars];
  _commutative = new int [nvars];
  for (int i=0; i<nvars; i++)
    {
      _derivative[i] = -1;
      _commutative[i] = -1;
    }
  for (int i=0; i<a.length(); i+=2)
    {
      if (a[i] >= 0)
	_derivative[a[i]] = a[i+1];
      if (a[i+1] >= 0)
	_commutative[a[i+1]] = a[i];
    }
}


WeylAlgebra::~WeylAlgebra()
{
  delete [] _derivative;
  delete [] _commutative;
}

WeylAlgebra *WeylAlgebra::create(const Ring *K, const Monoid *MF, const intarray &a)
{
  WeylAlgebra *obj = new WeylAlgebra(K,MF,a);
  return (WeylAlgebra *) intern(obj);
}

bool WeylAlgebra::equals(const object_element *o) const
{
  // MESXX
  return false;
}
int WeylAlgebra::hash() const
{
  // MESXX
  return 0;
}
void WeylAlgebra::write_object(object_writer &o) const
{
  // MESXX
}

WeylAlgebra *WeylAlgebra::read_object(object_reader &i)
{
  // MESXX
  return 0;
}

void WeylAlgebra::text_out(buffer &o) const
{
  o << "WeylAlgebra(";
  K->text_out(o);
  M->text_out(o);
  o << ")";
}

FreeModule *WeylAlgebra::make_FreeModule() const
{ 
  return new WeylFreeModule(this); 
}

FreeModule *WeylAlgebra::make_FreeModule(int n) const
{ 
  return new WeylFreeModule(this,n);
}

ring_elem WeylAlgebra::multinomial(const int *exptop, const int *exp) const
{
  ring_elem result = K->from_int(1);
  for (int i=0; i<nvars; i++)
    if (exptop[i] > 0)
      {
	for (int j=exptop[i]; j > exp[i]; j--)
	  {
	    ring_elem c = K->from_int(j);
	    ring_elem d = K->from_int(exptop[i]-j+1);
	    K->mult_to(result, c);
	    ring_elem e = K->divide(result, d);
	    K->remove(c);
	    K->remove(d);
	    K->remove(result);
	    result = e;
	  }
      }
  return result;
}

void WeylAlgebra::diff_subtract(const int *exp1, const int *exp2, int *result) const
{
  int i;
  for (i=0; i<nvars; i++) result[i] = 0;
  for (i=0; i<nvars; i++)
    {
      int c = exp1[i] - exp2[i];
      if (c > 0) result[_commutative[i]] = c;
    }
}
ring_elem WeylAlgebra::imp_mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
   // return c*m*f
{
  geobucket H(this);

  int i,j;
  int *exp = new int[nvars];
  int *expmonom = new int[nvars];
  int *expdiff = new int[nvars];
  int *expd = new int[nvars];
  int *expm = M->make_one();
  M->to_expvector(m, expmonom);
  for (i=0; i<nvars; i++)
    {
      exp[i] = 0;
      expdiff[i] = 0;
    }
  for (i=0; i<nvars; i++)
    {
      // Set expdiff, expmonom
      // expdiff consists of the derivatives in m.
      // expmonom consists of the commutative part of m.
      if (expmonom[i] > 0)
	{
	  int d=_commutative[i];
	  if (d >= 0)
	    {
	      expdiff[d] = expmonom[i];
	      expmonom[i] = 0;
	    }
	}
    }

  // loop thru terms less than expdiff:
  while (true) 
    {

      // Now we construct the proper element
      ring_elem g = PolynomialRing::diff_by_term(exp, f, true);
      ring_elem c1 = multinomial(expdiff,exp);
      K->mult_to(c1,c);

      // Compute expmonom + diff(expdiff) - exp
      // Place into expd.
      for (j=0; j<nvars; j++)
	{
	  expd[j] = expmonom[j];
	  int d = _commutative[j];
	  if (d >= 0)
	    {
	      expd[j] -= exp[d];
	      expd[j] += expdiff[d];
	    }
	}
      // diff_subtract(expdiff, exp, expd); // expdiff-exp --> back to differential op
      M->from_expvector(expd, expm);

      ring_elem h = PolynomialRing::imp_mult_by_term(g, c1, expm);
      remove(g);
      K->remove(c1);
      H.add(h);

      // Find the first non-maximal entry, increment it.
      i = 0;
      while (exp[i] == expdiff[i])
	{
	  i++;
	  if (i >= nvars)
	    {
	      // Time to return
	      delete [] expdiff;
	      delete [] exp;
	      delete [] expmonom;
	      delete [] expd;
	      M->remove(expm);
	      return H.value();
	    }
	}
      for (j=0;j<i;j++)
	exp[j] = 0;
      exp[i]++;
    }
}

ring_elem WeylAlgebra::power(const ring_elem f, mpz_t n) const
{
  int n1;
  if (Z::get_si(n1,n))
    return power(f,n1);
  else 
    {
      gError << "exponent too large";
      return (Nterm *)NULL;
    }
}

ring_elem WeylAlgebra::power(const ring_elem f, int n) const
{
  return PolynomialRing::power2(f,n);
}

#if 0
ring_elem WeylAlgebra::eval(const RingMap *map, const ring_elem f) const
{
  // The way we collect the result depends on whether the target ring
  // is a polynomial ring: if so, use a heap structure.  If not, just add to the result.

  const Ring *target = map->Ring_of();
  if (target->is_poly_ring())
    {
      intarray vp;
      geobucket H(target);
      
      for (Nterm *t = f; t != NULL; t = t->next)
	{
	  vp.shrink(0);
	  M->to_varpower(t->monom, vp);
	  ring_elem g = map->eval_term(K, t->coeff, vp.raw());
	  H.add(g);
	}
      return H.value();
    }
  else 
    {
      ring_elem result = target->from_int(0);
      intarray vp;
      
      for (Nterm *t = f; t != NULL; t = t->next)
	{
	  vp.shrink(0);
	  M->to_varpower(t->monom, vp);
	  ring_elem g = map->eval_term(K, t->coeff, vp.raw());
	  target->add_to(result, g);
	}
      return result;
    }
}
#endif
