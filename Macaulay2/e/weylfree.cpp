// Copyright 1998 Michael E. Stillman

#include "weylfree.hpp"
#include "geovec.hpp"

WeylFreeModule::WeylFreeModule(const Ring *R) 
  : FreeModule(R)
{
  W = R->cast_to_WeylAlgebra();
  assert(W != NULL);		// Since only Weyl algebras can instantiate, this
				// should never happen.
}

WeylFreeModule::WeylFreeModule(const Ring *R, int n) 
  : FreeModule(R,n)
{
  W = R->cast_to_WeylAlgebra();
  assert(W != NULL);		// Since only Weyl algebras can instantiate, this
				// should never happen.
}

WeylFreeModule::~WeylFreeModule()
{
}

vec WeylFreeModule::imp_mult_by_term(const ring_elem c, const int *m, const vec v) const
{
  //  return FreeModule::imp_mult_by_term(c,m,v);
   // return c*m*f

  geobucket H(this);

  int i,j;
  int nvars = M->n_vars();
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
	  int d= W->_commutative[i];
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
      vec g = FreeModule::diff_by_term(exp, v, true);
      ring_elem c1 = W->multinomial(expdiff,exp);
      K->mult_to(c1,c);

      // Compute expmonom + diff(expdiff) - exp
      // Place into expd.
      for (j=0; j<nvars; j++)
	{
	  expd[j] = expmonom[j];
	  int d = W->_commutative[j];
	  if (d >= 0)
	    {
	      expd[j] -= exp[d];
	      expd[j] += expdiff[d];
	    }
	}
      // diff_subtract(expdiff, exp, expd); // expdiff-exp --> back to differential op
      M->from_expvector(expd, expm);

      vec h = FreeModule::imp_mult_by_term(c1, expm, g);
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
