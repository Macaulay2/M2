// Copyright 1998 Michael E. Stillman

#include "weylfree.hpp"
#include "geovec.hpp"

stash *WeylFreeModule::mystash;

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

vec WeylFreeModule::weyl_diff(
	  const ring_elem c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const vec g) const  // An entire polynomial
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the commutative
  // monoid.

  vecterm head;
  head.next = 0;
  vec result = &head;

  int i;
  int nvars = W->nvars;
  const Ring *K = W->Ncoeffs();
  int *exp = new int[W->nderivatives];
  int *deriv_exp = new int[nvars];
  int *result_exp = new int[nvars];
  for (i=0; i<nvars; i++)
    deriv_exp[i] = 0;
  if (W->homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i=0; i<W->nderivatives; i++)
	{
	  sum += 2*derivatives[i];
	  deriv_exp[W->derivative[i]] = derivatives[i];
	  deriv_exp[W->commutative[i]] = derivatives[i];
	}
      deriv_exp[W->homog_var] = -sum;
    }
  else
    for (i=0; i<W->nderivatives; i++)
      {
	deriv_exp[W->derivative[i]] = derivatives[i];
	deriv_exp[W->commutative[i]] = derivatives[i];
      }

  for (vec t = g; t != 0; t = t->next)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is set.
      M->to_expvector(t->monom, result_exp);
      W->extractCommutativePart(result_exp, exp);
      if (W->divides(derivatives,exp))
	{
	  ring_elem a = W->diff_coefficients(c,derivatives,exp);
	  if (K->is_zero(a))
	    {
	      K->remove(a);
	      continue;
	    }
	  ring_elem b = K->mult(a, t->coeff);
	  K->remove(a);
	  if (K->is_zero(b))
	    {
	      K->remove(b);
	      continue;
	    }
	  // Now compute the new monomial:
	  vec tm = new_term();
	  tm->coeff = b;
	  tm->comp = t->comp;
	  for (int i=0; i<nvars; i++)
	    result_exp[i] += expf[i] - deriv_exp[i];
	  M->from_expvector(result_exp, tm->monom);

	  // Append to the result
	  result->next = tm;
	  result = tm;
	}
    }
  delete [] exp;
  delete [] result_exp;
  result->next = 0;
  return head.next;
}

vec WeylFreeModule::imp_mult_by_term(const ring_elem c, 
				  const int *m,
				  vec f) const
  // Computes c*m*f
{
  int *top_derivative = new int[W->nderivatives];
  int *current_derivative = new int[W->nderivatives];
  int *expf = new int[W->nvars];
  vecHeap result(this);

  M->to_expvector(m, expf);
  W->extractDerivativePart(expf, top_derivative);
  for (int i=0; i<W->nderivatives; i++) current_derivative[i] = 0;
  // Loop over each current_derivative <= top_derivative.
  do {
      ring_elem d = W->multinomial(c, top_derivative, current_derivative);
      vec h = weyl_diff(d,expf,current_derivative,f);
      K->remove(d);
      result.add(h);
  } while (W->increment(current_derivative, top_derivative));

  delete [] expf;
  delete [] top_derivative;
  delete [] current_derivative;
  return result.value();
}

#if 0
vec WeylFreeModule::imp_mult_by_term(const ring_elem c, const int *m, const vec v) const
{
  //  return FreeModule::imp_mult_by_term(c,m,v);
   // return c*m*f

  vecHeap H(this);

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
#endif
