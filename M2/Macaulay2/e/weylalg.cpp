// Copyright 1997 Michael E. Stillman

#include "weylalg.hpp"

#include "geopoly.hpp"
#include "text_io.hpp"

#include "weylfree.hpp"

WeylAlgebra::WeylAlgebra(
        const Ring *KK, 
        const Monoid *MF, 
	int npairs, 
        const int *deriv, const int *comm,
	int homog_var)
  : PolynomialRing(KK,MF)
{
  this->nderivatives = npairs;
  this->homogeneous_weyl_algebra = (homog_var >= 0);
  this->homog_var = homog_var;

  this->derivative = new int[nderivatives];
  this->commutative = new int[nderivatives];
  for (int i=0; i<nderivatives; i++)
    {
      derivative[i] = deriv[i];
      commutative[i] = comm[i];
    }
  initialize1();
}

WeylAlgebra::~WeylAlgebra()
{
  delete [] derivative;
  delete [] commutative;
}

WeylAlgebra *WeylAlgebra::create(const Ring *K, const Monoid *MF, 
				 int npairs,
				 const int *deriv,
				 const int *comm,
				 int homog_var)
{
  int nvars = MF->n_vars();
  if (homog_var >= nvars)
    return 0;
  for (int i=0; i<npairs; i++)
    {
      if (deriv[i] < 0 || deriv[i] >= nvars)
	return 0;
      if (comm[i] < 0 || comm[i] >= nvars)
	return 0;
    }
  WeylAlgebra *R = new WeylAlgebra(K,MF,npairs,deriv,comm,homog_var);

  // Now set whether this ring is graded.  This will be the case iff
  // deg(x_i) + deg(D_i) = 2 deg(h), for every i.
  if (homog_var < 0) 
    R->isgraded = false;
  else {
    R->isgraded = true;
    const Monoid *D = MF->degree_monoid();
    const int *degh = MF->degree_of_var(homog_var);
    int *deg2h = D->make_one();
    int *degxD = D->make_one();
    D->mult(degh,degh,deg2h);
    
    for (int j=0; j < npairs; j++)
      {
	const int *degx = MF->degree_of_var(comm[j]);
	const int *degD = MF->degree_of_var(deriv[j]);
	D->mult(degx,degD,degxD);
	if (D->compare(deg2h,degxD) != EQ)
	  {
	    R->isgraded = false;
	    emit_line("not graded");
	    break;
	  }
      }

    D->remove(deg2h);
    D->remove(degxD);
  }
  return R;
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

/////////////////
int WeylAlgebra::binomtop = 15;
int WeylAlgebra::diffcoeffstop = 10;
int **WeylAlgebra::binomtable = 0;
int **WeylAlgebra::diffcoeffstable = 0;

void WeylAlgebra::initialize1()
{
  if (binomtable == 0)
    {
      int i,j;

      binomtable = new int *[binomtop+1];
      for (i=0; i<=binomtop; i++)
	binomtable[i] = new int[i+1];
      binomtable[0][0] = 1;
      binomtable[1][0] = 1;
      binomtable[1][1] = 1;
      for (i=2; i<=binomtop; i++)
	{
	  binomtable[i][0] = 1;
	  binomtable[i][i] = 1;
	  for (j=1; j<i; j++)
	    binomtable[i][j] = binomtable[i-1][j-1] + binomtable[i-1][j];
	}

      diffcoeffstable = new int *[diffcoeffstop+1];
      for (i=0; i<=diffcoeffstop; i++)
	diffcoeffstable[i] = new int[i+1];
      diffcoeffstable[0][0] = 1;
      diffcoeffstable[1][0] = 1;
      diffcoeffstable[1][1] = 1;
      for (i=2; i<=diffcoeffstop; i++)
	{
	  diffcoeffstable[i][0] = 1;
	  for (j=1; j<=i; j++)
	    diffcoeffstable[i][j] = i * diffcoeffstable[i-1][j-1];
	}
#if 0
      // Display the binomial tables:
      cout << "---binom table---" << endl;
      for (i=0; i<=binomtop; i++)
	{
	  for (j=0; j<=i; j++)
	    cout << "  " << binomtable[i][j];
	  cout << endl;
	}
      cout << "---diff table---" << endl;
      for (i=0; i<=diffcoeffstop; i++)
	{
	  for (j=0; j<=i; j++)
	    cout << "  " << diffcoeffstable[i][j];
	  cout << endl;
	}
#endif
    }
}

ring_elem WeylAlgebra::binomial(int top, int bottom) const
{
  // This should be located elsewhere
  // Assumption: bottom <= top, top >= 0, bottom >= 0.
  if (bottom == 0) return K->from_int(1);
  if (bottom == 1) return K->from_int(top);
  if (top <= binomtop)
    return K->from_int(binomtable[top][bottom]);
  ring_elem result = K->from_int(1);
  for (int a=0; a<bottom; a++)
    {
      ring_elem b = K->from_int(top-a);
      ring_elem result1 = K->mult(result,b);
      K->remove(result);
      K->remove(b);
      ring_elem c = K->from_int(a+1);
      result = K->divide(result1,c);
      K->remove(c);
    }
  return result;
}

ring_elem WeylAlgebra::multinomial(ring_elem c, const int *top, const int *bottom) const
{
  // Assumed: top[i] >= bottom[i] for all i.
  ring_elem result = K->copy(c);
  for (int i=0; i<nderivatives; i++)
    if (bottom[i] > 0)
      {
	ring_elem a = binomial(top[i],bottom[i]);
	ring_elem b = K->mult(a,result);
	K->remove(a);
	K->remove(result);
	result = b;
      }
  return result;
}

bool WeylAlgebra::divides(const int *expbottom, const int *exptop) const
{
  for (int i=0; i<nderivatives; i++)
    if (expbottom[i] > exptop[i])
      return false;
  return true;
}

bool WeylAlgebra::increment(int *current_derivative, 
			    const int *top_derivative) const
{
  int i = 0;
  while (current_derivative[i] == top_derivative[i])
    {
      i++;
      if (i >= nderivatives)
	{
	  return false;
	}
    }
  for (int j=0; j<i; j++)
    current_derivative[j] = 0;
  current_derivative[i]++;
  return true;
}

void WeylAlgebra::extractDerivativePart(const int *exponents, int *result_derivatives) const
{
  // exponents: 0..nvars-1
  // result_derivatives: 0..nderivatives-1 is the result
  for (int i=0; i<nderivatives; i++)
    result_derivatives[i] = exponents[derivative[i]];
}
void WeylAlgebra::extractCommutativePart(const int *exponents, int *result_exp) const
{
  // exponents: 0..nvars-1
  // result_exp: 0..nderivatives-1 is the result
  for (int i=0; i<nderivatives; i++)
    result_exp[i] = exponents[commutative[i]];
}

ring_elem WeylAlgebra::diff_coefficients(const ring_elem c, 
					 const int *derivatives, 
					 const int *exponents) const
{
  ring_elem result = K->copy(c);
  for (int i=0; i<nderivatives; i++)
    {
      if (derivatives[i] == 0) continue;
      if (exponents[i] <= diffcoeffstop)
	{
	  ring_elem g = K->from_int(diffcoeffstable[exponents[i]][derivatives[i]]);
	  ring_elem h = K->mult(result,g);
	  K->remove(g);
	  K->remove(result);
	  result = h;
	  if (K->is_zero(result))
	    return result;
	}
      else for (int j=derivatives[i]-1; j>=0; j--)
	{
	  ring_elem g = K->from_int(exponents[i]-j);
	  ring_elem h = K->mult(result,g);
	  K->remove(g);
	  K->remove(result);
	  result = h;
	  if (K->is_zero(result))
	    return result;
      }
    }
  return result;
}

Nterm * WeylAlgebra::weyl_diff(
	  const ring_elem c,
	  const int *expf,  // The exponent vector of f
	  const int *derivatives, 
	  const Nterm *g) const  // An entire polynomial
{
  // This isn't really differentiation, but it is close.
  // It is the inner loop of the multiplication routine for the Weyl algebra.
  // Returns: sum of d*[n,derivative]*c*n*m/(derivative,derivatives) e_i, for each
  // term d*n*e_i of v which satisfies: x part of n is >= derivatives,
  // and where the multiplication and division of monomials is in the commutative
  // monoid.

  Nterm head;
  head.next = 0;
  Nterm *result = &head;

  int i;
  int *exp = new int[nderivatives];
  int *deriv_exp = new int[nvars];
  int *result_exp = new int[nvars];
  for (i=0; i<nvars; i++)
    deriv_exp[i] = 0;
  if (homogeneous_weyl_algebra)
    {
      int sum = 0;
      for (i=0; i<nderivatives; i++)
	{
	  sum += 2*derivatives[i];
	  deriv_exp[derivative[i]] = derivatives[i];
	  deriv_exp[commutative[i]] = derivatives[i];
	}
      deriv_exp[homog_var] = -sum;
    }
  else
    for (i=0; i<nderivatives; i++)
      {
	deriv_exp[derivative[i]] = derivatives[i];
	deriv_exp[commutative[i]] = derivatives[i];
      }

  for (const Nterm *t = g; t != 0; t = t->next)
    {
      // This first part checks whether the x-part of t->monom is divisible by
      // 'derivatives'.  If so, true is returned, and the resulting monomial is set.
      M->to_expvector(t->monom, result_exp);
      extractCommutativePart(result_exp, exp);
      if (divides(derivatives,exp))
	{
	  ring_elem a = diff_coefficients(c,derivatives,exp);
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
	  Nterm *tm = new_term();
	  tm->coeff = b;
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

ring_elem WeylAlgebra::imp_mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
  // Computes c*m*f
{
  int *top_derivative = new int[nderivatives];
  int *current_derivative = new int[nderivatives];
  int *expf = new int[nvars];
  polyheap result(this);

  M->to_expvector(m, expf);
  extractDerivativePart(expf, top_derivative);
  for (int i=0; i<nderivatives; i++) current_derivative[i] = 0;
  // Loop over each current_derivative <= top_derivative.
  do {
      ring_elem d = multinomial(c, top_derivative, current_derivative);
      Nterm * h = weyl_diff(d,expf,current_derivative,f);
      K->remove(d);
      result.add(h);
  } while (increment(current_derivative, top_derivative));

  delete [] expf;
  delete [] top_derivative;
  delete [] current_derivative;
  return result.value();
}

/////////////////
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

#if 0
ring_elem WeylAlgebra::imp_mult_by_term(const ring_elem f, 
			       const ring_elem c, const int *m) const
   // return c*m*f
{
  polyheap H(this);

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
#endif
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

  const Ring *target = map->get_ring();
  if (target->is_poly_ring())
    {
      intarray vp;
      polyheap H(target);
      
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
