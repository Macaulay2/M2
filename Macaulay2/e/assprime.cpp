// (c) 1994 Michael E. Stillman

#include "assprime.hpp"

AssociatedPrimes::AssociatedPrimes(const MonomialIdeal &I)
  : state(do_codim),
    min_codim(I.Ring_of()->n_vars()+1),
    nvars(I.Ring_of()->n_vars()),
    mi(I.radical()),
    ass_primes(I.Ring_of()),
    exps()
{
}

AssociatedPrimes::AssociatedPrimes(const MonomialIdeal &I, int cod)
  : state(do_primes),
    min_codim(cod),
    nvars(I.Ring_of()->n_vars()),
    mi(I.radical()),
    ass_primes(I.Ring_of()),
    exps()
{
}

int AssociatedPrimes::codimension()
{
  exps.shrink(0);
  int *exp = exps.alloc(nvars);
  for (int i=0; i<nvars; i++) exp[i] = 0;
  ass_prime_generator(mi.first_node(), exp, 0);
  state = do_primes;
  return min_codim;
}

MonomialIdeal AssociatedPrimes::associated_primes()
    // Place the associated primes of minimal codimension 
    // into a monomial ideal where each monomial corresponds to the prime
    // monomial ideal which is its support.
{
  if (state == do_codim)
    {
      codimension();
      state = do_primes;
    }
  exps.shrink(0);
  int *exp = exps.alloc(nvars);
  for (int i=0; i<nvars; i++) exp[i] = 0;
  ass_prime_generator(mi.first_node(), exp, 0);
  return ass_primes;
}

static int reduce_exp(const int *m, const int *exp)
     // Determine whether the varpower monomial 'm' 
     // can be in the monomial prime ideal 'exp'.
     // exp corresponds to the set: 
     //    exp[i]>0 means variable is in ideal
     //    exp[i]<0 means variable is not in ideal
     //    exp[i]=0 means variable may or may not be in the ideal.
     // Return: 0 if 'm' is in this ideal.
     // Return: 1 if 'm' could be in the ideal.
     // Return: -1 if 'm' cannot possibly be in this ideal.
{
  int is_one = 1;
  for (index_varpower i = m; i.valid(); ++i)
    {
      if (exp[i.var()] == 1) return 0;
      if (exp[i.var()] == 0) is_one = 0;
    }
  if (is_one) return -1;
  return 1;
}

static void to_prime_ideal(int n, int *exp)
{
  for (int i=0; i<n; i++)
    if (exp[i] <= 0) 
      exp[i] = 1;
    else
      exp[i] = 0;
}

void AssociatedPrimes::ass_prime_generator(Nmi_node *p, 
					   const int *exp1, int codim)
{
  int *exp = exps.alloc(nvars);
  for (int i=0; i<nvars; i++) exp[i] = exp1[i];
  for (;;)
    {
      if (p == NULL)
	{
	  if (state == do_codim)
	    { if (codim < min_codim) min_codim = codim; }
	  else
	    { 
	      to_prime_ideal(nvars, exp); 
	      Bag *b = new Bag(0);
	      varpower::from_ntuple(nvars, exp, b->monom());
	      ass_primes.insert(b);
	    }
	  return ;
	}
      const int *m = p->monom().raw();
      switch (reduce_exp(m, exp)) 
	{
	case 0 : 
	  p = mi.next(p);
	  break;
	case -1:
	  return ;
	case 1 : 
	  if (codim < min_codim)
	    for (index_varpower i = m; i.valid(); ++i)
	      if (exp[i.var()] == 0)
		{
		  exp[i.var()] = 1 ;
		  ass_prime_generator(mi.next(p), exp, codim+1) ;
		  exp[i.var()] = -1 ;
		}
	  return ;
	}
    }
}
