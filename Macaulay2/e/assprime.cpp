// (c) 1994 Michael E. Stillman

#include "assprime.hpp"

AssociatedPrimes::AssociatedPrimes(const MonomialIdeal &I)
  : state(do_codim),
    min_codim(I.Ring_of()->n_vars()+1),
    nvars(I.Ring_of()->n_vars()),
    mi(I.radical()),
    ass_primes(I.Ring_of())
{
  exps = new int *[nvars+1];
  for (int i=0; i<=nvars; i++)
    exps[i] = 0;
}

AssociatedPrimes::AssociatedPrimes(const MonomialIdeal &I, int cod)
  : state(do_primes),
    min_codim(cod),
    nvars(I.Ring_of()->n_vars()),
    mi(I.radical()),
    ass_primes(I.Ring_of())
{
  exps = new int *[nvars+1];
  for (int i=0; i<=nvars; i++)
    exps[i] = 0;
}
AssociatedPrimes::~AssociatedPrimes()
{
  for (int i=0; i<=nvars; i++)
    if (exps[i] != 0) delete [] exps[i];
  delete [] exps;
}

int AssociatedPrimes::codimension()
{
  exps[0] = new int[nvars];
  for (int i=0; i<nvars; i++) exps[0][i] = 0;
  ass_prime_generator(mi.first_node(), 0);
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
  if (exps[0] == 0) exps[0] = new int[nvars];
  for (int i=0; i<nvars; i++) exps[0][i] = 0;
  ass_prime_generator(mi.first_node(), 0);
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

void AssociatedPrimes::ass_prime_generator(Nmi_node *p, int codim)
{
  int i=codim+1;
  if (exps[i] == 0)
    exps[i] = new int[nvars];
  int *exp = exps[i];
  for (int j=0; j<nvars; j++) exp[j] = exps[codim][j];
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
		  ass_prime_generator(mi.next(p), codim+1) ;
		  exp[i.var()] = -1 ;
		}
	  return ;
	}
    }
}
