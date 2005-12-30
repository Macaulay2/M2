// (c) 1994-2005 Michael E. Stillman

#include "monideal_minprimes.hpp"
#include "text_io.hpp"

MinimalPrimes::MinimalPrimes(const MonomialIdeal * const &I)
  : state(do_codim),
    min_codim(I->get_ring()->n_vars()+1),
    nvars(I->get_ring()->n_vars()),
    mi(I->radical())
{
  exps = newarray(int *,nvars+2);
  for (int i=0; i<=nvars+1; i++)
    exps[i] = 0;
}

MinimalPrimes::~MinimalPrimes()
{
  for (int i=0; i<=nvars+1; i++)
    if (exps[i] != 0) deletearray(exps[i]);
  deletearray(exps);
}

int MinimalPrimes::codimension()
{
  exps[0] = newarray(int,nvars);
  for (int i=0; i<nvars; i++) exps[0][i] = 0;
  ass_prime_generator(mi->first_node(), 0);
  state = do_primes;
  return min_codim;
}

MonomialIdeal * MinimalPrimes::min_primes(int codim_limit0, int minprime_limit0)
    // Place the associated primes of minimal codimension 
    // into a monomial ideal where each monomial corresponds to the prime
    // monomial ideal which is its support.
{
  codim_limit = codim_limit0;
  minprime_limit = minprime_limit0;
  state = do_primes;
  n_minprimes = 0;

  if (exps[0] == 0) exps[0] = newarray(int,nvars);
  for (int i=0; i<nvars; i++) exps[0][i] = 0;
  ass_prime_generator(mi->first_node(), 0);

  buffer o;
  o << "number of tentative minprimes is " << Q.length();

  MonomialIdeal *result = new MonomialIdeal(mi->get_ring() , Q);

  o << " actual number is " << result->length() << newline;
  emit(o.str());

  return result;
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
      exp[i] = 0;
    else
      exp[i] = 1; // NOTE!! This is the OPPOSITE of the way it is 
                  // done in assprimes!
}

void MinimalPrimes::ass_prime_generator(Nmi_node *p, int codim)
{
  int i=codim+1;
  if (exps[i] == 0)
    exps[i] = newarray(int,nvars);
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
	      Q.insert(b);
	      n_minprimes++;
	    }
	  return ;
	}
      const int *m = p->monom().raw();
      switch (reduce_exp(m, exp)) 
	{
	case 0 : 
	  p = mi->next(p);
	  break;
	case -1:
	  return ;
	case 1 : 
	  if (codim < codim_limit)
	    for (index_varpower i2 = m; i2.valid(); ++i2)
	      if (exp[i2.var()] == 0)
		{
		  exp[i2.var()] = 1 ;
		  ass_prime_generator(mi->next(p), codim+1) ;
		  exp[i2.var()] = -1 ;
		  if (minprime_limit > 0 && n_minprimes >= minprime_limit)
		    return;
		}
	  return ;
	}
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
