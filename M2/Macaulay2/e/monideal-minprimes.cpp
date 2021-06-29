// (c) 1994-2005 Michael E. Stillman

#include "monideal-minprimes.hpp"
#include "text-io.hpp"

MinimalPrimes::MinimalPrimes(const MonomialIdeal *const &I)
    : state(do_codim),
      min_codim(I->get_ring()->n_vars() + 1),
      nvars(I->get_ring()->n_vars()),
      mi(I->radical())
{
  exps = newarray(int *, nvars + 2);
  for (int i = 0; i <= nvars + 1; i++) exps[i] = 0;

  primes = new MonomialIdeal(I->get_ring());
  codim_limit = 0;
}

MinimalPrimes::~MinimalPrimes()
{
  for (int i = 0; i <= nvars + 1; i++)
    if (exps[i] != 0) freemem(exps[i]);
  freemem(exps);
  delete primes;
  delete mi;
}

int MinimalPrimes::codimension()
{
  minprime_limit = -1;

  codim_limit = min_codim;
  exps[0] = newarray_atomic_clear(int, nvars);
  ass_prime_generator(mi->first_node(), 0);
  state = do_primes;
  return codim_limit;
}

#if 0
// MonomialIdeal * MinimalPrimes::min_primes(int codim_limit0, int minprime_limit0)
//     // Place the associated primes of minimal codimension
//     // into a monomial ideal where each monomial corresponds to the prime
//     // monomial ideal which is its support.
// {
//   codim_limit = codim_limit0;
//   minprime_limit = minprime_limit0;
//   state = do_primes;
//   n_minprimes = 0;
//
//   if (exps[0] == 0) exps[0] = newarray_atomic(int,nvars);
//   for (int i=0; i<nvars; i++) exps[0][i] = 0;
//   ass_prime_generator(mi->first_node(), 0);
//
//   buffer o;
//   o << "number of tentative minprimes is " << Q.length();
//
//   MonomialIdeal *result = new MonomialIdeal(mi->get_ring() , Q);
//
//   o << " actual number is " << result->length() << newline;
//   emit(o.str());
//
//   return result;
// }
#endif

static void to_prime_ideal(int n, int *exp)
{
  for (int i = 0; i < n; i++)
    if (exp[i] <= 0)
      exp[i] = 0;
    else
      exp[i] = 1;  // NOTE!! This is the OPPOSITE of the way it is
                   // done in assprimes!
}

static int alg1_reduce_exp(const int *m, const int *exp)
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
  while (*m != 0)
    {
      if (exp[*m] == 1) return 0;
      if (exp[*m] == 0) is_one = 0;
      m++;
    }
  if (is_one) return -1;
  return 1;
}

void MinimalPrimes::alg1_grab_prime(int depth)
{
  Bag *b = new Bag(0);
  for (int i = 0; i < nvars; i++)
    if (exp[i + 1] > 0)
      exp2[i] = 1;
    else
      exp2[i] = 0;
  varpower::from_ntuple(nvars, exp2, b->monom());
  Q.insert(b);
}

void MinimalPrimes::alg1_min_prime_generator(int *which, int depth)
// which: which monomial we are looking at right now
// current depth: starts at -1, goes down from there
//   so the current codim is -depth-1
// The following information is kept as well:
//  current_minprime: array 0..codim-1 of variables in the minprime
//  current_exp: array 0..nvars-1 of 0,1,-1's
//
//
{
  for (;;)
    {
      if (*which == 0)
        {
          alg1_grab_prime(depth);
          return;
        }
      switch (alg1_reduce_exp(which + 1, exp))
        {
          case 0:
            which = which + *which;
            break;
          case -1:
            return;
          case 1:
            if (depth > depth_limit)
              {
                int *m = which + 1;
                while (*m != 0)
                  {
                    int v = *m;
                    if (exp[v] == 0)
                      {
                        exp[v] = 1;
                        alg1_min_prime_generator(which + *which, depth - 1);
                        exp[v] = depth;
                      }
                    m++;
                  }
                // This code sets the 'exp' array back to the way it was
                m = which + 1;
                while (*m != 0)
                  {
                    int v = *m;
                    if (exp[v] == depth) exp[v] = 0;
                    m++;
                  }
              }
            return;
        }
    }
}

MonomialIdeal *MinimalPrimes::alg1_min_primes(int maxcodim, int count)
{
  // First, let's write out the (radical) monomial ideal in an array.
  // We need to know how large to make it.  So, we first add up all of the
  // degrees of the gens

  depth_limit = -maxcodim - 1;

  long len = 1;
  for (Index<MonomialIdeal> i = mi->first(); i.valid(); i++)
    {
      long d = varpower::simple_degree((*mi)[i]->monom().raw());
      len += d;
    }

  len += mi->length();
  len += mi->length();
  monoms = newarray_atomic(int, len);

  int next_monom = 0;

  for (Index<MonomialIdeal> i = mi->first(); i.valid(); i++)
    {
      int *m = (*mi)[i]->monom().raw();
      int d = varpower::simple_degree(m);

      monoms[next_monom++] = d + 2;

      for (index_varpower j = m; j.valid(); ++j)
        monoms[next_monom++] = j.var() + 1;
      monoms[next_monom++] = 0;
    }
  monoms[next_monom] = 0;

  exp = newarray_atomic_clear(int, nvars + 1);
  exp2 = newarray_atomic_clear(int, nvars);

  alg1_min_prime_generator(monoms, -1);

  freemem(monoms);
  freemem(exp);

  buffer o;
  o << "number of tentative minprimes is " << Q.length();

  MonomialIdeal *result = new MonomialIdeal(mi->get_ring(), Q);

  o << " actual number is " << result->length() << newline;
  emit(o.str());

  return result;
}

MonomialIdeal *MinimalPrimes::min_primes(int codim_limit0, int minprime_limit0)
// Place the associated primes of minimal codimension
// into a monomial ideal where each monomial corresponds to the prime
// monomial ideal which is its support.

// For this version: codim_limit0 says: all irred primes of codim smaller than
// this
// have been placed into 'primes'.
{
  minprime_limit = minprime_limit0;
  state = do_primes;
  n_minprimes = 0;

  if (exps[0] == 0) exps[0] = newarray_atomic_clear(int, nvars);

  while (codim_limit < codim_limit0)
    {
      codim_limit++;
      ass_prime_generator(mi->first_node(), 0);
    }

  MonomialIdeal *result = primes;
  primes = 0;
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

void MinimalPrimes::ass_prime_generator(Nmi_node *p, int codim)
{
  int i = codim + 1;
  if (exps[i] == 0) exps[i] = newarray_atomic(int, nvars);
  int *exp0 = exps[i];
  for (int j = 0; j < nvars; j++) exp0[j] = exps[codim][j];
  for (;;)
    {
      if (p == NULL)
        {
          if (state == do_codim)
            {
              if (codim < codim_limit) codim_limit = codim;
            }
          else
            {
              to_prime_ideal(nvars, exp0);
              Bag *b = new Bag(0);
              varpower::from_ntuple(nvars, exp0, b->monom());
              primes->insert(b);
              n_minprimes++;
            }
          return;
        }
      const int *m = p->monom().raw();
      switch (reduce_exp(m, exp0))
        {
          case 0:
            p = mi->next(p);
            break;
          case -1:
            return;
          case 1:
            if (codim < codim_limit)
              for (index_varpower i2 = m; i2.valid(); ++i2)
                if (exp0[i2.var()] == 0)
                  {
                    exp0[i2.var()] = 1;
                    ass_prime_generator(mi->next(p), codim + 1);
                    exp0[i2.var()] = -1;
                    if (minprime_limit > 0 && n_minprimes >= minprime_limit)
                      return;
                  }
            return;
        }
    }
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
