/*!
    @class MinimalPrimes
    @abstract   (description)
    @discussion (description)
*/


// (c) 1994-2005 Michael E. Stillman
#ifndef _monideal_minprimes_h_
#define _monideal_minprimes_h_

#include "monideal.hpp"
#include "queue.hpp"

class MinimalPrimes
    // A class which enables one to compute the codimension and 
    // associated primes of minimal codimension, of a monomial ideal.
{
  enum { do_codim, do_primes } state;
  int min_codim;
  int nvars;

  MonomialIdeal * mi;			// A radical monomial ideal

  MonomialIdeal * primes;
  queue<Bag *> Q; // Each monomial corresponds to a potential prime monomial
                  // ideal: a monomial is the product of the gens of
                  // the prime ideal.
  int codim_limit; // only consider monomial primes of codim <= this number.
  int minprime_limit; // -1 means find all.  >= 1 means stop after that number
  int n_minprimes; // number found so far, during associated_primes computation
  int **exps;

  int depth_limit; // -codim_limit-1
  int *exp;
  int *exp2;
  int *monoms;

  void alg1_grab_prime(int depth);
  void alg1_min_prime_generator(int *which, int depth);

  void ass_prime_generator(Nmi_node *p, int codim);

public:
  MinimalPrimes(const MonomialIdeal * const &mi);

  ~MinimalPrimes();

  int codimension();

  MonomialIdeal * associated_primes(int count);

  MonomialIdeal * alg1_min_primes(int maxcodim, int count); 

  MonomialIdeal * min_primes(int maxcodim, int count); 
  // maxcodim == nvars means get all of them
  // count == -1 means no limit
  // return value: A monomial ideal where each 
  //  monomial prime ideal is represented as a 
  //  monomial: the product of the generators.
  // NOTE: this is the complement of the gens from
  //  max_indep_sets

  MonomialIdeal * max_indep_sets(int count);

public:
  // Static routines:
  static int codimension(const MonomialIdeal *I);

  static MonomialIdeal *min_primes(const MonomialIdeal *I, int maxcodim, int count);

  static MonomialIdeal *max_indep_sets(const MonomialIdeal *I, int count);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
