/*!
    @class AssociatedPrimes
    @abstract   (description)
    @discussion (description)
*/

// (c) 1994 Michael E. Stillman
#ifndef _assprime_hh
#define _assprime_hh

#include "monideal.hpp"

class AssociatedPrimes
// A class which enables one to compute the codimension and
// associated primes of minimal codimension, of a monomial ideal.
{
  enum { do_codim, do_primes } state;
  int min_codim;
  int nvars;

  MonomialIdeal *mi;  // A radical monomial ideal

  MonomialIdeal *ass_primes;  // Assoc. primes of minimal codim, stored
                              // in a monideal, where each monomial
                              // corresponds to a prime monomial ideal
                              // whose generators are the variables occurring
                              // in the monomial.
  int minprime_limit;  // -1 means find all.  >= 1 means stop after that number
  int n_minprimes;  // number found so far, during associated_primes computation
  int **exps;

  void ass_prime_generator(Nmi_node *p, int codim);

 public:
  AssociatedPrimes(const MonomialIdeal *const &mi);
  AssociatedPrimes(const MonomialIdeal *const &mi, int codim);
  ~AssociatedPrimes();

  int codimension();
  MonomialIdeal *associated_primes(int count);

  MonomialIdeal *min_primes(int maxcodim, int count);
  // maxcodim == nvars means get all of them
  // count == -1 means no limit
  // return value: A monomial ideal where each
  //  monomial prime ideal is represented as a
  //  monomial: the product of the generators.
  // NOTE: this is the complement of the gens from
  //  max_indep_sets

  MonomialIdeal *max_indep_sets(int count);
};

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
