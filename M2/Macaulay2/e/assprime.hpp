/*!
    @class AssociatedPrimes
    @abstract   (description)
    @discussion (description)
*/

// (c) 1994 Michael E. Stillman
#ifndef _assprime_hh
#define _assprime_hh

/**
 * @file assprime.hpp
 * @brief `AssociatedPrimes` --- codimension and minimal-codimension associated primes of a monomial ideal.
 *
 * Declares `AssociatedPrimes`, a class that operates on a
 * `MonomialIdeal` (the constructor calls `I->radical()` so the
 * input need not be pre-radicalised). Its `state` enum has two
 * values, `do_codim` (cheap, just tracks `min_codim`) and
 * `do_primes` (expensive, enumerates the associated primes of
 * minimal codimension), and `codimension()` flips the state to
 * `do_primes` on the way out so the same instance can be stepped
 * through to `associated_primes(count)` without restarting (where
 * `count = -1` means "no limit"). Results come back as a
 * `MonomialIdeal` whose generators are squarefree monomials,
 * each encoding one prime by the variables that occur in it.
 *
 * The header also declares `min_primes(maxcodim, count)` and
 * `max_indep_sets(count)`, but these are vestigial: no
 * definition exists in `assprime.cpp` and no caller invokes them
 * --- the live implementations live on `MinimalPrimes` in
 * [[monideal-minprimes]]. The actual computation
 * (`ass_prime_generator`) walks the `Nmi_node` tree of the input
 * ideal: for each leading monomial `m` not yet covered, it picks
 * each variable `x_i` in `m` not already committed, adds it to
 * the prime under construction (codim+1), recurses, then marks
 * `x_i = -1` ("excluded from any further prime"). This is the
 * combinatorial split `ass(I) = ass(I + (x_i)) U ass(I, x_i = 1)`
 * with no Groebner bases or coefficient arithmetic.
 *
 * @see monideal.hpp
 * @see monideal-minprimes.hpp
 */

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
