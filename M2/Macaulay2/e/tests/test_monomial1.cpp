/* A test of the engine interface */
#include "../e/engine.h"
#include <assert.h>
/* Part 1: Monomials */

int main(int argc, char **argv)
{
  // Make sure we test: monomial = 1 case
  // Also test overflow cases: should get an error
  // Are negative exponents allowed?
  Monomial *a = IM2_Monomial_var(0,1);
  Monomial *a3 = IM2_Monomial_var(0,3);

  Monomial *b = IM2_Monomial_var(1,1);
  Monomial *b7 = IM2_Monomial_var(1,7);

  Monomial *c = IM2_Monomial_mult(a3,b7);
  Monomial *d = IM2_Monomial_mult(b7,a3);
  Monomial *e = IM2_Monomial_radical(c);
  Monomial *f = IM2_Monomial_mult(a,b);

  assert(IM2_Monomial_isequal(c,d));
  assert(IM2_Monomial_isequal(e,f));

  assert(IM2_Monomial_degree(c) == 21);
  return 0;
}
