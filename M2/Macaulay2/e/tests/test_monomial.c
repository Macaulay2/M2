/* A test of the engine interface */
#include "engine.h"
#include "util.h"

void test_monomials()
{
  /*
  Make sure we test: monomial = 1 case
  Also test overflow cases: should get an error
  Are negative exponents allowed?
  */
  int i;
  M2_arrayint m1, m2, m3;
  const Monomial *one;
  const Monomial *a, *a2, *b, *c1, *c2, *c3, *c4, *c5, *c6, *c7;

#if 0
  Monomial *a = IM2_Monomial_var(0,1);
  Monomial *a3 = IM2_Monomial_var(0,3);

  Monomial *b = IM2_Monomial_var(1,1);
  Monomial *b7 = IM2_Monomial_var(1,7);
#endif
  a = monom(6, 7,1,4,3,2,2);
  a2 = monom(6, 7,1,4,3,2,2);
  b = monom(4,7,1,4,4);

  assert(is_eq(IM2_Monomial_to_string(a), "c2e3h"));

  /* Given monomials a,b, call all of the functions: */
  c1 = IM2_Monomial_mult(a,b);
  c2 = IM2_Monomial_quotient(a,b);
  c3 = IM2_Monomial_power(a,143);
  c4 = IM2_Monomial_lcm(a,b);
  c5 = IM2_Monomial_gcd(a,b);
  c6 = IM2_Monomial_sat(a,b);
  c7 = IM2_Monomial_radical(a);
  /*  p  = IM2_Monomial_syz(a,b); */

  assert(IM2_Monomial_degree(a) == 6);
  assert(IM2_Monomial_hash(a) == IM2_Monomial_hash(a2));
  assert(IM2_Monomial_degree(a) + IM2_Monomial_degree(b) == IM2_Monomial_degree(c1));
  assert(!IM2_Monomial_is_one(a));
  assert(IM2_Monomial_is_equal( IM2_Monomial_mult(c4,c5), c1));
  
  /*  m1 = toarrayint(6, (int[]){8,3,4,1,3,2});*/
  m1 = arrayint(6,  8,3,4,1,3,2);

  a = IM2_Monomial_make(m1);
  a = monom(8, 6,5,4,3,1,1,0,8);
  assert(is_eq(IM2_Monomial_to_string(a), "a8be3g5"));

  one = IM2_Monomial_make(toarrayint(0,0));
#if 0
  assert(IM2_Monomial_is_equal(c,d));
  assert(IM2_Monomial_is_equal(e,f));

  assert(IM2_Monomial_degree(c) == 10);
  assert(1);
  printf("a3 = "); display_monomial(a3); printf("\n");
  /*  printf("a3 = %s\n", tocharstar(IM2_Monomial_to_string(a3)));*/
#endif
}

int main(int argc, char **argv)
{
  IM2_initialize();
  test_monomials();
  return 0;
}
