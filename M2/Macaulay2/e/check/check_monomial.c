/* A test of the engine interface */
#include "engine.h"
#include "util.h"
#include "src_test/Ctest.h"
#include "src_test/Csuite.h"

void test_monomials(Test *pTest)
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

  a = monom(6, 7,1,4,3,2,2);
  a2 = monom(6, 7,1,4,3,2,2);
  b = monom(4,7,1,4,4);

  ct_test(pTest,is_eq(IM2_Monomial_to_string(a), "c2e3h"));

  /* Given monomials a,b, call all of the functions: */
  c1 = IM2_Monomial_mult(a,b);
  c2 = IM2_Monomial_quotient(a,b);
  c3 = IM2_Monomial_power(a,143);
  c4 = IM2_Monomial_lcm(a,b);
  c5 = IM2_Monomial_gcd(a,b);
  c6 = IM2_Monomial_sat(a,b);
  c7 = IM2_Monomial_radical(a);
  /*  p  = IM2_Monomial_syz(a,b); */

  ct_test(pTest,IM2_Monomial_degree(a) == 6);
  ct_test(pTest,IM2_Monomial_hash(a) == IM2_Monomial_hash(a2));
  ct_test(pTest,
    IM2_Monomial_degree(a) + IM2_Monomial_degree(b) == IM2_Monomial_degree(c1));
  ct_test(pTest,!IM2_Monomial_is_one(a));
  ct_test(pTest,IM2_Monomial_is_equal( IM2_Monomial_mult(c4,c5), c1));
  /*  m1 = toarrayint(6, (int[]){8,3,4,1,3,2});*/
  m1 = arrayint(6,  8,3,4,1,3,2);

  a = IM2_Monomial_make(m1);
  a = monom(8, 6,5,4,3,1,1,0,8);
  ct_test(pTest,is_eq(IM2_Monomial_to_string(a), "a8be3g5"));

  one = IM2_Monomial_make(toarrayint(0,0));
}

Test * monomial_test(void)
{
  Test *result = ct_create("monomial tests", 0);
  ct_addTestFun(result, test_monomials);
  return result;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check"
// End:
*/

