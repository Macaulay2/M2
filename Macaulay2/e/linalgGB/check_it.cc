#include <assert.h>
#include <stdio.h>
#include "monoms.h"
#include "MonomialSet.h"

int main(int argc, char **argv)
{
  MonomialSet H(0);
  monomial internedm1, internedm2, internedm3;

  int div;
  int m1[] = {3, 6,1, 3,4, 1,1};
  int m2[] = {3, 7,1, 3,1, 1,2};
  int m3[] = {2, 3,3, 1,1};
  int result_mult[] = {4, 7,1,  6,1,  3,5,  1,3};
  int result_lcm[] = {4, 7,1,  6,1,  3,4,  1,2};
  int result_divides[] = {2, 6,1,  3,1};
  int m[20];
  monomial_mult(m1,m2,m);
  assert(monomial_equal(m,result_mult));
  monomial_lcm(m1,m2,m);
  assert(monomial_equal(m,result_lcm));
  div = monomial_divides(m3,m1,m);
  assert(div && monomial_equal(m,result_divides));

  H.find_or_insert(m1, internedm1);
  H.find_or_insert(m2, internedm2);
  H.find_or_insert(m3, internedm3);
  H.find_or_insert(m1, internedm1);
  return 0;
}

/*
// Local Variables:
//  compile-command: "make -C $M2BUILDDIR/Macaulay2/e/linalgGB check_it"
//  End:
*/
