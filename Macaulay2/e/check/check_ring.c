#include "engine.h"
#include "util.h"
#include "src_test/Ctest.h"
#include "src_test/Csuite.h"

void test_ZZ(Test *pTest)
{
  const Ring *ZZ = IM2_Ring_ZZ();
  const RingElement *f1;
  ct_test(pTest,is_eq(IM2_Ring_to_string(ZZ), "ZZ"));

  f1 = IM2_RingElement_from_Integer(ZZ, make_integer(13));
  ct_test(pTest,is_eq(IM2_RingElement_to_string(f1), "13"));
}

void test_ZZp(Test *pTest)
{
  const Ring *ZZp = IM2_Ring_ZZp(32003);
  ct_test(pTest,is_eq(IM2_Ring_to_string(ZZp), "ZZ/32003"));
}

void test_polyring(Test *pTest)
{
  const Ring * R = make_poly_ring(0,15);
  ct_test(pTest,is_eq(IM2_Ring_to_string(R), 
	       "ZZ[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,\n"
	       "  Degrees => {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},\n"
	       "  MonomialOrdering => {\n"
	       "    GRevLex => {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}\n"
	       "    }\n"
	       "  ]"));

}

void test_rings(Test *pTest)
{
  test_ZZ(pTest);
  test_ZZp(pTest);
  test_polyring(pTest);
}


Test * ring_test(void)
{
  Test *result = ct_create("ring tests", 0);
  ct_addTestFun(result, test_rings);
  return result;
}

/*
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/check"
// End:
*/
