#include "engine.h"
#include "util.h"

void test_ZZ()
{
  const Ring *ZZ = IM2_Ring_ZZ();
  const RingElement *f1;
  assert(is_eq(IM2_Ring_to_string(ZZ), "ZZ"));

  f1 = IM2_RingElement_from_Integer(ZZ, make_integer(13));
  assert(is_eq(IM2_RingElement_to_string(f1), "13"));
}

void test_ZZp()
{
  const Ring *ZZp = IM2_Ring_ZZp(32003, IM2_Monoid_trivial());
  assert(is_eq(IM2_Ring_to_string(ZZp), "ZZ/32003"));
}

void test_polyring()
{
  const Ring * R = make_poly_ring(0,15);
  assert(is_eq(IM2_Ring_to_string(R), 
	       "ZZ[x0,x1,x2,x3,x4,x5,x6,x7,x8,x9,x10,x11,x12,x13,x14,\n"
	       "  Degrees => {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1},\n"
	       "  MonomialOrdering => {\n"
	       "    GRevLex => {1,1,1,1,1,1,1,1,1,1,1,1,1,1,1}\n"
	       "    }\n"
	       "  ]"));

}

int main(int argc, char **argv)
{
  IM2_initialize();
  test_ZZ();
  test_ZZp();
  test_polyring();
  return 0;
}
