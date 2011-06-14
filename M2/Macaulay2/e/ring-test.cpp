#include "ring-test.hpp"

using namespace M2;

typedef RingWrap<RingInterfaceExample> RingZ;
typedef RElementWrap<RingInterfaceExample> ElementZ;

void add5(const RingZ *S, ElementZ &a)
{
  ElementZ b;
  S->init_set(b, 5);
  S->add_to(a, b);
}

void special(const RingInterfaceExample *S, RingInterfaceExample::ElementType *a)
{
  RingInterfaceExample::ElementType b;
  S->init_set(b, 5);
  S->add_to(*a, b);
}

#if 0
void foo()
{
  const ARing *R = new RingZ;
  RingZ S;
  ElementZ a;
  S.init_set(a, 5);
}
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// End:
