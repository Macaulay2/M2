#include "ring-test.hpp"

using namespace M2;

typedef RingWrap<RingInterfaceExample> RingZ;
typedef RElementWrap<RingInterfaceExample> ElementZ;

namespace M2 {
  template<>
  bool convert<RingInterfaceExample,RingInterfaceExample>(const RingInterfaceExample *A, 
							  const RingInterfaceExample *B, 
							  const RingInterfaceExample::ElementType &a, 
							  RingInterfaceExample::ElementType &b)
  {
    b = a;
    return true;
  }
  
  // One awful way to do convert:
  bool converter(const ARing *A, const ARing *B, const RElement &a, RElement &b)
  {
    int i=10;
    switch (i) {
    case 10:
      return convert(  & static_cast<const RingWrap<RingInterfaceExample> *>(A)->R_,
		       & static_cast<const RingWrap<RingInterfaceExample> *>(B)->R_,
		       static_cast<const RElementWrap<RingInterfaceExample> &>(a).val_,
		       static_cast<RElementWrap<RingInterfaceExample> &>(b).val_);
    default:
      return false;
    };
  }
};


void add5(const RingZ *S, ElementZ &a)
{
  ElementZ b;
  S->init_set(b, 5);
  S->add_to(a, b);
}

#if 0
void special(const RingInterfaceExample *S1, RingInterfaceExample::ElementType &a)
{
  ARing *S;
  RingInterfaceExample::ElementType b;
  S1->init_set(b, 5);
  S1->add_to(a, b);
  converter(S,S,RElementWrap<RingInterfaceExample>(a),RElementWrap<RingInterfaceExample>(b));
}
#endif

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
