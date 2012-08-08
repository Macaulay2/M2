#include <iostream>

#include "ring-test.hpp"

using namespace M2;

typedef ConcreteRing<RingInterfaceExample> RingZ;
typedef RElementWrap<RingInterfaceExample> ElementZ;

namespace M2 {

  void testit()
  {
    RingGF R(5,2);
    const ARing *A = new ConcreteRing<RingGF>(R);
    const ConcreteRing<RingGF> *S = A->cast_to_ConcreteRing<RingGF>();
    if (S == 0)
      {
        std::cout << "result was 0" << std::endl;
      }
    else
      {
        std::cout << "result was nonzero" << std::endl;
      }

    const ConcreteRing<RingZZp> *T = A->cast_to_ConcreteRing<RingZZp>();
    if (T == 0)
      {
        std::cout << "result was 0" << std::endl;
      }
    else
      {
        std::cout << "result was nonzero" << std::endl;
      }

    MatrixWrap< DenseMatrix<RingGF> > M ( S, 5, 10 );
    std::cout << "dense = " << M.isDense() << std::endl;
  }

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
  bool ARing::converter(const ARing *A, const ARing *B, const RElement &a, RElement &b)
  {
    int i=A->getRingID();
    int j=B->getRingID();
    int n=ring_top;
    switch(i + j*n) {
      case 11:
        return convert( & RING(RingInterfaceExample,A),
                        & RING(RingInterfaceExample,B),
                        constRELEM(RingInterfaceExample,a),
                        RELEM(RingInterfaceExample,b) );
      default:
        return false;
      };
  }

#if 0
  const ARing *rawRingZZp(unsigned long p)
  /* p must be a prime number <= 32767 */
  {
    if (p <= 1)
      {
        ERROR("ZZP: expected a prime number p in range 2 <= p <= 32749");
        return 0;
      }
    if (p < 32750)
      {
#if 0
        const ConcreteRing<RingLogZZp> *A = new ConcreteRing<RingLogZZp>;
        A->getInternalRing()->initialize(p);
        return A;
#endif
        return 0;
      }
    ConcreteRing<RingZZp> *B = new ConcreteRing<RingZZp>;
    B->getInternalRing().initialize(p);
    return B;
}
#endif


#if 0
void add5(const RingZ *S, ElementZ &a)
{
  ElementZ b;
  S->init_set(b, 5);
  S->add_to(a, b);
}
#endif

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

};

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
