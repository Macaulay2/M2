#include <memory>
#include <gtest/gtest.h>

#include "DMatTest.hpp"
#include "aring-zzp.hpp"
#include "aring-glue.hpp"

TEST(DMatZZp, create)
{
  typedef M2::ARingZZp RingZZp;
  typedef DMat<M2::ARingZZp> MatZZp;
  
  RingZZp* R = new RingZZp(101);

  //  M2::ConcreteRing<RingZZp>* R0 = M2::ConcreteRing<RingZZp>::create(R);
  auto R0 = M2::ConcreteRing<RingZZp>::create(R);

  MatZZp M(R0, R, 5, 5);  // Why does this need R0??
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
