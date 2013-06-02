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

  MatZZp M(*R, 5, 5);  // Why does this need R0??

  EXPECT_TRUE(& M.ring() == R);

  RingZZp::ElementType a,b;
  R->init(a);
  R->init(b);
  R->set_from_int(a, 13);
  R->set(M.entry(0,2), a);

  R->set(b, M.entry(0,2));
  EXPECT_TRUE(R->is_equal(a,b));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
