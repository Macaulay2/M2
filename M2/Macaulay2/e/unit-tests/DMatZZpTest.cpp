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
  MatZZp M(*R, 5, 5);

  EXPECT_TRUE(&M.ring() == R);

  RingZZp::ElementType a, b;
  R->init(a);
  R->init(b);
  R->set_from_long(a, 13);
  R->set(M.entry(0, 2), a);

  R->set(b, M.entry(0, 2));
  EXPECT_TRUE(R->is_equal(a, b));
}

TEST(DMatZZp, submatrix)
{
  typedef M2::ARingZZp RingZZp;
  typedef DMat<M2::ARingZZp> MatZZp;

  RingZZp* R = new RingZZp(101);
  MatZZp M(*R, 5, 5);

  EXPECT_TRUE(&M.ring() == R);

  RingZZp::ElementType a, b;
  R->init(a);
  R->init(b);

  R->set_from_long(a, 13);
  R->set(M.entry(0, 2), a);

  R->set(b, M.entry(0, 2));
  EXPECT_TRUE(R->is_equal(a, b));

  // No check is done that there is no aliasing here...
  // Should there be
  submatrix(M, 0, 0, 1, 1) = submatrix(M, 0, 2, 1, 1);
  R->set(b, M.entry(0, 0));
  EXPECT_TRUE(R->is_equal(a, b));

  submatrix(M, 0, 0, 2, 2) = 0;
  EXPECT_FALSE(MatrixOps::isZero(M));

  submatrix(M, 0, 2, 2, 2) = 0;
  EXPECT_TRUE(MatrixOps::isZero(M));

  R->set(M.entry(4, 4), a);
  EXPECT_FALSE(MatrixOps::isZero(M));

  submatrix(M) = 0;
  EXPECT_TRUE(MatrixOps::isZero(M));

  MatZZp N(*R, 2, 2);
  R->set(N.entry(0, 0), a);
  displayMat(N);
  std::cout << std::endl;
  displayMat(M);
  std::cout << std::endl;
  submatrix(M, 0, 1, 2, 2) = submatrix(N);
  submatrix(M, 0, 0, 2, 2) += submatrix(N);
  submatrix(M, 0, 0, 2, 2) *= a;
  displayMat(M);
  std::cout << std::endl;
  EXPECT_FALSE(MatrixOps::isZero(M));

  R->clear(a);
  R->clear(b);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
