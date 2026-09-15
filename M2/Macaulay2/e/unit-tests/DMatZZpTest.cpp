#include <memory>
#include <gtest/gtest.h>

#include "unit-tests/DMatTest.hpp"
#include "unit-tests/ARingMatrixTest.hpp"
#include "basic-mutable-matrices/mat-arith.hpp"
#include "basic-rings/aring-ZZp.hpp"
#include "basic-rings/aring-glue.hpp"

// Defined in ARingZZpTest.cpp.  Declaring the explicit specialization here is
// required: without it, using the generator below implicitly instantiates the
// primary template, which is ill-formed even though it happens to link.
template <>
void getElement<M2::ARingZZp>(const M2::ARingZZp& R,
                              int index,
                              M2::ARingZZp::ElementType& result);

TEST(DMatZZp, create)
{
  typedef M2::ARingZZp RingZZp;
  typedef DMat<M2::ARingZZp> MatZZp;

  RingZZp* R = new RingZZp(101);
  MatZZp M(*R, 5, 5);

  EXPECT_TRUE(&M.ring() == R);

  RingZZp::Element a(*R), b(*R);

  ARingElementGenerator<RingZZp> gen(*R);
  gen.nextElement(a);
  R->copy(M.entry(0, 2), a);

  R->copy(b, M.entry(0, 2));
  EXPECT_TRUE(R->is_equal(a, b));
}

TEST(DMatZZp, submatrix)
{
  typedef M2::ARingZZp RingZZp;
  typedef DMat<M2::ARingZZp> MatZZp;

  RingZZp* R = new RingZZp(101);
  MatZZp M(*R, 5, 5);

  EXPECT_TRUE(&M.ring() == R);

  RingZZp::Element a(*R), b(*R);

  // The isZero assertions below need 'a' to be nonzero, and the '*= a' case
  // needs it to be a unit.  Over the field ZZ/101 nonzero implies unit, so a
  // nonzero draw suffices -- but the draw must be checked: the deterministic
  // prefix of getElement<> starts at -24, which is 0 in characteristic 2 or 3.
  ARingElementGenerator<RingZZp> gen(*R);
  do
    {
      gen.nextElement(a);
    }
  while (R->is_zero(a));
  R->copy(M.entry(0, 2), a);

  R->copy(b, M.entry(0, 2));
  EXPECT_TRUE(R->is_equal(a, b));

  // No check is done that there is no aliasing here...
  // Should there be
  submatrix(M, 0, 0, 1, 1) = submatrix(M, 0, 2, 1, 1);
  R->copy(b, M.entry(0, 0));
  EXPECT_TRUE(R->is_equal(a, b));

  submatrix(M, 0, 0, 2, 2) = 0;
  EXPECT_FALSE(MatrixOps::isZero(M));

  submatrix(M, 0, 2, 2, 2) = 0;
  EXPECT_TRUE(MatrixOps::isZero(M));

  R->copy(M.entry(4, 4), a);
  EXPECT_FALSE(MatrixOps::isZero(M));

  submatrix(M) = 0;
  EXPECT_TRUE(MatrixOps::isZero(M));

  // The assertions below depend on specific positions of N being nonzero, so
  // use Identity rather than a generated fill: that holds whatever values the
  // element generator happens to produce.
  ARingMatrixGenerator<MatZZp> matgen(*R);
  MatZZp N(*R, 2, 2);
  matgen.nextMatrix(N, MatrixShape::Identity);

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
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
