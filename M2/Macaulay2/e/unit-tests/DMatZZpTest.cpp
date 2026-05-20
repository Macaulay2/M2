/**
 * @file unit-tests/DMatZZpTest.cpp
 * @brief gtest coverage for `DMat<M2::ARingZZp>` --- dense matrices over `Z/p`.
 *
 * Hosts `TEST(DMatZZp, create)` and `TEST(DMatZZp, submatrix)`
 * over `DMat<M2::ARingZZp>` instances at `Z/101`. `create`
 * pins down construction with `(ring, rows, cols)`,
 * single-entry get / set round-trips through `M.entry(i, j)`,
 * and the `M.ring()` accessor; `submatrix` covers `submatrix`
 * assignment, in-place `+=` and `*=`, and `MatrixOps::isZero`
 * on subblock zeros. `aring-glue.hpp` is pulled in alongside
 * the `ARingZZp` header so the legacy `Ring` surface the F4
 * engine routes through is exercised in the same pass.
 *
 * Companion files `MatrixIOTest.cpp` and `PolyRingTest.cpp`
 * (same `file-dmat-matrix-tests` markdown) cover the matrix /
 * polynomial-list I/O machinery and the polynomial-ring
 * constructors `DMat`-based code sits on top of.
 *
 * @see DMatTest.hpp
 * @see aring-zzp.hpp
 * @see aring-glue.hpp
 * @see NewF4Test.cpp
 */

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
