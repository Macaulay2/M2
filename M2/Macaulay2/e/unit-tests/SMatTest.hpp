#ifndef M2_UNIT_TESTS_SMAT_TEST_HPP_
#define M2_UNIT_TESTS_SMAT_TEST_HPP_

#include "basic-mutable-matrices/smat.hpp"
#include <gtest/gtest.h>
#include <cstddef>
#include <initializer_list>
#include <limits>
#include <memory>
#include <type_traits>
#include <vector>
#include "buffer.hpp"
#include "basic-rings/aring-tower.hpp"

namespace {
template <typename RT>
struct SMatRingFactory;
template <typename RT>
class SMatTest : public ::testing::Test
{
 protected:
  using Ring = RT;
  using Mat = SMat<Ring>;
  std::unique_ptr<Ring> ringOwner = SMatRingFactory<Ring>::make();
  Ring& ring = *ringOwner;

  void SetUp() override
  {
    if constexpr (std::is_same_v<Ring, M2::ARingTower>)
      GTEST_SKIP()
          << "ARingTower::set(integer) and init_set are unimplemented; "
             "populated SMat operations cannot be tested with this backend.";
  }

  // Returning an owning temporary keeps MPFR/GMP coefficients alive through
  // the matrix call, without copying their resource-owning C structs.
  class Scalar : public Ring::Element
  {
   public:
    Scalar(const Ring& R, int value) : Ring::Element(R) { R.set(*this, value); }
  };

  Scalar scalar(int value) const { return Scalar(ring, value); }

  ::testing::AssertionResult equal(const Ring& R,
                                   const typename Ring::ElementType& actual,
                                   const typename Ring::ElementType& expected)
  {
    // All worked values are integers or exact dyadic quotients, including
    // for RR/CC and interval rings; no rounding tolerance is needed here.
    if (R.is_equal(actual, expected)) return ::testing::AssertionSuccess();
    buffer a, e;
    R.elem_text_out(a, actual, true, false, false);
    R.elem_text_out(e, expected, true, false, false);
    return ::testing::AssertionFailure()
           << "expected " << e.str() << ", actual " << a.str();
  }

  void fill(Mat& matrix, std::initializer_list<int> values)
  {
    ASSERT_EQ(values.size(), matrix.numRows() * matrix.numColumns());
    auto value = values.begin();
    for (size_t r = 0; r < matrix.numRows(); ++r)
      for (size_t c = 0; c < matrix.numColumns(); ++c)
        matrix.set_entry(r, c, scalar(*value++));
  }

  void expectMatrix(const Mat& matrix,
                    size_t rows,
                    size_t cols,
                    const std::vector<int>& values)
  {
    ASSERT_EQ(matrix.numRows(), rows);
    ASSERT_EQ(matrix.numColumns(), cols);
    ASSERT_EQ(values.size(), rows * cols);
    const Ring& R = matrix.ring();
    typename Ring::Element actual(R), expected(R);
    bool zero = true;
    for (size_t c = 0; c < cols; ++c)
      {
        SCOPED_TRACE(::testing::Message() << "column " << c);
        size_t count = 0;
        size_t lead = std::numeric_limits<size_t>::max();
        for (size_t r = 0; r < rows; ++r)
          {
            SCOPED_TRACE(::testing::Message() << "row " << r);
            R.set(expected, values[r * cols + c]);
            bool nonzero = !R.is_zero(expected);
            EXPECT_EQ(matrix.get_entry(r, c, actual), nonzero);
            if (nonzero)
              {
                EXPECT_TRUE(equal(R, actual, expected));
                lead = r;
                ++count;
                zero = false;
              }
          }
        EXPECT_EQ(matrix.lead_row(c), lead);
        // Unlike get_entry, lead_row initializes its output on success.
        typename Ring::ElementType leading;
        size_t actualLead = matrix.lead_row(c, leading);
        EXPECT_EQ(actualLead, lead);
        if (actualLead != std::numeric_limits<size_t>::max())
          {
            if (count != 0)
              {
                R.set(expected, values[lead * cols + c]);
                EXPECT_TRUE(equal(R, leading, expected));
              }
            R.clear(leading);
          }

        // Check the stored nodes as well as lookup: zero nodes and broken row
        // ordering can be invisible when only the matrix entries are checked.
        auto it = matrix.begin();
        it.set(c);
        size_t previous = rows;
        size_t visited = 0;
        while (it.valid())
          {
            ASSERT_LT(visited, count);
            ASSERT_LT(it.row(), previous);
            R.set(expected, values[it.row() * cols + c]);
            EXPECT_FALSE(R.is_zero(it.value()));
            EXPECT_TRUE(equal(R, it.value(), expected));
            previous = it.row();
            ++visited;
            it.next();
          }
        EXPECT_EQ(visited, count);
      }
    EXPECT_EQ(matrix.is_zero(), zero);
  }
};

}  // namespace
#endif
