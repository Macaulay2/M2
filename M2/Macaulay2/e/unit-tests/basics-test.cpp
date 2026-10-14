// Copyright 2012 Michael E. Stillman

#include <gtest/gtest.h>

#include <string>
#include <vector>

#include "buffer.hpp"
#include "text-io.hpp"
#include "util.hpp"

TEST(Nothing, ideal)
{
  // Appending an empty string leaves the accumulated text unchanged.
  buffer output;
  output << "ideal" << "";
  EXPECT_STREQ(output.str(), "ideal");
}

TEST(Buffer, make1)
{
  // A fresh buffer exposes an empty, terminated string.
  buffer output;
  EXPECT_STREQ(output.str(), "");
}

TEST(Buffer, make2)
{
  // Appended text is preserved exactly in the buffer's string view.
  buffer output;
  output << "hi there";
  EXPECT_STREQ(output.str(), "hi there");
}

TEST(Util, m2array2stdvec)
{
  // Array conversion preserves the order of distinct integer entries.
  std::vector<int> a {1, 3, 6, 4};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  std::vector<int> c = M2_arrayint_to_stdvector<int>(b);
  EXPECT_EQ(a, c);
}

TEST(Util, m2arrayint_zero)
{
  // Empty arrays stay empty through both conversion directions.
  std::vector<int> a {};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  std::vector<int> c = M2_arrayint_to_stdvector<int>(b);
  EXPECT_EQ(a, c);
}

TEST(Util, m2array2stdvec_big)
{
  // Conversion explicitly narrows each entry to the engine integer array type.
  std::vector<long long> a {-1453853049583, 3, 6, 4, -2};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  std::vector<long long> c = M2_arrayint_to_stdvector<long long>(b);
  ASSERT_EQ(c.size(), a.size());
  for (size_t index = 0; index < a.size(); ++index)
    {
      SCOPED_TRACE(::testing::Message()
                   << "index " << index << ", input " << a[index]);
      EXPECT_EQ(c[index], static_cast<int>(a[index]));
    }
}

TEST(Util, m2array2stdvec_check)
{
  // Negative and positive integer entries survive a round trip.
  std::vector<int> a {-145385, 3, 6, 4, -2};
  M2_arrayint b = stdvector_to_M2_arrayint(a);
  auto c = M2_arrayint_to_stdvector<int>(b);
  EXPECT_EQ(a, c);
}

TEST(Util, m2strings_basic)
{
  // String arrays preserve names, digits, and underscores in order.
  std::vector<std::string> a {"a", "b", "c1", "d2", "e_3"};
  M2_ArrayString b = stdvector_to_M2_ArrayString(a);
  auto c = M2_ArrayString_to_stdvector(b);
  EXPECT_EQ(a, c);
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
