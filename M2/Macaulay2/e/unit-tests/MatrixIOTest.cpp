#include <string>
#include <vector>
#include <gtest/gtest.h>

#include "unit-tests/util-polyring-creation.hpp"
#include "unit-tests/RingElem.hpp"
#include "matrices/matrix.hpp"
#include "BasicPolyList.hpp"
#include "BasicPolyListParser.hpp"
TEST(MatrixIO, readPolynomial)
{
  // Signed coefficients, omitted unit coefficients, and constants round-trip
  // exactly.
  const std::vector<std::string> variables = {"x", "y", "z"};
  struct Case
  {
    const char* name;
    const char* input;
    size_t terms;
  };
  const Case cases[] = {{"mixed degrees", "13*x^2*y-x*y-2", 3},
                        {"negative leading unit", "-x+y^2-13*x*y*z+1", 4},
                        {"negative constant", "2*x+y^2-13*x*y*z-1", 4}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      const auto result = parseBasicPoly(sample.input, variables);
      EXPECT_EQ(result.termCount(), sample.terms);
      EXPECT_EQ(result.toString(variables), sample.input);
    }
}

TEST(MatrixIO, readPolynomialErrors)
{
  // Unknown names and malformed products or powers report parsing errors.
  const std::vector<std::string> variables = {"x", "y", "z"};
  struct Case
  {
    const char* name;
    const char* input;
  };
  const Case cases[] = {{"unknown variable", "3*w-2"},
                        {"missing factor", "3*-2"},
                        {"missing exponent", "3*x^*y-2"}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      EXPECT_THROW(parseBasicPoly(sample.input, variables), parsing_error);
    }
}

TEST(MatrixIO, readMsolve)
{
  // The msolve header and bracketed list produce four polynomial matrix
  // columns.
  std::string contents = R"(#Reduced Groebner basis data
#---
#field characteristic: 1235952427
#variable order:       x, y, z
#monomial order:       graded reverse lexicographical
#length of basis:      4 elements sorted by increasing leading monomials
#---
[1*x^1+2*y^1+2*z^1+1235952426,
1*y^1*z^1+494380972*z^2+370785728*y^1+247190485*z^1,
1*y^2+988761941*z^2+741571456*y^1+494380971*z^1,
1*z^3+924021576*z^2+700373042*y^1+653289140*z^1]:
)";

  auto result = parseMsolveFromString(contents);
  ASSERT_EQ(result.size(), 4);

  const PolynomialRing* R = simplePolynomialRing(1235952427, {"x", "y", "z"});
  ASSERT_NE(R, nullptr);
  const Matrix* M = toMatrix(R->make_FreeModule(1), result);

  ASSERT_NE(M, nullptr);
  ASSERT_EQ(M->n_rows(), 1);
  ASSERT_EQ(M->n_cols(), 4);
  const char* expected[] = {"x+2*y+2*z-1",
                            "y*z+494380972*z^2+370785728*y+247190485*z",
                            "y^2+988761941*z^2+741571456*y+494380971*z",
                            "z^3+924021576*z^2+700373042*y+653289140*z"};
  for (int column = 0; column < 4; ++column)
    {
      SCOPED_TRACE(::testing::Message() << "column " << column);
      EXPECT_EQ(RingElem(R, M->elem(0, column)),
                RingElem::fromString(R, expected[column]));
    }
}

TEST(MatrixIO, readMsolveBig1)
{
  // This benchmark needs a large external msolve fixture absent from the
  // repository.
  GTEST_SKIP()
      << "External msolve benchmark data is not part of the unit-test fixtures";
}

TEST(MatrixIO, readMsolveBig2)
{
  // This benchmark needs a large external msolve fixture absent from the
  // repository.
  GTEST_SKIP()
      << "External msolve benchmark data is not part of the unit-test fixtures";
}

TEST(MatrixIO, readMsolveBig3)
{
  // This benchmark needs a large external msolve fixture absent from the
  // repository.
  GTEST_SKIP()
      << "External msolve benchmark data is not part of the unit-test fixtures";
}

TEST(MatrixIO, readPolys)
{
  // Newline-separated polynomials preserve their matrix entries without a
  // header.
  std::string contents = R"(1*x^1+2*y^1+2*z^1+1235952426
1*y^1*z^1+494380972*z^2+370785728*y^1+247190485*z^1
1*y^2+988761941*z^2+741571456*y^1+494380971*z^1
1*z^3+924021576*z^2+700373042*y^1+653289140*z^1
)";

  std::vector<std::string> varnames = {"x", "y", "z"};
  auto result = parseBasicPolyListFromString(contents, varnames);
  ASSERT_EQ(result.size(), 4);

  const PolynomialRing* R = simplePolynomialRing(1235952427, varnames);
  ASSERT_NE(R, nullptr);
  const Matrix* M = toMatrix(R->make_FreeModule(1), result);

  ASSERT_NE(M, nullptr);
  ASSERT_EQ(M->n_rows(), 1);
  ASSERT_EQ(M->n_cols(), 4);
  const char* expected[] = {"x+2*y+2*z-1",
                            "y*z+494380972*z^2+370785728*y+247190485*z",
                            "y^2+988761941*z^2+741571456*y+494380971*z",
                            "z^3+924021576*z^2+700373042*y+653289140*z"};
  for (int column = 0; column < 4; ++column)
    {
      SCOPED_TRACE(::testing::Message() << "column " << column);
      EXPECT_EQ(RingElem(R, M->elem(0, column)),
                RingElem::fromString(R, expected[column]));
    }
}
