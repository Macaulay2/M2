#include <gtest/gtest.h>

#include "BasicPolyListParser.hpp"

#include <cstddef>
#include <vector>

namespace {

void expectTerm(const BasicPoly& poly,
                size_t termIndex,
                const mpz_class& coeff,
                const std::vector<long>& monomial)
{
  SCOPED_TRACE(::testing::Message() << "term " << termIndex);
  ASSERT_LT(termIndex, poly.mCoefficients.size());

  EXPECT_EQ(poly.mCoefficients[termIndex], coeff);

  size_t loc = 0;

  for (size_t i = 0; i < termIndex; ++i)
    {
      ASSERT_LT(loc, poly.mMonomials.size());
      ASSERT_GT(poly.mMonomials[loc], 0);
      loc += poly.mMonomials[loc];
    }

  ASSERT_LT(loc, poly.mMonomials.size());

  ASSERT_EQ(static_cast<size_t>(poly.mMonomials[loc]), monomial.size());

  ASSERT_LE(loc + monomial.size(), poly.mMonomials.size());
  for (size_t i = 0; i < monomial.size(); ++i)
    {
      EXPECT_EQ(poly.mMonomials[loc + i], monomial[i]) << "monomial word " << i;
    }
}

}  // namespace

TEST(ParseBasicPolyListFromString, ParsesSingleConstant)
{
  // A constant has no variable factors in its encoded monomial.
  auto polys = parseBasicPolyListFromString("5\n", {"x", "y"});

  ASSERT_EQ(polys.size(), 1u);

  expectTerm(polys[0], 0, 5, {1});
}

TEST(ParseBasicPolyListFromString, ParsesSingleVariable)
{
  // A variable has coefficient one and uses its declared index.
  auto polys = parseBasicPolyListFromString("x\n", {"x", "y"});

  ASSERT_EQ(polys.size(), 1u);

  expectTerm(polys[0], 0, 1, {3, 0, 1});
}

TEST(ParseBasicPolyListFromString, ParsesCoefficientAndExponent)
{
  // Explicit coefficients and powers survive the parser encoding.
  auto polys = parseBasicPolyListFromString("3*x^2\n", {"x", "y"});

  ASSERT_EQ(polys.size(), 1u);

  expectTerm(polys[0], 0, 3, {3, 0, 2});
}

TEST(ParseBasicPolyListFromString, ParsesMultipleTerms)
{
  // Mixed signs and the constant term retain their coefficients and order.
  auto polys = parseBasicPolyListFromString("2*x^2-3*y+5\n", {"x", "y"});

  ASSERT_EQ(polys.size(), 1u);

  const auto& poly = polys[0];

  ASSERT_EQ(poly.mCoefficients.size(), 3u);

  expectTerm(poly, 0, 2, {3, 0, 2});
  expectTerm(poly, 1, -3, {3, 1, 1});
  expectTerm(poly, 2, 5, {1});
}

TEST(ParseBasicPolyListFromString, ParsesMultiplePolynomials)
{
  // Newlines separate polynomials without losing either expression.
  auto polys = parseBasicPolyListFromString(
      "x+y\n"
      "x^2-y\n",
      {"x", "y"});

  ASSERT_EQ(polys.size(), 2u);
  expectTerm(polys[0], 0, 1, {3, 0, 1});
  expectTerm(polys[0], 1, 1, {3, 1, 1});
  expectTerm(polys[1], 0, 1, {3, 0, 2});
  expectTerm(polys[1], 1, -1, {3, 1, 1});
}

TEST(ParseBasicPolyListFromString, IgnoresCommentLines)
{
  // Whole-line comments do not consume or alter adjacent polynomials.
  auto polys = parseBasicPolyListFromString(
      "# comment\n"
      "x+y\n"
      "# another comment\n"
      "x^2\n",
      {"x", "y"});

  ASSERT_EQ(polys.size(), 2u);
  expectTerm(polys[0], 0, 1, {3, 0, 1});
  expectTerm(polys[0], 1, 1, {3, 1, 1});
  expectTerm(polys[1], 0, 1, {3, 0, 2});
}

TEST(ParseBasicPolyListFromString, SupportsLargeCoefficients)
{
  // Coefficients beyond machine integers retain every digit.
  auto polys =
      parseBasicPolyListFromString("123456789123456789123456789*x\n", {"x"});

  ASSERT_EQ(polys.size(), 1u);

  expectTerm(polys[0], 0, mpz_class("123456789123456789123456789"), {3, 0, 1});
}

TEST(ParseMsolveFromString, ParsesVariableHeader)
{
  // The msolve header defines the indices used in following monomials.
  auto polys = parseMsolveFromString(
      "#variable order: x y z\n"
      "x+y\n"
      "z^2\n");

  ASSERT_EQ(polys.size(), 2U);

  expectTerm(polys[0], 0, 1, {3, 0, 1});
  expectTerm(polys[0], 1, 1, {3, 1, 1});

  expectTerm(polys[1], 0, 1, {3, 2, 2});
}

TEST(ParseBasicPolyListFromString, HandlesTrailingWhitespace)
{
  // Trailing spaces leave the parsed terms unchanged.
  auto polys = parseBasicPolyListFromString("x+y ", {"x", "y"});

  ASSERT_EQ(polys.size(), 1U);
  expectTerm(polys[0], 0, 1, {3, 0, 1});
  expectTerm(polys[0], 1, 1, {3, 1, 1});
}

TEST(ParseBasicPolyListFromString, HandlesInputWithoutTrailingNewline)
{
  // The final polynomial is retained without a newline terminator.
  auto polys = parseBasicPolyListFromString("x+y", {"x", "y"});

  ASSERT_EQ(polys.size(), 1U);
  expectTerm(polys[0], 0, 1, {3, 0, 1});
  expectTerm(polys[0], 1, 1, {3, 1, 1});
}

TEST(ParseBasicPolyListFromString, HandlesWindowsLineEndings)
{
  // CRLF separators produce the same terms as ordinary newlines.
  auto polys = parseBasicPolyListFromString(
      "x+y\r\n"
      "x^2+y\r\n",
      {"x", "y"});

  ASSERT_EQ(polys.size(), 2U);
  expectTerm(polys[0], 0, 1, {3, 0, 1});
  expectTerm(polys[0], 1, 1, {3, 1, 1});
  expectTerm(polys[1], 0, 1, {3, 0, 2});
  expectTerm(polys[1], 1, 1, {3, 1, 1});
}

TEST(ParseBasicPolyListFromString, RejectsDoubleOperator)
{
  // Consecutive signs are rejected instead of silently changing a coefficient.
  EXPECT_THROW(parseBasicPolyListFromString("x+-y\n", {"x", "y"}),
               parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsDoubleCaret)
{
  // Repeated exponent markers are invalid syntax.
  EXPECT_THROW(parseBasicPolyListFromString("x^^2\n", {"x"}), parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsMissingExponent)
{
  // A power marker must be followed by an exponent.
  EXPECT_THROW(parseBasicPolyListFromString("x^\n", {"x"}), parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsUnknownCharacter)
{
  // Unexpected punctuation reports a parsing error.
  EXPECT_THROW(parseBasicPolyListFromString("@\n", {"x"}), parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsUnknownVariable)
{
  // Only names from the declared variable list may occur.
  EXPECT_THROW(parseBasicPolyListFromString("z\n", {"x", "y"}), parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsExponentOverflow)
{
  // An exponent outside the supported integer range is rejected.
  EXPECT_THROW(
      parseBasicPolyListFromString("x^999999999999999999999999\n", {"x"}),
      parsing_error);
}

TEST(ParseBasicPolyListFromString, HandlesWhitespaceAroundOperators)
{
  // Spaces around arithmetic operators do not change coefficients or powers.
  auto polys = parseBasicPolyListFromString("2*x^2 + 3*y\n", {"x", "y"});

  ASSERT_EQ(polys.size(), 1U);
  expectTerm(polys[0], 0, 2, {3, 0, 2});
  expectTerm(polys[0], 1, 3, {3, 1, 1});
}
