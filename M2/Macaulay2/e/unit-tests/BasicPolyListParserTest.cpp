#include <gtest/gtest.h>

#include "BasicPolyListParser.hpp"

namespace
{

void expectTerm(const BasicPoly& poly,
                size_t termIndex,
                const mpz_class& coeff,
                const std::vector<long>& monomial)
{
    ASSERT_LT(termIndex, poly.mCoefficients.size());

    EXPECT_EQ(poly.mCoefficients[termIndex], coeff);

    size_t loc = 0;

    for (size_t i = 0; i < termIndex; ++i)
    {
        loc += poly.mMonomials[loc];
    }

    ASSERT_LT(loc, poly.mMonomials.size());

    ASSERT_EQ(
        static_cast<size_t>(poly.mMonomials[loc]),
        monomial.size());

    for (size_t i = 0; i < monomial.size(); ++i)
    {
        EXPECT_EQ(poly.mMonomials[loc + i], monomial[i]);
    }
}

} // namespace

TEST(ParseBasicPolyListFromString, ParsesSingleConstant)
{
    auto polys =
        parseBasicPolyListFromString(
            "5\n",
            {"x", "y"});

    ASSERT_EQ(polys.size(), 1u);

    expectTerm(polys[0], 0, 5, {1});
}

TEST(ParseBasicPolyListFromString, ParsesSingleVariable)
{
    auto polys =
        parseBasicPolyListFromString(
            "x\n",
            {"x", "y"});

    ASSERT_EQ(polys.size(), 1u);

    expectTerm(polys[0], 0, 1, {3, 0, 1});
}

TEST(ParseBasicPolyListFromString, ParsesCoefficientAndExponent)
{
    auto polys =
        parseBasicPolyListFromString(
            "3*x^2\n",
            {"x", "y"});

    ASSERT_EQ(polys.size(), 1u);

    expectTerm(polys[0], 0, 3, {3, 0, 2});
}

TEST(ParseBasicPolyListFromString, ParsesMultipleTerms)
{
    auto polys =
        parseBasicPolyListFromString(
            "2*x^2-3*y+5\n",
            {"x", "y"});

    ASSERT_EQ(polys.size(), 1u);

    const auto& poly = polys[0];

    ASSERT_EQ(poly.mCoefficients.size(), 3u);

    expectTerm(poly, 0,  2, {3, 0, 2});
    expectTerm(poly, 1, -3, {3, 1, 1});
    expectTerm(poly, 2,  5, {1});
}

TEST(ParseBasicPolyListFromString, ParsesMultiplePolynomials)
{
    auto polys =
        parseBasicPolyListFromString(
            "x+y\n"
            "x^2-y\n",
            {"x", "y"});

    ASSERT_EQ(polys.size(), 2u);
}

TEST(ParseBasicPolyListFromString, IgnoresCommentLines)
{
    auto polys =
        parseBasicPolyListFromString(
            "# comment\n"
            "x+y\n"
            "# another comment\n"
            "x^2\n",
            {"x", "y"});

    ASSERT_EQ(polys.size(), 2u);
}

TEST(ParseBasicPolyListFromString, SupportsLargeCoefficients)
{
    auto polys =
        parseBasicPolyListFromString(
            "123456789123456789123456789*x\n",
            {"x"});

    ASSERT_EQ(polys.size(), 1u);

    expectTerm(
        polys[0],
        0,
        mpz_class("123456789123456789123456789"),
        {3, 0, 1});
}

TEST(ParseMsolveFromString, ParsesVariableHeader)
{
    auto polys =
        parseMsolveFromString(
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
    EXPECT_NO_THROW({
        auto polys =
            parseBasicPolyListFromString(
                "x+y ",
                {"x", "y"});

        ASSERT_EQ(polys.size(), 1U);
    });
}

TEST(ParseBasicPolyListFromString, HandlesInputWithoutTrailingNewline)
{
    EXPECT_NO_THROW({
        auto polys =
            parseBasicPolyListFromString(
                "x+y",
                {"x", "y"});

        ASSERT_EQ(polys.size(), 1U);
    });
}

TEST(ParseBasicPolyListFromString, HandlesWindowsLineEndings)
{
    auto polys =
        parseBasicPolyListFromString(
            "x+y\r\n"
            "x^2+y\r\n",
            {"x", "y"});

    ASSERT_EQ(polys.size(), 2U);
}

TEST(ParseBasicPolyListFromString, RejectsDoubleOperator)
{
    EXPECT_THROW(
        parseBasicPolyListFromString(
            "x+-y\n",
            {"x", "y"}),
        parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsDoubleCaret)
{
    EXPECT_THROW(
        parseBasicPolyListFromString(
            "x^^2\n",
            {"x"}),
        parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsMissingExponent)
{
    EXPECT_THROW(
        parseBasicPolyListFromString(
            "x^\n",
            {"x"}),
        parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsUnknownCharacter)
{
    EXPECT_THROW(
        parseBasicPolyListFromString(
            "@\n",
            {"x"}),
        parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsUnknownVariable)
{
    EXPECT_THROW(
        parseBasicPolyListFromString(
            "z\n",
            {"x", "y"}),
        parsing_error);
}

TEST(ParseBasicPolyListFromString, RejectsExponentOverflow)
{
    EXPECT_THROW(
        parseBasicPolyListFromString(
            "x^999999999999999999999999\n",
            {"x"}),
        parsing_error);
}

TEST(ParseBasicPolyListFromString, HandlesWhitespaceAroundOperators)
{
    EXPECT_NO_THROW({
        auto polys =
            parseBasicPolyListFromString(
                "2*x^2 + 3*y\n",
                {"x", "y"});

        ASSERT_EQ(polys.size(), 1U);
    });
}
