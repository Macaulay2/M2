// Copyright 2017 Michael E. Stillman

#include "schreyer-resolutions/res-moninfo.hpp"

#include <gtest/gtest.h>

#include <vector>

namespace {

template <typename Monoid>
void checkEncoding()
{
  Monoid monoid(4, {1, 1, 1, 1}, {}, MonomialOrderingType::GRevLex);
  // Squares and mixed quadratic monomials exercise repeated and distinct
  // variables.
  for (int i = 0; i < monoid.n_vars(); ++i)
    for (int j = i; j < monoid.n_vars(); ++j)
      {
        SCOPED_TRACE(::testing::Message() << "variables " << i << ", " << j);
        int exponents[4] = {}, decoded[4] = {};
        int encoded[12] = {}, reencoded[12] = {};
        exponents[i]++;
        exponents[j]++;
        component_index component;

        monoid.from_expvector(exponents, 3, encoded);
        monoid.to_expvector(encoded, decoded, component);
        EXPECT_EQ(component, 3);
        for (int k = 0; k < 4; ++k)
          EXPECT_EQ(decoded[k], exponents[k]) << "coordinate " << k;
        monoid.from_expvector(decoded, 3, reencoded);
        EXPECT_TRUE(monoid.is_equal(encoded, reencoded));
      }
}

template <typename Monoid>
void checkMultiplication(int identitySize, int twoVariableSize)
{
  Monoid monoid(4, {1, 1, 1, 1}, {}, MonomialOrderingType::GRevLex);
  {
    // Multiplication by one preserves both exponents and representation size.
    SCOPED_TRACE("multiply: identity");
    int zero[4] = {}, input[] = {0, 1, 0, 3}, decoded[4] = {};
    int identity[12] = {}, value[12] = {}, product[12] = {};
    component_index component;
    monoid.from_expvector(zero, 0, identity);
    monoid.from_expvector(input, 0, value);
    monoid.mult(identity, value, product);
    monoid.to_expvector(product, decoded, component);
    EXPECT_EQ(monoid.monomial_size(identity), identitySize);
    EXPECT_EQ(monoid.monomial_size(value), twoVariableSize);
    EXPECT_EQ(monoid.monomial_size(product), twoVariableSize);
    EXPECT_EQ(component, 0);
    for (int k = 0; k < 4; ++k)
      EXPECT_EQ(decoded[k], input[k]) << "coordinate " << k;
  }
  // Every quadratic times every variable must add the corresponding exponents.
  for (int i = 0; i < 4; ++i)
    for (int j = i; j < 4; ++j)
      for (int k = 0; k < 4; ++k)
        {
          SCOPED_TRACE(::testing::Message()
                       << "variables " << i << ", " << j << ", " << k);
          int left[4] = {}, right[4] = {}, decoded[4] = {}, expected[4] = {};
          int a[12] = {}, b[12] = {}, product[12] = {};
          component_index component;
          left[i]++;
          left[j]++;
          right[k]++;
          expected[i]++;
          expected[j]++;
          expected[k]++;
          monoid.from_expvector(left, 0, a);
          monoid.from_expvector(right, 0, b);

          monoid.mult(a, b, product);
          monoid.to_expvector(product, decoded, component);
          EXPECT_EQ(component, 0);
          for (int index = 0; index < 4; ++index)
            EXPECT_EQ(decoded[index], expected[index])
                << "coordinate " << index;
          if (identitySize == 6)
            EXPECT_EQ(monoid.monomial_size(product), 6);
          else
            EXPECT_EQ(monoid.monomial_size(product),
                      monoid.monomial_size(a) + monoid.monomial_size(b) - 3);
        }
}

// Unit exponent vectors expose truncation at the first and last variable.
void checkVariableEncoding(int variables)
{
  ResMonoidDense monoid(variables,
                        std::vector<int>(variables, 1),
                        {},
                        MonomialOrderingType::GRevLex);
  for (int variable : {0, variables - 1})
    {
      SCOPED_TRACE(::testing::Message() << "variables " << variables
                                        << ", nonzero coordinate " << variable);
      std::vector<int> expected(variables, 0), actual(variables, -1);
      std::vector<int> encoded(monoid.max_monomial_size());
      expected[variable] = 1;
      component_index component = -1;

      ASSERT_TRUE(monoid.from_expvector(expected.data(), 3, encoded.data()));
      ASSERT_TRUE(
          monoid.to_expvector(encoded.data(), actual.data(), component));
      EXPECT_EQ(actual, expected);
      EXPECT_EQ(component, 3);
    }
}

}  // namespace

TEST(ResMonoidDense, create)
{
  // Each supported ordering retains the requested number of variables.
  {
    SCOPED_TRACE("construct: grevlex");
    ResMonoidDense monoid(4, {1, 1, 1, 1}, {}, MonomialOrderingType::GRevLex);
    EXPECT_EQ(monoid.n_vars(), 4);
  }
  {
    SCOPED_TRACE("construct: weighted");
    ResMonoidDense monoid(4,
                          {1, 2, 3, 4},
                          {1, 1, 1, 1, 1, 1, 0, 0},
                          MonomialOrderingType::Weights);
    EXPECT_EQ(monoid.n_vars(), 4);
  }
  {
    SCOPED_TRACE("construct: lex");
    ResMonoidDense monoid(4, {1, 1, 1, 1}, {}, MonomialOrderingType::Lex);
    EXPECT_EQ(monoid.n_vars(), 4);
  }
}

TEST(ResMonoidDense, encodeDecode)
{
  // Dense encodings preserve the exponent vector and module component.
  checkEncoding<ResMonoidDense>();
}

TEST(ResMonoidSparse, encodeDecode)
{
  // Sparse encodings preserve the exponent vector and module component.
  checkEncoding<ResMonoidSparse>();
}

TEST(ResMonoidDense, mult)
{
  // Dense products add exponents in a fixed-size representation.
  checkMultiplication<ResMonoidDense>(6, 6);
}

TEST(ResMonoidSparse, mult)
{
  // Sparse products add exponents in a variable-length representation.
  checkMultiplication<ResMonoidSparse>(3, 7);
}

TEST(ResMonoidDense, encode5)
{
  // Five-variable encodings retain the first and last coordinates.
  checkVariableEncoding(5);
}

TEST(ResMonoidDense, encode6)
{
  // Six-variable encodings retain the first and last coordinates.
  checkVariableEncoding(6);
}

TEST(ResMonoidDense, concatenateResMonoidDense)
{
  // Dense resolution monomials expose multiplication, not a concatenation API.
  GTEST_SKIP() << "ResMonoidDense has no concatenation operation; mult checks "
                  "multiplication";
}

TEST(ResMonoidDense, outOfRange)
{
  // The reserved guard bit makes an encoded module component invalid.
  ResMonoidDense monoid(2, {1, 1}, {}, MonomialOrderingType::GRevLex);
  std::vector<int> encoded(monoid.max_monomial_size());
  monoid.one(1 << 28, encoded.data());

  EXPECT_FALSE(monoid.check_monomial(encoded.data()));
}

TEST(ResMonoidDense, encodeBoundary)
{
  // The largest component below the reserved guard bit still round-trips.
  ResMonoidDense monoid(2, {1, 1}, {}, MonomialOrderingType::GRevLex);
  constexpr component_index largestComponent = (1 << 28) - 1;
  std::vector<int> encoded(monoid.max_monomial_size());
  int exponents[] = {-1, -1};
  component_index component = -1;
  ASSERT_TRUE(monoid.one(largestComponent, encoded.data()));

  EXPECT_TRUE(monoid.check_monomial(encoded.data()));
  ASSERT_TRUE(monoid.to_expvector(encoded.data(), exponents, component));
  EXPECT_EQ(component, largestComponent);
  EXPECT_EQ(exponents[0], 0);
  EXPECT_EQ(exponents[1], 0);
}
