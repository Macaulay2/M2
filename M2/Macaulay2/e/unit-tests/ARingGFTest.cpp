// Copyright 2013 Michael E. Stillman

#include "basic-rings/aring-GF-flint.hpp"

#include <gtest/gtest.h>

#include <memory>
#include <string>
#include <vector>

#include "unit-tests/ARingTest.hpp"
#include "unit-tests/util-polyring-creation.hpp"

// Extension fields expose characteristic and dimension rather than cardinality.
template <>
mpz_class finiteFieldCardinality(const M2::ARingGFFlint& R)
{
  mpz_class order;
  mpz_ui_pow_ui(order.get_mpz_t(), R.characteristic(), R.dimension());
  return order;
}

template <>
void getElement<M2::ARingGFFlint>(const M2::ARingGFFlint& R,
                                  int index,
                                  M2::ARingGFFlint::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    R.random(result);
}

namespace {
using Ring = M2::ARingGFFlint;

// FLINT's Zech representation requires a primitive defining polynomial.
std::unique_ptr<Ring> makeField(int characteristic, const std::string& modulus)
{
  const auto* polynomialRing = simplePolynomialRing(characteristic, {"a"});
  if (polynomialRing == nullptr) return nullptr;
  const auto* quotient = simpleQuotientRing(polynomialRing, {modulus});
  if (quotient == nullptr) return nullptr;
  const auto* original = quotient->cast_to_PolynomialRing();
  if (original == nullptr) return nullptr;
  return std::make_unique<Ring>(*original, original->var(0));
}

void expectCoefficients(const Ring& R,
                        const Ring::ElementType& value,
                        const std::vector<long>& expected)
{
  std::vector<long> actual;
  R.getSmallIntegerCoefficients(value, actual);
  EXPECT_EQ(actual, expected);
}

TEST(ARingGFFlint, create)
{
  // The primitive cubic defines GF(125); integers land in its prime subfield.
  auto ring = makeField(5, "a^3+3*a+3");
  ASSERT_NE(ring, nullptr);
  const auto& R = *ring;

  EXPECT_EQ(ringName(R), "GF(5^3,Flint)");
  EXPECT_EQ(R.characteristic(), 5);
  EXPECT_EQ(R.dimension(), 3);
  EXPECT_EQ(finiteFieldCardinality(R), 125);

  // Negative, zero, positive and divisible integers use canonical coefficients.
  struct Case
  {
    const char* name;
    int input;
    std::vector<long> expected;
  };
  for (const auto& sample : {Case {"negative", -11, {4}},
                             Case {"zero", 0, {}},
                             Case {"positive", 27, {2}},
                             Case {"multiple of characteristic", 125, {}}})
    {
      SCOPED_TRACE(sample.name);
      Ring::Element value(R);
      R.set(value, sample.input);
      expectCoefficients(R, value, sample.expected);
    }

  // The defining polynomial vanishes, and its variable has degree one.
  {
    SCOPED_TRACE("quotient: defining polynomial and variable");
    Ring::Element relation(R), generator(R);
    R.fromSmallIntegerCoefficients(relation, {3, 3, 0, 1});
    R.getGenerator(generator);

    EXPECT_TRUE(R.is_zero(relation));
    expectCoefficients(R, generator, {0, 1});
  }
  testSomeMore(R);
}

TEST(ARingGFFlint, random)
{
  // Seeded GF(49) samples have canonical coefficients and survive conversion.
  // No frequency threshold is used: this checks representation, not luck.
  // a^2+a+3 is primitive over GF(7), as required by the Zech constructor.
  auto ring = makeField(7, "a^2+a+3");
  ASSERT_NE(ring, nullptr);
  const auto& R = *ring;
  constexpr unsigned long seed = 0x4746;
  seedRandom(seed);
  SCOPED_TRACE(::testing::Message() << "seed " << seed);

  for (int trial = 0; trial < 200; ++trial)
    {
      Ring::Element value(R), restored(R);
      R.random(value);
      std::vector<long> coefficients;
      R.getSmallIntegerCoefficients(value, coefficients);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << trial << ", coefficients "
                   << ::testing::PrintToString(coefficients));

      EXPECT_LE(coefficients.size(), R.dimension());
      for (auto coefficient : coefficients)
        {
          EXPECT_GE(coefficient, 0);
          EXPECT_LT(coefficient, R.characteristic());
        }
      R.fromSmallIntegerCoefficients(restored, coefficients);
      EXPECT_TRUE(R.is_equal(value, restored));
    }
}

TEST(ARingGFFlint, arithmetic)
{
  // Generated field identities include extension elements after the integer
  // prefix, so arithmetic is checked beyond the prime subfield.
  auto ring = makeField(5, "a^3+3*a+3");
  ASSERT_NE(ring, nullptr);
  constexpr unsigned long seed = 0x474641;
  seedRandom(seed);
  SCOPED_TRACE(::testing::Message() << "seed " << seed);

  testFiniteField(*ring, ntrials);
}

TEST(ARingGFFlint, arithmeticExamples)
{
  // (a+2)(a^2+3) = 2a^2+3 modulo a^3+3a+3, with exact coefficients.
  auto ring = makeField(5, "a^3+3*a+3");
  ASSERT_NE(ring, nullptr);
  const auto& R = *ring;
  Ring::Element a(R), b(R), result(R);
  R.fromSmallIntegerCoefficients(a, {2, 1});
  R.fromSmallIntegerCoefficients(b, {3, 0, 1});

  R.mult(result, a, b);

  expectCoefficients(R, result, {3, 0, 2});
}
}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
