#include "basic-rings/aring-ZZ-gmp.hpp"

#include <gtest/gtest.h>
#include <mpfr.h>

#include <initializer_list>
#include <string>

#include "basic-rings/aring-glue.hpp"
#include "unit-tests/ARingTest.hpp"

extern gmp_ZZ getRandomInteger();

template <>
void getElement<M2::ARingZZGMP>(const M2::ARingZZGMP& R,
                                int index,
                                M2::ARingZZGMP::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set(result, a);
    }
}

namespace {

class ARingZZGMP : public ::testing::Test
{
 protected:
  M2::ARingZZGMP R;

  std::string textOut(const M2::ARingZZGMP::ElementType& a,
                      bool p_one,
                      bool p_plus,
                      bool p_parens)
  {
    buffer o;
    R.elem_text_out(o, a, p_one, p_plus, p_parens);
    return std::string(o.str());
  }
};

TEST_F(ARingZZGMP, Construction)
{
  // The GMP integer ring has characteristic zero and unbounded cardinality.
  // static_cast avoids odr-using ringID, which has no out-of-line definition.
  EXPECT_EQ(static_cast<int>(M2::ARingZZGMP::ringID),
            static_cast<int>(M2::ring_ZZ));
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), static_cast<size_t>(0));
}

TEST_F(ARingZZGMP, DISABLED_ringName)
{
  // text_out currently reports the FLINT backend. Disabled until it identifies
  // GMP. https://github.com/Macaulay2/M2/issues/4695
  EXPECT_EQ(ringName(R), "ZZGMP");
}

TEST_F(ARingZZGMP, Storage)
{
  // set_var must give 1 for a coefficient ring.
  testCoefficientRingSetVar(R);
}

TEST_F(ARingZZGMP, Comparisons)
{
  // The only integer units are +/-1; equality and zero recognition must agree.

  M2::ARingZZGMP::Element a(R), b(R);

  {
    // Both unit signs and several nonunits expose overly broad unit predicates.
    SCOPED_TRACE("is_unit: the only units of ZZ are 1 and -1");
    R.set(a, 1);
    EXPECT_TRUE(R.is_unit(a));
    R.set(a, -1);
    EXPECT_TRUE(R.is_unit(a));
    for (int i : {-1000000, -2, 0, 2, 1000000})
      {
        SCOPED_TRACE(i);
        R.set(a, i);
        EXPECT_FALSE(R.is_unit(a));
      }
  }

  {
    // Setting a nonzero value after zero must replace the old contents.
    SCOPED_TRACE("is_zero");
    R.set_zero(a);
    EXPECT_TRUE(R.is_zero(a));
    R.set(a, 1);
    EXPECT_FALSE(R.is_zero(a));
  }

  {
    // Use separately assigned equal values, without requiring distinct hashes.
    SCOPED_TRACE("computeHashValue: equal values hash equally");
    R.set(a, 5);
    R.set(b, 5);
    EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
  }
}

TEST_F(ARingZZGMP, Conversions)
{
  // Conversions accept integers, reject fractions and reals, and check
  // machine-long range.

  M2::ARingZZGMP::Element a(R), expected(R);

  {
    // An integer longer than a machine word checks heap-backed conversion.
    SCOPED_TRACE("set: from mpz");
    mpz_t m;
    mpz_init(m);
    mpz_set_str(m, "123456789012345678901234567890", 10);
    R.set(a, m);
    EXPECT_EQ(textOut(a, true, false, false), "123456789012345678901234567890");
    mpz_clear(m);
  }

  {
    // Both integer overloads must agree on a representable value.
    SCOPED_TRACE("set: from int and from long agree");
    R.set(a, 17);
    R.set(expected, 17L);
    EXPECT_TRUE(R.is_equal(a, expected));
  }

  {
    // The same numerator with different denominators separates integral and
    // fractional inputs.
    SCOPED_TRACE("set: from mpq succeeds only when the denominator is 1");
    mpq_t q;
    mpq_init(q);
    mpq_set_si(q, 22, 1);
    EXPECT_TRUE(R.set(a, q));
    R.set(expected, 22);
    EXPECT_TRUE(R.is_equal(a, expected));

    mpq_set_si(q, 22, 7);
    mpq_canonicalize(q);
    EXPECT_FALSE(R.set(a, q));
    mpq_clear(q);
  }

  {
    // Even an integral-valued real is rejected by this unsupported conversion.
    SCOPED_TRACE("set: from gmp_RR is never supported");
    mpfr_t x;
    mpfr_init2(x, 53);
    mpfr_set_d(x, 2.0, MPFR_RNDN);
    EXPECT_FALSE(R.set(a, x));
    mpfr_clear(x);
  }

  {
    // Small signed integers fit; 2^200 cannot fit a machine long.
    SCOPED_TRACE("coerceToLongInteger");
    long result = 0;
    for (long i : {-1000000L, -1L, 0L, 1L, 1000000L})
      {
        SCOPED_TRACE(i);
        R.set(a, i);
        EXPECT_TRUE(R.coerceToLongInteger(result, a));
        EXPECT_EQ(result, i);
      }

    mpz_t big;
    mpz_init(big);
    mpz_ui_pow_ui(big, 2, 200);
    R.set(a, big);
    EXPECT_FALSE(R.coerceToLongInteger(result, a));
    mpz_clear(big);
  }

  {
    // Unsupported operations must report failure even for an integer
    // coefficient.
    SCOPED_TRACE("promote and lift: ARingZZGMP supports neither");
    ring_elem f;
    R.set(a, 3);
    R.to_ring_elem(f, a);
    EXPECT_FALSE(R.promote(globalZZ, f, a));
    EXPECT_FALSE(R.lift(globalZZ, a, f));
  }
}

TEST_F(ARingZZGMP, Arithmetic)
{
  // Integer inversion and exact division have independently known answers.

  M2::ARingZZGMP::Element a(R), b(R), c(R), expected(R);

  {
    // Unit signs invert exactly; nonunits use the documented zero result.
    SCOPED_TRACE("invert: the identity on units, zero on everything else");
    R.set(a, 1);
    R.invert(b, a);
    EXPECT_TRUE(R.is_equal(b, a));
    R.set(a, -1);
    R.invert(b, a);
    EXPECT_TRUE(R.is_equal(b, a));
    for (int i : {-7, 0, 2, 13})
      {
        SCOPED_TRACE(i);
        R.set(a, i);
        R.invert(b, a);
        EXPECT_TRUE(R.is_zero(b));
      }
  }

  {
    // Both quotient signs have small independently known answers.
    SCOPED_TRACE("divide: exact, including a negative dividend");
    R.set(a, 42);
    R.set(b, 7);
    R.divide(c, a, b);
    R.set(expected, 6);
    EXPECT_TRUE(R.is_equal(c, expected));

    R.set(a, -42);
    R.set(b, 7);
    R.divide(c, a, b);
    R.set(expected, -6);
    EXPECT_TRUE(R.is_equal(c, expected));
  }

  {
    // The GMP backend rejects inexact division with an exception.
    SCOPED_TRACE("divide: inexact division throws instead of truncating");
    R.set(a, 2);
    R.set(b, 3);
    EXPECT_THROW(R.divide(c, a, b), exc::engine_error);
  }
}

TEST_F(ARingZZGMP, Powers)
{
  // Known powers agree across exponent interfaces; negative integer powers are
  // rejected.

  M2::ARingZZGMP::Element a(R), b(R), expected(R);
  mpz_t n;
  mpz_init(n);

  {
    // Even and odd powers expose sign errors.
    SCOPED_TRACE("power: worked examples");
    R.set(a, 2);
    R.power(b, a, 10);
    R.set(expected, 1024);
    EXPECT_TRUE(R.is_equal(b, expected));

    R.set(a, -3);
    R.power(b, a, 3);
    R.set(expected, -27);
    EXPECT_TRUE(R.is_equal(b, expected));
  }

  {
    // Exponent 100 is supported, but 2^100 needs multiple limbs.
    SCOPED_TRACE("power_mpz: result larger than a machine word");
    R.set(a, 2);
    mpz_set_si(n, 100);
    R.power_mpz(b, a, n);
    mpz_t twoTo100;
    mpz_init(twoTo100);
    mpz_ui_pow_ui(twoTo100, 2, 100);
    R.set(expected, twoTo100);
    EXPECT_TRUE(R.is_equal(b, expected));
    mpz_clear(twoTo100);
  }

  {
    // ZZ does not contain the reciprocal of the chosen base 2.
    SCOPED_TRACE("power_mpz: a negative exponent throws");
    R.set(a, 2);
    mpz_set_si(n, -1);
    EXPECT_THROW(R.power_mpz(b, a, n), exc::engine_error);
  }

  mpz_clear(n);
}

TEST_F(ARingZZGMP, Syzygy)
{
  // syzygy returns (x, y) with a*x + b*y == 0.

  M2::ARingZZGMP::Element a(R), b(R), x(R), y(R), expected(R);

  {
    // A zero numerator uses the simple cancelling pair with a nonzero divisor.
    SCOPED_TRACE("syzygy: a == 0 gives (1, 0)");
    R.set_zero(a);
    R.set(b, 5);
    R.syzygy(a, b, x, y);
    R.set(expected, 1);
    EXPECT_TRUE(R.is_equal(x, expected));
    EXPECT_TRUE(R.is_zero(y));
  }

  {
    // A positive unit divisor needs no gcd reduction.
    SCOPED_TRACE("syzygy: b == 1 gives (1, -a)");
    R.set(a, 7);
    R.set(b, 1);
    R.syzygy(a, b, x, y);
    R.set(expected, 1);
    EXPECT_TRUE(R.is_equal(x, expected));
    R.set(expected, -7);
    EXPECT_TRUE(R.is_equal(y, expected));
  }

  {
    // A negative unit divisor reverses the second coefficient sign.
    SCOPED_TRACE("syzygy: b == -1 gives (1, a)");
    R.set(a, 7);
    R.set(b, -1);
    R.syzygy(a, b, x, y);
    R.set(expected, 1);
    EXPECT_TRUE(R.is_equal(x, expected));
    R.set(expected, 7);
    EXPECT_TRUE(R.is_equal(y, expected));
  }

  {
    // A common factor must be removed from the cancelling pair.
    SCOPED_TRACE("syzygy: general case, 4*3 + 6*(-2) == 0");
    R.set(a, 4);
    R.set(b, 6);
    R.syzygy(a, b, x, y);
    R.set(expected, 3);
    EXPECT_TRUE(R.is_equal(x, expected));
    R.set(expected, -2);
    EXPECT_TRUE(R.is_equal(y, expected));
  }

  {
    // Negating the divisor changes the sign of the second coefficient.
    SCOPED_TRACE("syzygy: general case with b negative, the other sign branch");
    R.set(a, 4);
    R.set(b, -6);
    R.syzygy(a, b, x, y);
    R.set(expected, 3);
    EXPECT_TRUE(R.is_equal(x, expected));
    R.set(expected, 2);
    EXPECT_TRUE(R.is_equal(y, expected));
  }
}

TEST_F(ARingZZGMP, Formatting)
{
  // Signs and unit-digit flags must preserve integer text.

  M2::ARingZZGMP::Element a(R);

  struct
  {
    const char* name;  // the case, for SCOPED_TRACE
    int value;         // the element to print
    bool p_one;        // print a bare 1?
    bool p_plus;       // prefix '+' when nonnegative?
    const char* expected;
  } cases[] = {
      {"zero", 0, true, false, "0"},
      {"zero, p_plus", 0, true, true, "+0"},
      {"one", 1, true, false, "1"},
      {"one, p_one false", 1, false, false, ""},
      {"one, p_plus", 1, true, true, "+1"},
      {"minus one", -1, true, false, "-1"},
      {"minus one, p_one false", -1, false, false, "-"},
      {"minus one, p_plus", -1, true, true, "-1"},
      {"positive", 42, true, false, "42"},
      {"positive, p_plus", 42, true, true, "+42"},
      {"negative", -42, true, false, "-42"},
      {"negative, p_plus", -42, true, true, "-42"},
  };

  for (auto& c : cases)
    {
      SCOPED_TRACE(c.name);
      R.set(a, c.value);
      EXPECT_EQ(textOut(a, c.p_one, c.p_plus, false), c.expected);
      EXPECT_EQ(textOut(a, c.p_one, c.p_plus, true), c.expected)
          << "p_parens changed the output";
    }
}

TEST_F(ARingZZGMP, Evaluation)
{
  // Integer coefficients evaluate unchanged into ZZ and QQ.

  M2::ARingZZGMP::Element a(R);
  const RingMap* toZZ = identityRingMap(globalZZ);
  const RingMap* toQQ = identityRingMap(globalQQ);

  for (int i : {-17, 0, 1, 2025})
    {
      SCOPED_TRACE(i);
      ring_elem result;
      R.set(a, i);

      R.eval(toZZ, a, 0, result);
      EXPECT_TRUE(globalZZ->is_equal(result, globalZZ->from_long(i)));

      R.eval(toQQ, a, 0, result);
      EXPECT_TRUE(globalQQ->is_equal(result, globalQQ->from_long(i)));
    }
}

TEST_F(ARingZZGMP, RandomizedProperties)
{
  // Generated integers satisfy the shared arithmetic and storage contracts.
  seedRandom(0x5a5a);
  SCOPED_TRACE("seed 0x5a5a");
  testStorage(R, ntrials);
  testComparisons(R, ntrials);
  testRingElemRoundTrip(R, ntrials);
  testFromRingElemConst(R, ntrials);
  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testAxioms(R, ntrials);
  testAliasing(R, ntrials);
  testSubtractMultiple(R, ntrials);
  testPowerAgreement(R, ntrials / 10);
  testPowerMpzOutOfRange(R);
  testSyzygy(R, ntrials);

  // Independent draws exercise the backend random generator.
  M2::ARingZZGMP::Element a(R), b(R);

  bool sawDistinct = false;
  R.random(a);
  for (int i = 0; i < ntrials; i++)
    {
      R.random(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ": first=" << describeElement(R, a)
                   << ", next=" << describeElement(R, b));
      EXPECT_TRUE(R.is_equal(b, b));
      if (not R.is_equal(a, b)) sawDistinct = true;
    }
  EXPECT_TRUE(sawDistinct);
}

TEST_F(ARingZZGMP, finiteFieldContracts)
{
  // The shared reciprocal and power helpers assume a finite field.
  GTEST_SKIP() << "ZZ is not a field; Arithmetic and Powers cover its "
                  "integer-specific operations";
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
