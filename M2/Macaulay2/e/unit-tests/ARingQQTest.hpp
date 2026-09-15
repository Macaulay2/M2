#ifndef M2_UNITTESTS__ARING_QQ_TEST_HPP__
#  define M2_UNITTESTS__ARING_QQ_TEST_HPP__

// Everything the two QQ implementations have in common.  Each .cpp
// specializes getElement<> and then instantiates this suite.

#  include <gtest/gtest.h>
#  include <gmp.h>

#  include <initializer_list>
#  include <string>

#  include "unit-tests/ARingTest.hpp"

#  include "basic-rings/aring-glue.hpp"
#  include "rings/ZZ.hpp"

namespace {

template <typename T>
class ARingQQ : public ::testing::Test
{
 protected:
  // Aliased to R in each test: a member of a dependent base needs this->
  T mRing;

  std::string textOut(const typename T::ElementType& a,
                      bool p_one,
                      bool p_plus,
                      bool p_parens)
  {
    buffer o;
    mRing.elem_text_out(o, a, p_one, p_plus, p_parens);
    return std::string(o.str());
  }

  // result = num/den
  void setFraction(typename T::ElementType& result, int num, int den)
  {
    mpq_t q;
    mpq_init(q);
    mpq_set_si(q, num, den);
    mpq_canonicalize(q);
    EXPECT_TRUE(mRing.set(result, q));
    mpq_clear(q);
  }
};

TYPED_TEST_SUITE_P(ARingQQ);

TYPED_TEST_P(ARingQQ, Storage)
{
  // A coefficient ring has no polynomial variables; each placeholder is 1.
  testCoefficientRingSetVar(this->mRing);
}

TYPED_TEST_P(ARingQQ, Construction)
{
  // QQ has characteristic zero and unbounded cardinality; backend names are
  // checked separately.

  const TypeParam& R = this->mRing;
  EXPECT_EQ(R.cardinality(), static_cast<size_t>(-1));
  EXPECT_EQ(R.characteristic(), static_cast<size_t>(0));
}

TYPED_TEST_P(ARingQQ, Comparisons)
{
  // Zero, units and equal hashes must agree with rational arithmetic.

  {
    // Zero and nonzero integers separate nonunits; a fraction exercises
    // denominator handling.
    SCOPED_TRACE("is_unit: QQ is a field, so every nonzero element is a unit");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R);

    // QQ is a field: everything but zero is a unit
    R.set_zero(a);
    EXPECT_TRUE(R.is_zero(a));
    EXPECT_FALSE(R.is_unit(a));
    for (int i : {-1000000, -1, 1, 1000000})
      {
        SCOPED_TRACE(i);
        R.set(a, i);
        EXPECT_FALSE(R.is_zero(a));
        EXPECT_TRUE(R.is_unit(a));
      }
    this->setFraction(a, 3, 4);
    EXPECT_TRUE(R.is_unit(a));
  }

  {
    // A unit numerator alone is insufficient: the denominator must also be one.
    SCOPED_TRACE("is_pm_one: numerator +/-1 and denominator 1");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R);

    R.set(a, 1);
    EXPECT_TRUE(R.is_pm_one(a));
    R.set(a, -1);
    EXPECT_TRUE(R.is_pm_one(a));

    R.set_zero(a);
    EXPECT_FALSE(R.is_pm_one(a));
    R.set(a, 2);
    EXPECT_FALSE(R.is_pm_one(a));
    this->setFraction(a, 1, 2);  // numerator 1, but denominator is not
    EXPECT_FALSE(R.is_pm_one(a));
    this->setFraction(a, -1, 3);
    EXPECT_FALSE(R.is_pm_one(a));
  }

  {
    // Equivalent reduced fractions must share their hash.
    SCOPED_TRACE("computeHashValue: equal values hash equally");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R), b(R);

    this->setFraction(a, 3, 4);
    this->setFraction(b, 6, 8);  // canonicalizes to 3/4
    EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));
  }
}

TYPED_TEST_P(ARingQQ, Conversions)
{
  // Integer and fraction conversions preserve value without depending on
  // storage size.

  {
    // Cover machine integers, a heap integer and reduced fractions.
    SCOPED_TRACE("set: machine integers, heap integer and reduced fractions");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R), expected(R);

    // from an int and from a long
    R.set(a, 17);
    R.set(expected, 17L);
    EXPECT_TRUE(R.is_equal(a, expected));
    EXPECT_EQ(this->textOut(a, true, false, false), "17");

    // from mpz
    mpz_t m;
    mpz_init(m);
    mpz_set_str(m, "123456789012345678901234567890", 10);
    R.set(a, m);
    EXPECT_EQ(this->textOut(a, true, false, false),
              "123456789012345678901234567890");
    mpz_clear(m);

    // from mpq, which always succeeds
    mpq_t q;
    mpq_init(q);
    mpq_set_si(q, 22, 7);
    mpq_canonicalize(q);
    EXPECT_TRUE(R.set(a, q));
    EXPECT_EQ(this->textOut(a, true, false, false), "22/7");

    // Supply a canonical fraction reduced from 4/6; callers own normalization.
    mpq_set_si(q, 4, 6);
    mpq_canonicalize(q);
    EXPECT_TRUE(R.set(a, q));
    EXPECT_EQ(this->textOut(a, true, false, false), "2/3");
    mpq_clear(q);
  }
}

TYPED_TEST_P(ARingQQ, Arithmetic)
{
  // Rational reciprocals and quotients have exact known answers.

  {
    // Both signs have exact reciprocals; zero must report failure and clear the
    // destination.
    SCOPED_TRACE("invert: signed fractions and zero");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R), b(R), expected(R);

    this->setFraction(a, 3, 4);
    EXPECT_TRUE(R.invert(b, a));
    this->setFraction(expected, 4, 3);
    EXPECT_TRUE(R.is_equal(b, expected));

    this->setFraction(a, -3, 4);
    EXPECT_TRUE(R.invert(b, a));
    this->setFraction(expected, -4, 3);
    EXPECT_TRUE(R.is_equal(b, expected));

    // inverting zero fails and zeroes the result
    R.set(b, 17);
    R.set_zero(a);
    EXPECT_FALSE(R.invert(b, a));
    EXPECT_TRUE(R.is_zero(b));
  }

  {
    // Rational division accepts nonintegral quotients but rejects division by
    // zero.
    SCOPED_TRACE("divide: need not be exact, and rejects a zero divisor");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R), b(R), c(R), expected(R);

    this->setFraction(a, 3, 4);
    this->setFraction(b, 2, 5);
    R.divide(c, a, b);
    this->setFraction(expected, 15, 8);
    EXPECT_TRUE(R.is_equal(c, expected));

    // unlike ZZ, division need not be exact
    R.set(a, 2);
    R.set(b, 3);
    R.divide(c, a, b);
    this->setFraction(expected, 2, 3);
    EXPECT_TRUE(R.is_equal(c, expected));

    R.set(a, 2);
    R.set_zero(b);
    EXPECT_THROW(R.divide(c, a, b), exc::division_by_zero_error);
  }
}

TYPED_TEST_P(ARingQQ, Powers)
{
  // Known rational powers agree across exponent interfaces, including
  // reciprocals.

  const TypeParam& R = this->mRing;
  typename TypeParam::Element a(R), b(R), c(R), one(R), expected(R);
  R.set(one, 1);

  this->setFraction(a, 2, 3);
  R.power(b, a, 3);
  this->setFraction(expected, 8, 27);
  EXPECT_TRUE(R.is_equal(b, expected));

  // Negative exponents invert the same explicitly chosen base.
  this->setFraction(a, 2, 3);
  R.power(b, a, -3);
  this->setFraction(expected, 27, 8);
  EXPECT_TRUE(R.is_equal(b, expected));

  // a^-n * a^n == 1
  for (int n = 1; n <= 8; n++)
    {
      SCOPED_TRACE(::testing::Message() << "power: reciprocal exponents " << n);
      this->setFraction(a, 2, 3);
      R.power(b, a, n);
      R.power(c, a, -n);
      R.mult(b, b, c);
      EXPECT_TRUE(R.is_equal(b, one));
    }

  // power_mpz routes through power, negative exponents included
  mpz_t n;
  mpz_init(n);
  mpz_set_si(n, -3);
  this->setFraction(a, 2, 3);
  R.power_mpz(b, a, n);
  this->setFraction(expected, 27, 8);
  EXPECT_TRUE(R.is_equal(b, expected));
  mpz_clear(n);
}

TYPED_TEST_P(ARingQQ, Syzygy)
{
  // The cancelling pair is (1, -a/b) for nonzero b, including a zero numerator.

  const TypeParam& R = this->mRing;
  typename TypeParam::Element a(R), b(R), x(R), y(R), expected(R);

  // over a field the syzygy is always (1, -a/b)
  this->setFraction(a, 2, 3);
  this->setFraction(b, 4, 5);
  R.syzygy(a, b, x, y);
  R.set(expected, 1);
  EXPECT_TRUE(R.is_equal(x, expected));
  this->setFraction(expected, -5, 6);
  EXPECT_TRUE(R.is_equal(y, expected));

  // A zero numerator still uses a nonzero divisor.
  this->setFraction(b, 4, 5);
  R.set_zero(a);
  R.syzygy(a, b, x, y);
  R.set(expected, 1);
  EXPECT_TRUE(R.is_equal(x, expected));
  EXPECT_TRUE(R.is_zero(y));
}

TYPED_TEST_P(ARingQQ, Formatting)
{
  // Signs, omitted unit digits and large output buffers preserve rational text.

  {
    // The table varies sign and unit flags; parentheses do not affect rational
    // text.
    SCOPED_TRACE("elem_text_out: p_one, p_plus and p_parens");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R);

    struct
    {
      const char* name;
      int num;
      int den;
      bool p_one;
      bool p_plus;
      const char* expected;
    } cases[] = {
        {"zero", 0, 1, true, false, "0"},
        {"zero, p_plus", 0, 1, true, true, "+0"},
        {"one", 1, 1, true, false, "1"},
        {"one, p_one false", 1, 1, false, false, ""},
        {"one, p_plus", 1, 1, true, true, "+1"},
        {"minus one", -1, 1, true, false, "-1"},
        {"minus one, p_one false", -1, 1, false, false, "-"},
        {"minus one, p_plus", -1, 1, true, true, "-1"},
        {"integer", 42, 1, true, false, "42"},
        {"integer, p_plus", 42, 1, true, true, "+42"},
        {"negative integer", -42, 1, true, false, "-42"},
        {"fraction", 3, 4, true, false, "3/4"},
        {"fraction, p_plus", 3, 4, true, true, "+3/4"},
        {"negative fraction", -3, 4, true, false, "-3/4"},
        {"negative fraction, p_plus", -3, 4, true, true, "-3/4"},
        // p_one only suppresses a bare 1, never a fraction with numerator 1
        {"one over two, p_one false", 1, 2, false, false, "1/2"},
    };

    for (auto& c : cases)
      {
        SCOPED_TRACE(c.name);
        this->setFraction(a, c.num, c.den);
        EXPECT_EQ(this->textOut(a, c.p_one, c.p_plus, false), c.expected);
        // p_parens is ignored
        EXPECT_EQ(this->textOut(a, c.p_one, c.p_plus, true), c.expected);
      }
  }

  {
    // More than 1000 characters forces the heap-buffer path.
    SCOPED_TRACE("elem_text_out: an element too large for the stack buffer");

    const TypeParam& R = this->mRing;
    typename TypeParam::Element a(R);

    mpq_t q;
    mpq_init(q);
    mpz_ui_pow_ui(mpq_numref(q), 10, 900);
    mpz_ui_pow_ui(mpq_denref(q), 7, 900);
    mpq_canonicalize(q);
    EXPECT_TRUE(R.set(a, q));

    char* expected = mpq_get_str(nullptr, 10, q);
    EXPECT_EQ(this->textOut(a, true, false, false), std::string(expected));
    mpq_clear(q);
  }
}

TYPED_TEST_P(ARingQQ, Evaluation)
{
  // Fractions evaluate into QQ, while only integral values evaluate into ZZ.

  const TypeParam& R = this->mRing;
  typename TypeParam::Element a(R);
  const RingMap* toQQ = identityRingMap(globalQQ);
  const RingMap* toZZ = identityRingMap(globalZZ);
  ring_elem result;

  // a rational maps into QQ unchanged
  this->setFraction(a, 3, 4);
  R.eval(toQQ, a, 0, result);
  ring_elem expected;
  mpq_t q;
  mpq_init(q);
  mpq_set_si(q, 3, 4);
  mpq_canonicalize(q);
  EXPECT_TRUE(globalQQ->from_rational(q, expected));
  EXPECT_TRUE(globalQQ->is_equal(result, expected));
  mpq_clear(q);

  // an integer-valued rational maps into ZZ
  R.set(a, 12);
  R.eval(toZZ, a, 0, result);
  EXPECT_TRUE(globalZZ->is_equal(result, globalZZ->from_long(12)));

  // a genuine fraction does not
  this->setFraction(a, 3, 4);
  EXPECT_THROW(R.eval(toZZ, a, 0, result), exc::engine_error);
}

TYPED_TEST_P(ARingQQ, RandomizedProperties)
{
  // Generated elements satisfy the common ARing contracts and survive copying.

  const TypeParam& R = this->mRing;
  typename TypeParam::Element a(R), b(R);

  seedRandom(0x5151);
  SCOPED_TRACE("seed 0x5151");
  testStorage(R, ntrials);
  testComparisons(R, ntrials);
  testRingElemRoundTrip(R, ntrials);
  testCoercions(R);
  testNegate(R, ntrials);
  testAdd(R, ntrials);
  testSubtract(R, ntrials);
  testMultiply(R, ntrials);
  testDivide(R, ntrials);
  testReciprocal(R, ntrials);
  testAxioms(R, ntrials);
  testAliasing(R, ntrials);
  testSubtractMultiple(R, ntrials);
  testPowerAgreement(R, ntrials / 10);
  testPowerMpzOutOfRange(R);
  testSyzygy(R, ntrials);

  // Independent draws must be well formed and need not all coincide.
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

TYPED_TEST_P(ARingQQ, finiteFieldPowerContract)
{
  // The shared testPower helper assumes finite cardinality.
  GTEST_SKIP() << "QQ is infinite; Powers and RandomizedProperties exercise "
                  "rational powers";
}

REGISTER_TYPED_TEST_SUITE_P(ARingQQ,
                            Construction,
                            Storage,
                            Comparisons,
                            Conversions,
                            Arithmetic,
                            Powers,
                            Syzygy,
                            Formatting,
                            Evaluation,
                            RandomizedProperties,
                            finiteFieldPowerContract);

}  // namespace

#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
