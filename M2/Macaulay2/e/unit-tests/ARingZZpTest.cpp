// Copyright 2012-2013 Michael E. Stillman

#include "basic-rings/aring-ZZp.hpp"

#include <gtest/gtest.h>
#include <gmp.h>

#include <cctype>
#include <cstddef>
#include <iostream>
#include <memory>
#include <sstream>
#include <initializer_list>
#include <string>

#include "basic-rings/aring-ZZp-ffpack.hpp"
#include "basic-rings/aring-ZZp-flint.hpp"
#include "basic-rings/reader.hpp"
#include "rings/ZZp.hpp"
#include "unit-tests/ARingTest.hpp"

static bool maxH_initialized = false;
static mpz_t maxH;

gmp_ZZ getRandomInteger()
{
  if (!maxH_initialized)
    {
      maxH_initialized = true;
      mpz_init(maxH);
      mpz_set_str(maxH, "100000000000", 10);
    }
  return rawRandomInteger(maxH);
}

template <>
void getElement<M2::ARingZZp>(const M2::ARingZZp& R,
                              int index,
                              M2::ARingZZp::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set(result, a);
    }
}

template <>
void getElement<M2::ARingZZpFFPACK>(const M2::ARingZZpFFPACK& R,
                                    int index,
                                    M2::ARingZZpFFPACK::ElementType& result)
{
  if (index < 50)
    R.set(result, index - 25);
  else
    {
      gmp_ZZ a = getRandomInteger();
      R.set(result, a);
    }
}

template <>
void getElement<M2::ARingZZpFlint>(const M2::ARingZZpFlint& R,
                                   int index,
                                   M2::ARingZZpFlint::ElementType& result)
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

// Check both ends of 0..p-1 using unsigned arithmetic; p may exceed 2^63.
// Reach the upper end through negative representatives that fit in long.
template <typename RT>
void testCoerceToLongInteger(const RT& R)
{
  typedef unsigned long ulong;
  const ulong charac = static_cast<ulong>(R.characteristic());
  const ulong window = 10000;
  const ulong top = (charac > window ? window : charac);

  typename RT::ElementType a;
  R.init(a);

  // bottom of the range: 0, 1, 2, ...
  for (ulong i = 0; i < top; i++)
    {
      SCOPED_TRACE(::testing::Message() << "lower residue " << i);
      R.set(a, static_cast<long>(i));
      long b = R.coerceToLongInteger(a);
      ulong c =
          (b < 0 ? static_cast<ulong>(b) + charac : static_cast<ulong>(b));
      EXPECT_EQ(c, i);
    }

  // top of the range: p-1, p-2, ... reached as -1, -2, ...
  for (ulong k = 1; k <= window && k <= charac; k++)
    {
      SCOPED_TRACE(::testing::Message() << "upper residue p-" << k);
      R.set(a, -static_cast<long>(k));
      long b = R.coerceToLongInteger(a);
      ulong c =
          (b < 0 ? static_cast<ulong>(b) + charac : static_cast<ulong>(b));
      EXPECT_EQ(c, charac - k);
    }

  R.clear(a);
}

TEST(RingZZp, create)
{
  // The ring reports the chosen prime and preserves basic field values.

  const Z_mod* R = Z_mod::create(101);
  ASSERT_NE(R, nullptr);
  EXPECT_EQ(R->characteristic(), 101);
}

TEST(ARingZZp, create)
{
  // The ring reports the chosen prime and preserves basic field values.

  M2::ARingZZp R(101);

  M2::ARingZZp::ElementType a;
  buffer o;

  R.init(a);

  EXPECT_EQ(ringName(R), "AZZ/101");
  EXPECT_EQ(R.cardinality(), 101);
  EXPECT_EQ(R.characteristic(), 101);
  // The variable placeholder must be a nonzero field generator.
  R.set_var(a, 0);
  R.elem_text_out(o, a, true, true, false);
  EXPECT_FALSE(R.is_zero(a));
  EXPECT_FALSE(std::string(o.str()).empty());
  R.clear(a);
}

TEST(ARingZZp, fromStream)
{
  // Parsing signed coefficients stops at the first nonnumeric token.

  std::istringstream i("+1234 +345 -235*a");
  M2::ARingZZp R(32003);
  M2::ARingZZp::Element a(R);
  const long expected[] = {1234, 345, -235};
  int n = 0;
  while (true)
    {
      while (isspace(i.peek())) i.get();

      if (!isdigit(i.peek()) && i.peek() != '+' && i.peek() != '-') break;

      fromStream(i, R, a);

      ASSERT_LT(n, 3);
      EXPECT_EQ(R.coerceToLongInteger(a), expected[n]);
      n++;
    }
  EXPECT_EQ(n, 3);
  EXPECT_EQ(i.peek(), '*');  // parsing stops at the '*' before the variable
}

TEST(ARingZZpFFPACK, create)
{
  // The ring reports the chosen prime and preserves basic field values.

  M2::ARingZZpFFPACK R(101);

  EXPECT_EQ(ringName(R), "ZZpFPACK(101,1)");
  testSomeMore(R);

  M2::ARingZZpFFPACK::ElementType a;
  R.init(a);
  R.set(a, 99);
  EXPECT_EQ(R.coerceToLongInteger(a), -2);  // 99 == -2 mod 101
  R.set(a, 101);
  EXPECT_TRUE(R.is_zero(a));
  R.set(a, 103);
  EXPECT_EQ(R.coerceToLongInteger(a), 2);
  R.clear(a);
}

TEST(ARingZZp, read)
{
  // A large signed coefficient reduces modulo p without consuming the variable
  // token.

  std::string a = "-42378489327498312749c3";
  std::istringstream i(a);

  M2::ARingZZp R(101);
  M2::Reader<M2::ARingZZp> reader(R);
  M2::ARingZZp::ElementType b, c;
  R.init(b);
  R.init(c);
  reader.read(i, b);
  R.set(c, 3);

  EXPECT_TRUE(R.is_equal(b, c));
  EXPECT_EQ(i.peek(), 'c');
  R.clear(b);
  R.clear(c);
}

////////////////////////////
// Flint ZZ/p arithmetic ///
////////////////////////////

TEST(ARingZZpFlint, create)
{
  // The ring reports the chosen prime and preserves basic field values.

  M2::ARingZZpFlint R(101);

  EXPECT_EQ(ringName(R), "AZZFlint/101");
  testSomeMore(R);

  M2::ARingZZpFlint::ElementType a;
  R.init(a);
  R.set(a, 99);
  EXPECT_EQ(R.coerceToLongInteger(a), -2);
  R.set(a, 101);
  EXPECT_TRUE(R.is_zero(a));
  R.set(a, 103);
  EXPECT_EQ(R.coerceToLongInteger(a), 2);
  R.clear(a);
}

// Run the shared field contract for each backend and supported modulus.
template <typename RT>
struct ARingFactory;

template <>
struct ARingFactory<M2::ARingZZp>
{
  static const char* name() { return "ARingZZp"; }
  // two newarray_atomic(int, p) tables and an O(p^2) primitive-root search
  static const char* limit() { return "table size; p <= 32749"; }
  static bool supports(unsigned long p) { return p <= 32749; }
  static std::unique_ptr<M2::ARingZZp> make(unsigned long p)
  {
    return std::unique_ptr<M2::ARingZZp>(new M2::ARingZZp(p));
  }
};

template <>
struct ARingFactory<M2::ARingZZpFFPACK>
{
  static const char* name() { return "ARingZZpFFPACK"; }
  // Givaro::Modular<double> needs p(p-1) exact in a 53-bit mantissa.  This is
  // the backend limit, not the smaller value getMaxModulus() advertises.
  static const char* limit() { return "double mantissa; p <= 94906266"; }
  static bool supports(unsigned long p)
  {
    return p <= static_cast<unsigned long>(
                    M2::ARingZZpFFPACK::FieldType::maxCardinality());
  }
  static std::unique_ptr<M2::ARingZZpFFPACK> make(unsigned long p)
  {
    return std::unique_ptr<M2::ARingZZpFFPACK>(
        new M2::ARingZZpFFPACK(static_cast<M2::ARingZZpFFPACK::UTT>(p)));
  }
};

template <>
struct ARingFactory<M2::ARingZZpFlint>
{
  static const char* name() { return "ARingZZpFlint"; }
  // flint nmod takes the whole unsigned 64-bit range.  Note that the generic
  // Ring interface holds the characteristic in a signed long, so above 2^63 a
  // ring reports a negative characteristic; the ARing class itself is fine.
  static const char* limit() { return "none below 2^64"; }
  static bool supports(unsigned long) { return true; }
  static std::unique_ptr<M2::ARingZZpFlint> make(unsigned long p)
  {
    return std::unique_ptr<M2::ARingZZpFlint>(new M2::ARingZZpFlint(p));
  }
};

struct ModulusCase
{
  unsigned long p;
  const char* why;
};

static const ModulusCase zzpModuli[] = {
    {2UL, "smallest prime; char 2 was long suspected of failing for ffpack"},
    {3UL, "smallest odd prime"},
    {101UL, "small generic"},
    {32749UL, "largest prime ARingZZp accepts"},
    {32771UL, "first prime above the ffpack getMaxModulus() stub"},
    {33500479UL, "historical ffpack/flint case"},
    {66000007UL, "historical ffpack/flint case"},
    {67108859UL, "historical ffpack/flint case"},
    {94906249UL, "largest prime at or below Givaro's real ceiling"},
    {2147483647UL, "largest prime < 2^31"},
    {9223372036854775783UL, "largest prime < 2^63"},
    {18446744073709551557UL, "largest prime < 2^64"},
};

template <typename RT>
class ZZpRing : public ::testing::Test
{
};

typedef ::testing::Types<M2::ARingZZp, M2::ARingZZpFFPACK, M2::ARingZZpFlint>
    ZZpTypes;
TYPED_TEST_SUITE(ZZpRing, ZZpTypes);

TYPED_TEST(ZZpRing, arithmetic)
{
  // Each supported backend/modulus pair satisfies the shared finite-field
  // contract.

  seedRandom(0x5a70);
  SCOPED_TRACE("seed 0x5a70");

  typedef ARingFactory<TypeParam> F;
  int ran = 0;
  for (const ModulusCase& m : zzpModuli)
    {
      if (sizeof(unsigned long) <= 4 && m.p > 0xffffffffUL)
        {
          std::cout << "  skipping modulus requiring a 64-bit unsigned long"
                    << std::endl;
          continue;
        }
      if (!F::supports(m.p))
        {
          std::cout << "  skipping " << F::name() << " p=" << m.p << " ("
                    << F::limit() << ")" << std::endl;
          continue;
        }
      SCOPED_TRACE(std::string(F::name()) + " p=" + std::to_string(m.p) + " (" +
                   m.why + ")");
      auto R = F::make(m.p);
      testFiniteField(*R, ntrials);
      testCoerceToLongInteger(*R);
      ran++;
    }
  EXPECT_GT(ran, 0);
}

// Backend range limits and their interface implications are in
// README.anton-dima.

// Multiplicative order of a mod p, computed directly.
static long multiplicativeOrder(long a, long p)
{
  long x = a % p;
  for (long k = 1; k < p; k++)
    {
      if (x == 1) return k;
      x = (x * a) % p;
    }
  return -1;
}

// The constructor silently narrows size_t to int. Disabled until it rejects
// unsupported moduli before narrowing or allocating tables.
// https://github.com/Macaulay2/M2/issues/4698
TEST(ARingZZp, DISABLED_constructorRejectsModulusAboveIntMax)
{
  // An unsupported modulus must be rejected before narrowing or allocating
  // tables.
  if (sizeof(size_t) <= 4) GTEST_SKIP() << "requires a 64-bit size_t";
  const size_t modulus = (size_t(1) << 32) + 101;

  EXPECT_THROW({ M2::ARingZZp R(modulus); }, exc::engine_error);
}

TEST(ARingZZp, findPrimitiveRoot)
{
  // p == 2 is special-cased to 1 (there is no element of order 1 otherwise).
  EXPECT_EQ(M2::ARingZZp::findPrimitiveRoot(2), 1);

  for (long p : {3L, 5L, 7L, 101L, 32749L})
    {
      SCOPED_TRACE(::testing::Message() << "primitive root: p=" << p);
      long g = M2::ARingZZp::findPrimitiveRoot(static_cast<int>(p));
      EXPECT_GT(g, 1);
      EXPECT_LT(g, p);
      // a primitive root is exactly an element of order p-1
      EXPECT_EQ(multiplicativeOrder(g, p), p - 1);
    }
}

TEST(ARingZZpFFPACK, advertisedMaxModulusIsBelowTheRealOne)
{
  // The advertised ceiling is below Givaro's supported ceiling; pin their
  // relationship. Backend limits and the historical stub are explained in
  // README.anton-dima.
  EXPECT_LT(
      static_cast<double>(M2::ARingZZpFFPACK::getMaxModulus()),
      static_cast<double>(M2::ARingZZpFFPACK::FieldType::maxCardinality()));
}

TEST(ARingZZpFFPACK, backendWorksFarAboveAdvertisedMaxModulus)
{
  // Concrete evidence for the test above: the class handles moduli far beyond
  // what getMaxModulus() advertises.  32771 is the first prime above the stub;
  // 94906249 is the largest prime at or below Givaro's real ceiling.
  seedRandom(0x5a70);
  SCOPED_TRACE("seed 0x5a70");

  for (unsigned long p : {32771UL, 33500479UL, 66000007UL, 94906249UL})
    {
      SCOPED_TRACE(::testing::Message() << "backend modulus " << p);
      M2::ARingZZpFFPACK R(static_cast<M2::ARingZZpFFPACK::UTT>(p));
      EXPECT_EQ(R.characteristic(), p);
      testCoerceToLongInteger(R);
      testMultiply(R, 100);
      testDivide(R, 100);
      testReciprocal(R, 100);
    }
}

TEST(ARingZZpFFPACK, generator)
{
  // The chosen generator is nonzero and has full multiplicative order.

  M2::ARingZZpFFPACK R(101);
  M2::ARingZZpFFPACK::ElementType g = R.getGenerator();
  EXPECT_FALSE(R.is_zero(g));
  EXPECT_EQ(multiplicativeOrder(R.coerceToLongInteger(g) < 0
                                    ? R.coerceToLongInteger(g) + 101
                                    : R.coerceToLongInteger(g),
                                101),
            100);

  // set_var is the only caller of getGenerator inside the class.
  M2::ARingZZpFFPACK::ElementType a;
  R.init(a);
  R.set_var(a, 0);
  EXPECT_TRUE(R.is_equal(a, g));
  R.clear(a);
}

TEST(ARingZZpFlint, generatorAndDiscreteLog)
{
  // The generator has order p-1 and discreteLog inverts all its powers.

  const long p = 101;
  M2::ARingZZpFlint R(p);

  M2::ARingZZpFlint::ElementType g, a, one;
  R.init(g);
  R.init(a);
  R.init(one);
  R.set(one, 1);
  R.getGenerator(g);

  EXPECT_FALSE(R.is_zero(g));
  EXPECT_EQ(multiplicativeOrder(R.coerceToLongInteger(g) < 0
                                    ? R.coerceToLongInteger(g) + p
                                    : R.coerceToLongInteger(g),
                                p),
            p - 1);

  // discreteLog inverts power on the generator, over a full period
  for (int k = 0; k < p - 1; k++)
    {
      SCOPED_TRACE(::testing::Message() << "discreteLog: exponent " << k);
      R.power(a, g, k);
      EXPECT_EQ(R.discreteLog(a), k);
    }

  // g^(p-1) == 1
  R.power(a, g, static_cast<int>(p - 1));
  EXPECT_TRUE(R.is_equal(a, one));

  // documented special case
  R.set_zero(a);
  EXPECT_EQ(R.discreteLog(a), -1);

  R.clear(g);
  R.clear(a);
  R.clear(one);
}

TEST(ARingZZpFlint, coerceToLongIntegerNear2to64)
{
  // Balanced representatives remain correct near the largest 64-bit prime.

  if (sizeof(unsigned long) <= 4)
    GTEST_SKIP() << "requires a 64-bit unsigned long";

  // Balanced representatives fit in long even though the modulus exceeds 2^63.
  const size_t p = 18446744073709551557UL;  // largest prime < 2^64
  M2::ARingZZpFlint R(p);

  // spot-check both sides of the p/2 boundary, where the wraparound happens
  const size_t vals[] = {0, 1, 2, p / 2 - 1, p / 2, p / 2 + 1, p - 2, p - 1};
  for (size_t f : vals)
    {
      SCOPED_TRACE(::testing::Message() << "balanced representative: " << f);
      M2::ARingZZpFlint::ElementType a;
      R.init(a);
      a = static_cast<M2::ARingZZpFlint::ElementType>(f);
      long expected =
          (f > p / 2) ? static_cast<long>(f - p) : static_cast<long>(f);
      EXPECT_EQ(R.coerceToLongInteger(a), expected);
      R.clear(a);
    }

  // Check both ends of the range as well as the midpoint cases above.
  testCoerceToLongInteger(R);
}

TYPED_TEST(ZZpRing, elementOperations)
{
  // Generated values check comparisons, equal hashes, valid products and
  // independent swaps.

  seedRandom(0x5a70);
  SCOPED_TRACE("seed 0x5a70");

  auto Rp = ARingFactory<TypeParam>::make(101);
  const TypeParam& R = *Rp;

  ARingElementGenerator<TypeParam> gen(R);
  typename TypeParam::ElementType a, b, c, d, e;
  R.init(a);
  R.init(b);
  R.init(c);
  R.init(d);
  R.init(e);

  for (int i = 0; i < 200; i++)
    {
      gen.nextElement(a);
      gen.nextElement(b);
      SCOPED_TRACE(::testing::Message()
                   << "trial " << i << ": a=" << describeElement(R, a)
                   << ", b=" << describeElement(R, b));

      // compare_elems agrees with is_equal and is antisymmetric
      int cmp = R.compare_elems(a, b);
      EXPECT_EQ(cmp == 0, R.is_equal(a, b));
      EXPECT_EQ(cmp, -R.compare_elems(b, a));

      // equal elements hash equally
      if (R.is_equal(a, b))
        EXPECT_EQ(R.computeHashValue(a), R.computeHashValue(b));

      // ARingZZp requires both factors nonzero; other backends also accept
      // zero.
      if (!R.is_zero(a) && !R.is_zero(b))
        {
          SCOPED_TRACE("subtract_multiple: both factors nonzero");
          R.set(c, 7);
          R.subtract_multiple(c, a, b);
          R.set(e, 7);
          R.mult(d, a, b);
          R.subtract(e, e, d);
          EXPECT_TRUE(R.is_equal(c, e));
        }

      // swap.  init_set, not set: for an integral ElementType set(c, a) binds
      // to the integer-coercion overload instead of copying.
      typename TypeParam::Element left(R, a), right(R, b);
      R.swap(left, right);
      EXPECT_TRUE(R.is_equal(left, b));
      EXPECT_TRUE(R.is_equal(right, a));
    }

  R.clear(a);
  R.clear(b);
  R.clear(c);
  R.clear(d);
  R.clear(e);
}

TEST(ARingZZp, ringElemRoundTrip)
{
  // Round trips preserve every field value, including the swapped encodings of
  // 0 and 1.
  const int p = 101;
  M2::ARingZZp R(p);
  M2::ARingZZp::ElementType a, b;
  R.init(a);
  R.init(b);

  for (int i = 0; i < p; i++)
    {
      SCOPED_TRACE(::testing::Message() << "residue " << i);
      R.set(a, i);
      ring_elem r;
      R.to_ring_elem(r, a);
      R.from_ring_elem(b, r);
      EXPECT_TRUE(R.is_equal(a, b));
      EXPECT_TRUE(R.is_equal(R.from_ring_elem_const(r), a));
    }

  // the swap itself: the field's 0 and 1 are the two elements that move
  R.set_zero(a);
  ring_elem r;
  R.to_ring_elem(r, a);
  EXPECT_EQ(r.get_int(), p - 1);

  R.set(a, 1);
  R.to_ring_elem(r, a);
  EXPECT_EQ(r.get_int(), 0);

  R.clear(a);
  R.clear(b);
}

// Zero factors violate the current NONZERO precondition and give a wrong
// result. This regression is disabled until the contract and implementation
// support zero. https://github.com/Macaulay2/M2/issues/4699
TEST(ARingZZp, DISABLED_subtractMultipleByZero)
{
  // Extending the contract must make either zero factor preserve the
  // accumulator.
  M2::ARingZZp R(101);
  struct ProductCase
  {
    const char* name;
    int left, right;
  };
  const ProductCase cases[] = {{"zero left factor", 0, 5},
                               {"zero right factor", 5, 0}};
  for (const auto& sample : cases)
    {
      SCOPED_TRACE(sample.name);
      M2::ARingZZp::Element a(R), b(R), result(R);
      R.set(a, sample.left);
      R.set(b, sample.right);
      R.set(result, 7);

      R.subtract_multiple(result, a, b);

      EXPECT_EQ(R.coerceToNonnegativeLongInteger(result), 7);
    }
}

TEST(ARingZZp, coerceToNonnegativeLongInteger)
{
  // All residues return their canonical representative in 0..p-1.

  const int p = 101;
  M2::ARingZZp R(p);
  M2::ARingZZp::ElementType a;
  R.init(a);
  for (int i = 0; i < p; i++)
    {
      SCOPED_TRACE(::testing::Message() << "residue " << i);
      R.set(a, i);
      EXPECT_EQ(R.coerceToNonnegativeLongInteger(a), i);
    }
  R.clear(a);
}

}  // namespace

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
