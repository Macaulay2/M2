// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"
#include "mathicgb/MonoMonoid.hpp"

#include "mathicgb/MonoArena.hpp"
#include "mathicgb/MathicIO.hpp"
#include <gtest/gtest.h>
#include <sstream>

using namespace mgb;

// Do all-pairs testing -- see monoidPict.in. Especially see that file before
// editing this list of types.
typedef ::testing::Types<
  MonoMonoid<int32,1,1,1>,
  MonoMonoid<int32,0,1,1>,
  MonoMonoid<int32,0,0,1>,
  MonoMonoid<int32,0,0,0>,
  MonoMonoid<int16,1,1,1>,
  MonoMonoid<int16,0,1,1>,
  MonoMonoid<int16,0,0,1>,
  MonoMonoid<int16,0,0,0>,
  MonoMonoid<int8,1,0,1>,
  MonoMonoid<int8,0,1,0>,
  MonoMonoid<int32,1,1,0>
> MonoidTypes;

template <typename T>
class Monoids : public ::testing::Test {};
TYPED_TEST_SUITE(Monoids, MonoidTypes);

// expect(i,j) encodes a matrix with interesting bit patterns that
// are supposed to be likely to surface errors in how monomials are
// stored inside a vector.
uint32 expect(size_t mono, size_t var, size_t varCount) {
  const auto unique = (static_cast<uint32>(var + varCount * mono + 1) % 127);

  while (true) {
    // 000
    if (mono == 0)
      return 0;
    --mono;

    // 100
    // 010
    // 001
    if (mono < varCount)
      return var == mono ? unique : 0;
    mono -= varCount;

    // 000
    // 100
    // 110
    // 111
    if (mono < varCount + 1)
      return var < mono ? unique : 0;
    mono -= varCount + 1;

    // 111
    // 011
    // 001
    // 000
    if (mono < varCount + 1)
      return var >= mono ? unique : 0;
    mono -= varCount + 1;

    // 101
    // 010
    if (mono < 4)
      return (var % 2) == (mono % 2) ? unique : 0;
    mono -= 4;

    // 100100
    // 010010
    // 001001
    if (mono < 6)
      return (var % 3) == (mono % 3) ? unique : 0;
    mono -= 6;

    // mix the patterns
    mono += var % 17;
  }
};

TYPED_TEST(Monoids, VarCount) {
  typedef TypeParam Monoid;
  ASSERT_EQ(0, Monoid(0).varCount());
  ASSERT_EQ(1000 * 1000, Monoid(1000 * 1000).varCount());
  ASSERT_EQ(1, Monoid(1).varCount());
  ASSERT_EQ(2, Monoid(2).varCount());
  ASSERT_EQ(12, Monoid(12).varCount());
}

template<class Monoid, class MonoVector>
void testMonoVector() {
  typedef typename Monoid::VarIndex VarIndex;

  Monoid monoid(13);
  MonoVector v(monoid);
  MonoVector v2(monoid);
  ASSERT_EQ(v2.monoid(), monoid);
  const auto varCount = monoid.varCount();

  ASSERT_TRUE(v.empty());
  size_t count = 1000;


  // Not a correctness error, but empty vectors should preferably not
  // use any memory.
  ASSERT_EQ(0, v.memoryBytesUsed());

  for (size_t i = 0; i < count; ++i) {
    ASSERT_EQ(i, v.size());
    v.push_back(); // push_back, no param
    ASSERT_GT(v.memoryBytesUsed(), 0u);
    ASSERT_FALSE(v.empty()); // empty
    ASSERT_EQ(i + 1, v.size()); // size


    ASSERT_TRUE(monoid.isIdentity(v.back())); // isIdentity true, back non-const
    bool allZero = true;
    for (VarIndex var = 0; var < varCount; ++var) {
      const auto exponent = expect(i, var, varCount);
      if (exponent != 0) {
	allZero = false;
	monoid.setExponent(var, exponent, v.back());
      }
    }
    ASSERT_EQ(allZero, monoid.isIdentity(v.back())); // isIdentity false
    v2.push_back(v.back()); // push_back with param
    ASSERT_TRUE(monoid.equal(v.back(), v2.back()));
  }
  auto it = v.begin();
  ASSERT_EQ(it, v.cbegin());
  for (size_t i = 0; i < count; ++i, ++it) {
    typename MonoVector::const_iterator tmp;
    tmp = it;
    ASSERT_EQ(tmp, it);
    ASSERT_TRUE(v.end() != it);

    for (VarIndex var = 0; var < monoid.varCount(); ++var) {
      ASSERT_EQ(expect(i, var, varCount), monoid.exponent(*it, var));
    }
  }
  ASSERT_EQ(v.end(), it);
  ASSERT_EQ(v.cend(), it);

  ASSERT_EQ(v, v2); // operator== true
  monoid.setExponent(0, 1 + monoid.exponent(v2.back(), 0), v2.back());
  ASSERT_TRUE(v != v2); // operator!=, true, same length

  auto& vc = const_cast<const MonoVector&>(v);
  ASSERT_TRUE(monoid.equal(v.front(), *v2.begin())); // front, non-const
  ASSERT_TRUE(monoid.equal(vc.front(), *v2.begin())); // front, const
  ASSERT_TRUE(monoid.equal(vc.back(), v.back())); // back, non-const

  auto v3(v2); // copy constructor
  ASSERT_EQ(v3.monoid(), monoid);
  ASSERT_TRUE(v != v3 && v2 == v3);
  v2.swap(v); // member swap
  ASSERT_TRUE(v == v3 && v2 != v3);
  std::swap(v, v2); // std::swap
  ASSERT_TRUE(v != v3 && v2 == v3);
  using std::swap;
  swap(v, v2); // let compiler decide which swap to use
  ASSERT_TRUE(v == v3 && v2 != v3);
  swap(v, v2); // get back to original state
  ASSERT_TRUE(v != v3 && v2 == v3);

  ASSERT_FALSE(v3 != v2); // operator!=, false, same length
  v3.push_back();
  ASSERT_TRUE(v3 != v2); // operator!=, true, different length
  

  ASSERT_FALSE(v3 == v);
  v3 = v; // copy assignment
  ASSERT_EQ(v3.monoid(), monoid);
  ASSERT_EQ(v3, v);

  ASSERT_FALSE(v3.empty());
  v2 = std::move(v3); // move assignment
  ASSERT_EQ(v2.monoid(), monoid);
  ASSERT_EQ(v2, v);
  ASSERT_TRUE(v3.empty());

  ASSERT_FALSE(v2.empty());
  auto v4(std::move(v2)); // move constructor
  ASSERT_EQ(v4.monoid(), monoid);
  ASSERT_TRUE(v2.empty());
  ASSERT_EQ(v4, v);

  ASSERT_FALSE(v.empty());
  v.clear();
  ASSERT_TRUE(v.empty());
}

TYPED_TEST(Monoids, MonoVector) {
  typedef TypeParam Monoid;
  typedef typename Monoid::MonoVector MonoVector;

  testMonoVector<Monoid, MonoVector>(); 
}

TYPED_TEST(Monoids, MonoArena) {
  typedef TypeParam Monoid;
  typedef MonoArena<Monoid> Arena;

  Monoid monoid(15);
  Arena a(monoid);
  a.push_back();
  return;

  testMonoVector<Monoid, MonoArena<Monoid>>();
}

TYPED_TEST(Monoids, ReadWriteMonoid) {
  typedef TypeParam Monoid;
  typedef typename Monoid::VarIndex VarIndex;

  const auto check = [](
    const char* const inStr,
    const char* const outStr,
    const VarIndex varCount,
    const VarIndex gradingCount
    ) -> void {
    for (int i = 0; i < 2; ++i) {
      const char* str = i == 0 ? inStr : outStr;
      if (str == 0)
        continue;

      std::istringstream inStream(str);
      Scanner in(inStream);
      const auto order = MathicIO<Monoid>().
        readOrderWithVarCount(Monoid::HasComponent, in);
      const Monoid monoid(order);
      const auto constructedOrder =
        monoid.makeOrder
          (order.componentsAscendingDesired(), order.schreyering());
      std::ostringstream out;
      MathicIO<Monoid>().writeOrderWithVarCount
        (constructedOrder, Monoid::HasComponent, out);
      ASSERT_EQ(outStr, out.str());
      ASSERT_EQ(varCount, order.varCount());
      ASSERT_EQ(varCount, monoid.varCount());
      ASSERT_EQ(varCount, constructedOrder.varCount());
      ASSERT_EQ(gradingCount, monoid.gradingCount());
    }
  };
  check("0 0\n", "0\nrevlex 0\n", 0, 0);
  check("1 1\n 2\n", "1\nrevlex 1\n 2\n", 1, 1);
  check("1 2\n 3\n 4\n", "1\nrevlex 2\n 3\n 4\n", 1, 2);
  check("2 2\n 3 4\n 5 6\n", "2\nrevlex 2\n 3 4\n 5 6\n", 2, 2);
  check("4 1\n 1 1 1 1\n", "4\nrevlex 1\n 1 1 1 1\n", 4, 1);

  check("0 lex 0", "0\nlex 0\n", 0, 0);
  check("1 lex 1 2", "1\nlex 1\n 2\n", 1, 1);
  check("1 lex 2 3 4", "1\nlex 2\n 3\n 4\n", 1, 2);
  check("2 lex 2 3 4 5 6", "2\nlex 2\n 3 4\n 5 6\n", 2, 2);
  check("4 lex 1 1 1 1 1", "4\nlex 1\n 1 1 1 1\n", 4, 1);

  if (Monoid::HasComponent) {
    check("2 2\n component\n 5 6\n", "2\nrevlex 2\n component\n 5 6\n", 2, 2);
    check
      ("2 2\n 3 4\n revcomponent\n","2\nrevlex 2\n 3 4\n revcomponent\n", 2, 2);
    check("0 lex 1 component", "0\nlex 1\n component\n", 0, 1);
    check("1 lex 1 revcomponent", "1\nlex 1\n revcomponent\n", 1, 1);
    check("5 lex 1 revcomponent", "5\nlex 1\n revcomponent\n", 5, 1);
  }
}

TYPED_TEST(Monoids, MonoPool) {
  typedef TypeParam Monoid;
  typedef typename Monoid::VarIndex VarIndex;
  typedef typename Monoid::Mono Mono;

  for (int q = 0; q < 2; ++q) {
    Monoid monoid(13);
    typename Monoid::MonoPool pool(monoid);
    const auto varCount = monoid.varCount();

    const auto count = 1000;
    std::vector<Mono> monos;
    for (int i = 0; i < count; ++i) {
      pool.alloc();
      pool.free(pool.alloc());
      auto m1 = pool.alloc();
      ASSERT_TRUE(monoid.isIdentity(*m1));
      auto m2 = pool.alloc();
      ASSERT_TRUE(monoid.isIdentity(*m2));
      for (VarIndex var = 0; var < varCount; ++var) {
        monoid.setExponent(var, 1, *m1);
        monoid.setExponent(var, 1, *m2);
      }
      if (i > 10) {
        using std::swap;
        swap(m2, monos[i - 10]);
      }
      monos.emplace_back(std::move(m1));
    }
    
    // This ensures that we get to each entry in monos exactly once.
    MATHICGB_ASSERT((count % 17) != 0); 
    int i = 0;
    do {
      MATHICGB_ASSERT(!monos[i].isNull());
      ASSERT_FALSE(monoid.isIdentity(*monos[i]));
      pool.free(std::move(monos[i]));
      ASSERT_TRUE(monos[i].isNull());
      pool.free(std::move(monos[i]));
      ASSERT_TRUE(monos[i].isNull());
      i = (i + 17) % count;
    } while (i != 0);

    // If the ordering of monomials inside the pool has anything to do with
    // allocation and deallocation order, then the monomials inside the
    // pool are at this point all jumbled around. All the entries were also
    // non-zero before, so we test that new allocations are the identity.

    for (int i = 0; i < count; ++i) {
      monos[i] = pool.alloc();
      ASSERT_TRUE(monoid.isIdentity(*monos[i]));
      for (VarIndex var = 0; var < varCount; ++var)
        monoid.setExponent(var, expect(i, var, varCount), *monos[i]);
    }
    for (int i = 0; i < count; ++i) {
      for (VarIndex var = 0; var < varCount; ++var) {
        ASSERT_EQ(expect(i, var, varCount), monoid.exponent(*monos[i], var));
      }
    }
    // everything should be free'd now. Let's do all that again.
  }
}

namespace {
  template<class M>
  typename M::MonoVector parseVector(
    M& monoid,
    const char* str,
    const bool readComponent
  ) {
    typename M::MonoVector v(monoid);
    Scanner in(str);
    MathicIO<M>().readMonomialVector(readComponent, in, v);
    return v;
  }
}


TYPED_TEST(Monoids, setExponentAndComponent) {
  typedef TypeParam Monoid;
  Monoid m(100);
  const std::string str = Monoid::HasComponent ?
    "1<0> a<0> z<0> A<0> Z<0> ab<0> a2<0> a2b<0> ab2<0> "
      "a20b30<0> 1<1> a<2> a2<3> ab<11>\n" :
    "1 a z A Z ab a2 a2b ab2 a20b30\n";
  auto v2 = parseVector(m, str.c_str(), Monoid::HasComponent);
  std::ostringstream v2Out;
  MathicIO<Monoid>().writeMonomialVector(v2, Monoid::HasComponent, v2Out);
  ASSERT_EQ(str, v2Out.str());

  decltype(v2) v(m);
  v.push_back(); // 1

  v.push_back(); // a
  m.setExponent(0, 1, v.back());
 
  v.push_back(); // z
  m.setExponent(25, 1, v.back());

  v.push_back(); // A
  m.setExponent(26, 1, v.back());

  v.push_back(); // Z
  m.setExponent(51, 1, v.back());

  v.push_back(); // ab
  m.setExponent(0, 1, v.back());
  m.setExponent(1, 1, v.back());

  v.push_back(); // a2
  m.setExponent(0, 2, v.back());

  v.push_back(); // a2b
  m.setExponent(0, 2, v.back());
  m.setExponent(1, 1, v.back());

  v.push_back(); // ab2
  m.setExponent(0, 1, v.back());
  m.setExponent(1, 2, v.back());

  v.push_back(); // a20b30
  m.setExponent(0, 20, v.back());
  m.setExponent(1, 30, v.back());

  if (Monoid::HasComponent) {
    v.push_back(); // 1<1>
    m.setComponent(1, v.back());

    v.push_back(); // a<2>
    m.setComponent(2, v.back());
    m.setExponent(0, 1, v.back());

    v.push_back(); // a2<3>
    m.setComponent(3, v.back());
    m.setExponent(0, 2, v.back());

    v.push_back(); // ab<11>
    m.setComponent(11, v.back());
    m.setExponent(0, 1, v.back());
    m.setExponent(1, 1, v.back());
  }

  std::ostringstream vOut;
  MathicIO<Monoid>().writeMonomialVector(v, Monoid::HasComponent, vOut);
  ASSERT_EQ(str, vOut.str());

  ASSERT_EQ(v, v2);
}

TYPED_TEST(Monoids, MultiplyDivide) {
  typedef TypeParam Monoid;
  Monoid m(49);
  typename Monoid::MonoPool pool(m);
  auto monoOwner = pool.alloc();
  auto mono = *monoOwner;
  auto check = [&](const char* const str, const bool component) -> void {
    if (component && !Monoid::HasComponent)
      return;
    auto v = parseVector(m, str, component);
    MATHICGB_ASSERT(v.size() == 3);
    const auto& a = v.front();
    const auto& b = *++v.begin();
    const auto& c = v.back();
    ASSERT_EQ(m.hashOfProduct(a, b), m.hash(c));
    ASSERT_EQ(m.hashOfProduct(a, b), m.hashOfProduct(b, a));

    // isProductOf
    ASSERT_TRUE(m.isProductOf(a, b, c));
    ASSERT_TRUE(m.isProductOfHintTrue(a, b, c));
    ASSERT_TRUE(m.isTwoProductsOfHintTrue(a, a, b, c, c));
    

    // a*b == c using multiply
    m.multiply(a, b, mono);
    ASSERT_TRUE(m.equal(c, mono));
    ASSERT_TRUE(m.compare(c, mono) == Monoid::EqualTo);
    ASSERT_EQ(m.hash(c), m.hash(mono));

    // c/a == b using divide
    m.divide(a, c, mono);
    ASSERT_TRUE(m.equal(b, mono));
    ASSERT_TRUE(m.compare(b, mono) == Monoid::EqualTo);
    ASSERT_EQ(m.hash(b), m.hash(mono));

    // c/b == a using divideInPlace
    m.copy(c, mono);
    m.divideInPlace(b, mono);
    ASSERT_TRUE(m.equal(a, mono));
    ASSERT_TRUE(m.compare(a, mono) == Monoid::EqualTo);
    ASSERT_EQ(m.hash(a), m.hash(mono));

    // a*b == c using multiplyInPlace
    m.copy(a, mono);
    m.multiplyInPlace(b, mono);
    ASSERT_TRUE(m.equal(c, mono));
    ASSERT_TRUE(m.compare(c, mono) == Monoid::EqualTo);
    ASSERT_EQ(m.hash(c), m.hash(mono));

    // divides, check properties that mono=a*b should have
    ASSERT_TRUE(m.divides(mono, c));
    ASSERT_TRUE(m.divides(c, mono));
    ASSERT_TRUE(m.divides(a, mono));
    ASSERT_TRUE(m.divides(b, mono));

    // divides, general
    ASSERT_TRUE(m.divides(m, mono, c));
    ASSERT_TRUE(m.divides(m, c, mono));
    ASSERT_TRUE(m.divides(m, a, mono));
    ASSERT_TRUE(m.divides(m, b, mono));

    if (!m.isIdentity(a)) {
      ASSERT_TRUE(m.lessThan(b, mono));
      ASSERT_FALSE(m.lessThan(mono, b));
      ASSERT_TRUE(m.compare(mono, b) == Monoid::GreaterThan);
      ASSERT_FALSE(m.divides(mono, b));
      ASSERT_FALSE(m.divides(m, mono, b));

      ASSERT_FALSE(m.isProductOf(a, c, b));
      ASSERT_FALSE(m.isProductOfHintTrue(a, c, b));
      ASSERT_FALSE(m.isTwoProductsOfHintTrue(c, c, a, b, b));
      ASSERT_FALSE(m.isTwoProductsOfHintTrue(b, c, a, c, b));
      ASSERT_FALSE(m.isTwoProductsOfHintTrue(c, b, a, b, c));
    } else {
      ASSERT_TRUE(m.equal(b, mono));
      ASSERT_TRUE(m.compare(b, mono) == Monoid::EqualTo);
      ASSERT_TRUE(m.divides(mono, b));
      ASSERT_TRUE(m.divides(m, mono, b));
    }

    if (!m.isIdentity(b)) {
      ASSERT_TRUE(m.lessThan(a, mono));
      ASSERT_FALSE(m.lessThan(mono, a));
      ASSERT_TRUE(m.compare(mono, a) == Monoid::GreaterThan);
      ASSERT_FALSE(m.divides(mono, a));
      ASSERT_FALSE(m.divides(m, mono, a));

      ASSERT_FALSE(m.isProductOf(c, b, a));
      ASSERT_FALSE(m.isProductOfHintTrue(b, c, a));
      ASSERT_FALSE(m.isTwoProductsOfHintTrue(c, c, b, a, a));
      ASSERT_FALSE(m.isTwoProductsOfHintTrue(a, c, b, c, a));
      ASSERT_FALSE(m.isTwoProductsOfHintTrue(c, a, b, a, c));
    } else {
      ASSERT_TRUE(m.equal(a, mono));
      ASSERT_TRUE(m.compare(a, mono) == Monoid::EqualTo);
      ASSERT_TRUE(m.divides(m, mono, a));
    }

    // Check that aliased parameters work.
    m.multiply(mono, mono, mono);
    m.divide(mono, mono, mono);
    MATHICGB_ASSERT(m.isIdentity(mono));

    // Check that negative exponents work.
    if (Monoid::HasComponent && m.component(a) != m.component(b))
      return;
    m.divideToNegative(a, b, mono);
    m.multiply(a, mono, mono);
    ASSERT_TRUE(m.equal(mono, b));
    
    m.divideToNegative(b, a, mono);
    m.multiply(b, mono, mono);
    ASSERT_TRUE(m.equal(mono, a));
  };
  check("1 1 1", false);
  check("a<5> 1<0> a<5>", true);
  check("1 Vx Vx", false);
  check("aV bx abxV", false);
  check("a a2 a3", false);
  check("V<2> V2<0> V3<2>", true);
  check("arlgh svug arlg2hsvu", false);
  check
    ("abcdefghiV<7> ab2c3d4e5f6g7h8i9V11<0> a2b3c4d5e6f7g8h9i10V12<7>", true);
}

TYPED_TEST(Monoids, LcmColon) {
  typedef TypeParam Monoid;
  Monoid mNonConst(49);
  auto& m = mNonConst;
  typename Monoid::MonoPool pool(m);
  auto monoOwner = pool.alloc();
  auto mono = *monoOwner;
  auto mono2Owner = pool.alloc();
  auto mono2 = *mono2Owner;
  auto check = [&](const char* const str, const bool component) -> void {
    if (component && !Monoid::HasComponent)
      return;
    auto v = parseVector(m, str, component);
    MATHICGB_ASSERT(v.size() == 3);
    const auto& a = v.front();
    const auto& b = *++v.begin();
    const auto& lcm = v.back();

    // isLcm (+general)
    ASSERT_TRUE(m.isLcm(a, b, lcm));
    ASSERT_TRUE(m.isLcm(m, a, m, b, lcm));
    m.copy(lcm, mono);
    m.setExponent(1, m.exponent(mono, 1) + 1, mono);
    ASSERT_FALSE(m.isLcm(a, b, mono));
    ASSERT_FALSE(m.isLcm(m, a, m, b, mono));

    // dividesLcm
    ASSERT_TRUE(m.dividesLcm(lcm, a, b));
    ASSERT_FALSE(m.dividesLcm(mono, a, b));
    ASSERT_TRUE(m.dividesLcm(a, a, a));
    ASSERT_TRUE(m.dividesLcm(a, a, b));
    ASSERT_TRUE(m.dividesLcm(b, b, b));
    ASSERT_TRUE(m.dividesLcm(b, b, a));

    // dividesLcm, general
    ASSERT_TRUE(m.dividesLcm(m, lcm, m, a, b));
    ASSERT_FALSE(m.dividesLcm(m, mono, m, a, b));
    ASSERT_TRUE(m.dividesLcm(m, a, m, a, a));
    ASSERT_TRUE(m.dividesLcm(m, a, m, a, b));
    ASSERT_TRUE(m.dividesLcm(m, b, m, b, b));
    ASSERT_TRUE(m.dividesLcm(m, b, m, b, a));

    // lcm(a, b)
    m.lcm(a, b, mono);
    ASSERT_TRUE(m.equal(mono, lcm));
    ASSERT_TRUE(m.compare(mono, lcm) == Monoid::EqualTo);
    ASSERT_EQ(m.hash(lcm), m.hash(mono));

    // lcm(b, a), general
    m.lcm(m, b, m, a, mono);
    ASSERT_TRUE(m.equal(mono, lcm));
    ASSERT_TRUE(m.compare(mono, lcm) == Monoid::EqualTo);
    ASSERT_EQ(m.hash(lcm), m.hash(mono));

    // colons
    m.colons(a, b, mono, mono2);
    m.multiply(b, mono, mono);
    m.multiply(a, mono2, mono2);
    ASSERT_TRUE(m.equal(mono, lcm));
    ASSERT_TRUE(m.compare(mono, lcm) == Monoid::EqualTo);
    ASSERT_TRUE(m.equal(mono2, lcm));
    ASSERT_TRUE(m.compare(mono2, lcm) == Monoid::EqualTo);
  };
  check("1 1 1", false);
  check("a<2> 1<2> a<2>", true);
  check("1 Vx Vx", false);
  check("aV bx abxV", false);
  check("a a2 a2", false);
  check("V<3> V2<3> V2<3>", true);
  check("arlgh svug arlghsvu", false);
  check("a6b7c8d9efghiV ab2c3d4e5f6g7h8i9V11 a6b7c8d9e5f6g7h8i9V11", false);
}

TYPED_TEST(Monoids, Order) {
  typedef TypeParam Monoid;
  typedef typename Monoid::Order Order;
  typedef typename Monoid::Exponent Exponent;
  typedef typename Monoid::ConstMonoRef ConstMonoRef;

  auto check = [](
    const Monoid& m,
    const char* const sorted
  ) -> void {
    auto toStr = [&](ConstMonoRef mono) {
      std::ostringstream out;
      MathicIO<Monoid>().writeMonomial(m, false, mono, out);
      return out.str();
    };
    auto v = parseVector(m, sorted, false);
    for (auto greater = v.begin(); greater != v.end(); ++greater) {
      ASSERT_EQ(m.compare(*greater, *greater), Monoid::EqualTo);
      ASSERT_TRUE(m.equal(*greater, *greater));
      ASSERT_FALSE(m.lessThan(*greater, *greater));
      
      for (auto lesser = v.begin(); lesser != greater; ++lesser) {
        ASSERT_FALSE(m.equal(*lesser, *greater));
        ASSERT_TRUE(m.lessThan(*lesser, *greater))
          << "String   = " << sorted << '\n'
          << "*lesser  = " << toStr(*lesser) << '\n'
          << "*greater = " << toStr(*greater) << '\n';
        ASSERT_FALSE(m.lessThan(*greater, *lesser));
        ASSERT_EQ(m.compare(*lesser, *greater), Monoid::LessThan);
        ASSERT_EQ(m.compare(*greater, *lesser), Monoid::GreaterThan);
      }
    }
  };

  const auto sortedTotalDegreeRevLex =
    "1 Z A z c b a c2 bc ac b2 ab a2 c3 abc b3 a3";
  check(Monoid(52), sortedTotalDegreeRevLex);
  check(
    Monoid(
      Order(52, std::vector<Exponent>(52, 1),
      Order::RevLexBaseOrderFromRight)
    ),
    sortedTotalDegreeRevLex
  );
  check(
    Monoid(
      Order(52, std::vector<Exponent>(52, 7),
      Order::RevLexBaseOrderFromRight)
    ),
    sortedTotalDegreeRevLex
  );
  std::vector<Exponent> revLexGradings(52, 1);
  for (size_t grading = 51; grading != static_cast<size_t>(-1); --grading)
    for (size_t var = 0; var < 52; ++var)
      revLexGradings.push_back(var == grading ? -1 : 0);
  check(
    Monoid(
      Order(52, std::vector<Exponent>(revLexGradings),
      Order::RevLexBaseOrderFromRight)
    ),
    sortedTotalDegreeRevLex
  );
  check(
    Monoid(
      Order(52, std::move(revLexGradings),
      Order::LexBaseOrderFromRight)
    ),
    sortedTotalDegreeRevLex
  );

  Exponent dupGradingsArray[] = {
     5, 2, 3,
    10, 4, 6, // duplicate, just multiplied by 2
    -6, 9, 4,
    -6, 9, 4,
    -6, 9, 4,
    -6, 9, 4,
    -6, 9, 4
  };
  std::vector<Exponent> dupGradings(
    dupGradingsArray,
    dupGradingsArray + sizeof(dupGradingsArray)/sizeof(*dupGradingsArray)
  );

  //   b:  2  9
  //   c:  3  4
  //   a:  5 -7
  //  bc:  5 20
  //  c2:  6  8
  //  b3:  6 27
  // bc3: 11 21
  // ab3: 11 21
  const auto sortedDupGradingsRevLex = "1 b c a bc c2 b3 bc3 ab3";
  check(
    Monoid(Order(3, std::move(dupGradings), Order::RevLexBaseOrderFromRight)),
    sortedDupGradingsRevLex
  );

  Exponent lexGradingsArray[] = {
    0, 0, 1,
    0, 1, 0,
    1, 0, 0
  };
  std::vector<Exponent> lexGradings(
    lexGradingsArray,
    lexGradingsArray + sizeof(lexGradingsArray) / sizeof(*lexGradingsArray)
  );
  const auto sortedLexFromRight =
    "1 a a2 a3 b ab a2b b2 ab2 b3 c ac bc abc c2 ac2 bc2 c3";
  auto lexGradingsCheck = [&](typename Order::BaseOrder baseOrder) {
    check(
      Monoid(Order(3, std::vector<Exponent>(lexGradings), baseOrder)),
      sortedLexFromRight
    );
  };
  lexGradingsCheck(Order::LexBaseOrderFromRight);
  lexGradingsCheck(Order::RevLexBaseOrderFromRight);
  lexGradingsCheck(Order::LexBaseOrderFromLeft);
  lexGradingsCheck(Order::RevLexBaseOrderFromLeft);
  check(
    Monoid(Order(3, std::vector<Exponent>(), Order::LexBaseOrderFromRight)),
    sortedLexFromRight
  );
  const auto sortedLexFromLeft =
    "1 c c2 c3 b cb bc2 b2 b2c b3 a ac ab abc a2 a2c a2b a3";
  check(
    Monoid(Order(3, std::vector<Exponent>(), Order::LexBaseOrderFromLeft)),
    sortedLexFromLeft
  );
}

TYPED_TEST(Monoids, RelativelyPrime) {
  typedef TypeParam Monoid;
  Monoid m(49);
  typename Monoid::MonoPool pool(m);
  auto mono = pool.alloc();
  auto mono2 = pool.alloc();
  auto check = [&](const char* str, bool relativelyPrime) -> void {
    auto v = parseVector(m, str, false);
    MATHICGB_ASSERT(v.size() == 2);
    ASSERT_EQ(relativelyPrime, m.relativelyPrime(v.front(), v.back()));
    ASSERT_EQ(relativelyPrime, m.relativelyPrime(v.back(), v.front()));
  };
  check("1 1", true);
  check("1 abcdefgh", true);
  check("abc defgh", true);
  check("bdfh aceg", true);
  check("bdefh aceg", false);
  check("abcdefgh abcdefgh", false);
  check("fgh abcdef", false);
}

TYPED_TEST(Monoids, SetExponents) {
  typedef TypeParam Monoid;
  typedef typename Monoid::VarIndex VarIndex;
  typedef typename Monoid::MonoVector MonoVector;
  Monoid m(5);
  MonoVector v(m);
  v.push_back();
  typename Monoid::Exponent exponents[] = {1, 2, 3, 4, 5};
  m.setExternalExponents(exponents, v.back());
  for (VarIndex var = 0; var < m.varCount(); ++var) {
    ASSERT_EQ(exponents[var], m.externalExponent(v.back(), var));
  }
}

TYPED_TEST(Monoids, HasAmpleCapacityTotalDegree) {
  typedef TypeParam Monoid;
  typedef typename Monoid::Order Order;
  typedef typename Monoid::Exponent Exponent;
  typedef typename Monoid::VarIndex VarIndex;

  for (VarIndex varCount = 1; varCount < 33; ++varCount) {
    Monoid monoidTotalDegree(varCount);
    
    std::vector<Exponent> ones(varCount, 1);
    Monoid monoidTotalDegreeImplicit
      (Order(varCount, std::move(ones), Order::RevLexBaseOrderFromRight));

    std::vector<Exponent> mostlyOnes(varCount, 1);
    mostlyOnes[0] = 7;
    Monoid monoidGeneral(
      Order(varCount, std::move(mostlyOnes),
      Order::RevLexBaseOrderFromRight)
    );

    Monoid* monoids[] = {
      &monoidTotalDegree,
      &monoidTotalDegreeImplicit,
      &monoidGeneral
    };
    for (int j = 0; j < 3; ++j) {
      auto& m = *monoids[j];
      const auto firstDeg = (j == 2 ? 7 : 1);
      ASSERT_EQ(varCount, m.varCount());

      typename Monoid::MonoPool p(m);
      auto monoOwner = p.alloc();
      auto mono = *monoOwner;
      const auto last = m.varCount() - 1;
      const auto max = std::numeric_limits<Exponent>::max() / 2;

      // pure power, first variable
      m.setIdentity(mono);
      m.setExponent(0, max / firstDeg, mono);
      ASSERT_TRUE(m.hasAmpleCapacity(mono));
      m.setExponent(0, max / firstDeg + 1, mono);
      ASSERT_FALSE(m.hasAmpleCapacity(mono));

      if (varCount == 1)
        continue;

      // pure power, last variable
      m.setIdentity(mono);
      m.setExponent(last, max, mono);
      ASSERT_TRUE(m.hasAmpleCapacity(mono));
      m.setExponent(last, max + 1, mono);
      ASSERT_FALSE(m.hasAmpleCapacity(mono));

      // no exponent is too high but the degree is
      m.setIdentity(mono);
      m.setExponent(0, 12, mono);
      m.setExponent(last, max - 12 * firstDeg, mono);
      ASSERT_TRUE(m.hasAmpleCapacity(mono));
      m.setExponent(0, 13, mono);
      ASSERT_FALSE(m.hasAmpleCapacity(mono));
    }
  }
}

TYPED_TEST(Monoids, CopyEqualConversion) {
  typedef TypeParam Monoid;
  typedef typename Monoid::Order Order;
  typedef typename Monoid::Exponent Exponent;
  typedef typename Monoid::VarIndex VarIndex;
  static const bool HasComponent = Monoid::HasComponent;
  typedef MonoMonoid<Exponent, HasComponent, false, false> MonoidNone;
  typedef MonoMonoid<Exponent, HasComponent, true, true> MonoidAll;
  for (VarIndex varCount = 1; varCount < 33; ++varCount) {
    const Order order(
      varCount,
      std::vector<Exponent>(varCount, 1),
      Order::RevLexBaseOrderFromRight
    );
    MonoidNone none(order);
    Monoid some(Monoid::create(none));
    MonoidAll all(MonoidAll::create(some));

    auto none1Owner = none.alloc();
    auto none2Owner = none.alloc();
    auto none3Owner = none.alloc();
    auto some1Owner = some.alloc();
    auto some2Owner = some.alloc();
    auto some3Owner = some.alloc();
    auto all1Owner = all.alloc();
    auto all2Owner = all.alloc();
    auto all3Owner = all.alloc();

    auto none1 = *none1Owner;
    auto none2 = *none2Owner;
    auto none3 = *none3Owner;
    auto some1 = *some1Owner;
    auto some2 = *some2Owner;
    auto some3 = *some3Owner;
    auto all1 = *all1Owner;
    auto all2 = *all2Owner;
    auto all3 = *all3Owner;

    none.setExponent(0, 1, none1);
    none.setExponent(varCount / 2, 2, none1);
    none.setExponent(varCount - 1, 3, none1);
    none.copy(none1, none2);
    none.setExponent(0, 4, none2);

    some.setExponent(0, 1, some1);
    some.setExponent(varCount / 2, 2, some1);
    some.setExponent(varCount - 1, 3, some1);
    some.copy(some1, some2);
    some.setExponent(0, 4, some2);

    all.setExponent(0, 1, all1);
    all.setExponent(varCount / 2, 2, all1);
    all.setExponent(varCount - 1, 3, all1);
    all.copy(all1, all2);
    all.setExponent(0, 4, all2);

    // compare on none
    ASSERT_TRUE(none.equal(none, none1, none1));
    ASSERT_TRUE(none.equal(some, some1, none1));
    ASSERT_TRUE(none.equal(all, all1, none1));
    ASSERT_FALSE(none.equal(none, none1, none2));
    ASSERT_FALSE(none.equal(some, some1, none2));
    ASSERT_FALSE(none.equal(all, all1, none2));

    // compare on some
    ASSERT_TRUE(some.equal(none, none1, some1));
    ASSERT_TRUE(some.equal(some, some1, some1));
    ASSERT_TRUE(some.equal(all, all1, some1));
    ASSERT_FALSE(some.equal(none, none1, some2));
    ASSERT_FALSE(some.equal(some, some1, some2));
    ASSERT_FALSE(some.equal(all, all1, some2));

    // compare on all
    ASSERT_TRUE(all.equal(none, none1, all1));
    ASSERT_TRUE(all.equal(some, some1, all1));
    ASSERT_TRUE(all.equal(all, all1, all1));
    ASSERT_FALSE(all.equal(none, none1, all2));
    ASSERT_FALSE(all.equal(some, some1, all2));
    ASSERT_FALSE(all.equal(all, all1, all2));

    // convert some->none
    none.copy(some, some1, none3);
    ASSERT_TRUE(none.equal(none1, none3));
    ASSERT_FALSE(none.equal(none2, none3));
    none.copy(some, some2, none3);
    ASSERT_FALSE(none.equal(none1, none3));
    ASSERT_TRUE(none.equal(none2, none3));

    /// convert some->all
    all.copy(some, some1, all3);
    ASSERT_TRUE(all.equal(all1, all3));
    ASSERT_FALSE(all.equal(all2, all3));
    all.copy(some, some2, all3);
    ASSERT_FALSE(all.equal(all1, all3));
    ASSERT_TRUE(all.equal(all2, all3));

    // convert none->some
    some.copy(none, none1, some3);
    ASSERT_TRUE(some.equal(some1, some3));
    ASSERT_FALSE(some.equal(some2, some3));
    some.copy(none, none2, some3);
    ASSERT_FALSE(some.equal(some1, some3));
    ASSERT_TRUE(some.equal(some2, some3));

    // convert Y->some
    some.copy(none, none1, some3);
    ASSERT_TRUE(some.equal(some1, some3));
    ASSERT_FALSE(some.equal(some2, some3));
    some.copy(none, none2, some3);
    ASSERT_FALSE(some.equal(some1, some3));
    ASSERT_TRUE(some.equal(some2, some3));
  }
}

