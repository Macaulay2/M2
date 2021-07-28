// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"
#include "mathicgb/MathicIO.hpp"

#include <gtest/gtest.h>

using namespace mgb;

TEST(MathicIO, Combined) {
  const char* const str =
    "32003 6\n"
    "1 1 1 1 1 1 1\n"
    "_revlex revcomponent\n"
    "3\n"
    " -bc+ad\n"
    " -b2+af\n"
    " -bc2+a2e\n";
  std::istringstream inStream(str);
  Scanner in(inStream);
  auto p = MathicIO<>().readRing(true, in);
  auto& ring = *p.first;
  auto basis = MathicIO<>().readBasis(ring, false, in);
}

TEST(MathicIO, ReadWriteRing) {
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::VarIndex VarIndex;
  typedef Monoid::Exponent Exponent;
  typedef PolyRing::Field BaseField;
  typedef BaseField::RawElement RawElement;

  auto check = [&](
    const char* const inStr,
    const char* outStr,
    const RawElement charac,
    const VarIndex varCount,
    const VarIndex gradingCount,
    const bool withComponent
  ) {
    for (int i = 0; i < 2; ++i) {
      const char* str = i == 0 ? inStr : outStr;
      if (str == 0)
        continue;
      Scanner in(str);
      const auto p = MathicIO<>().readRing(withComponent, in);
      const auto& monoid = p.first->monoid();
      const auto& field = p.first->field();
      ASSERT_EQ(charac, field.charac());
      ASSERT_EQ(varCount, monoid.varCount());
      ASSERT_EQ(gradingCount, monoid.gradingCount());
      std::ostringstream out;
      MathicIO<>().writeRing(*p.first, p.second, withComponent, out);
      ASSERT_EQ(outStr, out.str());
    }
  };

  check("101 2 1 3 4", "101 2\nrevlex 1\n 3 4\n", 101, 2, 1, false);
  check(
    "101 2 schreyer lex 1 3 4 _lex revcomponent",
    "101 2\nschreyer lex 1\n 3 4\n _lex\n revcomponent\n", 101, 2, 1, true
  );
  check(
    "101 2 2 component 3 4",
    "101 2\nrevlex 2\n component\n 3 4\n", 101, 2, 2, true
  );
}

TEST(MathicIO, ReadWriteMonomial) {
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::VarIndex VarIndex;
  typedef Monoid::Exponent Exponent;
  static const auto NoComponent = static_cast<Exponent>(-1);

  Monoid m(28);

  // canonicalStr is str as it should appear after being read and
  // printed. If canonicalStr is null then this is the same as str.
  auto check = [&](
    const char* const str,
    const Exponent component,
    const VarIndex var1,
    const Exponent exp1,
    const VarIndex var2,
    const Exponent exp2,
    const char* const canonicalStr
  ) {
    const bool doComponent = component != NoComponent;

    // read monomial from string
    auto monoRead = m.alloc();
    Scanner in(str);
    MathicIO<>().readMonomial(m, doComponent, *monoRead, in);

    // directly make monomial
    auto monoSet = m.alloc();
    if (var1 != -1)
      m.setExponent(var1, exp1, *monoSet);
    if (var2 != -1)
      m.setExponent(var2, exp2, *monoSet);
    if (doComponent)
      m.setComponent(component, *monoSet);
    ASSERT_TRUE(m.equal(*monoRead, *monoSet)) << "Str: " << str;

    // print monomial
    std::ostringstream out;
    MathicIO<>().writeMonomial(m, doComponent, *monoRead, out);
    const auto correctStr = canonicalStr == 0 ? str : canonicalStr;
    ASSERT_EQ(correctStr, out.str());
  };

  check("1", NoComponent,  -1,-1,  -1,-1,  0);
  check("1<0>", 0,  -1,-1,  -1,-1,  0);
  check("1<1>", 1,  -1,-1,  -1,-1,  0);
  check("1<999>", 999,  -1,-1,  -1,-1,  0);

  check("a1", NoComponent,  0,1,  -1,-1,  "a");
  check("b10<0>", 0,  1,10,  -1,-1,  0);
  check("A11", NoComponent,  26,11, -1,-1,  0);
  check("B99<1>", 1,   27,99,  -1,-1,  0);

  check("ab", NoComponent,  0,1,  1,1,  0);
  check("ba", NoComponent,  0,1,  1,1,  "ab");
  check("a0c3b1", NoComponent,  1,1,  2,3,  "bc3");
  check("ab<2>", 2,  0,1,  1,1,  0);
}

TEST(MathicIO, ReadWriteBasis) {
  typedef PolyRing::Monoid Monoid;
  typedef PolyRing::Field Field;
  PolyRing ring(Field(101), Monoid(28));

  auto check = [&](
    const char* const inStr,
    const char* const outStr,
    const bool doComponent
  ) {
    for (int i = 0; i < 2; ++i) {
      const char* str = i == 0 ? inStr : outStr;
      if (str == 0)
        continue;

      Scanner in(str);
      const auto basis = MathicIO<>().readBasis(ring, doComponent, in);
      std::ostringstream out;
      MathicIO<>().writeBasis(basis, doComponent, out);
      const auto correctStr = outStr == 0 ? inStr : outStr;
      ASSERT_EQ(correctStr, out.str());
    }
  };

  check("0", "0\n", false);
  check("0", "0\n", true);
  check("1 0", "1\n 0\n", false);
  check("1 0", "1\n 0\n", true);
  
  check("1 1", "1\n 1\n", false);
  check("1 a<0>", "1\n a<0>\n", true);
  check("2 a b", "2\n a\n b\n", false);
}

TEST(MathicIO, ReadWritePoly) {
  typedef PolyRing::Monoid Monoid;
  typedef PolyRing::Field Field;
  PolyRing ring(Field(101), Monoid(28));

  auto check = [&](
    const char* const inStr,
    const char* const outStr,
    const bool doComponent
  ) {
    for (int i = 0; i < 2; ++i) {
      const char* str = i == 0 ? inStr : outStr;
      if (str == 0)
        continue;

      Scanner in(str);
      const auto poly = MathicIO<>().readPolyDoNotOrder(ring, doComponent, in);
      std::ostringstream out;
      MathicIO<>().writePoly(poly, doComponent, out);
      const auto correctStr = outStr == 0 ? inStr : outStr;
      ASSERT_EQ(correctStr, out.str());
    }
  };

  check("+0", "0", false);
  check("-0", "0", false);
  check("+1", "1", false);
  check("\t  a\t", "a", false);
  check("3a+1b5+2c6", "3a+b5+2c6", false);

  check("+0", "0", true);
  check("-0", "0", true);
  check("+1<5>", "1<5>", true);
  check("\t  a<0>\t", "a<0>", true);
  check("3a<1>+1b5<2>+2c6<3>", "3a<1>+b5<2>+2c6<3>", true);
}

TEST(MathicIO, ReadWriteTerm) {
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::VarIndex VarIndex;
  typedef Monoid::Exponent Exponent;
  typedef PolyRing::Field Field;
  typedef Field::Element Coefficient;
  typedef Field::RawElement RawCoefficient;
  static const auto NoComponent = static_cast<Exponent>(-1);

  PolyRing ring(Field(101), Monoid(28));
  auto&& m = ring.monoid();
  auto&& f = ring.field();

  auto check = [&](
    const char* const inStr,
    const char* const outStr,
    const bool doComponent,
    const RawCoefficient coef
  ) {
    for (int i = 0; i < 2; ++i) {
      const char* str = i == 0 ? inStr : outStr;
      if (str == 0)
        continue;

      // read term from string
      auto monoRead = m.alloc();
      Coefficient readCoef = f.zero();
      Scanner in(str);
      MathicIO<>().readTerm(ring, doComponent, readCoef, *monoRead, in);
      
      ASSERT_EQ(coef, readCoef.value());

      // print monomial
      std::ostringstream out;
      MathicIO<>().writeTerm
        (ring, doComponent, readCoef, *monoRead, false, out);
      const auto correctStr = outStr == 0 ? inStr : outStr;
      ASSERT_EQ(correctStr, out.str());
    }
  };

  check("1", 0, false, 1);
  check("-1", "-1", false, f.minusOne().value());
  check("+1", "1", false, 1);
  check("2", 0, false, 2);
  check("+102", "1", false, 1);

  check("1<0>", 0, true, 1);
  check("+1<2>", "1<2>", true, 1);
  check("2<3>", 0, true, 2);
  check("+3<4>", "3<4>", true, 3);
  check("-3<4>", "-3<4>", true, f.negative(f.toElement(3)).value());

  check("+1a<0>", "a<0>", true, 1);
  check("+2b", "2b", false, 2);
}

TEST(MathicIO, ReadWriteBaseField) {
  Scanner in("101");
  auto field = MathicIO<>().readBaseField(in);
  ASSERT_EQ(101, field.charac());
  std::ostringstream out;
  MathicIO<>().writeBaseField(field, out);
  ASSERT_EQ("101", out.str());
}

TEST(MathicIO, ReadWriteOrder) {
  typedef PolyRing::Monoid Monoid;
  typedef Monoid::VarIndex VarIndex;

  const auto check = [](
    const char* const inStr,
    const char* const outStr,
    const VarIndex varCount,
    const VarIndex gradingCount,
    const bool withComponent,
    const bool componentsAscendingDesired,
    const bool schreyering
  ) -> void {
    for (int i = 0; i < 2; ++i) {
      const char* str = i == 0 ? inStr : outStr;
      if (str == 0)
        continue;

      Scanner in(str);
      const auto order = MathicIO<>().readOrder(varCount, withComponent, in);
      ASSERT_EQ(varCount, order.varCount());
      ASSERT_EQ(gradingCount, order.gradingCount()) << inStr;
      ASSERT_EQ(componentsAscendingDesired, order.componentsAscendingDesired());
      ASSERT_EQ(schreyering, order.schreyering()) << inStr;

      std::ostringstream out;
      MathicIO<>().writeOrder(order, withComponent, out);
      ASSERT_EQ(outStr, out.str());
    }
  };
  check("0\n", "revlex 0\n", 0, 0,  0,1,0);
  check("1\n 2\n", "revlex 1\n 2\n", 1, 1,  0,1,0);
  check("2\n 3\n 4\n", "revlex 2\n 3\n 4\n", 1, 2,  0,1,0);
  check("2\n 3 4\n 5 6\n", "revlex 2\n 3 4\n 5 6\n", 2, 2,  0,1,0);
  check("1\n 1 1 1 1\n", "revlex 1\n 1 1 1 1\n", 4, 1,  0,1,0);

  check("lex 0", "lex 0\n", 0, 0,  0,1,0);
  check("lex 1 2", "lex 1\n 2\n", 1, 1,  0,1,0);
  check("lex 2 3 4", "lex 2\n 3\n 4\n", 1, 2,  0,1,0);
  check("lex 2 3 4 5 6", "lex 2\n 3 4\n 5 6\n", 2, 2,  0,1,0);
  check("lex 1 1 1 1 1", "lex 1\n 1 1 1 1\n", 4, 1,  0,1,0);

  check("2 component\n 5 6\n", "revlex 2\n component\n 5 6\n", 2, 1, 1, 1, 0);
  check("2 3 4\nrevcomponent\n","revlex 2\n 3 4\n revcomponent\n",2, 1, 1,0,0);
  check("lex 1 component", "lex 1\n component\n", 0, 0,  1, 1, 0);
  check("lex 1 revcomponent", "lex 1\n revcomponent\n", 1, 0, 1, 0, 0);
  check("lex 1 revcomponent", "lex 1\n revcomponent\n", 5, 0, 1, 0, 0);

  check(
    "schreyer lex 1 1 _revlex revcomponent",
    "schreyer revlex 1\n 1\n _revlex\n revcomponent\n",
    1, 1, 1, 0, 1
  );
}
