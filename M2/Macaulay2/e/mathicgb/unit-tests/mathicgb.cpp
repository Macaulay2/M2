// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"

#include "mathicgb.h"
#include <gtest/gtest.h>

using namespace mgb;

namespace {
  template<class Stream>
  void makeBasis(Stream& s) {
    MATHICGB_ASSERT(s.varCount() >= 3);
    MATHICGB_ASSERT(s.comCount() >= 1);

    s.idealBegin();
      s.appendPolynomialBegin(); // x^2 - y
        s.appendTermBegin(0);
          s.appendExponent(0,2);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(1,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
      s.appendPolynomialBegin(2); // x^3-z
        s.appendTermBegin(0);
          s.appendExponent(0,3);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(2,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
    s.idealDone();
  }

  template<class Stream>
  void makeGroebnerBasis(Stream& s) {
    s.idealBegin(3);
      s.appendPolynomialBegin(2); // x^2 - y
        s.appendTermBegin(0);
          s.appendExponent(0, 2);
          s.appendExponent(1, 0);
          s.appendExponent(2, 0);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(0, 0);
          s.appendExponent(1, 1);
          s.appendExponent(2, 0);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
      s.appendPolynomialBegin(2); // xy - z
        s.appendTermBegin(0);
          s.appendExponent(0, 1);
          s.appendExponent(1, 1);
          s.appendExponent(2, 0);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(0, 0);
          s.appendExponent(1, 0);
          s.appendExponent(2, 1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
      s.appendPolynomialBegin(2); // y^2 - xz
        s.appendTermBegin(0);
          s.appendExponent(0, 0);
          s.appendExponent(1, 2);
          s.appendExponent(2, 0);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(0, 1);
          s.appendExponent(1, 0);
          s.appendExponent(2, 1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
    s.idealDone();
  }

  template<class Stream>
  void makeCyclic5Basis(Stream& s) {
    s.idealBegin(5); // polyCount
    s.appendPolynomialBegin(5);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(1, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(2, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(3, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(5);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(5);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(5);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(2);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.idealDone();
  }

  template<class Stream>
  void makeSimpleIdeal(Stream& s) { // variables a,b,c,d.  a2-bc, ab-cd.
    s.idealBegin();
      s.appendPolynomialBegin(); // a^2-b*c
        s.appendTermBegin(0);
          s.appendExponent(0,2);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(1,1);
          s.appendExponent(2,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
      s.appendPolynomialBegin(2); // a*b-c*d
        s.appendTermBegin(0);
          s.appendExponent(0,1);
          s.appendExponent(1,1);
        s.appendTermDone(1);
        s.appendTermBegin(0);
          s.appendExponent(2,1);
          s.appendExponent(3,1);
        s.appendTermDone(s.modulus() - 1);
      s.appendPolynomialDone();
    s.idealDone();
  }

  template<class Stream>
  void makeSimpleIdealGroebnerBasis(Stream& s) { // variables a,b,c,d.  a2-bc, ab-cd.
    // Groebner basis is {a^2-b*c,a*b-c*d,a*c*d-b^2*c,b^3*c-c^2*d^2}
    // (order: eliminate 1 variable: [1 0 0 0; 1 1 1 1]).
    s.idealBegin(4); // polyCount
    s.appendPolynomialBegin(2);
    s.appendTermBegin(0);
    s.appendExponent(0, 2); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(2);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(2);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 2); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(2);
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 3); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 2); // index, exponent
    s.appendExponent(3, 2); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.idealDone();
  }

  template<class Stream>
  void makeSimpleModuleBasis(Stream& s) {
    MATHICGB_ASSERT(s.varCount() >= 4);
    MATHICGB_ASSERT(s.comCount() >= 4);
    // The basis is
    //   c2<0>-b<1>+d<2>
    //   bd<0>-a<1>+c<2>
    //   ac<0>-b<2>-d<3>
    //   b2<0>-a<2>-c<3>
    const auto minusOne = s.modulus() - 1;
    s.idealBegin(4);
      s.appendPolynomialBegin(3); //   c2<0>-b<1>+d<2>
        s.appendTermBegin(0);
          s.appendExponent(2, 2);
        s.appendTermDone(1);
        s.appendTermBegin(1);
          s.appendExponent(1, 1);
        s.appendTermDone(minusOne);
        s.appendTermBegin(2);
          s.appendExponent(3, 1);
        s.appendTermDone(1);
      s.appendPolynomialDone();
 
      s.appendPolynomialBegin(3); // bd<0>-a<1>+c<2>
        s.appendTermBegin(0);
          s.appendExponent(1, 1);
          s.appendExponent(3, 1);
        s.appendTermDone(1);
        s.appendTermBegin(1);
          s.appendExponent(0, 1);
        s.appendTermDone(minusOne);
        s.appendTermBegin(2);
          s.appendExponent(2, 1);
        s.appendTermDone(1);
      s.appendPolynomialDone();

      s.appendPolynomialBegin(3); // ac<0>-b<2>-d<3>
        s.appendTermBegin(0);
          s.appendExponent(0, 1);
          s.appendExponent(2, 1);
        s.appendTermDone(1);
        s.appendTermBegin(2);
          s.appendExponent(1, 1);
        s.appendTermDone(minusOne);
        s.appendTermBegin(3);
          s.appendExponent(3, 1);
        s.appendTermDone(minusOne);
      s.appendPolynomialDone();

      s.appendPolynomialBegin(3); // b2<0>-a<2>-c<3>
        s.appendTermBegin(0);
          s.appendExponent(1, 2);
        s.appendTermDone(1);
        s.appendTermBegin(2);
          s.appendExponent(0, 1);
        s.appendTermDone(minusOne);
        s.appendTermBegin(3);
          s.appendExponent(2, 1);
        s.appendTermDone(minusOne);
      s.appendPolynomialDone();
 
    s.idealDone();
  }

  template<class Stream>
  void makeSimpleModuleGroebnerBasis(Stream& s) {
    s.idealBegin(5); // polyCount
    s.appendPolynomialBegin(3);
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 2); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(1);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendTermBegin(2);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(3);
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(1);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendTermBegin(2);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(3);
    s.appendTermBegin(0);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(2);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendTermBegin(3);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(3);
    s.appendTermBegin(0);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 2); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(2);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendTermBegin(3);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.appendPolynomialBegin(4);
    s.appendTermBegin(1);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(1); // coefficient
    s.appendTermBegin(2);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 1); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 0); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendTermBegin(2);
    s.appendExponent(0, 1); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 0); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendTermBegin(3);
    s.appendExponent(0, 0); // index, exponent
    s.appendExponent(1, 0); // index, exponent
    s.appendExponent(2, 1); // index, exponent
    s.appendExponent(3, 1); // index, exponent
    s.appendExponent(4, 0); // index, exponent
    s.appendTermDone(100); // coefficient
    s.appendPolynomialDone();
    s.idealDone();
  }
}

TEST(MathicGBLib, NullIdealStream) {
  {
    mgb::NullIdealStream stream(2, 3, 1);
    ASSERT_EQ(2, stream.modulus());
    ASSERT_EQ(3, stream.varCount());
    ASSERT_EQ(1, stream.comCount());
    makeBasis(stream);
  }

  {
    mgb::NullIdealStream stream(2, 3, 4);
    ASSERT_EQ(2, stream.modulus());
    ASSERT_EQ(3, stream.varCount());
    ASSERT_EQ(4, stream.comCount());
    makeBasis(stream);
  }

  {
    mgb::NullIdealStream stream(101, 0, 0);
    ASSERT_EQ(101, stream.modulus());
    ASSERT_EQ(0, stream.varCount());
    ASSERT_EQ(0, stream.comCount());
  }
}

TEST(MathicGBLib, IdealStreamLog) {
  {
    const char* const idealStr = 
      "s.idealBegin();\n"
      "s.appendPolynomialBegin();\n"
      "s.appendTermBegin(0);\n"
      "s.appendExponent(0, 2); // index, exponent\n"
      "s.appendTermDone(1); // coefficient\n"
      "s.appendTermBegin(0);\n"
      "s.appendExponent(1, 1); // index, exponent\n"
      "s.appendTermDone(6); // coefficient\n"
      "s.appendPolynomialDone();\n"
      "s.appendPolynomialBegin(2);\n"
      "s.appendTermBegin(0);\n"
      "s.appendExponent(0, 3); // index, exponent\n"
      "s.appendTermDone(1); // coefficient\n"
      "s.appendTermBegin(0);\n"
      "s.appendExponent(2, 1); // index, exponent\n"
      "s.appendTermDone(6); // coefficient\n"
      "s.appendPolynomialDone();\n"
      "s.idealDone();\n";

    std::ostringstream out1;
    mgb::IdealStreamLog<> stream1(out1, 7, 3, 1);

    mgb::IdealStreamChecker<decltype(stream1)> checker(stream1);

    std::ostringstream out2;
    mgb::IdealStreamLog<decltype(checker)> stream2(out2, checker);

    std::ostringstream out3;
    mgb::IdealStreamLog<decltype(stream2)> stream3(out3, stream2);

    ASSERT_EQ(7, stream1.modulus());
    ASSERT_EQ(3, stream1.varCount());
    ASSERT_EQ(7, checker.modulus());
    ASSERT_EQ(3, checker.varCount());
    ASSERT_EQ(7, stream2.modulus());
    ASSERT_EQ(3, stream2.varCount());
    ASSERT_EQ(7, stream3.modulus());
    ASSERT_EQ(3, stream3.varCount());

    makeBasis(stream3);
    const auto str1 = std::string(
      "IdealStreamLog s(stream, 7, 3, 1);\n"
    ) + idealStr;
    ASSERT_EQ(str1, out1.str())
      << "Displayed expected:\n" << out1.str()
      << "Displayed actual:\n" << str1 << std::endl;

    const auto str2 = std::string(
      "IdealStreamLog s(stream, log); // modulus=7, varCount=3, comCount=1\n"
    ) + idealStr;
    ASSERT_EQ(str2, out2.str()) << "Displayed expected:\n" << out2.str();
    ASSERT_EQ(str2, out3.str()) << "Displayed expected:\n" << out3.str();
  }

  // The ideal <> in no variables and no components
  {
    std::ostringstream out;
    mgb::IdealStreamLog<> stream(out, 101, 0, 0);
    ASSERT_EQ(101, stream.modulus());
    ASSERT_EQ(0, stream.varCount());
    stream.idealBegin(0);
    stream.idealDone();
  }

  // The ideal <0> in no variables and no components
  {
    std::ostringstream out;
    mgb::IdealStreamLog<> stream(out, 101, 0, 0);
    ASSERT_EQ(101, stream.modulus());
    ASSERT_EQ(0, stream.varCount());
    stream.idealBegin(1);
      stream.appendPolynomialBegin(0); // 0
      stream.appendPolynomialDone();
    stream.idealDone();
  }

  // The ideal <1, 0> in no variables and 1 component
  {
    std::ostringstream out;
    mgb::IdealStreamLog<> stream(out, 101, 0, 0);
    ASSERT_EQ(101, stream.modulus());
    ASSERT_EQ(0, stream.varCount());
    stream.idealBegin(2);
      stream.appendPolynomialBegin(0); // 1
        stream.appendTermBegin(0);
        stream.appendTermDone(1);
      stream.appendPolynomialDone();
      stream.appendPolynomialBegin(0); // 0
      stream.appendPolynomialDone();
    stream.idealDone();
  }
}

TEST(MathicGBLib, ZeroIdealGB) {
  mgb::GroebnerConfiguration configuration(2, 0, 0);
  mgb::GroebnerInputIdealStream input(configuration);
  std::ostringstream out;
  mgb::IdealStreamLog<> logStream(out, 2, 0, 0);

  input.idealBegin(0);
  input.idealDone();
  mgb::computeGroebnerBasis(input, logStream);

  const auto msg =
    "IdealStreamLog s(stream, 2, 0, 0);\n"
    "s.idealBegin(0); // polyCount\n"
    "s.idealDone();\n";
  EXPECT_EQ(msg, out.str());
}

TEST(MathicGBLib, OneIdealGB) {
  mgb::GroebnerConfiguration configuration(2, 0, 1);
  mgb::GroebnerInputIdealStream input(configuration);
  std::ostringstream out;
  mgb::IdealStreamLog<> logStream(out, 2, 0, 1);

  input.idealBegin(1);
  input.appendPolynomialBegin(1);
  input.appendTermBegin(0);
  input.appendTermDone(1);
  input.appendPolynomialDone();
  input.idealDone();
  mgb::computeGroebnerBasis(input, logStream);

  const auto msg =
     "IdealStreamLog s(stream, 2, 0, 1);\n"
     "s.idealBegin(1); // polyCount\n"
     "s.appendPolynomialBegin(1);\n"
     "s.appendTermBegin(0);\n"
     "s.appendTermDone(1); // coefficient\n"
     "s.appendPolynomialDone();\n"
     "s.idealDone();\n";
  EXPECT_EQ(msg, out.str());
}

TEST(MathicGBLib, EasyGB) {
  mgb::GroebnerConfiguration configuration(101, 3, 1);
  mgb::GroebnerInputIdealStream input(configuration);
  std::ostringstream computedStr;
  mgb::IdealStreamLog<> computed(computedStr, 101, 3, 1);
  mgb::IdealStreamChecker<decltype(computed)> checked(computed);

  makeBasis(input);
  mgb::computeGroebnerBasis(input, checked);

  std::ostringstream correctStr;
  mgb::IdealStreamLog<> correct(correctStr, 101, 3, 1);
  mgb::IdealStreamChecker<decltype(correct)> correctChecked(correct);
  makeGroebnerBasis(correctChecked);

  EXPECT_EQ(correctStr.str(), computedStr.str())
    << "\nDisplayed expected:\n" << correctStr.str()
    << "\nDisplayed computed:\n" << computedStr.str();
}

TEST(MathicGBLib, EasyReGB) {
  mgb::GroebnerConfiguration configuration(101, 3, 1);
  mgb::GroebnerInputIdealStream input(configuration);
  std::ostringstream computedStr;
  mgb::IdealStreamLog<> computed(computedStr, 101, 3, 1);
  mgb::IdealStreamChecker<decltype(computed)> checked(computed);

  makeGroebnerBasis(input);
  mgb::computeGroebnerBasis(input, checked);

  std::ostringstream correctStr;
  mgb::IdealStreamLog<> correct(correctStr, 101, 3, 1);
  mgb::IdealStreamChecker<decltype(correct)> correctChecked(correct);
  makeGroebnerBasis(correctChecked);

  EXPECT_EQ(correctStr.str(), computedStr.str())
    << "\nDisplayed expected:\n" << correctStr.str()
    << "\nDisplayed computed:\n" << computedStr.str();
}

TEST(MathicGBLib, Cyclic5) {
  for (int i = 0; i < 2; ++i) {
    mgb::GroebnerConfiguration configuration(101, 5, 1);
    const auto reducer = i == 0 ?
      mgb::GroebnerConfiguration::ClassicReducer :
      mgb::GroebnerConfiguration::MatrixReducer;
    configuration.setReducer(reducer);
    mgb::GroebnerInputIdealStream input(configuration);
    makeCyclic5Basis(input);

    mgb::NullIdealStream computed
      (input.modulus(), input.varCount(), input.comCount());

    mgb::computeGroebnerBasis(input, computed);
  }
}

TEST(MathicGBLib, SimpleModuleIdeal) {
  for (int i = 0; i < 2; ++i) {
    mgb::GroebnerConfiguration configuration(101, 5, 4);
    const auto reducer = i == 0 ?
      mgb::GroebnerConfiguration::ClassicReducer :
      mgb::GroebnerConfiguration::MatrixReducer;
    configuration.setReducer(reducer);
    mgb::GroebnerInputIdealStream input(configuration);
    std::ostringstream computedStr;
    mgb::IdealStreamLog<> computed(computedStr, 101, 5, 4);
    mgb::IdealStreamChecker<decltype(computed)> checked(computed);

    makeSimpleModuleBasis(input);
    mgb::computeGroebnerBasis(input, checked);

    std::ostringstream correctStr;
    mgb::IdealStreamLog<> correct(correctStr, 101, 5, 4);
    mgb::IdealStreamChecker<decltype(correct)> correctChecked(correct);
    makeSimpleModuleGroebnerBasis(correctChecked);

    EXPECT_EQ(correctStr.str(), computedStr.str())
      << "\nDisplayed expected:\n" << correctStr.str()
      << "\nDisplayed computed:\n" << computedStr.str();
  }
}

namespace {
  class TestCallback : public mgb::GroebnerConfiguration::Callback {
  public:
    TestCallback(int count, Action action): mCount(count), mAction(action) {}

    virtual Action call() {
      --mCount;
      return mCount == 0 ? mAction : ContinueAction;
    }

  private:
    int mCount;
    const Action mAction;
  };
}

TEST(MathicGBLib, EarlyExit) {
  typedef mgb::GroebnerConfiguration::Callback::Action Action;
  auto check = [](bool useClassic, int count, Action action) {
    mgb::GroebnerConfiguration configuration(101, 5, 1);
    const auto reducer = useClassic ?
      mgb::GroebnerConfiguration::ClassicReducer :
      mgb::GroebnerConfiguration::MatrixReducer;
    configuration.setReducer(reducer);
    TestCallback callback(count, action);
    configuration.setCallback(&callback);
    mgb::GroebnerInputIdealStream input(configuration);
    makeCyclic5Basis(input);

    std::ostringstream strOut;
    mgb::IdealStreamLog<> out(strOut, 101, 5, 1);
    mgb::computeGroebnerBasis(input, out);
    return strOut.str().size();
  };

  for (int useClassic = 0; useClassic < 2; ++useClassic) {
    size_t none = check(useClassic, 5, Action::StopWithNoOutputAction);
    size_t minSize = check(useClassic, 1, Action::StopWithPartialOutputAction);
    size_t midSize = check(useClassic, 4, Action::StopWithPartialOutputAction);
    size_t maxSize = check(useClassic, 1, Action::ContinueAction);
    ASSERT_LT(none, 38u); // the stream writes a header even for no output
    ASSERT_LT(none, minSize);
    ASSERT_LT(minSize, midSize);
    ASSERT_LT(midSize, maxSize);
  }
}

TEST(MathicGBLib, SimpleEliminationGB) {
  typedef mgb::GroebnerConfiguration::Exponent Exponent;
  Exponent v[] = {1,0,0,0,  1,1,1,1};
  std::vector<Exponent> gradings(v, v + sizeof(v)/sizeof(*v));

  for (int i = 0; i < 2; ++i) {
    mgb::GroebnerConfiguration configuration(101, 4, 1);
    const auto reducer = i == 0 ?
      mgb::GroebnerConfiguration::ClassicReducer :
      mgb::GroebnerConfiguration::MatrixReducer;
    configuration.setReducer(reducer);
    configuration.setMonomialOrder(
      mgb::GroebnerConfiguration::BaseOrder::RevLexDescendingBaseOrder, 
      gradings
    );

    mgb::GroebnerInputIdealStream input(configuration);
    std::ostringstream computedStr;
    mgb::IdealStreamLog<> computed(computedStr, 101, 4, 1);
    mgb::IdealStreamChecker<decltype(computed)> checked(computed);

    makeSimpleIdeal(input);
    mgb::computeGroebnerBasis(input, checked);

    std::ostringstream correctStr;
    mgb::IdealStreamLog<> correct(correctStr, 101, 4, 1);
    mgb::IdealStreamChecker<decltype(correct)> correctChecked(correct);
    makeSimpleIdealGroebnerBasis(correctChecked);

    EXPECT_EQ(correctStr.str(), computedStr.str())
      << "\nDisplayed expected:\n" << correctStr.str()
      << "\nDisplayed computed:\n" << computedStr.str();
#if 0
    mgb::GroebnerInputIdealStream input(configuration);
    makeSimpleIdeal(input);
    mgb::NullIdealStream computed(input.modulus(), input.varCount());
    mgb::computeGroebnerBasis(input, computed);
#endif
  }
}

TEST(MathicGBLib, GlobalOrderOrNot) {
  mgb::GroebnerConfiguration conf(101, 4, 1);
  const auto lex =
    mgb::GroebnerConfiguration::BaseOrder::LexAscendingBaseOrder;
  const auto revLex =
    mgb::GroebnerConfiguration::BaseOrder::RevLexDescendingBaseOrder;

  typedef mgb::GroebnerConfiguration::Exponent Exponent;
  std::vector<Exponent> mat;

  mat.clear();
  ASSERT_TRUE(conf.setMonomialOrder(lex, mat));
  ASSERT_FALSE(conf.setMonomialOrder(revLex, mat));

  Exponent mat2[] = {1,2,3,0};
  mat.assign(mat2, mat2 + sizeof(mat2)/sizeof(*mat2));
  ASSERT_TRUE(conf.setMonomialOrder(lex, mat));
  ASSERT_FALSE(conf.setMonomialOrder(revLex, mat));

  Exponent mat3[] = {1,0,0,0,  -3,0,1,2};
  mat.assign(mat3, mat3 + sizeof(mat3)/sizeof(*mat3));
  ASSERT_TRUE(conf.setMonomialOrder(lex, mat));
  ASSERT_FALSE(conf.setMonomialOrder(revLex, mat));

  Exponent mat4[] = {1,1,0,0,  3,0,1,2};
  mat.assign(mat4, mat4 + sizeof(mat4)/sizeof(*mat4));
  ASSERT_TRUE(conf.setMonomialOrder(lex, mat));
  ASSERT_TRUE(conf.setMonomialOrder(revLex, mat));

  Exponent mat5[] = {1,0,0,0,  3,1,1,-2};
  mat.assign(mat5, mat5 + sizeof(mat5)/sizeof(*mat5));
  ASSERT_FALSE(conf.setMonomialOrder(lex, mat));
  ASSERT_FALSE(conf.setMonomialOrder(revLex, mat));
}
