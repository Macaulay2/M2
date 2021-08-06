// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "mathicgb/stdinc.h"
#include "mathicgb/Scanner.hpp"

#include <gtest/gtest.h>

using namespace mgb;

namespace {
  const char* const alpha = "abcdefghijkl";
  const char* const alphaSpaced = "a bc def ghij kl";
  const char* const alphas[] = {"a", "bc", "def", "ghij", "kl"};
}

TEST(Scanner, NoOp) {
  std::istringstream in;
  Scanner sc(in);
}

TEST(Scanner, PeekAndGet) {
  std::stringstream s(alphaSpaced);
  Scanner in(s);
  for (size_t i = 0; alpha[i] != '\0'; ++i) {
    ASSERT_EQ(alphaSpaced[i], in.peek());
    ASSERT_EQ(alphaSpaced[i], in.get());
  }
}

TEST(Scanner, Match) {
  std::stringstream s(alphaSpaced);
  Scanner in(s);
  for (size_t i = 0; alpha[i] != '\0'; ++i) {
    ASSERT_FALSE(in.match('!'));
    ASSERT_FALSE(in.matchEOF());
    ASSERT_TRUE(in.match(alpha[i]));
  }
  ASSERT_TRUE(in.matchEOF());
}

TEST(Scanner, ExpectChar) {
  std::stringstream s(alphaSpaced);
  Scanner in(s);
  for (size_t i = 0; alpha[i] != '\0'; ++i)
    in.expect(alpha[i]);
  in.expectEOF();
}

TEST(Scanner, ExpectTwoChars) {
  Scanner in(alphaSpaced);
  for (size_t i = 0; alpha[i] != '\0'; ++i) {
    if (i % 2 == 0)
      in.expect('!', alpha[i]);
    else
      in.expect(alpha[i], '!');
  }
  in.expectEOF();
}

TEST(Scanner, ExpectString) {
  Scanner in(alphaSpaced);
  const auto size = sizeof(alphas) / sizeof(*alphas);
  for (size_t i = 0; i < size; ++i) {
    if (i % 2 == 0)
      in.expect(alphas[i]);
    else
      in.expect(std::string(alphas[i]));
  }
}

TEST(Scanner, MatchString) {
  Scanner in(alphaSpaced);
  const auto size = sizeof(alphas) / sizeof(*alphas);
  for (size_t i = 0; i < size; ++i) {
    ASSERT_FALSE(in.match("ef"));
    ASSERT_FALSE(in.match("deq"));
    ASSERT_TRUE(in.match(alphas[i]));
  }
}

TEST(Scanner, readModular) {
  PrimeField<unsigned char> f(11);
  std::stringstream s("0 1 1 +0 -0 +1 -1 15 255 -255");
  Scanner in(s);
  ASSERT_EQ(f.zero(), in.readModular(f));
  ASSERT_EQ(f.one(), in.readModular(f));
  ASSERT_EQ(f.minusOne(), in.readModular(f, true));
  ASSERT_EQ(f.zero(), in.readModular(f));
  ASSERT_EQ(f.zero(), in.readModular(f));
  ASSERT_EQ(f.one(), in.readModular(f));
  ASSERT_EQ(f.minusOne(), in.readModular(f));
  ASSERT_EQ(f.toElement(4), in.readModular(f));
  ASSERT_EQ(f.toElement(2), in.readModular(f));
  ASSERT_EQ(f.toElement(9), in.readModular(f));
}

TEST(Scanner, readInteger) {
  std::stringstream s("0 1 +0 -0 +1 -1 127 -128 128");
  Scanner in(s);
  ASSERT_EQ(0, in.readInteger<signed char>());
  ASSERT_EQ(1, in.readInteger<char>());
  ASSERT_EQ(0, in.readInteger<unsigned char>());
  ASSERT_EQ(0, in.readInteger<char>());
  ASSERT_EQ(1, in.readInteger<char>());
  ASSERT_EQ(-1, in.readInteger<signed char>());
  ASSERT_EQ(127, in.readInteger<char>());
  ASSERT_EQ(-128, in.readInteger<signed char>());
  ASSERT_EQ(-128, in.readInteger<signed char>(true));
}

TEST(Scanner, WhiteAndLineCount) {
  std::stringstream s(" \t\n\rx\n\n\ny");
  Scanner in(s);
  ASSERT_EQ(1, in.lineCount());
  ASSERT_TRUE(in.peek() == ' ');
  ASSERT_TRUE(in.peekWhite());
  in.eatWhite();
  ASSERT_TRUE(in.peek() == 'x');
  ASSERT_TRUE(in.match('x'));
  ASSERT_EQ(2, in.lineCount());
  ASSERT_TRUE(in.match('y'));
  in.expectEOF();
  ASSERT_EQ(5, in.lineCount());
}
