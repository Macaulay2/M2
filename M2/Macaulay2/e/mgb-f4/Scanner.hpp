// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_SCANNER_GUARD
#define MATHICGB_SCANNER_GUARD

#include "PrimeField.hpp"
#include "Unchar.hpp"
#include <string>
#include <cstdio>
#include <vector>
#include <type_traits>
#include <limits>
#include <sstream>
#include <istream>
#include <cctype>

MATHICGB_NAMESPACE_BEGIN

/// This class offers an input interface which is more convenient and
/// often more efficient than dealing with a FILE* or std::istream
/// directly. It keeps track of the current line number to report
/// better error messages. Only one Scanner should be reading from a
/// given FILE* or std::istream due to buffering and line number counting.
///
/// All input methods whose documentation does not specifically say
/// otherwise skip whitespace.
///
/// There are four concepts for consuming input through a Scanner:
///
/// Read X: Require an X to be in the input, and return what is read.
///
/// Expect X: Require the exact value X to be in the input and skip past it.
///
/// Match X: Return true if the exact value X is in the input, and in that case
///   skip past it. Otherwise return false and do nothing else.
///
/// MatchRead X: Return true of an X is in the input. In that case,
///   read the X into a reference parameter and return true.
///
/// Peek X: Return true if X is the next thing int he input. Do not skip
///   past anything. May or may not skip whitespace depending on what X is.
///
/// If a requirement is not met, Scanner reports a syntax error.
class Scanner {
public:
  /// Construct a Scanner object reading from the input FILE*.
  Scanner(FILE* input);

  /// Construct a Scanner object reading from the input std::istream.
  Scanner(std::istream& input);

  /// Construct a Scanner object reading from the input string.
  Scanner(const char* const input);

  /// Construct a Scanner object reading from the input string.
  Scanner(const std::string& input);

  /// Reads a single character from the stream.
  int get();

  /// Takes count characters off the stream.
  void ignore(size_t count);

  /// Return true if the next character is c, and in that case skip
  /// past it.
  bool match(char c);

  /// Return true if no more input.
  bool matchEOF();

  bool match(const char* const str);

  /// Require the next character to be equal to expected. This
  /// character is skipped past.
  void expect(char expected);

  /// Require the next character to be equal to a or b. This character
  /// is skipped past.
  void expect(char a, char b);

  /// Require the following characters to be equal to str. These
  /// characters are skipped past.
  void expect(const char* str);

  /// Require the following characters to be equal to str. These
  /// characters are skipped past.
  void expect(const std::string& str) {expect(str.c_str());}

  /// Require that there is no more input.
  void expectEOF();

  /// Reads a T. T must be an integer type with a std::numeric_limits
  /// specialization. Negative numbers are allows if T is signed.
  template<class T>
  T readInteger(bool negate = false);

  template<class T>
  typename PrimeField<T>::Element readModular(
    const PrimeField<T>& field,
    const bool negate = false
  );

  /// Reads a T if it is there. Does not recognize + or - as the start
  /// of an integer.
  template<class T>
  bool matchReadIntegerNoSign(T& t, bool negate = false);

  /// Returns the next character or EOF. Does not skip whitespace.
  int peek() {return mChar;}

  /// Returns true if the next character is a digit. Does not skip
  /// whitespace.
  bool peekDigit() {return std::isdigit(peek());}

  bool peekAlpha() {return std::isalpha(peek());}

  /// Returns true if the next character is whitespace. Does not skip
  /// whitespace. Whitespace is defined by std::isspace().
  bool peekWhite() {return isspace(peek());}

  /// Returns true if the next character is + or -. Does not skip
  /// whitespace.
  bool peekSign() {return peek() == '+' || peek() == '-';}

  /// Returns the number of newlines seen plus one. Does not skip
  /// whitespace.
  uint64 lineCount() const {return mLineCount;}

  /// Reads past any whitespace.
  inline void eatWhite();

  void reportError(std::string msg) const;

private:
  void errorExpectTwo(char a, char b, int got);
  void errorExpectOne(char expected, int got);

  void reportErrorUnexpectedToken(const std::string& expected, int got);
  void reportErrorUnexpectedToken
    (const std::string& expected, const std::string& got);

  bool ensureBuffer(size_t min);
  bool readBuffer(size_t minRead);

  FILE* mFile;
  std::istream* mStream;
  uint64 mLineCount;
  int mChar; // next character on stream

  std::vector<char> mBuffer;
  std::vector<char>::iterator mBufferPos;
};

inline bool Scanner::matchEOF() {
  eatWhite();
  return peek() == EOF;
}

inline bool Scanner::match(char c) {
  eatWhite();
  if (c == peek()) {
    get();
    return true;
  } else
    return false;
}

inline void Scanner::expect(char a, char b) {
  eatWhite();
  int got = get();
  if (got != a && got != b)
    errorExpectTwo(a, b, got);
}

inline void Scanner::expect(char expected) {
  eatWhite();
  int got = get();
  if (got != expected)
    errorExpectOne(expected, got);
}

inline void Scanner::eatWhite() {
  while (peekWhite())
    get();
}

inline int Scanner::get() {
  if (mChar == '\n')
    ++mLineCount;
  int oldChar = mChar;
  if (mBufferPos == mBuffer.end() && !readBuffer(1))
    mChar = EOF;
  else {
    mChar = *mBufferPos;
    ++mBufferPos;
  }
  return oldChar;
}

inline void Scanner::ignore(size_t count) {
  for (size_t i = 0; i < count; ++i)
    get();
}



template<class T>
T Scanner::readInteger(const bool negate) {
  static_assert(std::numeric_limits<T>::is_integer, "");

  eatWhite();
  const bool minus = !match('+') && match('-');
  const bool positive = minus == negate;
  if (!peekDigit())
    reportErrorUnexpectedToken("an integer", "");
  // Skip leading zeroes and return if the number is zero.
  if (peek() == '0') {
    while (peek() == '0')
      get();
    if (!peekDigit())
      return static_cast<T>(0);
  }

  MATHICGB_ASSERT(peekDigit());
  MATHICGB_ASSERT(peek() != 0);

  // Checking this here allows us to recognize -0 as non-negative.
  if (!positive && !std::numeric_limits<T>::is_signed)
    reportErrorUnexpectedToken("a positive integer", "");

  const auto min = std::numeric_limits<T>::min();
  const auto max = std::numeric_limits<T>::max();
  auto t = static_cast<T>(0);
  while (peekDigit()) {
    const auto c = static_cast<char>(get());
    const auto d = positive ? c - '0' : -(c - '0');
    if (positive ? t > (max - d) / 10 : t < (min - d) / 10) {
      std::ostringstream err;
      err << "an integer in the range [" << unchar(min)
          << ", " << unchar(max) << ']';
      reportErrorUnexpectedToken(err.str(), "");
    }
    t = t * 10 + d;
  }
  MATHICGB_ASSERT(t != static_cast<T>(0)); // We already handled zero above.
  return t;
}

template<class T>
typename PrimeField<T>::Element Scanner::readModular(
  const PrimeField<T>& field,
  const bool negate
) {
  static_assert(std::numeric_limits<T>::is_integer, "");

  // Otherwise we need to consider that the most negative value's
  // negative cannot be represented as a positive number when reading.
  // todo: reinstate this assert once we get rid of signed coefficients.
  //static_assert(!std::is_signed<T>::value, "");

  eatWhite();
  const bool minus = !match('+') && match('-');
  const bool positive = minus == negate;
  if (!peekDigit())
    reportErrorUnexpectedToken("an integer", "");
  const auto e = field.toElement(readInteger<T>(false));
  return positive ? e : field.negative(e);
}

template<class T>
bool Scanner::matchReadIntegerNoSign(T& t, bool negate) {
  if (peekDigit()) {
    t = readInteger<T>(negate);
    return true;
  } else
    return false;
}

MATHICGB_NAMESPACE_END
#endif
