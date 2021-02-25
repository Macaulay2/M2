// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#include "stdinc.h"
#include "Scanner.hpp"

#include "mathic/mathic.h"
#include <limits>
#include <sstream>
#include <cstring>

MATHICGB_NAMESPACE_BEGIN

static const size_t BufferSize =
#ifdef MATHICGB_DEBUG
  1;
#else
  10 * 1024;
#endif



void reportSyntaxError(std::string s, uint64 lineNumber) {
  std::ostringstream out;
  out << "Syntax error on line " << lineNumber << ": " << s;
  mathic::reportError(out.str());
}

void Scanner::reportError(std::string msg) const {
  reportSyntaxError(msg, lineCount());
}


Scanner::Scanner(FILE* input):
  mFile(input),
  mStream(0),
  mLineCount(1),
  mChar(' '),
  mBuffer(BufferSize),
  mBufferPos(mBuffer.end())
{
  get();
}

Scanner::Scanner(std::istream& input):
  mFile(0),
  mStream(&input),
  mLineCount(1),
  mChar(' '),
  mBuffer(BufferSize),
  mBufferPos(mBuffer.end())
{
  get();
}

Scanner::Scanner(const char* const input):
  mFile(0),
  mStream(0),
  mLineCount(1),
  mChar(' '),
  mBuffer(input, input + std::strlen(input)),
  mBufferPos(mBuffer.begin())
{
  get();
}

Scanner::Scanner(const std::string& input):
  mFile(0),
  mStream(0),
  mLineCount(1),
  mChar(' '),
  mBuffer(input.begin(), input.end()),
  mBufferPos(mBuffer.begin())
{
  get();
}

bool Scanner::match(const char* const str) {
  eatWhite();
  MATHICGB_ASSERT(str != 0);
  const auto size = std::strlen(str);
  if (!ensureBuffer(size))
    return false;
  if (size == 0)
    return true;
  if (peek() != *str)
    return false;
  if (std::strncmp(&*mBufferPos, str + 1, size - 1) != 0)
    return false;
  ignore(size);
  return true;
}

bool Scanner::ensureBuffer(size_t min) {
  const auto got = size_t(std::distance(mBufferPos, mBuffer.end()) + 1);
  return got >= min || readBuffer(min - got);
}

void Scanner::expect(const char* str) {
  MATHICGB_ASSERT(str != 0);
  eatWhite();

  const char* it = str;
  while (*it != '\0') {
    int character = get();
    if (*it == character) {
      ++it;
      continue;
    }

    // Read the rest of what is there to improve error message.
    // TODO: read at least one char in total even if not alnum.
    std::ostringstream got;
    if (character == EOF && it == str)
      got << "no more input";
    else {
      got << '\"' << std::string(str, it);
      if (isalnum(character))
        got << static_cast<char>(character);
      while (isalnum(peek()))
        got << static_cast<char>(get());
      got << '\"';
    }

    reportErrorUnexpectedToken(str, got.str());
  }
}

void Scanner::expectEOF() {
  eatWhite();
  if (get() != EOF)
    reportErrorUnexpectedToken("no more input", "");
}

void Scanner::errorExpectTwo(char a, char b, int got) {
  MATHICGB_ASSERT(a != got && b != got);
  std::ostringstream err;
  err << '\'' << a << "' or '" << b << '\'';
  reportErrorUnexpectedToken(err.str(), got);
}

void Scanner::errorExpectOne(char expected, int got) {
  MATHICGB_ASSERT(expected != got);
  std::ostringstream err;
  err << '\'' << expected << '\'';
  reportErrorUnexpectedToken(err.str(), got);
}

void Scanner::reportErrorUnexpectedToken(const std::string& expected, int got) {
  std::ostringstream gotDescription;
  if (got == EOF)
    gotDescription << "no more input";
  else
    gotDescription << '\'' << static_cast<char>(got) << '\'';
  reportErrorUnexpectedToken(expected, gotDescription.str());
}

void Scanner::reportErrorUnexpectedToken(
  const std::string& expected,
  const std::string& got
) {
  std::ostringstream errorMsg;
  errorMsg << "Expected " << expected;
  if (got != "")
    errorMsg << ", but got " << got;
  errorMsg << '.';
  reportError(errorMsg.str());
}

bool Scanner::readBuffer(size_t minRead) {
  auto saveCount = std::distance(mBufferPos, mBuffer.end());
  if (mBufferPos != mBuffer.begin() && mBufferPos != mBuffer.end())
    std::copy(mBufferPos, mBuffer.end(), mBuffer.begin());
  mBuffer.resize(std::max(saveCount + minRead, mBuffer.capacity()));
  auto readInto = reinterpret_cast<char*>(mBuffer.data() + saveCount);
  auto readCount = mBuffer.size() - saveCount;

  size_t didReadCount = 0;
  if (mFile != 0) {
    didReadCount = fread(readInto, 1, readCount, mFile);
  } else if (mStream != 0) {
    mStream->read(readInto, readCount);
    const std::streamsize maxSizeT =
      std::numeric_limits<std::make_signed<size_t>::type>::max();
    if (mStream->gcount() > maxSizeT)
      throw std::bad_alloc();
    didReadCount = static_cast<size_t>(mStream->gcount());
  }
  mBuffer.resize(saveCount + didReadCount);
  mBufferPos = mBuffer.begin();

  return didReadCount >= minRead;
}

MATHICGB_NAMESPACE_END
