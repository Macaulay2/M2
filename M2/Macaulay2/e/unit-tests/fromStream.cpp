// Read an object from the given stream.
// 'result' must be previously 'init'ed
// 'i' is incremented to directly past the part read
// an exception is raised on bad input?

/**
 * @file unit-tests/fromStream.cpp
 * @brief Per-ring `fromStream` specialisations shared across the legacy `Ring*Test.cpp` suite.
 *
 * Centralises the explicit-instantiation definitions of the
 * `fromStream<T>(istream, R, result)` family declared in
 * `RingTest.hpp`. `fromStream<M2::ARingZZp>` reads an `int` and
 * calls `R.set_from_long`; `fromStream<Z_mod>` skips whitespace,
 * accepts the optional sign, parses digits, and hands back a
 * `ring_elem`; `fromStream<RingZZ>` follows the same shape for
 * arbitrary-precision integers (work in progress --- the
 * digit-collection loop is marked `// NOT DONE BEING WRITTEN`).
 * The unified file keeps the parser quirks in one place so
 * individual test files stay focused on test logic.
 *
 * Companion to `RingTest.hpp` (which declares the templates),
 * `M2-cpp-replacement.cpp` (stub for `system_interrupted`), and
 * `util-polyring-creation.{cpp,hpp}` (one-line ring builders)
 * under the same `file-test-harness` markdown family.
 *
 * @see RingTest.hpp
 * @see aring-zzp.hpp
 * @see ZZp.hpp
 * @see ZZ.hpp
 * @see RingZZpTest.cpp
 */

#include "RingTest.hpp"
#include "aring-zzp.hpp"
#include "ZZp.hpp"

template <typename T>
std::istream &fromStream(std::istream &i,
                         const T &R,
                         typename T::ElementType &result);

template <typename T>
bool fromStream(std::istream &i, const T &R, ring_elem &result);

template <>
std::istream &fromStream<M2::ARingZZp>(std::istream &i,
                                       const M2::ARingZZp &R,
                                       M2::ARingZZp::ElementType &result)
{
  int a;
  i >> a;
  R.set_from_long(result, a);
  return i;
}

template <>
bool fromStream<Z_mod>(std::istream &i, const Z_mod &R, ring_elem &result)
{
  while (isspace(i.peek())) i.get();

  if (!isdigit(i.peek()) && i.peek() != '+' && i.peek() != '-') return false;

  int a;
  i >> a;
  result = R.from_long(a);
  return true;
}

template <>
bool fromStream<RingZZ>(std::istream &i, const RingZZ &R, ring_elem &result)
{
  while (isspace(i.peek())) i.get();

  if (!isdigit(i.peek()) && i.peek() != '+' && i.peek() != '-') return false;

  const int original_s_len = 100;
  char original_s[original_s_len];
  char *s_str = original_s;
  char *s = s_str;
  //  int s_len = original_s_len;
  //  int len = 0;
  while (isdigit(i.peek()))
    {
      // NOT DONE BEING WRITTEN!!!!!!
    }
  *s++ = '\0';

  int a;
  i >> a;
  result = R.from_long(a);
  return true;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e/unit-tests check  "
// indent-tabs-mode: nil
// End:
