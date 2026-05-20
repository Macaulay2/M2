#ifndef _exceptions_h_
#define _exceptions_h_

/**
 * @file exceptions.hpp
 * @brief `namespace exc` --- internal C++ exception types and the `TRY` / `CATCH` macro pair.
 *
 * Declares a small hierarchy rooted at `std::runtime_error`:
 * `engine_error` (the base catch-all), `overflow_exception`
 * (arithmetic overflow, thrown by `overflow.hpp`),
 * `division_by_zero_error` (with both a `(const std::string&)`
 * constructor and a zero-arg fallback that fills in the fixed
 * message `"division by zero"`), and `internal_error`
 * (invariant violation that should not be reachable). These
 * exist so templated engine code can unwind cleanly through
 * arbitrary call depth without checking an error return after
 * every operation. The companion `TRY` / `CATCH` macros at the
 * bottom standardise the boundary translation: the `CATCH`
 * block catches `const exc::engine_error&`, routes its
 * `what()` into `ERROR(...)` from `error.h`, and `return NULL`s
 * --- so the message reaches the interpreter via the file-
 * static error slot in `error.c`.
 *
 * C++ exceptions must not propagate to the interpreter (the
 * `.d`-generated C glue cannot unwind C++ stacks), so every
 * entry point from the interpreter side either runs inside
 * `TRY/CATCH` or is responsible for translating manually.
 * Concrete arings, overflow checks, and engine invariant
 * assertions are the main sources of throws.
 *
 * @see error.h
 * @see overflow.hpp
 */

#include <stdexcept>
#include <string>

namespace exc {
struct engine_error : public std::runtime_error
{
  explicit engine_error(const std::string &msg) : std::runtime_error(msg) {}
};
struct overflow_exception : public engine_error
{
  explicit overflow_exception(const std::string &msg) : engine_error(msg) {}
};
struct division_by_zero_error : public engine_error
{
  explicit division_by_zero_error(const std::string &msg) : engine_error(msg) {}
  explicit division_by_zero_error() : engine_error(std::string{"division by zero"}) {}
};
struct internal_error : public engine_error
{
  explicit internal_error(const std::string &msg) : engine_error(msg) {}
};
}

#define TRY \
  try       \
    {
#define CATCH                         \
  }                                   \
  catch (const exc::engine_error& __x424621) \
  {                                   \
    ERROR(__x424621.what());          \
    return NULL;                      \
  }

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
