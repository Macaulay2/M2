#ifndef __timing_hpp_
#define __timing_hpp_

/**
 * @file timing.hpp
 * @brief Inline `std::chrono::steady_clock` wrappers and elapsed-time conversion helpers.
 *
 * Provides `timer()` / `now()` (both returning a
 * `std::chrono::steady_clock::time_point`, hard-coded to the
 * steady clock) for capturing high-resolution timestamps, plus
 * templated `nanoseconds(duration)` / `microseconds(duration)`
 * / `seconds(duration)` helpers that pull the count out of any
 * `std::chrono::duration`-shaped type via `duration_cast`
 * (`seconds` returns a `double` by scaling the nanosecond
 * count). The `TIME(t, call)` macro at the bottom wraps a
 * `call` with `now()` brackets and accumulates the nanosecond
 * delta into `t`, which is how the GB and resolution drivers
 * collect their `clock_*` timing fields.
 *
 * Used by `SLP-imp.hpp`'s NAG evaluator (per-step timestamps
 * that drive continuation-step heuristics) and by the F4 GB
 * engines for the per-reduction trace timings dumped on
 * `M2_gbTrace` runs. The `StopConditions` machinery in
 * `comp.hpp` does **not** have a time-limit field --- its
 * stop conditions are degree / basis-element / syzygy / pair /
 * codim / subring / length limits only.
 *
 * @see SLP-imp.hpp
 */

#include <chrono>

inline std::chrono::steady_clock::time_point timer()
{
  return std::chrono::steady_clock::now();
}
inline std::chrono::steady_clock::time_point now()
{
  return std::chrono::steady_clock::now();
}

template <typename DurationType>
long nanoseconds(DurationType time_diff)
{
  return std::chrono::duration_cast<std::chrono::nanoseconds>(time_diff)
      .count();
}

template <typename DurationType>
long microseconds(DurationType time_diff)
{
  return std::chrono::duration_cast<std::chrono::microseconds>(time_diff)
      .count();
}

template <typename DurationType>
double seconds(DurationType time_diff)
{
  return 1.0e-9 *
         static_cast<double>(
             std::chrono::duration_cast<std::chrono::nanoseconds>(time_diff)
                 .count());
}

#define TIME(t, call)                  \
  {                                    \
    auto __now1 = now();               \
    call;                              \
    auto __now2 = now();               \
    t += nanoseconds(__now2 - __now1); \
  }

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
