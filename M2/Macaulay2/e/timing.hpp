#ifndef __timing_hpp_
#define __timing_hpp_
#include <chrono>

inline std::chrono::steady_clock::time_point timer() { return std::chrono::steady_clock::now(); }
inline std::chrono::steady_clock::time_point now() { return std::chrono::steady_clock::now(); }

template<typename DurationType>
long nanoseconds(DurationType time_diff)
{
  return std::chrono::duration_cast<std::chrono::nanoseconds>(time_diff).count();
}

template<typename DurationType>
long microseconds(DurationType time_diff)
{
  return std::chrono::duration_cast<std::chrono::microseconds>(time_diff).count();
}

template<typename DurationType>
double seconds(DurationType time_diff)
{
  return 1.0e-9 * static_cast<double>(std::chrono::duration_cast<std::chrono::nanoseconds>(time_diff).count());
}

#define TIME(t,call) {auto __now1 = now(); call; auto __now2 = now(); t += nanoseconds(__now2-__now1);}

#endif
// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
