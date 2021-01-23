#ifdef M2_STDINC_M2_GUARD
#error stdinc-m2.hpp included twice. Only include stdinc-m2.hpp once per cpp file.
#endif
#define M2_STDINC_M2_GUARD

#warning TODO: remove e/stdinc-m2.hpp

#include <cstddef>
#include <memory>
#include <utility>

template <typename T, typename... Args>
std::unique_ptr<T> make_unique(Args&&... args)
{
  return std::unique_ptr<T>(new T(std::forward<Args>(args)...));
}

#ifndef NDEBUG
#include <cassert>
#define M2_ASSERT(X) \
  do                 \
    {                \
      assert(X);     \
    }                \
  while (0)
#define MATHICGB_IF_DEBUG(X) X
#else
#define M2_ASSERT(X)
#define MATHICGB_IF_DEBUG(X)
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
