#ifndef M2TBB_HPP
#define M2TBB_HPP

/**
 * @file m2tbb.hpp
 * @brief Engine TBB shim --- single point of inclusion for every parallel primitive.
 *
 * Brings in `mathicgb/mtbb.hpp` after consulting `M2/config.h`
 * for the configure-time `WITH_TBB` flag. When TBB is not
 * available the header defines `MATHICGB_NO_TBB`, which routes
 * mathicgb's abstractions (`parallel_for`, `parallel_for_each`,
 * atomic counters, mutexes, ...) to the serial fallback
 * versions in the second half of `mtbb.hpp`. Engine code that
 * wants parallelism must include this header rather than
 * `<tbb/...>` directly so that flipping `WITH_TBB` at configure
 * time is the only change required.
 *
 * Consumers include `f4/f4.hpp`, `VectorArithmetic.hpp`,
 * `NCAlgebras/NCF4.hpp` (parallel row reduction), and the
 * `schreyer-resolution/` files that drive the Schreyer-frame
 * scheduler and the dep-graph.
 *
 * @see schreyer-resolution/res-schreyer-frame.hpp
 * @see NCAlgebras/NCF4.hpp
 */

// The plan: All uses of TBB go through the following interface.

#include <M2/config.h>                    // make sure WITH_TBB is set before including mtbb.hpp

#ifndef WITH_TBB
#define MATHICGB_NO_TBB 1
#endif

#include "mathicgb/mtbb.hpp"

#endif
