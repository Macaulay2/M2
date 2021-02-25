#include "DivMask.h"

#ifdef MATHIC_TRACK_DIV_MASK_HIT_RATIO
namespace mathic {
  namespace DivMaskStats {
    unsigned long maskComputes = 0;
    unsigned long maskChecks = 0;
    unsigned long maskHits = 0;
    unsigned long divChecks = 0;
    unsigned long divDivides = 0;
    unsigned long divHits = 0;
  }
}
#else
// The purpose of dummy is to silence a MSVC linker warning
// that says that this file is not adding anything to the build.
// commented out since it gives unused warning too:
//   namespace {void dummy(){}}
#endif
