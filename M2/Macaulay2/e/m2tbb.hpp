#ifndef M2TBB_HPP
#define M2TBB_HPP

// The plan: All uses of TBB go through the following interface.

#include <M2/config.h>                    // make sure WITH_TBB is set before including mtbb.hpp

#ifndef WITH_TBB
#define MATHICGB_NO_TBB 1
#endif

#include "mathicgb/mtbb.hpp"

#endif
