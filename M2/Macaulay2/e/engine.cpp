// Copyright 1994-2020 by Michael E. Stillman

#include "interface/random.h"
#include "aring-glue.hpp"    // for initializeRationalRing
#include "engine-exports.h"  // for M2_tostring, M2_string
#include "error.h"           // for error_message
#include "hash.hpp"          // for MutableEngineObject
#include "mem.hpp"           // for doubles, doubling_stash
#include "poly.hpp"          // for PolyRing
#include "style.hpp"         // for GEOHEAP_SIZE

unsigned int MutableEngineObject::mNextMutableHashValue = 13;

// unsigned long mutable_object::next_hash_sequence_number = 1000;
// long object::next_hash_sequence_number = -7;

const int heap_size[GEOHEAP_SIZE] = {4,
                                     16,
                                     64,
                                     256,
                                     1024,
                                     4096,
                                     16384,
                                     65536,
                                     262144,
                                     1048576,
                                     4194304,
                                     16777216,
                                     67108864,
                                     268435456,
                                     1073741824};

static bool initialized = false;

/** Initialize the engine.
 *  This routine must be called before any other engine routine is called.
 *  May be called multiple times.  The subsequent calls do nothing.
 */
extern "C" // TODO: remove when this function is in e/interface
void IM2_initialize()
{
  if (initialized) return;
  initialized = true;
  doubles = new doubling_stash;

  // This next routine initializes: globalZZ, trivial_monoid, trivial_poly_ring,
  // and makes sure their degree rings are interconnected.
  PolyRing::get_trivial_poly_ring();

  initializeRationalRing();

  rawRandomInitialize();
}

/** Engine error handling mechanism.
 *  Any engine routine which encounters an error (e.g. Rings not
 *  the same) often returns a NULL value, and sets an error
 *  message, which can be obtained from this routine.  Any routine that can set
 *  this may return a type such as "MatrixOrNull".  This routine
 *  clears the error message and returns it.
 */

M2_string IM2_last_error_message() { return M2_tostring(error_message()); }

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
