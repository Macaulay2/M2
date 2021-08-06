// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_F4_REDUCER_GUARD
#define MATHICGB_F4_REDUCER_GUARD

#include <string>

MATHICGB_NAMESPACE_BEGIN

class Reducer;
class PolyRing;

/// Create an F4 reducer with extra parameters for writing out the matrix.
/// Set file to "" to disable writing of matrices.
std::unique_ptr<Reducer> makeF4Reducer(
 const PolyRing& ring,
 bool oldType,
 std::string file,
 size_t minEntries
);

// This translation unit has to expose something that is needed elsewhere.
// Otherwise, the compiler will think it is not needed and exclude the
// whole thing, despite there being important global objects in the .cpp file.
void f4ReducerDependency();

MATHICGB_NAMESPACE_END

#endif
