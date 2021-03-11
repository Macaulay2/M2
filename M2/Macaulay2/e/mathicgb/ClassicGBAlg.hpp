// MathicGB copyright 2012 all rights reserved. MathicGB comes with ABSOLUTELY
// NO WARRANTY and is licensed as GPL v2.0 or later - see LICENSE.txt.
#ifndef MATHICGB_CLASSIC_GB_ALG_GUARD
#define MATHICGB_CLASSIC_GB_ALG_GUARD

#include <functional>

MATHICGB_NAMESPACE_BEGIN

class Reducer;
class Basis;

struct ClassicGBAlgParams {
  Reducer* reducer;
  int monoLookupType;
  bool preferSparseReducers;
  size_t sPairQueueType;

  unsigned int breakAfter;
  unsigned int printInterval;
  unsigned int sPairGroupSize;
  size_t reducerMemoryQuantum;
  bool useAutoTopReduction;
  bool useAutoTailReduction;
  std::function<bool(void)> callback;
};

Basis computeGBClassicAlg(Basis&& inputBasis, ClassicGBAlgParams params);
Basis computeModuleGBClassicAlg(Basis&& inputBasis, ClassicGBAlgParams params);

MATHICGB_NAMESPACE_END
#endif
