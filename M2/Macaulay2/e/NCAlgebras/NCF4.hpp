#ifndef __nc_f4_hpp__
#define __nc_f4_hpp__

#include "../newdelete.hpp"
#include "FreeAlgebra.hpp"
#include "WordTable.hpp"
#include "OverlapTable.hpp"

#include <vector>

class NCF4 : public our_new_delete
{
private:
  const FreeAlgebra& mFreeAlgebra;

  WordTable mWordTable;
  OverlapTable mOverlapTable;

  const ConstPolyList mInput;
  std::vector<int> mGeneratorDegrees; // heft degree or sugar degree of corresponding mGroebner element.

  ConstPolyList mGroebner;
  std::vector<int> mGroebnerDegrees; // sugar degree.  -1 means removed.

  bool mIsGraded;
  int mTopComputedDegree;
  int mHardDegreeLimit;
public:
  NCF4(const FreeAlgebra& A,
       const ConstPolyList& input,
       int hardDegreeLimit,
       int strategy
       );

  const FreeAlgebra& freeAlgebra() const { return mFreeAlgebra; }
};
  
#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e  "
// indent-tabs-mode: nil
// End:
