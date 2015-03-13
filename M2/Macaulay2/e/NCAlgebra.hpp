#ifndef _nc_algebra_hpp_
#define _nc_algebra_hpp_

#include "ringelem.hpp"
#include "engine.h"

class buffer;
class Ring;

#include "ring.hpp"

/**
 * \ingroup polynomialrings
 */
class NCFreeAlgebra : public Ring
{
  NCFreeAlgebra(const Ring* K,
                M2_ArrayString names);
public:
  static NCFreeAlgebra* create(const Ring* K,
                               M2_ArrayString names);

private:
  std::vector<std::string> mVariableNames;
  const Ring& mCoefficientRing;
};


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
