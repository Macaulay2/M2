#include "NCAlgebra.hpp"
#include <vector>
#include <string>
NCFreeAlgebra* NCFreeAlgebra::create(const Ring* K,
                      M2_ArrayString names)
{
  M2_ASSERT(K != nullptr);
  //return new NCFreeAlgebra(K, names);
  return nullptr;
}

NCFreeAlgebra::NCFreeAlgebra(const Ring* K,
                             M2_ArrayString names)
  : mCoefficientRing(*K)
{
  for (auto i=0; i<names->len; i++)
    mVariableNames.push_back(std::string(names->array[i]->array, names->array[i]->len));
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
