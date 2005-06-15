#include "lingb.hpp"

#include "SPairSet.cpp"
#include "linalgGB.cpp"

template class LinAlgGB<CoefficientRingZZp>;
template int SPairSet::find_new_pairs<>(
   const LinAlgGB<CoefficientRingZZp>::gb_array &gb,
   bool remove_disjoints);


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// End:
