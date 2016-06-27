// Copyright 2014  Michael E. Stillman

#ifndef _util_hpp_
#define _util_hpp_

#include <vector>
#include "engine-includes.hpp"

//M2_arrayint stdvector_to_M2_arrayint(std::vector<size_t> &v);
//M2_arrayint stdvector_to_M2_arrayint(std::vector<long> &v);
void M2_ArrayString_to_stdvector(M2_ArrayString strs, std::vector<std::string> &result);

template<typename inttype>
M2_arrayint stdvector_to_M2_arrayint(std::vector<inttype> &v)
{
  M2_arrayint result = M2_makearrayint(static_cast<int>(v.size()));
  for (size_t i = 0; i < v.size(); i++)
    result->array[i] = static_cast<int>(v[i]);
  return result;
}


#endif

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

