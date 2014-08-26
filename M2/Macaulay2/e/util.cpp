// Copyright 2014  Michael E. Stillman

#include "util.hpp"
#include <string>

M2_arrayint stdvector_to_M2_arrayint(std::vector<size_t> &v)
{
  M2_arrayint result = M2_makearrayint(static_cast<int>(v.size()));
  for (size_t i = 0; i < v.size(); i++)
    result->array[i] = static_cast<int>(v[i]);
  return result;
}

M2_arrayint stdvector_to_M2_arrayint(std::vector<long> &v)
{
  M2_arrayint result = M2_makearrayint(static_cast<int>(v.size()));
  for (size_t i = 0; i < v.size(); i++)
    result->array[i] = static_cast<int>(v[i]);
  return result;
}

void M2_ArrayString_to_stdvector(M2_ArrayString strs, std::vector<std::string> &result)
{
  for (size_t i = 0; i< strs->len; i++)
    {
      M2_string a = strs->array[i];
      std::string b(a->array, a->len);
      result.push_back(b);
    }
}


// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:

