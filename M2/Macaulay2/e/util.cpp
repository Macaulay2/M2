// Copyright 2014-2016  Michael E. Stillman

#include "stdinc-m2.hpp"
#include "util.hpp"
#include <string>

void M2_ArrayString_to_stdvector(M2_ArrayString strs,
                                 std::vector<std::string> &result)
{
  for (size_t i = 0; i < strs->len; i++)
    {
      M2_string a = strs->array[i];
      std::string b((char *)a->array, a->len);
      result.push_back(b);
    }
}

std::vector<std::string> M2_ArrayString_to_stdvector(M2_ArrayString strs)
{
  std::vector<std::string> result;
  for (size_t i = 0; i < strs->len; i++)
    {
      M2_string a = strs->array[i];
      result.emplace_back((char *)a->array, a->len);
    }
  return result;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
