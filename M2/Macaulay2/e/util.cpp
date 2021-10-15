// Copyright 2014-2016  Michael E. Stillman

#include "util.hpp"

M2_string string_std_to_M2(const std::string& s)
{
  // The function M2_tostringn does not modify s.data()...
  return M2_tostringn(const_cast<char *>(s.data()), static_cast<int>(s.size()));
}

std::string string_M2_to_std(const M2_string s)
{
  return std::string((char*)(s->array), s->len);
}

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

M2_ArrayString toM2ArrayString(const std::vector<std::string>& strs)
{
  int n = static_cast<int>(
      strs.size());  // needed since M2_ArrayString len field is int
  int i;
  M2_ArrayString a = getmemarraytype(M2_ArrayString, n);
  a->len = n;
  for (i = 0; i < n; i++) a->array[i] = M2_tostring(strs[i].c_str());
  return a;
}

// Local Variables:
// compile-command: "make -C $M2BUILDDIR/Macaulay2/e "
// indent-tabs-mode: nil
// End:
