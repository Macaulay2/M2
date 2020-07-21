#include "engine.h"

#include <boost/regex.hpp>
#include <iterator>
#include <iostream>
#include <string>

M2_arrayint rawRegexSearch(const M2_string pattern,
                           const int start,
                           const int range,
                           const M2_string text,
                           const M2_bool ignorecase)
{
  M2_arrayint m = M2_makearrayint(0);
  if (start < 0 || text->len < start) return m;

  auto syntax_options =
      boost::regex_constants::ECMAScript | boost::regex_constants::no_except;
  boost::regex expression(M2_tocharstar(pattern), syntax_options);
  boost::cmatch matches;

  // match_continuous
  // match_posix
  auto head = (const char*)&text->array[start];
  auto tail = (const char*)&text->array[start + range];

  try
    {
      auto status = boost::regex_search(head, tail, matches, expression);
      if (!status) return m;
    }
  catch (std::invalid_argument& err)
    {
      std::cerr << "Exception thrown in regex: " << err.what() << std::endl;
      return m;
    }

  m = M2_makearrayint(2 * matches.size());
  for (auto i = 0; i < matches.size(); i++)
    {
      m->array[2 * i] = std::distance(matches.prefix().first, matches[i].first);
      m->array[2 * i + 1] = std::distance(matches[i].first, matches[i].second);
    }
  return m;
}
