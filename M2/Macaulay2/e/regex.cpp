#include "engine.h"

#include <boost/regex.hpp>
#include <iostream>

#define DEBUG_REGEX 0

using namespace boost;

enum RawRegexFlags {
  /* Keep this enum in sync with RawRegexFlags in Macaulay2/m2/regex.m2 */
  REGEX_FLAVOR_ECMAScript = (1 << 0), /* ECMAScript flavor (default) */
  REGEX_FLAVOR_BASIC = (1 << 1),      /* POSIX BRE flavor */
  REGEX_FLAVOR_EXTENDED = (1 << 2),   /* POSIX ERE flavor */

  REGEX_SYNTAX_ICASE = (1 << 8),     /* ignore case */
  REGEX_SYNTAX_NOSUBS = (1 << 9),    /* ignore subexpressions */
  REGEX_SYNTAX_NO_MOD_M = (1 << 14), /* don't match ^ $ with newlines */
  REGEX_SYNTAX_NO_MOD_S = (1 << 15), /* don't match . with newlines */

  REGEX_MATCH_ANY = (1 << 16),        /* return any match */
  REGEX_MATCH_CONTINUOUS = (1 << 17), /* match must start at the beginning */
};

regex rawRegexCompile(const M2_string pattern, const int flags)
{
  regex_constants::syntax_option_type regex_flags =
      regex::no_except; /* don't throw exceptions */
  regex_flags |= flags & REGEX_FLAVOR_ECMAScript ? regex::ECMAScript : 0;
  regex_flags |= flags & REGEX_FLAVOR_BASIC ? regex::basic : 0;
  regex_flags |= flags & REGEX_FLAVOR_EXTENDED ? regex::extended : 0;
  regex_flags |= flags & REGEX_SYNTAX_ICASE ? regex::icase : 0;
  regex_flags |= flags & REGEX_SYNTAX_NOSUBS ? regex::nosubs : 0;
  regex_flags |= flags & REGEX_SYNTAX_NO_MOD_M ? regex::no_mod_m : 0;
  regex_flags |= regex::no_mod_s; /* forced for backwards compatibility */

  regex expression(M2_tocharstar(pattern), regex_flags);
  return expression;
}

M2_arrayint rawRegexSearch(const M2_string pattern,
                           int start,
                           int range,
                           const M2_string text,
                           const int flags)
{
#if DEBUG_REGEX > 0
  std::cerr << "regexp:\t" << M2_tocharstar(pattern) << std::endl
            << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  M2_arrayint m = M2_makearrayint(0);
  if (start < 0 || text->len < start) return m;

  auto expression = rawRegexCompile(pattern, flags);
  if (expression.status() != 0)
    {
      std::cerr << "regex: could not compile pattern: "
                << M2_tocharstar(pattern) << std::endl;
      return m;
    }

  bool status = false;
  cmatch matches {};

  auto lead = start;
  auto head = (const char*)&text->array;
  auto tail = (const char*)&text->array + text->len;

  // https://www.boost.org/doc/libs/1_73_0/libs/regex/doc/html/boost_regex/ref/match_flag_type.html
  regex_constants::match_flag_type flag = match_default;
  flag |= flags & REGEX_MATCH_ANY ? match_any : flag;
  flag |= flags & REGEX_MATCH_CONTINUOUS ? match_continuous : flag;

  if (lead == 0 and range == text->len)
    {
      /* not fully backwards compatible with nonzero range */
      flag |= lead > 0 ? match_prev_avail : flag;

      if (range < 0) start = std::max(0, start + range);
      if (range == 0) flag |= match_continuous;

      status = regex_search(head + lead, tail, matches, expression, flag);
    }
  else
    {
      /* backwards compatible with GNU regex, but doesn't support lookaround */
      flag |= match_continuous;

      if (range >= 0)
        for (; lead <= std::min(start + range, text->len); lead++)
          {
            flag |= lead != 0 ? match_prev_avail : flag;
            status = regex_search(head + lead, tail, matches, expression, flag);
            if (status) break;
          }

      flag |= match_prev_avail;
      if (range < 0)
        for (; std::max(0, start + range) <= lead; lead--)
          {
            flag &= lead == 0 ? ~match_prev_avail : flag;
            status = regex_search(head + lead, tail, matches, expression, flag);
            if (status) break;
          }
    }
  if (!status) return m;

  m = M2_makearrayint(2 * matches.size());
  for (auto i = 0; i < matches.size(); i++)
    {
      m->array[2 * i] =
          lead + std::distance(matches.prefix().first, matches[i].first);
      m->array[2 * i + 1] = std::distance(matches[i].first, matches[i].second);
    }
  return m;
}

M2_string rawRegexReplace(const M2_string pattern,
                          const int start,
                          const int range,
                          const M2_string replacement,
                          const M2_string text,
                          const int flags)
{
#if DEBUG_REGEX > 1
  std::cerr << "regexp:\t" << M2_tocharstar(pattern) << std::endl
            << "subst.:\t" << M2_tocharstar(replacement) << std::endl
            << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  auto expression = rawRegexCompile(pattern, flags);
  if (expression.status() != 0)
    {
      std::cerr << "regex: could not compile pattern: "
                << M2_tocharstar(pattern) << std::endl;
      return text;
    }

  std::ostringstream stream(std::ios::out);
  std::ostream_iterator<char> stream_iter(stream);

  // TODO: add format_first_only option
  regex_constants::match_flag_type flag = format_default;

  auto head = (const char*)&text->array + start;
  auto tail = (const char*)&text->array + start + range;
  auto substitute = M2_tocharstar(replacement);
  regex_replace(stream_iter, head, tail, expression, substitute, flag);

  std::string output(stream.str());
  return M2_tostring(output.c_str());
}

M2_ArrayString rawRegexSelect(const M2_string pattern,
                              int start,
                              int range,
                              const M2_string replacement,
                              const M2_string text,
                              const int flags)
{
  std::vector<std::string> strings;
  std::vector<char*> cstrings;

  while (start < text->len)
    {
      auto match = rawRegexSearch(
          pattern, start, range, text, flags | REGEX_SYNTAX_NOSUBS);
      if (match->len == 0) break;

      auto pair = match->array;
      auto part =
          rawRegexReplace(pattern, pair[0], pair[1], replacement, text, flags);

      std::string str = M2_tocharstar(part);
      strings.push_back(str);

      range = start + range - pair[0] - pair[1];
      start = pair[0] + pair[1];
    }

  cstrings.reserve(strings.size());

  for (auto& str : strings) cstrings.push_back(&str[0]);
  return M2_tostrings(cstrings.size(), cstrings.data());
}
