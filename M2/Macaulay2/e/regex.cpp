#include "engine.h"

#include <boost/regex.hpp>
#include <iostream>

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

M2_arrayint rawRegexSearch(const M2_string pattern,
                           int start,
                           int range,
                           const M2_string text,
                           const int flags)
{
  M2_arrayint m = M2_makearrayint(0);
  if (start < 0 || text->len < start) return m;

  regex_constants::syntax_option_type regex_flags =
      regex::no_except; /* don't throw exceptions */
  regex_flags |= flags & REGEX_FLAVOR_ECMAScript ? regex::ECMAScript : 0;
  regex_flags |= flags & REGEX_FLAVOR_BASIC ? regex::basic : 0;
  regex_flags |= flags & REGEX_FLAVOR_EXTENDED ? regex::extended : 0;
  regex_flags |= flags & REGEX_SYNTAX_ICASE ? regex::icase : 0;
  regex_flags |= flags & REGEX_SYNTAX_NOSUBS ? regex::nosubs : 0;
  regex_flags |= flags & REGEX_SYNTAX_NO_MOD_M ? regex::no_mod_m : 0;
  regex_flags |= regex::no_mod_s; /* forced for backwards compatibility */

  cmatch matches {};
  regex expression(M2_tocharstar(pattern), regex_flags);
  if (expression.status() != 0)
    {
      std::cerr << "regex: invalid pattern" << std::endl;
      return m;
    }

  // https://www.boost.org/doc/libs/1_73_0/libs/regex/doc/html/boost_regex/ref/match_flag_type.html
  regex_constants::match_flag_type match_flags = match_default;
  match_flags |= flags & REGEX_MATCH_ANY ? match_any : match_flags;
  match_flags |=
      flags & REGEX_MATCH_CONTINUOUS ? match_continuous : match_flags;

  if (range < 0) start = std::max(0, start + range);
  if (range == 0) match_flags |= match_continuous;
  if (range <= 0) range = text->len - start;
  if (range > 0) range = std::min(range, text->len - start);

  /* TODO: not quite backwards compatible with $ and negative range */
  if (start + range < text->len) match_flags |= match_not_eol;
  if (start > 0) match_flags |= match_prev_avail;

  auto head = (const char*)&text->array[start];
  auto tail = (const char*)&text->array[start + range];
  auto status = regex_search(head, tail, matches, expression, match_flags);
  if (!status) return m;

  m = M2_makearrayint(2 * matches.size());
  for (auto i = 0; i < matches.size(); i++)
    {
      m->array[2 * i] =
          start + std::distance(matches.prefix().first, matches[i].first);
      m->array[2 * i + 1] = std::distance(matches[i].first, matches[i].second);
    }
  return m;
}
