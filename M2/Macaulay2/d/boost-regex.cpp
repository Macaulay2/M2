#include "strings-exports.h"

#include <boost/regex.hpp>
#include <string.h>
#include <iostream>

#define DEBUG_REGEX 0

/* Keep this enum in sync with RegexFlags in m2/regex.m2 */
enum RegexFlags {
  /* The first section is based on standard Boost syntax option types. */
  // https://www.boost.org/doc/libs/release/libs/regex/doc/html/boost_regex/ref/syntax_option_type/syntax_option_type_perl.html
  perl = 0, /* ECMAScript flavor (default) */
  //  basic
  extended = (1 << 1), /* POSIX ERE flavor */
  literal = (1 << 2),  /* treat the pattern text as literal */
  //  awk
  //  grep
  //  egrep
  icase = (1 << 5),  /* ignore case */
  nosubs = (1 << 6), /* ignore subexpressions */
  //  optimize
  collate = (1 << 8), /* make [a-b] locale sensitive */

  /* flags for Perl and POSIX */
  //  newline_alt
  //  no_except: this is always on
  //  save_subexpression_location

  /* flags for Perl */
  no_mod_m = (1 << 12), /* don't match ^ $ with newlines */
  no_mod_s = (1 << 13), /* don't match . with newlines */
  //  mod_s
  //  mod_x
  //  no_empty_expressions

  /* flags for POSIX ERE */
  no_escape_in_lists = (1 << 17), /* disable \ escapes in lists */
  no_bk_refs = (1 << 18),         /* disable backreferences */

  /* The rest are based on standard Boost match flag types. */
  // https://www.boost.org/doc/libs/release/libs/regex/doc/html/boost_regex/ref/match_flag_type.html
  //  match_not_bob
  //  match_not_eob
  //  match_not_bol
  //  match_not_eol
  //  match_not_bow
  //  match_not_eow
  match_any = (1 << 25),        /* return any match */
  match_not_null = (1 << 26),   /* match must be nonempty */
  match_continuous = (1 << 27), /* match must start at the beginning */
  //  match_partial
  //  match_single_line
  match_prev_avail = (1 << 30),      /* --first is a valid iterator position */
  match_not_dot_newline = (1 << 31), /* doesn't match . with newlines */
  //  match_not_dot_null
  //  match_posix
  //  match_perl
  //  match_nosubs
  //  match_extra

  //  format_sed
  //  format_perl
  //  format_literal

  //  format_no_copy
  //  format_first_only
  //  format_all
};

/* returns a compile state machine for regex */
boost::regex regex_compile(const M2_string pattern, const long flags)
{
  typedef std::pair<int /* pattern_hash */, long /* flags */> ExpressionKey;
  static std::map<ExpressionKey, boost::regex> cache;

  /* attempt to recover a compiled expression from a map */
  auto key = ExpressionKey {strings_hash(pattern), flags};
  auto hit = cache.find(key);
  if (hit != cache.end()) return hit->second;
#if DEBUG_REGEX & 1
  std::cerr << "flags:\t" << flags << std::endl
            << "regexp:\t" << M2_tocharstar(pattern) << std::endl;
#endif

  /* parse and set the flags */
  boost::regex_constants::syntax_option_type regex_flags =
      boost::regex::no_except; /* never throw exceptions */
  regex_flags |= flags & extended ? boost::regex::extended : 0;
  regex_flags |= flags & literal ? boost::regex::literal : 0;
  regex_flags |= flags & icase ? boost::regex::icase : 0;
  regex_flags |= flags & nosubs ? boost::regex::nosubs : 0;
  regex_flags |= flags & collate ? boost::regex::collate : 0;
  regex_flags |= flags & no_mod_m ? boost::regex::no_mod_m : 0;
  regex_flags |= flags & no_mod_s ? boost::regex::no_mod_s : 0;
  /* the following are on by default on POSIX ERE, so we disable them */
  regex_flags &= flags & no_escape_in_lists ? regex_flags
                                            : ~boost::regex::no_escape_in_lists;
  regex_flags &= flags & no_bk_refs ? regex_flags : ~boost::regex::no_bk_refs;

  /* compile the state machine */
  boost::regex expression(M2_tocharstar(pattern), regex_flags);
  if (expression.status() != 0)
    std::cerr << "regex: could not compile the regular expression: "
              << M2_tocharstar(pattern) << std::endl;

  /* cache the compiled state machine */
  auto res = cache.emplace(key, expression);
  if (!res.second)
    std::cerr << "regex: cache conflict detected between compiled expressions: "
              << res.first->second.str() << " (old expression) and "
              << expression.str() << " (new expression)" << std::endl;

  return expression;
}

extern "C" {

/* returns an array of pairs (s, r), indicating starting point
 * and length of the first match and its capture groups */
M2_arrayint regex_search(const M2_string pattern,
                         const size_t start,
                         const size_t range,
                         const M2_string text,
                         long flags)
{
#if DEBUG_REGEX & 2
  std::cerr << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  M2_arrayint m = M2_makearrayint(0);
  if (text->len < start) return m;

  /* setting the default flags for search */
  flags = flags == -1 ? perl | no_mod_s : flags;

  auto expression = regex_compile(pattern, flags);
  if (expression.status() != 0) return m;

  bool status = false;
  boost::cmatch matches {};

  auto head = (const char*)&text->array + start;
  auto tail = (const char*)&text->array + text->len;

  boost::regex_constants::match_flag_type flag = boost::match_default;
  flag |= flags & match_any ? boost::match_any : flag;
  flag |= flags & match_not_null ? boost::match_not_null : flag;
  flag |= flags & match_continuous ? boost::match_continuous : flag;
  flag |= flags & match_not_dot_newline ? boost::match_not_dot_newline : flag;
  flag |= start > 0 ? boost::match_prev_avail : flag;

  status = boost::regex_search(head, tail, matches, expression, flag);
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

/* returns a string where matched substrings are formatted according to input */
M2_string regex_replace(const M2_string pattern,
                        const int start,
                        const int range,
                        const M2_string format,
                        const M2_string text,
                        long flags)
{
#if DEBUG_REGEX & 4
  std::cerr << "format:\t" << M2_tocharstar(format) << std::endl
            << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  /* setting the default flags for replace */
  flags = flags == -1 ? perl | no_mod_s : flags;

  auto expression = regex_compile(pattern, flags);
  if (expression.status() != 0) return text;

  std::ostringstream stream(std::ios::out);
  std::ostream_iterator<char> stream_iter(stream);

  auto head = (const char*)&text->array + start;
  auto tail = (const char*)&text->array + start + range;

  // TODO: add format_first_only option
  boost::regex_constants::match_flag_type flag = boost::format_default;
  flag |= flags & match_not_dot_newline ? boost::match_not_dot_newline : flag;
  flag |= start > 0 ? boost::match_prev_avail : flag;

  auto substitute = M2_tocharstar(format);
  boost::regex_replace(stream_iter, head, tail, expression, substitute, flag);

  std::string output(stream.str());
  return M2_tostring(output.c_str());
}

/* returns an array of substrings of text matching the pattern,
 * where each match is formatted according to format */
M2_ArrayString regex_select(const M2_string pattern,
                            int start,
                            int range,
                            const M2_string format,
                            const M2_string text,
                            long flags)
{
#if DEBUG_REGEX & 8
  std::cerr << "format:\t" << M2_tocharstar(format) << std::endl
            << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  /* setting the default flags for select */
  flags = (flags == -1 ? perl | no_mod_s : flags) | match_not_null;

  std::string str;
  std::vector<std::string> strings;
  std::vector<char*> cstrings;

  while (start < text->len)
    {
      auto match = regex_search(pattern, start, range, text, flags);
      if (match->len == 0) break;

      auto pair = match->array;
      auto part = regex_replace(pattern, pair[0], pair[1], format, text, flags);

      str = M2_tocharstar(part);
      strings.push_back(str);

      range = start + range - pair[0] - pair[1];
      start = pair[0] + pair[1];
    }

  cstrings.reserve(strings.size());

  for (auto& str : strings) cstrings.push_back(&str[0]);
  return M2_tostrings(cstrings.size(), cstrings.data());
}

/* returns a an array of substrings of text separated by the pattern */
M2_ArrayString regex_separate(const M2_string pattern,
                              int start,
                              int range,
                              const M2_string text,
                              long flags)
{
#if DEBUG_REGEX & 16
  std::cerr << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  /* setting the default flags for separate */
  flags = (flags == -1 ? perl | no_mod_s : flags) | match_not_null;

  std::string str;
  std::vector<std::string> strings;
  std::vector<char*> cstrings;

  while (start < text->len)
    {
      auto match = regex_search(pattern, start, range, text, flags);
      if (match->len == 0) break;

      auto pair = match->array;
      auto part = M2_substr(text, start, pair[0] - start);

      str = M2_tocharstar(part);
      strings.push_back(str);

      range = start + range - pair[0] - pair[1];
      start = pair[0] + pair[1];
    }

  if (start <= text->len)
    {
      str = M2_tocharstar(M2_substr(text, start, text->len - start));
      strings.push_back(str);
    }

  cstrings.reserve(strings.size());

  for (auto& str : strings) cstrings.push_back(&str[0]);
  return M2_tostrings(cstrings.size(), cstrings.data());
}

} /* extern "C" */
