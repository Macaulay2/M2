/* Copyright 2020 by Mahrud Sayrafi */

#include "strings-exports.h"

#include <boost/regex.hpp>
#include <iostream>

#define DEBUG_REGEX 0

/* returns a compiled state machine for regex */
boost::regex regex_compile(const M2_string pattern, const int flags)
{
  typedef std::pair<int /* pattern_hash */, int /* flags */> ExpressionKey;
  static std::map<ExpressionKey, boost::regex> cache;

  /* attempt to recover a compiled expression from a map */
  auto key = ExpressionKey {strings_hash(pattern), flags};
  auto hit = cache.find(key);
  if (hit != cache.end()) return hit->second;

  /* parse and set the flags */
  boost::regex_constants::syntax_option_type regex_flags = flags;

#if DEBUG_REGEX & 1
  std::cerr << "flags:\t" << flags << std::endl
            << "regexp:\t" << M2_tocharstar(pattern) << std::endl;
#endif

  /* compile the state machine */
  // clang-format off
  boost::regex expression;
  try { expression = boost::regex(M2_tocharstar(pattern), regex_flags); }
  catch (std::exception& err) { throw; }
  // clang-format on

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
                         int regex_flags,
                         int match_flags)
{
#if DEBUG_REGEX & 2
  std::cerr << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  M2_arrayint m = M2_makearrayint(0);
  if (text->len < start) return m;

  auto expression = regex_compile(pattern, regex_flags);
  if (expression.status() != 0) return m;

  bool status = false;
  boost::cmatch matches {};

  auto head = (const char*)&text->array + start;
  auto tail = (const char*)&text->array + text->len;

  boost::regex_constants::match_flag_type flag =
      boost::regex_constants::match_flag_type(match_flags |
                                              boost::match_default);
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
                        int regex_flags,
                        int match_flags)
{
#if DEBUG_REGEX & 4
  std::cerr << "format:\t" << M2_tocharstar(format) << std::endl
            << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  auto expression = regex_compile(pattern, regex_flags);
  if (expression.status() != 0) return text;

  std::ostringstream stream(std::ios::out);
  std::ostream_iterator<char> stream_iter(stream);

  auto head = (const char*)&text->array + start;
  auto tail = (const char*)&text->array + start + range;

  // TODO: add format_first_only option
  boost::regex_constants::match_flag_type flag =
      boost::regex_constants::match_flag_type(match_flags |
                                              boost::format_default);
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
                            int regex_flags,
                            int match_flags)
{
#if DEBUG_REGEX & 8
  std::cerr << "format:\t" << M2_tocharstar(format) << std::endl
            << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  /* adjusting the default match flags for select to avoid infinite loops */
  match_flags = match_flags | boost::match_not_null;

  std::string str;
  std::vector<std::string> strings;
  std::vector<char*> cstrings;

  while (start < text->len)
    {
      auto match =
          regex_search(pattern, start, range, text, regex_flags, match_flags);
      if (match->len == 0) break;

      auto pair = match->array;
      auto part = regex_replace(
          pattern, pair[0], pair[1], format, text, regex_flags, match_flags);

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
                              int regex_flags,
                              int match_flags)
{
#if DEBUG_REGEX & 16
  std::cerr << "string:\t" << M2_tocharstar(text) << std::endl;
#endif

  /* adjusting the default match flags for separate to avoid infinite loops */
  match_flags = match_flags | boost::match_not_null;

  std::string str;
  std::vector<std::string> strings;
  std::vector<char*> cstrings;

  while (start < text->len)
    {
      auto match =
          regex_search(pattern, start, range, text, regex_flags, match_flags);
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
