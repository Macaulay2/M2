--- status: Rewritten July 2020
--- author(s): Dan, Mahrud
--- notes: functions below are all defined in regex.m2

doc ///
  Key
     regex
    (regex, String,         String)
    (regex, String, ZZ,     String)
    (regex, String, ZZ, ZZ, String)
    [regex, POSIX]
    POSIX
  Headline
    evaluate a regular expression search
  Usage
    regex(re, start)
    regex(re, start, str)
    regex(re, start, range, str)
  Inputs
    re:String
      a @TO2 {"regular expressions", "regular expression"}@ describing a pattern
    start:ZZ
      positive, the position in @TT "str"@ at which to begin the search.
      when omitted, the search starts at the beginning of the string.
    range:ZZ
      restricts matches to those beginning at a position between @TT "start"@ and @TT "start + range"@;
      when 0, the pattern is matched only at the starting position;
      when negative, only positions to the left of the starting position are examined for matches;
      when omitted, the search extends to the end of the string.
    str:String
      the subject string to be searched
    POSIX=>Boolean
      if true, interpret the @TT "re"@ using the POSIX Extended flavor, otherwise the Perl flavor
  Outputs
    :List
      a list of pairs of integers; each pair denotes the beginning position and the length of a substring.
      Only the leftmost matching substring of @TT "str"@ and the capturing groups within it are returned.
      If no match is found, the output is @TO "null"@.
  Description
    Text
      The value returned is a list of pairs of integers corresponding to the parenthesized subexpressions
      successfully matched, suitable for use as the first argument of @TO "substring"@. The first member
      of each pair is the offset within @TT "str"@ of the substring matched, and the second is the length.

      See @TO "regular expressions"@ for a brief introduction to the topic.
    Example
      s = "The cat is black.";
      m = regex("(\\w+) (\\w+) (\\w+)",s)
      substring(m#0, s)
      substring(m#1, s)
      substring(m#2, s)
      substring(m#3, s)

      s = "aa     aaaa";
      m = regex("a+", 0, s)
      substring(m#0, s)
      m = regex("a+", 2, s)
      substring(m#0, s)
      m = regex("a+", 2, 3, s)

      s = "line 1\nline 2\r\nline 3";
      m = regex("^.*$", 8, -8, s)
      substring(m#0, s)
      m = regex("^", 10, -10, s)
      substring(0, m#0#0, s)
      substring(m#0#0, s)
      m = regex("^.*$", 4, -10, s)
      substring(m#0, s)
      m = regex("a.*$", 4, -10, s)

    Text
      By default, the regular expressions are interpreted using the Perl flavor, which
      supports features such as lookaheads and lookbehinds for fine-tuning the matches.
      This syntax is used in Perl and JavaScript languages.
    Example
      regex("A(?!C)", "AC AB")
      regex("A(?=B)", "AC AB")

    Text
      Alternatively, one can choose the POSIX Extended flavor of regex using @TT "POSIX => true"@.
      This syntax is similar to the one used by the Unix utilities @TT "egrep"@ and @TT "awk"@ and
      enforces the @BOLD "leftmost, longest"@ rule for finding matches. If there's a tie, the rule
      is applied to the first subexpression.
    Example
      s = "<b>bold</b> and <b>strong</b>";
      m = regex("<b>(.*)</b>", s, POSIX => true);
      substring(m#1, s)

    Text
      In the Perl flavor, one can specify whether repetitions should be possessive or non-greedy.
    Example
      m = regex("<b>(.*?)</b>", s);
      substring(m#1, s)
  SeeAlso
    "regular expressions"
    "strings and nets"
    match
    replace
    separate
    regexQuote
    (select, String, String, String)
    substring
///

doc ///
  Key
    "regular expressions"
  Headline
    syntax for regular expressions
  Description
    Text

      A regular expression is a string that specifies a pattern that describes a set of matching subject strings.
      Typically the string is compiled into a deterministic finite automaton whose execution, guided by the
      subject string, determines whether there is a match.

      Characters match themselves, except for the following special characters.

      @PRE {CODE {".  [  {  }  (  )  \\  *  +  ?  |  ^  $"}}@

      Regular expressions are constructed inductively as follows: the empty regular expression matches the
      empty string; a concatenation of regular expressions matches the concatenation of the corresponding
      matching strings.  Regular expressions separated by the character @TT "|"@ match strings matched by any
      of them.  Parentheses can be used for grouping, except that now, with the use of the @ TO "Boost" @
      library, their insertion may alter the matching of subexpressions of ambiguous expressions.
      Additionally, the substrings matched by parenthesized subexpressions are captured for later use in
      replacement strings.

      @HEADER3 "Special characters"@

      @UL {
          {TT ".", " -- match any character except newline"},
          {TT "^", " -- match the beginning of the string or the beginning of a line"},
          {TT "$", " -- match the end of the string or the end of a line"},
          {TT "(...)", " -- marked sub-expression, may be referred to by a back-reference"},
          {TT "*", " -- match previous expression 0 or more times"},
          {TT "+", " -- match previous expression 1 or more times"},
          {TT "?", " -- match previous expression 1 or 0 times"},
          {TT "{m}", " -- match previous expression exactly m times"},
          {TT "{m,n}", " -- match previous expression at least m and at most n times"},
          {TT "{,n}", " -- match previous expression at most n times"},
          {TT "{m,}", " -- match previous expression at least m times"},
          {TT "\\i", " -- match the same string that the i-th parenthesized sub-expression matched"},
          {TT "|", " -- match expression to left or expression to right"},
          {TT "[...]", " -- match listed characters, ranges, or classes"},
          {TT "[^...]", " -- match non-listed characters, ranges, or classes"},
          {TT "\\b", " -- match word boundary"},
          {TT "\\B", " -- match within word"},
          {TT "\\<", " -- match beginning of word"},
          {TT "\\>", " -- match end of word"},
          {TT "\\w", " -- match word-constituent character"},
          {TT "\\W", " -- match non-word-constituent character"},
          {TT "\\`", " -- match beginning of string"},
          {TT "\\'", " -- match end of string"}
          }@

      The special character @TT "\\"@ may be confusing, as inside a string delimited by quotation marks
      (@TT ////"..."////@), you type two of them to specify a special character, whereas inside a string
      delimited by triple slashes (@TT "////...////"@), you only need one. Thus regular expressions delimited
      by triple slashes are more readable. To match @TT "\\"@ against itself, double the number of backslashes.

      In order to match one of the special characters itself, precede it with a backslash or use @TO regexQuote@.

      @HEADER3 "Character classes"@

      There are the following character classes.

      @UL {
          {TT "[:alnum:]", " -- letters and digits"},
          {TT "[:alpha:]", " -- letters"},
          {TT "[:blank:]", " -- a space or tab"},
          {TT "[:cntrl:]", " -- control characters"},
          {TT "[:digit:]", " -- digits"},
          {TT "[:graph:]", " -- same as [:print:] except omits space"},
          {TT "[:lower:]", " -- lowercase letters"},
          {TT "[:print:]", " -- printable characters"},
          {TT "[:punct:]", " -- neither control nor alphanumeric characters"},
          {TT "[:space:]", " -- space, tab, carriage return, newline, vertical tab, and form feed"},
          {TT "[:upper:]", " -- uppercase letters"},
          {TT "[:xdigit:]", " -- hexadecimal digits"},
          }@

      @HEADER2 "Flavors of Regular Expressions"@

      The regular expression functions in Macaulay2 are powered by calls to the
      @HREF {"https://www.boost.org/doc/libs/release/libs/regex/", "Boost.Regex"}@
      C++ library, which supports multiple flavors, or standards, of regular expression.

      Since Macaulay2 @TO2{"changes, 1.17","v1.17"}@, the Perl flavor is the default. In general, the Perl flavor
      supports all patterns designed for the POSIX Extended flavor, but allows for more fine-tuning in the patterns.
      Alternatively, the POSIX Extended flavor can be chosen by passing the option @TT "POSIX => true"@.
      One key difference is what happens when there is more that one way to match a regular expression:

      @UL {
         {BOLD "Perl", " -- the \"first\" match is arrived at by a depth-first search."},
         {BOLD "POSIX", " -- the \"best\" match is obtained using the \"leftmost-longest\" rule;"},
         }@

      If there's a tie in the POSIX flavor, the rule is applied to the first parenthetical subexpression.

      @HEADER2 "Additional Perl Regular Expression Syntax"@

      The Perl flavor adds the following, non-backward compatible constructions:

      @UL {
          {TT "(?#...)", " -- ignored and treated as a comment"},
          {TT "(?:...)", " -- non-marked sub-expression, may not be referred to by a back-reference"},
          {TT "(?=...)", " -- positive lookahead; consumes zero characters, only if pattern matches"},
          {TT "(?!...)", " -- negative lookahead; consumes zero characters, only if pattern does not match"},
          {TT "(?<=...)", " -- positive lookbehind; consumes zero characters, only if pattern could be matched against the characters preceding the current position (pattern must be of fixed length)"},
          {TT "(?<!...)", " -- negative lookbehind; consumes zero characters, only if pattern could not be matched against the characters preceding the current position (pattern must be of fixed length)"},
          {TT "*?", " -- match the previous atom 0 or more times, while consuming as little input as possible"},
          {TT "+?", " -- match the previous atom 1 or more times, while consuming as little input as possible"},
          {TT "??", " -- match the previous atom 1 or 0 times, while consuming as little input as possible"},
          {TT "{m,}?", " -- match the previous atom m or more times, while consuming as little input as possible"},
          {TT "{m,n}?", " -- match the previous atom at between m and n times, while consuming as little input as possible"},
          {TT "*+", " -- match the previous atom 0 or more times, while giving nothing back"},
          {TT "++", " -- match the previous atom 1 or more times, while giving nothing back"},
          {TT "?+", " -- match the previous atom 1 or 0 times, while giving nothing back"},
          {TT "{m,}+", " -- match the previous atom m or more times, while giving nothing back"},
          {TT "{m,n}+", " -- match the previous atom at between m and n times, while giving nothing back"},
          {TT "\\g1", " -- match whatever matched sub-expression 1"},
          {TT "\\g{1}", " -- match whatever matched sub-expression 1"},
          {TT "\\g-1", " -- match whatever matched the last opened sub-expression"},
          {TT "\\g{-2}", " -- match whatever matched the last but one opened sub-expression"},
          {TT "\\g{that}", " -- match whatever matched the sub-expression named \"that\""},
          {TT "\\k<that>", " -- match whatever matched the sub-expression named \"that\""},
          {TT "(?<NAME>...)", " -- named sub-expression, may be referred to by a named back-reference"},
          {TT "(?'NAME'...)", " -- named sub-expression, may be referred to by a named back-reference"},
          }@

      @HEADER2 "Complete References"@

      For complete documentation on regular expressions supported in Macaulay2, see the Boost.Regex manual on
      @HREF {"https://www.boost.org/doc/libs/release/libs/regex/doc/html/boost_regex/syntax/perl_syntax.html", "Perl"}@ and
      @HREF {"https://www.boost.org/doc/libs/release/libs/regex/doc/html/boost_regex/syntax/basic_extended.html", "Extended"}@
      flavors, or read the entry for @TT "regex"@ in section 7 of the unix man pages.

      In addition to the functions mentioned below, regular expressions appear in @TO "about"@, @TO "apropos"@,
      @TO "findFiles"@, @TO "copyDirectory"@, and @TO "symlinkDirectory"@.
  Subnodes
    :functions that accept regular expressions
    match
    regex
    separate
    (select, String, String, String)
    (replace, String, String, String)
    regexQuote
///

doc ///
  Key
     regexQuote
    (regexQuote, String)
  Headline
    escape special characters in regular expressions
  Usage
    regexQuote s
  Inputs
    s:String
  Outputs
    :String
      obtained from @TT "s"@ by escaping all of the characters that have special meanings in regular expressions
      (@TT "\\, ^, $, ., |, ?, *, +, (, ), [, ], {, }"@) with a @TT "\\"@.
  Description
    Example
      match("2+2", "2+2")
      regexQuote "2+2"
      match(oo, "2+2")
  SeeAlso
    "regular expressions"
    regex
    match
///
