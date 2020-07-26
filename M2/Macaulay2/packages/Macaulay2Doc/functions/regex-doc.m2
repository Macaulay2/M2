--- status: Rewritten July 2020
--- author(s): Dan, Mahrud
--- notes: functions below are all defined in regex.m2

doc ///
  Key
     regex
    (regex, String,         String)
    (regex, String, ZZ,     String)
    (regex, String, ZZ, ZZ, String)
    [regex, Flags]
  Headline
    evaluate a regular expression search
  Usage
    m = regex(re, start)
    m = regex(re, start, str)
    m = regex(re, start, range, str)
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
    Flags=>ZZ
      option for choosing the regex flavor, such as @TO "RegexPOSIX"@ and @TO "RegexPerl"@, or passing internal
      flags to the @HREF {"https://www.boost.org/doc/libs/release/libs/regex/", "Boost.Regex"}@ library.
  Outputs
    m:List
      a list of pairs of integers; each pair denotes the beginning position and the length of a substring.
      Only the first matching substring of @TT "str"@ and the capturing groups within it are returned.
      If no match is found, the output is @TO "null"@.
  Description
    Text
      The value returned is a list of pairs of integers corresponding to the parenthesized subexpressions
      successfully matched, suitable for use as the first argument of @TO "substring"@. The first member
      of each pair is the offset within @TT "str"@ of the substring matched, and the second is the length.

      See @TO "regular expressions"@ for a brief introduction to the topic.

      By default, the POSIX Extended flavor of regex is used. This syntax is used by the Unix utilities
      @TT "egrep"@ and @TT "awk"@. This flavor follows the @BOLD "leftmost, longest"@ rule for finding
      matches.
    Example
      s = "The cat is black.";
      word = ////\b([a-z]+)\b////;
      m = regex(word|" "|word,s)
      substring(m#0,s)
      substring(m#1,s)
      substring(m#2,s)

      s = "aa     aaaa";
      m = regex("a+",0,s)
      substring(m#0,s)
      m = regex("a+",2,s)
      substring(m#0,s)
      m = regex("a+",2,3,s)

      s = "line 1\nline 2\nline 3";
      m = regex("^.*$",8,-8,s)
      substring(m#0,s)
      m = regex("^",10,-10,s)
      substring(0,m#0#0,s)
      substring(m#0#0,s)
      m = regex("^.*$",4,-10,s)
      substring(m#0,s)
      m = regex("a.*$",4,-10,s)

    Text
      Alternatively, one can choose the ECMAScript flavor of regex, which supports more features, such as
      lookaheads and lookbehinds, for fine tuning the matches. This syntax is used in Perl and JavaScript languages.
    Example
      regex("A(?!C)", "AC AB", Flags => RegexPerl)
      regex("A(?=B)", "AC AB", Flags => RegexPerl)

      s = "<b>bold</b> and <b>strong</b>";
      m = regex("<b>(.*)</b>",  s, Flags => RegexPOSIX)
      substring(m#1#0, m#1#1, s)
      m = regex("<b>(.*?)</b>", s, Flags => RegexPerl)
      substring(m#1#0, m#1#1, s)
  SeeAlso
    "regular expressions"
    match
    replace
    regexQuote
    separateRegexp
    (select, String, String)
    (select, String, String, String)
    substring
///

doc ///
  Key
    "regular expressions"
    "RegexPOSIX"
    "RegexPerl"
  Headline
    syntax for regular expressions
  Description
    Text
      A regular expression is a string that specifies a pattern that describes a
      set of matching subject strings.  Regular expressions are constructed
      inductively as follows.  Ordinary (non-special) characters match themselves.
      A concatenation of regular expressions matches the concatenation of
      corresponding matching subject strings.  Regular expressions separated by the
      character @TT "|"@ match strings matched by any.  Parentheses can be used
      for grouping, and results about which substrings of the target string matched
      which parenthesized subexpression of the regular expression can be returned.

      The special characters are those appearing in the following constructions.  The special character @TT "\\"@
      may be confusing, as inside a string delimited by quotation marks (@TT ////"..."////@), you type two of
      them to get one, whereas inside a string delimited by triple slashes (@TT "////...////"@),
      you type one to get one.  Thus regular expressions delimited by triple slashes are more readable.

      @UL {
          {TT ".", " -- match any character except newline"},
          {TT "^", " -- match the beginning of the string or the beginning of a line"},
          {TT "$", " -- match the end of the string or the end of a line"},
          {TT "*", " -- match previous expression 0 or more times"},
          {TT "+", " -- match previous expression 1 or more times"},
          {TT "?", " -- match previous expression 1 or 0 times"},
          {TT "(...)", " -- subpattern grouping"},
          {TT "|", " -- match expression to left or expression to right"},
          {TT "{m}", " -- match previous expression exactly m times"},
          {TT "{m,n}", " -- match previous expression at least m and at most n times"},
          {TT "{,n}", " -- match previous expression at most n times"},
          {TT "{m,}", " -- match previous expression at least m times"},
          {TT "\\i", " -- match the same string that the i-th parenthesized subpattern matched"},
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

      In order to match one of the special characters itself, precede it with a backslash or use @TO regexQuote@.

      We illustrate the use of regular expressions with @TO (regex, String, String)@.

    Example
      s = "1abcddddeF2";
      regex("d", s)
      substring(oo#0,s)
      regex("d*", s)
      substring(oo#0,s)
      regex("d+", s)
      substring(oo#0,s)
      regex("d+", "1abceF2")
      regex("cdd+e", s)
      substring(oo#0,s)
      regex("cd(d+)e", s)
      substring(oo#0,s),substring(oo#1,s)
      regex("[a-z]+", s)
      substring(oo#0,s)
      t = "Dog cat cat.";
      regex("[[:alpha:]]+", t)
      regex("([[:alpha:]]+) *\\1",t)
      substring(oo#0,t),substring(oo#1,t)

    Text
      For complete documentation on regular expressions see the entry for @TT "regex"@ in
      section 7 of the unix man pages, or read the the GNU @TT "regex"@ manual.

      In addition to the functions mentioned below, regular expressions appear in @TO "about"@, @TO "findFiles"@, and @TO "pretty"@.
  Subnodes
    :functions that accept regular expressions
    match
    regex
    replace
    (select, String, String)
    (select, String, String, String)
    separateRegexp
  Caveat
    Regular expression matching is done by calls to a C library, where the convention is that
    the end of a string is signalled by a byte containing zero, whereas in Macaulay2, strings may contain
    bytes containing zero.  Hence regular expression parsing and matching ignore any bytes containing zero,
    as well as any subsequent bytes, potentially yielding surprising results
///
