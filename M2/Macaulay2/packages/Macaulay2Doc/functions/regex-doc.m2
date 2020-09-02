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
    separate
    (replace, String, String, String)
    (select, String, String, String)
    regexQuote
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

    Tree
      :Syntax for special characters
        :Wildcard
          @TT "."@	-- match any character except the newline character
        :Anchors
          @TT "^"@	-- match the beginning of the string or the beginning of a line
          @TT "$"@	-- match the end of the string or the end of a line
        :Sub-expressions
          @TT "(...)"@	-- marked sub-expression, may be referred to by a back-reference
          @TT "\\i"@	-- match the same string that the i-th parenthesized sub-expression matched
        :Repeats
          @TT "*"@	-- match previous expression 0 or more times
          @TT "+"@	-- match previous expression 1 or more times
          @TT "?"@	-- match previous expression 1 or 0 times
          @TT "{m}"@	-- match previous expression exactly m times
          @TT "{m,n}"@	-- match previous expression at least m and at most n times
          @TT "{,n}"@	-- match previous expression at most n times
          @TT "{m,}"@	-- match previous expression at least m times
        :Alternation
          @TT "|"@	-- match expression to left or expression to right
        :Word and buffer boundaries
          @TT "\\b"@	-- match word boundary
          @TT "\\B"@	-- match within word
          @TT "\\<"@	-- match beginning of word
          @TT "\\>"@	-- match end of word
          @TT "\\`"@	-- match beginning of string
          @TT "\\'"@	-- match end of string
        :Character sets
          @TT "[...]"@	-- match any single character that is a member of the set
           @TT "[abc]"@	-- match either @TT "a"@, @TT "b"@, or @TT "c"@
           @TT "[A-C]"@	-- match any character from @TT "A"@ through @TT "Z"@
          @TT "[^...]"@	-- match non-listed characters, ranges, or classes
        :Character classes
          @TT "[:alnum:]"@	-- any alpha-numeric character
          @TT "[:alpha:]"@	-- any alphabetic character
          @TT "[:blank:]"@	-- any whitespace or tab character
          @TT "[:cntrl:]"@	-- any control character
          @TT "[:digit:]"@	-- any decimal digit
          @TT "[:graph:]"@	-- any graphical character (same as [:print:] except omits space)
          @TT "[:lower:]"@	-- any lowercase character
          @TT "[:print:]"@	-- any printable character
          @TT "[:punct:]"@	-- any punctuation character
          @TT "[:space:]"@	-- any whitespace, tab, carriage return, newline, vertical tab, and form feed
          @TT "[:unicode:]"@	-- any unicode character with code point above 255 in value
          @TT "[:upper:]"@	-- any uppercase character
          @TT "[:word:]"@	-- any word character (alphanumeric characters plus the underscore
          @TT "[:xdigit:]"@	-- any hexadecimal digit character
        :"Single character" character classes
          @TT "\\d"@	-- same as @TT "[[:digit:]]"@
          @TT "\\l"@	-- same as @TT "[[:lower:]]"@
          @TT "\\s"@	-- same as @TT "[[:space:]]"@
          @TT "\\u"@	-- same as @TT "[[:upper:]]"@
          @TT "\\w"@	-- same as @TT "[[:word:]]"@
          @TT "\\D"@	-- same as @TT "[^[:digit:]]"@
          @TT "\\L"@	-- same as @TT "[^[:lower:]]"@
          @TT "\\S"@	-- same as @TT "[^[:space:]]"@
          @TT "\\U"@	-- same as @TT "[^[:upper:]]"@
          @TT "\\W"@	-- same as @TT "[^[:word:]]"@

    Text
      The special character @TT "\\"@ may be confusing, as inside a string delimited by quotation marks
      (@TT ////"..."////@), you type two of them to specify a special character, whereas inside a string
      delimited by triple slashes (@TT "////...////"@), you only need one. Thus regular expressions delimited
      by triple slashes are more readable. To match @TT "\\"@ against itself, double the number of backslashes.

      In order to match one of the special characters itself, precede it with a backslash or use @TO regexQuote@.

    Text
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

    Tree
      :Non-marking grouping; i.e., a grouping that does not generate a sub-expression
        @TT "(?#...)"@	-- ignored and treated as a comment
        @TT "(?:...)"@	-- non-marked sub-expression, may not be referred to by a back-reference
        @TT "(?=...)"@	-- positive lookahead; consumes zero characters, only if pattern matches
        @TT "(?!...)"@	-- negative lookahead; consumes zero characters, only if pattern does not match
        @TT "(?<=..)"@	-- positive lookbehind; consumes zero characters, only if pattern could be matched against the characters preceding the current position (pattern must be of fixed length)
        @TT "(?<!..)"@	-- negative lookbehind; consumes zero characters, only if pattern could not be matched against the characters preceding the current position (pattern must be of fixed length)
        @TT "(?>...)"@	-- match independently of the surrounding pattern and the expression will never backtrack into the pattern
      :Non-greedy repeats
        @TT "*?"@	-- match the previous atom 0 or more times, while consuming as little input as possible
        @TT "+?"@	-- match the previous atom 1 or more times, while consuming as little input as possible
        @TT "??"@	-- match the previous atom 1 or 0 times, while consuming as little input as possible
        @TT "{m,}?"@	-- match the previous atom m or more times, while consuming as little input as possible
        @TT "{m,n}?"@	-- match the previous atom at between m and n times, while consuming as little input as possible
      :Possessive repeats
        @TT "*+"@	-- match the previous atom 0 or more times, while giving nothing back
        @TT "++"@	-- match the previous atom 1 or more times, while giving nothing back
        @TT "?+"@	-- match the previous atom 1 or 0 times, while giving nothing back
        @TT "{m,}+"@	-- match the previous atom m or more times, while giving nothing back
        @TT "{m,n}+"@	-- match the previous atom at between m and n times, while giving nothing back
      :Back references
        @TT "\\g1"@	-- match whatever matched sub-expression 1
        @TT "\\g{1}"@	-- match whatever matched sub-expression 1
        @TT "\\g-1"@	-- match whatever matched the last opened sub-expression
        @TT "\\g{-2}"@	-- match whatever matched the last but one opened sub-expression
        @TT "\\g{that}"@	-- match whatever matched the sub-expression named "that"
        @TT "\\k<that>"@	-- match whatever matched the sub-expression named "that"
        @TT "(?<NAME>...)"@	-- named sub-expression, may be referred to by a named back-reference
        @TT "(?'NAME'...)"@	-- named sub-expression, may be referred to by a named back-reference
    Text
      See references below for more in depth syntax for controlling the backtracking algorithm.

    Text
      @HEADER2 "String Formatting Syntax"@

      The replacement string in @TO (replace, String, String, String)@ and @TO (select, String, String, String)@
      supports additional syntax for escape sequences as well as inserting captured sub-expressions:

    Tree
     :Using Perl regular expression syntax (default)
      :Syntax for inserting captured sub-expressions:
        @TT "$&"@	-- outputs what matched the whole expression
        @TT "$`"@	-- outputs the text between the end of the last match (or beginning if no previous match was found) and the start of the current match
        @TT "$'"@	-- outputs the text following the end of the current match
        @TT "$+"@	-- outputs what matched the last marked sub-expression in the regular expression
        @TT "$^N"@	-- outputs what matched the last sub-expression to be actually matched
        @TT "$n"@	-- outputs what matched the n-th sub-expression
        @TT "${n}"@	-- outputs what matched the n-th sub-expression
        @TT "$+{NAME}"@	-- outputs what matched the sub-expression named @TT "NAME"@ (Perl syntax only)
        @TT "$$"@	-- outputs a literal @TT "$"@
      :Syntax for manipulating the captured groups:
        @TT "\\l"@	-- converts the next character to be outputted to lower case
        @TT "\\u"@	-- converts the next character to be outputted to upper case
        @TT "\\L"@	-- converts all subsequent characters to be outputted to lower case, until it reaches @TT "\\E"@
        @TT "\\U"@	-- converts all subsequent characters to be outputted to upper case, until it reaches @TT "\\E"@
        @TT "\\E"@	-- terminates a @TT "\\U"@ or @TT "\\L"@ sequence
        @TT "\\"@	-- specifies an escape sequence (e.g. @TT "\\\\"@)
     :Using POSIX Extended syntax (@TT "POSIX => true"@)
      :Syntax for inserting captured groups:
        @TT "&"@	-- outputs what matched the whole expression
        @TT "\\0"@	-- outputs what matched the whole expression
        @TT "\\n"@	-- if @TT "n"@ is in the range 1-9, outputs what matched the n-th sub-expression
        @TT "\\"@	-- specifies an escape sequence (e.g. @TT "\\&"@)

    Text
      For the complete list, including characters escape sequences, see the Boost.Regex manual on
      @HREF {"https://www.boost.org/doc/libs/release/libs/regex/doc/html/boost_regex/format/perl_format.html", "format string syntax"}@.

    Tree
     :String processing functions that accept regular expressions
       match
       regex
       separate
       (select, String, String, String)
       (replace, String, String, String)
     :Other functions that accept regular expressions
       about
       apropos
       findFiles
       copyDirectory
       symlinkDirectory

    Text
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
