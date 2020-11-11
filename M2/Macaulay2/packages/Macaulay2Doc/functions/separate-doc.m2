--- status: Rewritten July 2020
--- author(s): Lily Silverstein, Mahrud
--- notes: functions below are all defined in regex.m2

doc ///
Node
 Key
   separate
  (separate, String)
  (separate, String, String)
  (separate, String, ZZ, String)
  [separate, POSIX]
  "separateRegexp"
 Headline
  split a string into substrings using a regular expression
 Usage
  separate str
  separate(re, str)
  separate(re, n, str)
 Inputs
  re:String
   a @TO2 {"regular expressions", "regular expression"}@ describing a pattern
  str:String
   the string to split
  n:ZZ
   the index of the parenthesized expression to split on
  POSIX=>Boolean
   if true, interpret the @TT "re"@ using the POSIX Extended flavor, otherwise the Perl flavor
 Outputs
  :List
   a list of strings obtained by breaking @TT "str"@ at every match to the pattern @TT "re"@, or, if a natural
   number @TT "n"@ is specified, using the $n$-th parenthesized expression in @TT "re"@ as the separator.
   If no @TT "re"@ is specified, @TT "str"@ is split at every new line.
 Description
  Text
   For an introduction to regular expressions, see @TO "regular expressions"@.

   Example 1: The command @TT "separate(s)"@ breaks the string at every occurrence of @TT "\\r\\n"@ or @TT "\\n"@.
  Example
   s = "A string with both Unix-style\nand Windows-style\r\nnew line characters."
   separate s
  Text
   This is equivalent to using the @TO lines@ function.
  Example
   lines s

  Text
   Example 2: use commas, periods, and semicolons as separators.
  Example
   separate("[,.;]", "Example: a string. That, is punctuated, weirdly; for demonstration purposes.")

  Text
   Example 3: match any number of consecutive spaces.
  Example
   t = separate("[ ]+", "this    string has   different   lengths of    spacing  between     words")
  Text
   We can now correct the original string using the @TO demark@ and @TO replace@ functions.
  Example
   replace("has", "does not have", demark(" ", t))

  Text
   Example 4: delete every word starting with "x" from a string, by using @TO concatenate@ together with @TO separate@.
  Example
   s = "algng xjfr kfjxse xhgfj xooi xwj kvexr anvi endj xkfi";
   concatenate separate(" x[A-Za-z]*", s)

  Text
   Example 5:
   The optional argument @TT "n"@ allows us to specify a separator that differs from the match criteria.
   In the previous example, words beginning with "x" were both the match and the separator.
   In this example, we match words beginning with "x", but separate the string using the leading "x".
   With @TO concatenate@, this results in deleting just the "x" from words starting with "x"
   (not the same as removing every "x").
  Example
   concatenate separate(" (x)[A-Za-z]*", 1, s)
  Text
   @TT "separateRegexp"@ is a deprecated synonym for @TT "separate"@.
 Caveat
  For backwards compatibility, if the pattern is a single character and it is an unescaped special character,
  such as @TT "+"@, @TT "*"@, or @TT "."@, then it is treated as a literal character. In future code, the pattern
  must be escaped.
 SeeAlso
  "regular expressions"
  "strings and nets"
  regex
  lines
  demark
  concatenate

Node
  Key
     lines
    (lines, String)
    (lines, String, String)
  Headline
    split a string into lines
  Usage
    lines s
    lines(nl, s)
  Inputs
    s:String
    nl:String
  Outputs
    :List
      an array of strings obtained from the string @TT "s"@ by breaking
      it at the characters specified by the string @TT "nl"@
  Description
    Text
      The form @TT "lines s"@ is designed to break lines correctly when the file
      follows the Unix or MS-DOS convention. In other words, it will break a line
      at @TT "\\r\\n"@ or @TT "\\n"@.
    Example
      lines "a\nb\nc\n"
      peek oo
      lines "a\nb\nc\nd"
      peek oo
      lines( "\\.", "a.b.c." )
      peek oo
      lines( "\\.", "a.b.c.d" )
      peek oo
  Caveat
    For backwards compatibility, if the pattern is a single character and it is an unescaped special character,
    such as @TT "+"@, @TT "*"@, or @TT "."@, then it is treated as a literal character. In future code, the pattern
    must be escaped.
  SeeAlso
    "newline"
    separate
///
