--- status: Rewritten July 2020
--- author(s): Lily Silverstein, Mahrud
--- notes: functions below are all defined in regex.m2

doc ///
 Key
   separate
  (separate, String)
  (separate, String, String)
  (separate, String, ZZ, String)
   separateRegexp
  (separateRegexp, String, String)
  (separateRegexp, String, ZZ, String)
  [separate, Flags]
  [separateRegexp, Flags]
 Headline
  split a string into substrings
 Usage
  separate str
  separate(pat, str)
  separate(pat, n, str)
 Inputs
  pat:String
   a pattern
  str:String
   the string to split
  n:ZZ
   the index of the parenthesized expression to split on
  Flags=>ZZ
   option for choosing the regex flavor, such as @TO "RegexPOSIX"@ and @TO "RegexPerl"@
 Outputs
  :List
   a list of strings obtained by breaking @TT "str"@ at every match to the pattern  @TT "pat"@, which may be either
   a literal string or a regular expression, or, if a natural number @TT "n"@ is specified, using the @TT "n"@-th
   parenthesized expression in @TT "pat"@ as the separator. If no @TT "pat"@ is specified, @TT "str"@ is split at
   every new line.
 Description
  Text
   @TT "separateRegexp"@ is a synonym for @TT "separate"@ with the @TT "Flags"@ option set to @TO "RegexPOSIX"@, indicating
   that @TT "pat"@ is a regular expression. For an introduction to regular expressions, see @TO "regular expressions"@.

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
   separateRegexp("[,.;]", "Example: a string. That, is punctuated, weirdly; for demonstration purposes.")
   separate("[,.;]", "Example: a string. That, is punctuated, weirdly; for demonstration purposes.", Flags => RegexPOSIX)

  Text
   Example 3: match any number of consecutive spaces.
  Example
   t = separate("[ ]+", "this    string has   different   lengths of    spacing  between     words", Flags => RegexPOSIX)
  Text
   We can now correct the original string using the @TO demark@ and @TO replace@ functions.
  Example
   replace("has", "does not have", demark(" ", t))

  Text
   Example 4: delete every word starting with "x" from a string, by using @TO concatenate@ together with @TO separate@.
  Example
   s = "algng xjfr kfjxse xhgfj xooi xwj kvexr anvi endj xkfi";
   concatenate separate(" x[A-Za-z]*", s, Flags => RegexPOSIX)
--  Text
--   This is equivalent to using the @TO select@ function, and using the lookbehind syntax of @TO RegexPOSIX@.
--  Example
--   concatenate select("(?<!x)\\w+", s, Flags => RegexPOSIX)

  Text
   Example 5:
   The optional argument @TT "n"@ allows us to specify a separator that differs from the match criteria.
   In the previous example, words beginning with "x" were both the match and the separator.
   In this example, we match words beginning with "x", but separate the string using the leading "x".
   With @TO concatenate@, this results in deleting just the "x" from words starting with "x"
   (not the same as removing every "x").
  Example
   concatenate separate(" (x)[A-Za-z]*", 1, s, Flags => RegexPOSIX)
 SeeAlso
  "regular expressions"
  "strings and nets"
  concatenate
  demark
  format
  lines
///
