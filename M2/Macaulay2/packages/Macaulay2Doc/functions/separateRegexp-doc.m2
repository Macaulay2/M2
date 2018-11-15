-- author: Lily Silverstein

doc ///
 Key
  separateRegexp
  (separateRegexp, String, String)
  (separateRegexp, String, ZZ, String)
 Headline
  split a string into substrings, using a regular expression to define the separators
 Usage
  separateRegexp(x, s)
  separateRegexp(x, n, s)
 Inputs
  x:String
   a regular expression
  s:String
   the string to split
  n:ZZ
 Outputs
  :List
    the substrings obtained by breaking {\tt s}
    at every match to the regular expression {\tt x},
    using {\tt x} as the separator, or,
    if a natural number {\tt n} is specified, using the
    {\tt n}th parenthesized expression in {\tt x} as the separator
 Description
  Text
   For an introduction to regular expressions, see @TO "regular expressions"@.
   
   Example 1: use commas, periods, and semicolons as separators.
  Example
   separateRegexp("[,.;]", "Example: a string. That, is punctuated, weirdly; for demonstration purposes.")
  Text
   Example 2: match any number of consecutive spaces.
  Example
   separateRegexp("[ ]+", "this    string has   different   lengths of    spacing  between     words")
  Text
   Example 3: delete every word starting with "x" from a string, by
   using @TO concatenate@ together with {\tt separateRegexp}.
  Example
   s = "algng xjfr kfjxse xhgfj xooi xwj kvexr anvi endj xkfi";
   concatenate(separateRegexp(" x[A-Za-z]*", s))
  Text
   Example 4: 
   The optional argument {\tt n} allows us to specify a separator
   that differs from the match criteria. In the previous example,
   words beginning with "x" were both the match and the separator.
   In this example, we match words beginning with "x", 
   but separate the string using the leading "x". With 
   @TO concatenate@, this results in deleting just the "x" 
   from words starting with "x" (not the same as removing every "x").
  Example
   concatenate(separateRegexp(" (x)[A-Za-z]*", 1, s))       
  Text
   Example 5: match words beginning with "x", but use the rest of
   the word as the separator.
  Example
   separateRegexp(" (x)([A-Za-z]*)", 2, s)
 SeeAlso
  concatenate
  format
  separate
  substring
  "strings and nets"
  "regular expressions"
///

