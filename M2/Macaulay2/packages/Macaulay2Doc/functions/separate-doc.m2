-- author: Lily Silverstein

doc ///
 Key 
  separate
  (separate, String)
  (separate, String, String)
 Headline
  split a string into substrings
 Usage
  separate s
  separate (x, s)
 Inputs
  s:String
  x:String
    containing 1 or 2 characters
 Outputs
   :List
    a list of strings obtained by breaking {\tt s}
    at every occurrence of {\tt x}, or,
    if no {\tt x} is specified, at every new line
 Description
  Text
   We illustrate several different ways we can separate the following string into substrings.
  Example
   s = "This is an example of a string.\nIt contains some letters, spaces, and punctuation.\r\nIt also contains some new line characters.\r\nIn fact, for some reason, both Unix-style\nand Windows-style\r\nnew line characters are present."
  Text
   The command {\tt separate(s)} breaks {\tt s} at every occurrence of {\tt "\backslash r\backslash n"} or {\tt "\backslash n"}.
  Example
   separate(s)
  Text
   This is equivalent to using the @TO lines@ function.
  Example
   lines s
  Text
   Instead of breaking at new line characters, we can specify which character to break at.
   For instance, we can separate at every comma:
  Example
   separate(",", s)
  Text
   or at every space:
  Example
   separate(" ", s)
  Text
   In the last two examples we can see line breaks appear in the 
   output substrings, since we are no longer separating at them.
   (They are printed in the console as actual new lines, not 
       using escape characters.)
   
   Now let's try breaking at the string "om". This occurs three times in our
   string (in three uses of the word "some"), so {\tt s} is separated into four substrings.
   The separating characters "om" do not appear in any of the substrings.  
  Example
   t = separate("om", s)
  Text
   We can recover the original string using the @TO demark@ function.
  Example
   demark("om", t)
  Text
   In general, {\tt s = demark(x, separate(x, s))}.
   The exception to this rule is that {\tt demark("\backslash n", separate(s))} isn't
   necessarily equal to {\tt s}; this code will replace any {\tt "\backslash r\backslash n"} line breaks 
   in {\tt s}  with {\tt "\backslash n"} characters.
   
   To use a string longer than 2 characters to separate, and for much greater flexibility and
   control in specifying separation rules, see @TO separateRegexp@.
 SeeAlso
  lines
  demark
  format
  separateRegexp
  "strings and nets"
  "regular expressions"
///
