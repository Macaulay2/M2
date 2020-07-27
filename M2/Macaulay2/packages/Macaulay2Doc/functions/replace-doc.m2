--- status: Rewritten July 2020
--- author(s): Dan, Mahrud
--- notes: 

doc ///
  Key
    replace
  Headline
    replacement in strings and lists
///

doc ///
  Key
    (replace, String, String, String)
    [replace, Flags]
  Headline
    regular expression replacement of substrings
  Usage
    s = replace(re, replacement, str)
  Inputs
    re:String
      a regular expression
    replacement:String
      a replacement string
    str:String
      a subject string to be processed
    Flags=>ZZ
      option for choosing the regex flavor, such as @TO "RegexPOSIX"@ and @TO "RegexPerl"@
  Outputs
    s:String
      the string obtained from @TT "str"@ by replacing its substrings matching @TT "re"@ by copies of @TT "replacement"@.
  Description
    Text
      If a backslash followed by a digit occurs in @TT "replacement"@ then in the result they are replaced
      by the string matching the corresponding parenthesized subexpression of @TT "re"@.
    Example
      replace("[a-z]+", "x", "Dog cat cat.")
      replace("([a-z]+)", "(\\1)", "Dog cat cat.")
  SeeAlso
    "regular expressions"
    regex
    match
///

doc ///
  Key
    (replace, ZZ, Thing, VisibleList)
  Headline
    copy a list, replacing an element
  Usage
    s = replace(i, t, l)
  Inputs
    i:ZZ
    t:Thing
    l:VisibleList
  Outputs
    s:VisibleList
      a copy of the list @TT "l"@ in which @TT "t"@ has replaced the element at position @TT "i"@.
  Description
    Text
      A negative value of @TT "i"@ is taken relative to the end of the list.
    Example
      replace(4,   t, 0..10)
      replace(0,   t, 0..10)
      replace(10,  t, 0..10)
      replace(-1,  t, 0..10)
      replace(-11, t, 0..10)
///
