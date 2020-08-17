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
    [(replace, String, String, String), POSIX]
  Headline
    replace substrings matching a regular expression
  Usage
    replace(re, replacement, str)
  Inputs
    re:String
      a @TO2 {"regular expressions", "regular expression"}@ describing a pattern
    replacement:String
      a replacement or regex formatting string, may include backreferences
    str:String
      a subject string to be processed
    POSIX=>Boolean
      if true, interpret the @TT "re"@ using the POSIX Extended flavor, otherwise the Perl flavor
  Outputs
    :String
      the string obtained from @TT "str"@ by replacing its substrings matching @TT "re"@ by copies of @TT "replacement"@.
  Description
    Text
      For an introduction to regular expressions, see @TO "regular expressions"@.
    Example
      replace("[a-z]+", "x", "Dog cat cat.")
    Text
      The @TT "replacement"@ string can contain backreferences such as @TT "$1"@ or @TT "\\\\1"@, which
      will be replaced by the string matching the corresponding parenthesized subexpression of @TT "re"@.
    Example
      replace("(\\w+)\\.?", "A \\1.", "Dog cat cat.")
    Text
      Special operators such as the lowercase operator @TT "\\\\L"@ may also be used to manipulate the
      replacement substring.
    Example
      replace("(\\w+)\\.?", "A \\L$1.", "Dog cat cat.")
  SeeAlso
    "regular expressions"
    "strings and nets"
    regex
    match
    separate
    (select, String, String, String)
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
