--- status: Rewritten July 2020
--- author(s): Dan, Mahrud
--- notes: 

doc ///
  Key
    replace
  Headline
    replacement in strings and lists
  Subnodes
    (replace, ZZ, Thing, BasicList)
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
      following the @TO2 {"regular expressions", "formatting syntax"}@
    str:String
      a subject string to be processed
    POSIX=>Boolean
      if true, interpret the @TT "re"@ using the POSIX Extended flavor, otherwise the Perl flavor
  Outputs
    :String
      obtained from @TT "str"@ by replacing its substrings matching @TT "re"@ by copies of @TT "replacement"@
  Description
    Text
      For an introduction to regular expressions, see @TO "regular expressions"@.
    Example
      replace("[a-z]+", "x", "Dog cat cat.")
    Text
      The @TT "replacement"@ string may contain @TO2 {"regular expressions", "formatting syntax"}@
      such as backreferences @TT "$1"@ or @TT "\\\\1"@, which will be replaced by the string matching
      the corresponding parenthesized subexpression of @TT "re"@.
    Example
      replace("(\\w+)\\.?", "A \\1.", "Dog cat cat.")
    Text
      Special operators such as the lowercase operator @TT "\\\\L"@ may also be used to manipulate the
      replacement substring.
    Example
      replace("(\\w+)\\.?", "A \\L$1.", "Dog cat cat.")
    Text
      Lookaheads and lookbehinds can be used to precisely control the regular expression matches.
    Example
      s = "catfish cats dogs";
      replace("cat(?!fish)s?", "\\U$&", s)
      replace("\\w+(?=s\\b)", "\\U$&", s)
      s = "goldfish swordfish catfish catdog";
      replace("\\w+(?=fish)", "\\U$&", s)
      replace("(?<=cat)\\w+", "\\U$&", s)
    Text
      The @TT "POSIX => true"@ option can be used to specify the POSIX Extended flavor for the regular
      expression used to match. Note that only the Perl flavor allows the use of lookaheads and lookbehinds.
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
    (replace, ZZ, Thing, BasicList)
  Headline
    copy a list, replacing an element
  Usage
    s = replace(i, t, l)
  Inputs
    i:ZZ
    t:Thing
    l:BasicList
  Outputs
    s:BasicList
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
