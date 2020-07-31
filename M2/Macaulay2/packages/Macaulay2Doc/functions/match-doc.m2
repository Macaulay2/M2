--- status: Rewritten July 2020
--- author(s): Dan, Mahrud
--- notes: functions below are all defined in regex.m2

doc ///
  Key
     match
    (match, String, String)
    [match, Flags]
    [match, Strategy]
    "lastMatch"
  Headline
    regular expression matching
  Usage
    match(re, str)
    match(patterns, str)
  Inputs
    re:String
      a @TO2 {"regular expressions", "regular expression"}@ describing a pattern
    str:String
      a subject string to be searched
    patterns:List
      a list of regular expressions
    Flags=>Symbol
      the regex flavor: either @TO "RegexPOSIX"@ or @TO "RegexPerl"@; @TO null@ indicates POSIX Extended flavor.
    Strategy=>Function
      logical quantifier for matching a list of patterns, typically @TO all@ or @TO any@
  Outputs
    :Boolean
      whether the string @TT "str"@ is a match for the regular expression @TT "re"@,
      or at least one of @TT "patterns"@ when @TT "Strategy => any"@ (default)
  Consequences
    Item
      the variable @TO "lastMatch"@ is set to the value returned by @TO "regex"@ which is called by @TO "match"@.
  Description
    Text
      For an introduction to regular expressions, see @TO "regular expressions"@.
    Example
      s = "three dogs, two catfishes, and a cat"
      match("cat", s)
      lastMatch
      substring(first lastMatch, s)
      match ("cats", s)
      lastMatch
    Text
      The @TT "Flags"@ option can be used to specify the regular expression flavor used to match.
      For instance, @TT "Flags => RegexPerl"@ allows the use of lookahead and lookbehinds.
    Example
      s = "catfish cat dog"
      match("cat(?!fish)", s, Flags => RegexPerl)
      substring(lastMatch#0#0, lastMatch#0#1 + 4, s)

      match("cat(?=fish)", s, Flags => RegexPerl)
      substring(lastMatch#0#0, lastMatch#0#1 + 4, s)

      match("(?<!cat)fish", "cat catfish dog", Flags => RegexPerl)
      match("(?<!cat)fish", "cat swordfish dog", Flags => RegexPerl)
    Text
      When the first input is a list, by default the output is true if @TT "str"@ is a match for at least
      one of the given regular expressions.
    Example
      match({"Cat", "Dog"}, "CatDog")
      match({"Cat", "Dog"}, "Catfish")
    Text
      Optinally, @TT "Strategy => all"@ indicates that the string should match every pattern in the list
      to be a match.
    Example
      match({"Cat", "Dog"}, "CatDog", Strategy => all)
      not match({"Cat", "Dog"}, "Catfish", Strategy => all)
    Text
      The @TT "Strategy"@ option is not used when the first input is not a list.
  SeeAlso
    "regular expressions"
    "strings and nets"
    regex
    replace
    substring
    any
    all
///
