--- status: Rewritten July 2020
--- author(s): Dan, Mahrud
--- notes: functions below are all defined in regex.m2

doc ///
  Key
     match
    (match, String, String)
    [match, Flags]
    "lastMatch"
  Headline
    regular expression matching
  Usage
    b = match(re, str)
  Inputs
    re:String
      a regular expression
    str:String
      a subject string to be searched
    Flags=>ZZ
      option for choosing the regex flavor, such as @TO "RegexPOSIX"@ and @TO "RegexPerl"@
  Outputs
    b:Boolean
      whether the regular expression @TT "re"@ matches the string @TT "str"@
  Consequences
    Item
      the variable @TO "lastMatch"@ is set to the value returned by @TO "regex"@ which is called by @TO "match"@.
  Description
    Example
      match("asd", "--asd--")
      lastMatch
      match ("asd", "--as--")
      lastMatch
  SeeAlso
    "regular expressions"
    regex
    replace
///
