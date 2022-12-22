doc ///
  Key
    changeBase
    (changeBase, ZZ, ZZ)
    (changeBase, String, ZZ)
    (changeBase, String, ZZ, ZZ)
  Headline
    change integer base
  Usage
    changeBase(n, newbase)
    changeBase(str, oldbase)
    changeBase(str, oldbase, newbase)
  Inputs
    n:ZZ
    str:String
    oldbase:ZZ
    newbase:ZZ
  Outputs
    :{ZZ, String}
  Description
    Text
      When both arguments are integers, the return value is a string
      representation of the first argument in the base given by the second
      argument.
    Example
      changeBase(255, 16)
    Text
      Bases from 2 to 62 are supported.  If @TT "newbase"@ is between 11 and
      36, then the digits representing 10 through @TT "newbase - 1"@ are the
      lowercase letters.
    Example
      for n from 10 to 35 list changeBase(n, 36)
    Text
      If @TT "newbase"@ is between 37 and 62, then the digits representing 10
      through 35 are uppercase letters and the digits representing 36 through
      @TT "newbase - 1"@ are lowercase letters.
    Example
      for n from 10 to 61 list changeBase(n, 62)
    Text
      If the first argument is a string, then we get the inverse operation,
      i.e., the @TO ZZ@ object that the string represents in the given base.
    Example
      changeBase("ff", 16)
    Text
      Uppercase letters are also allowed when @TT "oldbase"@ is between 11 and
      36, as there is no ambiguity.
    Example
      changeBase("FF", 16)
    Text
      If @TT "oldbase"@ is 0, then the base is determined by the standard C
      prefix, i.e., @TT "0b"@ or @TT "0B"@ for binary, @TT "0"@ for octal,
      @TT "0x"@ or @TT "0X"@ for hexadecimal, and no prefix for decimal.
    Example
      apply({"0b10", "0B10", "010", "0x10", "0X10", "10"}, s -> changeBase(s, 0))
    Text
      If a string and two integer arguments are given, then the integer
      represented by the string is converted from one base to another,
      returning a string.
    Example
      changeBase("ff", 16, 2)
///
