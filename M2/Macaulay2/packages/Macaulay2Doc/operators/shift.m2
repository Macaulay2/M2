doc ///
  Key
    symbol <<
  Headline
    a binary operator, used for bit shifting or file output
  SeeAlso
    "printing to a file"
  Subnodes
    "left shift"
    (symbol <<, List, List)
///
doc ///
  Key
    "left shift"
    (symbol <<, ZZ, ZZ)
    (symbol <<, RR, ZZ)
    (symbol <<, CC, ZZ)
    (symbol <<, RRi, ZZ)
  Usage
    x << j
    I << j
  Inputs
    x:
    j:
    I:RRi
  Outputs
    :
      the number obtained from @TT "x"@ by shifting its binary representation
      leftward @TT "j"@ places
    :RRi
      an interval containing all shifts of the binary representations elements
      of @TT "I"@ by @TT "j"@ places leftward
  Description
    Example
      256 << 5
      256. << 555
  SeeAlso
    "right shift"
///

doc ///
  Key
    symbol >>
  Headline
    a binary operator, used for bit shifting or attaching optional inputs to functions
  Caveat
    Due to its low precedence, parentheses must be used on the left hand side
    when installing methods for this operator, e.g., @CODE "(X >> Y) := f"@.
  SeeAlso
    (symbol >>, OptionTable, Function)
  Subnodes
    "right shift"
///

doc ///
  Key
    "right shift"
    (symbol >>, ZZ, ZZ)
    (symbol >>, RR, ZZ)
    (symbol >>, CC, ZZ)
    (symbol >>, RRi, ZZ)
  Usage
    x >> j
    x >> I
  Inputs
    x:
    j:
    I:RRi
  Outputs
    :
      the integer obtained from @TT "x"@ by shifting its binary representation
      rightward @TT "j"@ places
    :RRi
      an interval containing all shifts of the binary representations elements
      of @TT "I"@ by @TT "j"@ places rightward
  Description
    Example
      256 >> 5
      256. >> 555
  SeeAlso
    "left shift"
///
