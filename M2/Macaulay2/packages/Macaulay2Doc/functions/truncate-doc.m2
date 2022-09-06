doc ///
  Key
    (truncate, Number)
    (truncate, Constant)
  Headline
    round a number toward zero
  Usage
    truncate x
  Inputs
    x:Number
  Outputs
    :ZZ -- the truncation of @TT "x"@
  Description
    Text
      The given number is rounded toward zero.
    Example
      truncate 3.7
      truncate(-3.7)
    Text
      For complex numbers, the real part is truncated.
    Example
      truncate(2.5 + ii)
  SeeAlso
    (floor, Number)
    (ceiling, Number)
///
