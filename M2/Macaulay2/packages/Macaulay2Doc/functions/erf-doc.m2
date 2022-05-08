doc ///
  Key
    erf
  Headline
    error function
  Usage
    erf x
  Inputs
    x:RR
  Outputs
    :RR -- the error function of @TT "x"@
  Description
    Text
      The @wikipedia "error function"@
      \(\operatorname{erf} x = \frac{2}{\sqrt\pi}\int_0^x e^{-t^2}\,dt\).
    Example
      erf 2
  SeeAlso
    erfc
    inverseErf
///

doc ///
  Key
    erfc
  Headline
    complementary error function
  Usage
    erfc x
  Inputs
    x:RR
  Outputs
    :RR -- the complementary error function of @TT "x"@
  Description
    Text
      The complementary @wikipedia "error function"@
      \(\operatorname{erfc} x = \frac{2}{\sqrt\pi}\int_x^\infty e^{-t^2}\,dt\).
    Example
      erfc 2
  SeeAlso
    erf
    inverseErf
///

doc ///
  Key
    inverseErf
  Headline
    inverse error function
  Usage
    inverseErf y
  Inputs
    y:RR
  Outputs
    :RR -- the inverse error function of @TT "y"@
  Description
    Text
      The inverse of @TO "erf"@.
    Example
      erf 2
      inverseErf oo
  Caveat
    This function always returns a double precision real number (53 bits)
    regardless of the precision of its arguments.
  SeeAlso
    erf
    erfc
///
