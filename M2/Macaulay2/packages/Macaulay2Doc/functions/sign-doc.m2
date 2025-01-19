doc ///
  Key
    sign
    (sign, Number)
    (sign, Constant)
  Headline
    sign (signum) function
  Usage
    sign x
  Inputs
    x:Number
  Outputs
    :{ZZ,CC}
  Description
    Text
      When @VAR "x"@ is real, then this returns 1 if it is positive, -1 if it
      is negative, and 0 if it is zero.
    Example
      sign 5
      sign (-3)
      sign 0
    Text
      If @VAR "x"@ is complex and nonzero, then this returns $x/|x|$.
    Example
      sign(-7*ii)
  SeeAlso
    "GradedLieAlgebras::sign(LieElement)"
    "Permutations::sign(Permutation)"
///
