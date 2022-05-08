doc ///
  Key
    Beta
  Headline
    Beta function
  Usage
    Beta(x, y)
  Inputs
    x:RR
    y:RR
  Outputs
    :RR -- the beta function of @TT "x"@ and @TT "y"@
  Description
    Text
      This is the @wikipedia "beta function"@
      \(B(x,y) = \int_0^1 t^{x - 1}(1 - t)^{y - 1}\,dt\).
    Example
      Beta(3, 4)
  SeeAlso
    regularizedBeta
    inverseRegularizedBeta
///

doc ///
  Key
    regularizedBeta
  Headline
    regularized beta function
  Usage
    regularizedBeta(x, a, b)
  Inputs
    x:RR
    a:RR
    b:RR
  Outputs
    :RR
  Description
    Text
      This is the regularized (incomplete) @wikipedia "beta function"@
      \(I_x(a, b) = \frac{1}{B(a, b)}\int_0^x t^{a - 1} (1 - t)^{b - 1}\,dt\).
    Example
      regularizedBeta(1/2, 3, 4)
      1/Beta(3,4) * integrate(t -> t^2 * (1 - t)^3, 0, 1/2)
  Caveat
    This function always returns a double precision real number (53 bits)
    regardless of the precision of its arguments.
  SeeAlso
    Beta
    inverseRegularizedBeta
///

doc ///
  Key
    inverseRegularizedBeta
  Headline
    inverse of the regularized beta function
  Usage
    inverseRegularizedBeta(y, a, b)
  Inputs
    y:RR
    a:RR
    b:RR
  Outputs
    :RR
  Description
    Text
      This is the inverse of @TO regularizedBeta@ with respect to the first
      argument.
    Example
      regularizedBeta(1/2, 3, 4)
      inverseRegularizedBeta(oo, 3, 4)
  Caveat
    This function always returns a double precision real number (53 bits)
    regardless of the precision of its arguments.
  SeeAlso
    Beta
    regularizedBeta
///
