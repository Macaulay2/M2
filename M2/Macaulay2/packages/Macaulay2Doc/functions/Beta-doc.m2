doc ///
  Key
    Beta
   (Beta,CC,CC)
   (Beta,CC,RR)
   (Beta,RR,CC)
   (Beta,RR,RR)
   (Beta,RR,RRi)
   (Beta,RRi,RR)
   (Beta,RRi,RRi)
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
   (regularizedBeta,CC,CC,CC)
   (regularizedBeta,CC,CC,RR)
   (regularizedBeta,CC,RR,CC)
   (regularizedBeta,CC,RR,RR)
   (regularizedBeta,RR,CC,CC)
   (regularizedBeta,RR,CC,RR)
   (regularizedBeta,RR,RR,CC)
   (regularizedBeta,RR,RR,RR)
   (regularizedBeta,RR,RR,RRi)
   (regularizedBeta,RR,RRi,RR)
   (regularizedBeta,RR,RRi,RRi)
   (regularizedBeta,RRi,RR,RR)
   (regularizedBeta,RRi,RR,RRi)
   (regularizedBeta,RRi,RRi,RR)
   (regularizedBeta,RRi,RRi,RRi)
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
  SeeAlso
    Beta
    inverseRegularizedBeta
///

doc ///
  Key
    inverseRegularizedBeta
   (inverseRegularizedBeta,RR,RR,RR)
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
  SeeAlso
    Beta
    regularizedBeta
///
