doc ///
  Key
    Gamma
   (Gamma,CC)
   (Gamma,CC,CC)
   (Gamma,CC,RR)
   (Gamma,RR)
   (Gamma,RR,CC)
   (Gamma,RR,RR)
   (Gamma,RR,RRi)
   (Gamma,RRi)
   (Gamma,RRi,RR)
   (Gamma,RRi,RRi)
  Headline
    Gamma function
  Usage
    Gamma x
    Gamma(s, x)
  Inputs
    s:RR
    x:RR
  Outputs
    :RR
  Description
    Text
      With one argument, this is the @wikipedia "gamma function"@
      \(\Gamma(x) = \int_0^\infty t^{x-1}e^{-t}\,dt\).
    Example
      Gamma 6
    Text
      With two arguments, it is the (upper)
      @wikipedia "incomplete gamma function"@
      \(\Gamma(s, x) = \int_x^\infty t^{s-1}e^{-t}\,dt\).
    Example
      Gamma(3, 7)
  SeeAlso
    regularizedGamma
    inverseRegularizedGamma
///

doc ///
  Key
    regularizedGamma
   (regularizedGamma,CC,CC)
   (regularizedGamma,CC,RR)
   (regularizedGamma,RR,CC)
   (regularizedGamma,RR,RR)
   (regularizedGamma,RR,RRi)
   (regularizedGamma,RRi,RR)
   (regularizedGamma,RRi,RRi)
  Headline
    upper regularized gamma function
  Usage
    regularizedGamma(s, x)
  Inputs
    s:RR
    x:RR
  Outputs
    :RR
  Description
    Text
      This is the (upper) regularized gamma function
      \(Q(s, x) = \frac{\Gamma(s,x)}{\Gamma(s)}\).
    Example
      regularizedGamma(3, 7)
      Gamma(3, 7) / Gamma 3
  SeeAlso
    Gamma
    inverseRegularizedGamma
///

doc ///
  Key
    inverseRegularizedGamma
   (inverseRegularizedGamma,RR,RR)
  Headline
    inverse of the upper regularized gamma function
  Usage
    inverseRegularizedGamma(s, q)
  Inputs
    s:RR
    q:RR
  Outputs
    :RR
  Description
    Text
      This is the inverse of @TO regularizedGamma@ with respect to the second
      argument.
    Example
      regularizedGamma(3, 7)
      inverseRegularizedGamma(3, oo)
  SeeAlso
    Gamma
    regularizedGamma
///

doc ///
  Key
    lngamma
    (lngamma, RR)
    (lngamma, CC)
    (lngamma, RRi)
    (lngamma, Number)
  Headline
    logarithm of the Gamma function
  Usage
    lngamma x
  Inputs
    x:RR
  Outputs
    :{RR, CC}
      the logarithm of the @TO Gamma@ function of @TT "x"@ as a real
      or complex number.
  Description
    Example
      lngamma 2.1
      lngamma(-1.1)
      lngamma(-2.1)
      lngamma (-2.000000000000000000000000000000001p120)
  SeeAlso
    Gamma
///
