--- author(s): M2Fest2005 DE

doc ///
  Key
    binomial
    (binomial, Number, Number)
    (binomial, Number, Constant)
    (binomial, Constant, Number)
    (binomial, Constant, Constant)
    (binomial, RingElement, ZZ)
  Headline
    binomial coefficient
  Usage
    binomial(n,k)
  Inputs
    n:{ZZ,RingElement}
    k:ZZ -- must be non-negative
  Outputs
    :Number
      the binomial coefficient, the coefficient of @TEX "$x^k$"@ in
      @TEX "$(1+x)^n$"@ or @TEX "$n(n-1)...(n-k+1)/k!$"@.
  Description
    Example
      binomial(13,6)
      binomial(-1,3)
    Text
      When either @TT "n"@ or @TT "k"@ are not @TO ZZ@ objects, the
      @TO Gamma@ function is used, i.e., @TEX "$\\binom{n}{k} =
      \\frac{\\Gamma(n + 1)}{\\Gamma(k + 1)\\Gamma(n - k + 1)}$"@.
    Example
      binomial(7.5, pi)
    Text
      A polynomial may also be used for @TT "n"@.
    Example
      R = QQ[x]
      binomial(x + 3, 3)
///
