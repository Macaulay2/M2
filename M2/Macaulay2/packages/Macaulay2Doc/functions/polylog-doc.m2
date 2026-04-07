doc ///
  Key
    polylog
  Headline
    polylogarithm function
  Usage
    polylog_s z
  Inputs
    s:Number
    z:Number
  Description
    Text
      The @wikipedia "polylogarithm"@ function is defined by
      $\operatorname{Li}_s(z) = \sum_{k=1}^\infty\frac{z^k}{k^s}$.
      For example, $\operatorname{Li}_s(1)= \zeta(s)$.
    Example
      polylog_2 1
      zeta 2
    Text
      The polylogarithm is also available using the synonym @M2CODE "Li"@.
    Example
      Li_2 1
///
