document {
    Key => {(roots, RingElement), roots, [(roots,RingElement),Unique], [(roots,RingElement),Precision] },
    Headline => "compute the roots of a polynomial or rational function",
    Usage => "roots p",
    Inputs => {
      "p" => "a univariate polynomial or rational function over ZZ, QQ, RR or CC.",
      Precision => { "the number of precision bits used to compute the roots.", "The default ", TO "precision", " is 53 bits for polynomials over ", TO "ZZ", " or ", TO "QQ", " and the same as the coefficient ring for ", TT "RR[x]", " or ", TT "CC[x]", "." },
      Unique => Boolean => { "whether to return multiple roots one or multiple times." },
    },
    Outputs => {List => {"The roots of p each one represented as an elements of ", TO "CC", ".", }},
    EXAMPLE {
      "RR_100[x]",
      "p = x^13 + 5*x^9 + 7*x^4 + x +1",
      "roots p",
      "o3#0",
    },
    EXAMPLE {
      "ZZ[x]",
      "p = x^13 + 5*x^9 + 7*x^4 + x +1",
      "roots(p^2, Precision=>150, Unique=>true)",
      "o7#0",
    },
    PARA {
      "If ", VAR "p", " is a rational function, then the roots of its ",
      "numerator are returned."},
    EXAMPLE {
      "QQ[x]",
      "p = (x^2 - 1)/x",
      "roots p"
    },
    PARA {
      "The roots are computed using ", TO "MPSolve", ".",
    },
}
