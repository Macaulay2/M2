undocumented {
    (symbol **, Option, Option),
    (symbol **, Matrix, Number),
    (symbol **, Number, Matrix),
    (symbol **, Number, RingElement),
    (symbol **, PolynomialRing, PolynomialRing),
    (symbol **, PolynomialRing, QuotientRing),
    (symbol **, QuotientRing, PolynomialRing),
    (symbol **, QuotientRing, QuotientRing),
    (symbol **, RingElement, Matrix),
    (symbol **, RingElement, Number),
    (symbol **, RingElement, RingElement),
    (symbol **, Thing, InexactFieldFamily),
}

document {
    Key => symbol **,
    Headline => "a binary operator, usually used for tensor product or Cartesian product",
    SeeAlso => {symbol ^**}
}

document {
    Key => symbol ^**,
    Headline => "a binary operator, usually used for tensor or Cartesian power",
}

doc ///
Node
  Key
    (symbol **, List, List)
  Headline
    Cartesian product of two lists
  Description
    Example
      {1, 2} ** {10, 20, 30}
///
