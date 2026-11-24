undocumented {
    (symbol -, ZZ, ProjectiveHilbertPolynomial),
    (symbol -, ProjectiveHilbertPolynomial),
    (symbol -, ProjectiveHilbertPolynomial, ProjectiveHilbertPolynomial),
    (symbol -, ProjectiveHilbertPolynomial, ZZ),
}

doc ///
  Key
    minus
  Headline
    additive inverse
  Usage
    minus(x)
  Description
    Text
      @TT "minus(x)"@ yields @TT "-x"@.

      See also @TO "difference"@.
///

doc ///
  Key
    difference
  Headline
    difference
  Usage
    difference(x,y)
  Description
    Text
      @TT "difference(x,y)"@ returns @TT "x-y"@.
///

doc ///
  Key
    symbol -
    (symbol -, ZZ)
    (symbol -, ZZ, CC)
    (symbol -, ZZ, CCi)
    (symbol -, ZZ, QQ)
    (symbol -, ZZ, RR)
    (symbol -, ZZ, RRi)
    (symbol -, ZZ, ZZ)
    (symbol -, QQ)
    (symbol -, QQ, CC)
    (symbol -, QQ, CCi)
    (symbol -, QQ, QQ)
    (symbol -, QQ, RR)
    (symbol -, QQ, RRi)
    (symbol -, QQ, ZZ)
    (symbol -, RR)
    (symbol -, RR, CC)
    (symbol -, RR, CCi)
    (symbol -, RR, QQ)
    (symbol -, RR, RR)
    (symbol -, RR, RRi)
    (symbol -, RR, ZZ)
    (symbol -, RRi)
    (symbol -, RRi, CC)
    (symbol -, RRi, CCi)
    (symbol -, RRi, QQ)
    (symbol -, RRi, RR)
    (symbol -, RRi, RRi)
    (symbol -, RRi, ZZ)
    (symbol -, CC)
    (symbol -, CC, CC)
    (symbol -, CC, CCi)
    (symbol -, CC, InfiniteNumber)
    (symbol -, CC, QQ)
    (symbol -, CC, RR)
    (symbol -, CC, RRi)
    (symbol -, CC, ZZ)
    (symbol -, CCi)
    (symbol -, CCi, CC)
    (symbol -, CCi, CCi)
    (symbol -, CCi, QQ)
    (symbol -, CCi, RR)
    (symbol -, CCi, RRi)
    (symbol -, CCi, ZZ)
    (symbol -, Constant)
    (symbol -, Constant, Constant)
    (symbol -, Constant, InexactNumber)
    (symbol -, Constant, Number)
    (symbol -, Constant, RingElement)
    (symbol -, InexactNumber, Constant)
    (symbol -, InexactNumber, RingElement)
    (symbol -, InfiniteNumber)
    (symbol -, InfiniteNumber, CC)
    (symbol -, InfiniteNumber, InfiniteNumber)
    (symbol -, InfiniteNumber, Number)
    (symbol -, Matrix)
    (symbol -, Matrix, Matrix)
    (symbol -, Matrix, Number)
    (symbol -, Matrix, RingElement)
    (symbol -, MutableMatrix)
    (symbol -, MutableMatrix, MutableMatrix)
    (symbol -, Number, Constant)
    (symbol -, Number, InfiniteNumber)
    (symbol -, Number, Matrix)
    (symbol -, Number, RingElement)
    (symbol -, Number, Vector)
    (symbol -, RingElement)
    (symbol -, RingElement, Constant)
    (symbol -, RingElement, InexactNumber)
    (symbol -, RingElement, Matrix)
    (symbol -, RingElement, Number)
    (symbol -, RingElement, RingElement)
    (symbol -, RingElement, Vector)
    (symbol -, Tally)
    (symbol -, List)
    (symbol -, Vector)
    (symbol -, Vector, Number)
    (symbol -, Vector, RingElement)
    (symbol -, Vector, Vector)
  Headline
    a unary or binary operator, usually used for negation or subtraction
  Usage
    -y
    x-y
  Description
    Text
      In most cases, this operator refers to standard negation or subtraction.

      In many cases, the integer 1 can be used as the identity, and scalars
      function as multiples of the identity. For example, the 1 below refers to
      the identity matrix and the 2 to twice the identity matrix.
    Example
      M = matrix{{1,2,3},{2,3,4},{3,4,6}}
      M-1, M-2
    Text
      @HEADER3 "Intervals"@

      If one of the inputs is an @TO "RRi"@ or a @TO "CCi"@, the output is an
      interval containing all differences of pairs in the inputs.
    Example
      2-interval(1,3)
      interval(1,3)-interval(-1,2)
      interval(-1,1)-interval(-1,1)
      interval(1,2+3*ii)-(2+ii)
  SeeAlso
    "difference"
    "minus"
///

doc ///
  Key
    (symbol -, List, List)
  Headline
    difference of two vectors
  Usage
    v-w
  Inputs
    v:
      a list interpreted as a vector
    w:
      a list interpreted as a vector
  Outputs
    :
      the difference of the two vectors
  Description
    Example
      {1,2,3} - {1,5,6}
///
