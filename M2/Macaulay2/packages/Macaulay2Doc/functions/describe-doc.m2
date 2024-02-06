--- status: moved Aug 2022
--- author(s):
--- notes:

doc ///
Node
  Key
     describe
    (describe, FractionField)
    (describe, GaloisField)
    (describe, Matrix)
    (describe, Module)
    (describe, Monoid)
    (describe, PolynomialRing)
    (describe, QuotientRing)
    (describe, RingMap)
    (describe, Thing)
    (describe, Vector)
  Headline
    real description
  Usage
    describe X
  Inputs
    X:Thing
  Outputs
    :Expression
  Description
    Text
      @TT "describe X"@ returns an @TT "Expression"@ containing the real description
      of @TT "X"@, bypassing the feature that causes certain types of things to acquire,
      for brevity, the names of global variables to which they are assigned.

      For example, it also displays the options used at creation of polynomial rings.
    Example
      R = ZZ/101[a,b,c_1,c_2];
      R
      describe R
      toString describe R
      toExternalString R
      QQ[x, dx, WeylAlgebra => { x => dx }]
      describe oo
  SeeAlso
    toString
    toExternalString
///
