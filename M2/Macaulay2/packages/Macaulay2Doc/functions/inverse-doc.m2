--- status: moved December 2020

doc ///
Node
  Key
    inverse
  Headline
    compute the inverse

Node
  Key
    (inverse, Matrix)
    (inverse, MutableMatrix)
    (inverse, RingMap)
  Usage
    inverse f
  Inputs
    f:{Matrix,MutableMatrix,RingMap}
  Outputs
    :{Matrix,MutableMatrix,RingMap}
      the inverse of @TT "f"@
  SourceCode
    (inverse, Matrix)
  Subnodes
    InverseMethod
///

undocumented methods InverseMethod
document {
    Key => InverseMethod,
    Headline => "compute reciprocals",
    TT "InverseMethod", " -- a key used under which is stored a method for computing multiplicative inverses.",
    PARA{},
    "Internal routines for computing powers call upon that method when the exponent is negative.",
    EXAMPLE ///
        code QQ.InverseMethod
        code CC.InverseMethod
	R = QQ[x];
	f = map(R, R, {2*x})
        RingMap.InverseMethod f
    ///
}
