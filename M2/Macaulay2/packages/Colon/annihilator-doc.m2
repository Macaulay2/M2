-- moved from Macaulay2/packages/Macaulay2Doc/functions/ann-doc.m2

doc ///
Node
  Key
    annihilator
   (annihilator, Ideal)
   (annihilator, Module)
   (annihilator, RingElement)
   (annihilator, CoherentSheaf)
   [annihilator, Strategy]
  Headline
    the annihilator ideal
  Usage
    annihilator M
  Inputs
    M:
      a module, an ideal, a ring element, or a coherent sheaf
    Strategy=>Symbol
      either @TO "Quotient"@ or @TO "Intersection"@
  Outputs
    :Ideal
      the annihilator ideal $ann(M) = \{ f \in R | fM = 0 \}$ where @TT "R"@ is the ring of @TT "M"@
  Description
    Text
      You may use @TT "ann"@ as a synonym for @TT "annihilator"@.

      As an example, we compute the annihilator of the canonical module of the rational quartic curve.
    Example
      R = QQ[a..d];
      J = monomialCurveIdeal(R,{1,3,4})
      M = Ext^2(R^1/J, R)
      annihilator M
    Text
      For another example, we compute the annihilator of an element in a quotient ring.
    Example
      A = R/(a*b,a*c,a*d)
      ann a
    Text
      Macaulay2 uses two algorithms to compute annihilators. The default is to compute the annihilator
      of each generator of the module @TT "M"@ and to intersect these two by two. Each annihilator is
      done using a submodule quotient. The other algorithm computes the annihilator in one large computation
      and is used if @TT "Strategy => Quotient"@ is specified.
    Example
      annihilator(M, Strategy => Quotient)
  SeeAlso
    (quotient, Module, Module)
    Ext
///
