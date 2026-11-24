--- status: draft
--- author(s): Decker, Popescu
--- notes:

-- TODO: combine some of these nodes
doc ///
  Key
    codim
  Headline
    compute the codimension
  SeeAlso
    dim
  Subnodes
    [(codim, Ideal), Generic]
///

doc ///
  Key
    [(codim, Ideal), Generic]
    [(codim, MonomialIdeal), Generic]
    [(codim, Module), Generic]
    [(codim,PolynomialRing), Generic]
    [(codim,QuotientRing), Generic]
  Usage
    codim(...,Generic=>true)
  Consequences
    Item
      Allows the computation of the codimension to proceed without an error
      message, even if the ring is defined over the integers. The computation
      proceeds by effectively tensoring first with the rational numbers.
///
doc ///
  Key
    (codim,QuotientRing)
    (codim, PolynomialRing)
  Usage
    codim R
  Inputs
    R:
  Outputs
    :ZZ
      the codimension of @TT "R"@
  Description
    Text
      Computes the codimension of the presentation ideal of @TT "R"@ over its
      ambient polynomial ring.
    Example
      R = QQ[x,y]/(ideal(x,y) * ideal(x-1))
      codim R
    Text
      However, the following may not be the expected result.
    Example
      R = QQ[x,y]/(ideal(x,y) * ideal(x-1))
      codim R
      codim (R/x)
  SeeAlso
    (dim,QuotientRing)
///
doc ///
  Key
    (codim,Module)
  Headline
    codimension of the support of a module
  Usage
    codim M
  Inputs
    M:
      a module over a ring @TT "R"@
  Outputs
    :ZZ
  Description
    Text
      Computes the codimension of the support of the module as given by
      @TT "dim(R) - dim(M)"@.
    Example
      R = ZZ/101[a..d];
      M = coker matrix{{a,b},{c,d}}
      codim M
    Text
      The returned value is the usual codimension if @TT "R"@ is an integral
      domain or, more generally, equidimensional.
  SeeAlso
    (dim,Module)
///


doc ///
  Key
    (codim,MonomialIdeal)
  Usage
    codim I
  Inputs
    I:
  Outputs
    :ZZ
  Description
    Text
      Computes the codimension of the monomial ideal @TT "I"@.
    Example
      R = ZZ/101[a..e];
      codim monomialIdeal (b,c,d)
      codim monomialIdeal (b^3,c^2)
  SeeAlso
    (dim,Ideal)
    (dim,MonomialIdeal)
    (codim,Ideal)
///

doc ///
  Key
    (codim,Ideal)
  Usage
    codim I
  Inputs
    I:
  Outputs
    :ZZ
      @TT "dim(R) - dim(R/I)"@, where @TT "R"@ is the ring containing @TT "I"@.
  Description
    Text
      When R is equidimensional, this quantity is the codimension of the ideal
      @TT "I"@.
    Example
      R = ZZ/101[a..e];
      I = monomialCurveIdeal(R,{2,3,5,7})
      J = ideal presentation singularLocus(R/I);
      codim J
      radical J
    Text
      The following may not be the expected result, because the ring is not
      equidimensional.
    Example
      R = QQ[x,y]/(ideal(x,y) * ideal(x-1))
      codim ideal(x,y)
  SeeAlso
    (dim,Ideal)
    (dim,MonomialIdeal)
    (codim,MonomialIdeal)
///
