--- status: DRAFT 
--- author(s): L.Gold
--- note:

doc ///
  Key
    hilbertSeries
  Headline
    compute the Hilbert series
  Description
    Text
      The Hilbert series is the formal power series in the variables of the
      degrees ring whose coefficients are the dimensions of the corresponding
      graded component.Note that the series is provided as a type of expression
      called a @TO "Divide"@.
  SeeAlso
    "degreesRing"
    "reduceHilbert"
    "poincare"
    "Complexes :: poincareN"
    "hilbertPolynomial"
    "hilbertFunction"
  Subnodes
    [hilbertSeries, Order]
    [hilbertSeries, Reduce]
///

doc ///
  Key
    (hilbertSeries, PolynomialRing)
    (hilbertSeries,QuotientRing)
  Headline
    compute the Hilbert series of a ring
  Usage
    hilbertSeries R
  Inputs
    R:
  Outputs
    :Divide
      the Hilbert series
  Description
    Text
      We compute the @TO2(hilbertSeries, "Hilbert series")@ of a ring.
    Example
      R = ZZ/101[x, Degrees => {2}]/ideal(x^2);
      s = hilbertSeries R
      numerator s
      poincare R
    Text
      Recall that the variables of the power series are the variables of the
      @TO2 (degreesRing, "degrees ring")@.
    Example
      R=ZZ/101[x, Degrees => {{1,1}}]/ideal(x^2);
      s = hilbertSeries R
      numerator s
      poincare R
///

doc ///
  Key
    (hilbertSeries,Module)
  Headline
    compute the Hilbert series of the module
  Usage
    hilbertSeries M
  Inputs
    M:
  Outputs
    :Divide
      the Hilbert series
  Description
    Text
      We compute the @TO2 (hilbertSeries, "Hilbert series")@ of a module.
    Example
      R = ZZ/101[x, Degrees => {2}];
      M = module ideal x^2
      s = hilbertSeries M
      numerator s
      poincare M
    Text
      Recall that the variables of the power series are the variables of the
      @TO2 (degreesRing, "degrees ring")@.
    Example
      R=ZZ/101[x, Degrees => {{1,1}}];
      M = module ideal x^2;
      s = hilbertSeries M
      numerator s
      poincare M
///

doc ///
  Key
    (hilbertSeries, Ideal)
  Headline
    compute the Hilbert series of the quotient of the ambient ring by the ideal
  Usage
    hilbertSeries I
  Inputs
    I:Ideal
  Outputs
    :Divide
      the Hilbert series
  Description
    Text
      We compute the @TO2 (hilbertSeries, "Hilbert series")@ of @TT "R/I"@, the
      quotient of the ambient ring by the ideal. Caution: For an ideal @TT "I"@
      running @TT "hilbertSeries I "@calculates the Hilbert series of
      @TT "R/I"@.
    Example
      R = ZZ/101[x, Degrees => {2}];
      I = ideal x^2
      s = hilbertSeries I
      numerator s
      poincare I
      reduceHilbert s
    Text
      Recall that the variables of the power series are the variables of the
      @TO2 (degreesRing, "degrees ring")@.
    Example
      R=ZZ/101[x, Degrees => {{1,1}}];
      I = ideal x^2;
      s = hilbertSeries I
      numerator s
      poincare I
      reduceHilbert s
  Caveat
    As is often the case, calling this function on an ideal @TT "I"@ actually
    computes it for @TT "R/I"@ where @TT "R"@ is the ring of @TT "I"@.
///
doc ///
  Key
    (hilbertSeries,ProjectiveHilbertPolynomial)
  Headline
    compute the Hilbert series of a projective Hilbert polynomial
  Usage
    hilbertSeries P
  Inputs
    P:ProjectiveHilbertPolynomial
  Outputs
    :Divide
      the Hilbert series
  Description
    Text
      We compute the @TO2 (hilbertSeries, "Hilbert series")@ of a projective
      Hilbert polynomial.
    Example
      P = projectiveHilbertPolynomial 3
      s = hilbertSeries P
      numerator s
    Text
      Computing the @TO2 (hilbertSeries, "Hilbert series")@ of a projective
      variety can be useful for finding the h-vector of a simplicial complex
      from its f-vector. For example, consider the octahedron. The ideal below
      is its Stanley-Reisner ideal. We can see its f-vector (1, 6, 12, 8) in
      the Hilbert polynomial, and then we get the h-vector (1,3,3,1) from the
      coefficients of the Hilbert series projective Hilbert polynomial.
    Example
      R = QQ[a..h];
      I = ideal (a*b, c*d, e*f);
      P=hilbertPolynomial(I)
      s = hilbertSeries P
      numerator s
///

doc ///
  Key
    [hilbertSeries, Order]
  Headline
    display the truncated power series expansion
  Usage
    hilbertSeries(..., Order => n)
  Inputs
    n:ZZ
  Consequences
    Item
      The output is no longer of type @TO "Divide"@. It is a polynomial in the
      @TO2 (degreesRing, "degrees ring")@.
  Description
    Text
      We compute the Hilbert series both without and with the optional
      argument. In the second case notice the terms of power series expansion
      up to, but not including, degree 5 are displayed rather than expressing
      the series as a rational function. The polynomial expression is an
      element of a Laurent polynomial ring that is the
      @TO2 (degreesRing, "degrees ring")@ of the ambient ring.
    Example
      R = ZZ/101[x,y];
      hilbertSeries(R/x^3)
      hilbertSeries(R/x^3, Order =>5)
    Text
      If the ambient ring is multigraded, then the
      @TO2 (degreesRing, "degrees ring")@ has multiple variables.
    Example
      R = ZZ/101[x,y, Degrees=>{{1,2},{2,3}}];
      hilbertSeries(R/x^3, Order =>5)
    Text
      The heft vector provides a suitable monomial ordering and degrees in the
      ring of the Hilbert series.
    Example
      R = QQ[a..d,Degrees=>{{-2,-1},{-1,0},{0,1},{1,2}}]
      hilbertSeries(R, Order =>3)
      degrees ring oo
      heft R
///

doc ///
  Key
    [hilbertSeries,Reduce]
  Headline
    reduce the Hilbert series
  Usage
    hilbertSeries(..., Reduce => true)
  Consequences
    Item
      the resulting rational function is reduced by cancelling factors of the
      numerator that occur explicitly as factors of the denominator. See also
      @TO "reduceHilbert"@.
  Description
    Example
      R = QQ[x,y,z];
      hilbertSeries ideal (x,y)
      hilbertSeries(ideal (x,y), Reduce => true)
///
	  
