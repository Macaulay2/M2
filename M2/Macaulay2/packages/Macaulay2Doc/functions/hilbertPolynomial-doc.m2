--- status: DRAFT
--- author(s): L. Gold
--- notes: show ex of eventually = HF

doc ///
  Key
    hilbertPolynomial
  Headline
    compute the Hilbert polynomial
  Description
    Text
      In a singly graded ambient ring, the
      @TO2(hilbertFunction, "Hilbert function")@ eventually is a polynomial
      called the Hilbert polynomial. By default this polynomial is written in
      terms of the Hilbert polynomials of projective spaces because it is a
      good form for extracting geometric information from the polynomial. The
      Hilbert polynomial of @TT "P^i"@ is @TT "z |--> binomial(z + i, i)."@
  SeeAlso
    "degreesRing"
    "reduceHilbert"
    "poincare"
    "Complexes :: poincareN"
    "hilbertSeries"
    "hilbertFunction"
  Subnodes
    [hilbertPolynomial, Projective]
    ProjectiveHilbertPolynomial
    projectiveHilbertPolynomial
/// 

doc ///
  Key
    (hilbertPolynomial,Ring)
  Headline
    compute the Hilbert polynomial of the ring
  Usage
    hilbertPolynomial R
  Inputs
    R:Ring
  Outputs
    :ProjectiveHilbertPolynomial
      unless the option Projective is false
  Description
    Text
      We compute the @TO2(hilbertPolynomial, "Hilbert polynomial")@ of a
      coordinate ring of the rational quartic curve in @TT "P^3."@
    Example
      R = ZZ/101[a..d];
      S = coimage map(R, R, {a^4, a^3*b, a*b^3, b^4});
      presentation S
      h =  hilbertPolynomial S
      hilbertPolynomial(S, Projective=>false)
    Text
      The rational quartic curve in @TT "P^3"@ is therefore 'like' 4 copies of
      @TT "P^1"@, with three points missing. One can see this by noticing that
      there is a deformation of the rational quartic to the union of 4 lines,
      or 'sticks', which intersect in three successive points.

      These Hilbert polynomials can serve as
      @TO2 (hilbertFunction, "Hilbert functions")@ too since the values of the
      Hilbert polynomial eventually are the same as the Hilbert function.
    Example
      apply(5, k-> h(k))
      apply(5, k-> hilbertFunction(k,S))
///

doc ///
  Key
    (hilbertPolynomial,Module)
  Headline
    compute the Hilbert polynomial of the module
  Usage
    hilbertPolynomial M
  Inputs
    M:Module
  Outputs
    :ProjectiveHilbertPolynomial
      unless the option Projective is false
  Description
    Text
      We compute the @TO2(hilbertPolynomial, "Hilbert polynomial")@ of a
      module.
    Example
      R = QQ[a..d];
      M = module monomialCurveIdeal(R, {1,3,4});
      h = hilbertPolynomial M
      hilbertPolynomial(M, Projective=>false)
      --     "These Hilbert polynomials can serve as ",
      --      TO2 (hilbertFunction,"Hilbert functions"),
      --      " too since the values of the Hilbert polynomial eventually are
      --      the same as the Hilbert function. ",
      --      EXAMPLE {
      -- 	  "apply(5, k-> h(k))",
      -- 	  "apply(5, k-> hilbertFunction(k,M))"
      -- 	  }
///

doc ///
  Key
    (hilbertPolynomial,Ideal)
  Headline
    compute the Hilbert polynomial of the quotient of the ambient ring by the ideal
  Usage
    hilbertPolynomial I
  Inputs
    I:Ideal
  Outputs
    :ProjectiveHilbertPolynomial
      unless the option Projective is false
  Description
    Text
      We compute the @TO2(hilbertPolynomial, "Hilbert polynomial")@ of the
      quotient of the ambient ring by an ideal.
    Example
      R = QQ[a..d];
      I = monomialCurveIdeal(R, {1,3,4});
      h = hilbertPolynomial I
      hilbertPolynomial (R/I)
      hilbertPolynomial(I, Projective=>false)
    Text
      These Hilbert polynomials can serve as
      @TO2 (hilbertFunction, "Hilbert functions")@ too since the values of the
      Hilbert polynomial eventually are the same as the Hilbert function.
    Example
      apply(10, k-> h(k))
      apply(10, k-> hilbertFunction(k,I))
  Caveat
    As is often the case, calling this function on an ideal @TT "I"@ actually
    computes it for @TT "R/I"@ where @TT "R"@ is the ring of @TT "I"@.
///

doc ///
  Key
    [hilbertPolynomial, Projective]
  Headline
    choose how to display the Hilbert polynomial
  Usage
    hilbertPolynomial(...,Projective => b
  Inputs
    b:Boolean
      either true or false
  Description
    Text
      @TT "Projective => true"@ is an option to @TO "hilbertPolynomial"@ which
      specifies that the Hilbert polynomial produced should be expressed in
      terms of the Hilbert polynomials of projective spaces. This is the
      default.

      @TT "Projective => false"@ is an option to @TO "hilbertPolynomial"@ which
      specifies that the Hilbert polynomial produced should be expressed as a
      polynomial in the variable @TT "i"@.

      We compute the @TO2(hilbertPolynomial, "Hilbert polynomial")@ of a
      coordinate ring of the rational quartic curve in @TT "P^3."@
    Example
      R = ZZ/101[a..d];
      S = coimage map(R, R, {a^4, a^3*b, a*b^3, b^4});
      hilbertPolynomial S
      hilbertPolynomial(S, Projective=>false)
    Text
      When the option Projective is false, the variable @TT "i"@ is a local
      variable. The command @TT "use ring"@will make @TT "i"@into a global
      variable.
  SeeAlso
    "ProjectiveHilbertPolynomial"
///
