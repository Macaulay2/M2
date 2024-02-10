-- -*- coding: utf-8 -*-
--- status: 
--- author(s): L. Gold, Mahrud
--- notes: 

doc ///
Node
  Key
     poincare
    (poincare, Ring)
    (poincare, Module)
    (poincare, Ideal)
    (poincare, MonomialIdeal)
    installHilbertFunction
  Headline
    assemble degrees of a ring, module, or ideal into a polynomial
  Usage
    poincare M
  Inputs
    M:{Ring,Module,Ideal}
  Outputs
    :RingElement
      in the @TO2 (degreesRing, "degrees ring")@ of the ambient ring, which is a Laurent polynomial ring whose
      monomial @TT "T_0^(d_0) ... T_(r-1)^(d_(r-1))"@ correspond to the degree @TT "{d_0,...,d_(n-1)}"@
  Description
    Text
      This function computes the numerator of the reduced @TO2 (hilbertSeries, "Hilbert series")@ of a
      graded ring, or a graded module or ideal over a graded ring, which encodes information about the
      twists in the minimal free resolution of the object.
    Example
      S = ZZ/101[w..z];
      M = module monomialCurveIdeal(S, {1,3,4});
      betti res M
      poincare M
      hilbertSeries M
    Text
      When $S$ is a $\ZZ^r$-graded ring in $n$ variables and $M$ a graded $S$-module, the Hilbert series
      of $M$ is by definition the formal power series
      $$ HS(T_0,\dots,T_{n-1}) = \sum_{d\in\ZZ^r} \mathrm{dim}(M_d) T_0^{d_0} \cdots T_{n-1}^{d_{n-1}}. $$
      The coefficient of the term $T_0^{d_0} \cdots T_{n-1}^{d_{n-1}}$ in the series is the number of
      basis elements of $M_{d_0,...,d_{n-1}}$. When the multi-degree has a single component, the coefficient
      of $T^d$ is dimension of $M_d$.

      Since the Hilbert series is additive in exact sequences, the function @TT "poincare"@ can be used
      to get information about the minimal free resolution and hence the syzygies of $M$. For more details,
      see @TO "Hilbert functions and free resolutions"@.

      This polynomial is an element of the @TO2 (degreesRing, "degrees ring")@ of $S$. Notice that the
      monomial ordering used in the degrees ring is @TO "RevLex"@, so the polynomials in it will be
      displayed with the smallest exponents first.
  Synopsis
    Heading
      compute the polynomial for a ring
    Usage
      poincare R
    Description
      Example
        R = ZZ/101[x]/ideal(x^2);
        poincare R
        numerator hilbertSeries R
      Text
        Recall that the variables of the polynomial are the variables of the degrees ring.
      Example
        R = ZZ/101[x,y, DegreeRank => 2]/ideal(x^2*y);
        poincare R
        numerator hilbertSeries R
  Synopsis
    Heading
      compute the polynomial for a module
    Usage
      poincare M
    Description
      Example
        R = ZZ/101[w..z];
        M = module monomialCurveIdeal(R, {1,3,4});
        poincare M
        numerator reduceHilbert hilbertSeries M
  Synopsis
    Heading
      compute the polynomial for the quotient of a ring by an ideal
    Usage
      poincare I
    Description
      Text
        The this command computes the numerator of the Hilbert series of the @TO comodule@ @TT "R^1/I"@,
	where @TT "R"@ is the ring of @TT "I"@.
      Example
        R = ZZ/101[w..z];
        I = monomialCurveIdeal(R, {1,3,4});
        poincare I
	poincare comodule I
        numerator hilbertSeries I
        numerator reduceHilbert hilbertSeries I
	poincare module I
  Description
    Text
      @HEADER2 "Caching of the result of poincare"@

      When @TT "poincare M"@ is called on a module $M$, the result is cached in  @TT "M.cache.poincare"@
      for future reference. For ideals, the polynomial is cached in the @TO comodule@ of the ideal
      and for matrices, it is cached in the @TO cokernel@ of the matrix.
      This can speed the computation of Gröbner bases. For details, see @TO "computing Groebner bases"@.

      If the numerator $p$ of the Hilbert series for a module $M$ is known by other means, it can be
      manually stored by running:
    Code
      EXAMPLE { PRE //// poincare M = p -- cache poincare //// }
    Text
      This functionality used to be available by running @TT "installHilbertFunction(M, p)"@.

      For example, in the following example, the Hilbert function of 3 random polynomials should be the
      same as the Hilbert function for a complete intersection.
    Example
      R = ZZ/101[a..g];
      I = ideal random(R^1, R^{3:-3});
      p = poincare ideal(a^3, b^3, c^3)
      poincare I = p
      gbTrace = 3
      time poincare I
      time gens gb I;
    Text
      In this case, the savings is minimal, but often it can be dramatic.
      Another important situation is to compute a Gröbner basis using a different monomial order.
    Example
      R = QQ[a..d];
      I = ideal random(R^1, R^{3:-3});
      time p = poincare I
      S = QQ[a..d, MonomialOrder => Eliminate 2]
      J = substitute(I, S)
      poincare J = p
      gbTrace = 3
      time gens gb J;
      selectInSubring(1, gens gb J)
  Caveat
    This function may behave unpredictably in presence of variables of degree zero.
  SeeAlso
    poincareN
    (poincare, ChainComplex)
    degreesRing
    hilbertFunction
    hilbertSeries
    hilbertPolynomial
    reduceHilbert
    selectInSubring
    "Elimination::Elimination"
///

-- This will be consumed into the Complexes package
doc ///
Node
  Key
    (poincare, ChainComplex)
  Headline
    assemble degrees of a chain complex into a polynomial
  Usage
    poincare C
  Inputs
    C:ChainComplex
  Outputs
    :RingElement
      in the Laurent polynomial ring @TO2 (degreesRing, "degrees ring")@, whose variables correspond to the degrees of the ambient ring
  Description
    Text
      We compute @TO poincare@ for a chain complex.
    Example
      R = ZZ/32003[a..h];
      C = res ideal(a*b, c*d, e*f)
      poincare C
    Text
      Note that since the Hilbert series is additive in exact sequences, for a free resolution this only depends
      on the Betti numbers of the resolution. For more details, see @TO "Hilbert functions and free resolutions"@.
    Example
      b = betti C
      poincare b
  SeeAlso
    poincareN
    degreesRing
    hilbertFunction
    hilbertSeries
    hilbertPolynomial
    reduceHilbert
    (poincare, BettiTally)

Node
  Key
     poincareN
    (poincareN, ChainComplex)
  Headline
    assemble degrees into polynomial
  Usage
    poincareN C
  Inputs
    C:ChainComplex
  Outputs
    :RingElement
      in the Laurent polynomial ring @TO2 (degreesRing, "degrees ring")@, whose variables correspond to the degrees of the ambient ring
  Description
    Text
      This function encodes information about the degrees of basis elements of a free chain complex in a
      polynomial. The polynomial has terms $S^i T_0^{d_0} \cdots T_{n-1}^{d_{n-1}}$ in it for each basis
      element of @TT "C_i"@ with multi-degree @TT "{d_0,...,d_(n-1)}"@.
    Example
      R = ZZ/101[a,b,c, Degrees=>{1,1,2}];
      C = res cokernel vars R
      betti C
      p = poincareN C
    Text
      Setting the @TT "S"@ variable to -1 gives the polynomial calculated by @TO (poincare, ChainComplex)@.
    Example
      sub(p, {S => -1})
      poincare C
    Text
      Conversely, setting it to 1 gives the same polynomial for the direct sum of components of the complex.
    Example
      sub(p, {S => 1})
      poincare sum C
  SeeAlso
    poincare
    degreesRing
    hilbertFunction
    hilbertSeries
    hilbertPolynomial
    reduceHilbert
    (poincare, BettiTally)
    (sum, ChainComplex)
///
