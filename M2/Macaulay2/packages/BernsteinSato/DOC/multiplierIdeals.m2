--- old format (commented out) ---
-- document {
--      Key => {(multiplierIdeal, Ideal, QQ), (multiplierIdeal, Ideal, ZZ), (multiplierIdeal, Ideal, List),
-- 	  [multiplierIdeal, Strategy], [jumpingCoefficients, Strategy],
-- 	  [multiplierIdeal, DegreeLimit], [jumpingCoefficients, DegreeLimit],
-- 	  multiplierIdeal},
--      Headline => "multiplier ideal",
--      Usage => "mI = multiplierIdeal(I,c)",
--      Inputs => {
-- 	  "I" => {"an ideal in a polynomial ring"},
-- 	  "c" => {"coefficient (or a list of coefficients)"}
-- 	  },
--      Outputs => {
-- 	  "mI" => Ideal => {"multiplier ideal ", EM "J_I(c)", " (or a list of)"}
-- 	  },
--      PARA {
-- 	  "Computes the multiplier ideal for given ideal and coefficient. "
-- 	  },
--      "There are three options for ", BOLD "Strategy", ":",
-- 	  UL {
-- 	       { BOLD "ViaElimination", " -- the default;"},
-- 	       { BOLD "ViaLinearAlgebra", " -- skips one expensive elimination step by using linear algebra;"},
-- 	       { BOLD "ViaColonIdeal", " -- same as elimination, but may be slightly faster."}
-- 	  },
--      "The option ", BOLD "DegreeLimit",
--      " specifies the maximal degree of polynomials to consider for membership in the multiplier ideal.",
--      "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
--      Caveat => {
-- 	  "When ", BOLD "Strategy=>ViaLinearAlgebra", " the option ", BOLD "DegreeLimit",
-- 	  " must be specified. The output it guaranteed to be the whole multiplier ideal only when dim(I)=0. ",
-- 	  "For positive-dimensional input the up-to-specified-degree part of the multiplier ideal is returned."
-- 	  },
--      EXAMPLE lines ///
-- R = QQ[x_1..x_4];
-- multiplierIdeal(ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
--      	  ///,
--      SeeAlso => { "jumpingCoefficients" }
--      }

doc ///
  Key
    (multiplierIdeal, Ideal, QQ)
    (multiplierIdeal, Ideal, ZZ)
    (multiplierIdeal, Ideal, List)
    [multiplierIdeal, Strategy]
    [jumpingCoefficients, Strategy]
    [multiplierIdeal, DegreeLimit]
    [jumpingCoefficients, DegreeLimit]
    multiplierIdeal
  Headline
    multiplier ideal
  Usage
    mI = multiplierIdeal(I, c)
  Inputs
    I:Ideal
      an ideal in a polynomial ring
    c:QQ
      coefficient (or a list of coefficients)
  Outputs
    mI:Ideal
      multiplier ideal $J_I(c)$ (or a list of)
  Description
    Text
      Computes the multiplier ideal for the given ideal and coefficient.

      There are three options for @TT "Strategy"@:

      @UL {
	   { TT "ViaElimination", " -- the default;"},
	   { TT "ViaLinearAlgebra", " -- skips one expensive elimination step by using linear algebra;"},
	   { TT "ViaColonIdeal", " -- same as elimination, but may be slightly faster."}
	  }@

      The option @TT "DegreeLimit"@
      specifies the maximal degree of polynomials to consider for membership in the multiplier ideal.
      See [@TO2 ("WeylAlgebras :: Works Cited", "BL10")@] for details.
    Example
      R = QQ[x_1..x_4];
      multiplierIdeal(ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
  Caveat
    When @TT "Strategy=>ViaLinearAlgebra"@ the option @TT "DegreeLimit"@
    must be specified. The output is guaranteed to be the whole multiplier ideal only when $\dim(I) = 0$.
    For positive-dimensional input the up-to-specified-degree part of the multiplier ideal is returned.
  SeeAlso
    jumpingCoefficients
///

--- old format (commented out) ---
-- document {
--      Key => {(jumpingCoefficients, Ideal), (jumpingCoefficients, Ideal, QQ, QQ), (jumpingCoefficients, Ideal, QQ, ZZ), (jumpingCoefficients, Ideal, ZZ, QQ), (jumpingCoefficients, Ideal, ZZ, ZZ), jumpingCoefficients},
--      Headline => "jumping coefficients and corresponding multiplier ideals",
--      Usage => "(cs, mI) = jumpingCoefficients I, (cs, mI) = jumpingCoefficients(I,a,b)",
--      Inputs => {
-- 	  "I" => {"an ideal in a polynomial ring"}
-- 	  },
--      Outputs => {
-- 	  "cs" => List => {"the list of jumping coefficients"},
-- 	  "mI" => List => {"the list of corresponding multiplier ideals"}
-- 	  },
--      PARA {
-- 	  "Computes the jumping coefficients and their multiplier ideals in an open interval (a,b). By default a = 0, b = ", TO "analyticSpread", " I. ",
-- 	  "The options are passed to ", TO "multiplierIdeal",".",
-- 	  },
--      "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
--      EXAMPLE lines ///
-- R = QQ[x_1..x_4];
-- jumpingCoefficients ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}
--      	  ///,
--      SeeAlso => { "multiplierIdeal" }
--      }

doc ///
  Key
    (jumpingCoefficients, Ideal)
    (jumpingCoefficients, Ideal, QQ, QQ)
    (jumpingCoefficients, Ideal, QQ, ZZ)
    (jumpingCoefficients, Ideal, ZZ, QQ)
    (jumpingCoefficients, Ideal, ZZ, ZZ)
    jumpingCoefficients
  Headline
    jumping coefficients and corresponding multiplier ideals
  Usage
    (cs, mI) = jumpingCoefficients I
    (cs, mI) = jumpingCoefficients(I, a, b)
  Inputs
    I:Ideal
      an ideal in a polynomial ring
  Outputs
    cs:List
      the list of jumping coefficients
    mI:List
      the list of corresponding multiplier ideals
  Description
    Text
      Computes the jumping coefficients and their multiplier ideals in an open interval $(a,b)$. By default $a = 0$ and $b$ is the analytic spread of $I$ (see @TO "analyticSpread"@).
      The options are passed to @TO "multiplierIdeal"@.

      See [@TO2 ("WeylAlgebras :: Works Cited", "BL10")@] for details.
    Example
      R = QQ[x_1..x_4];
      jumpingCoefficients ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}
  SeeAlso
    multiplierIdeal
///

--- old format (commented out) ---
-- document {
--      Key => {(hasRationalSing, List), hasRationalSing},
--      Headline => "check if a complete intersection has at most rational singularities",
--      Usage => "b = hasRationalSing F",
--      Inputs => {
-- 	  "F" => {"a regular sequence (of polynomials)"}
-- 	  },
--      Outputs => {
-- 	  "b" => Boolean => {"answers: are the singularities of the given variety at most rational?"}
-- 	  },
--      PARA {
-- 	  },
--      EXAMPLE lines ///
-- R = QQ[x_1..x_4];
-- multiplierIdeal(ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
--      	  ///,
--      SeeAlso => { "jumpingCoefficients" }
--      }

doc ///
  Key
    (hasRationalSing, List)
    hasRationalSing
  Headline
    check if a complete intersection has at most rational singularities
  Usage
    b = hasRationalSing F
  Inputs
    F:List
      a regular sequence (of polynomials)
  Outputs
    b:Boolean
      true if the singularities of the given variety are at most rational, otherwise false
  Description
    Text
      Tests whether the affine variety $V(F)$ cut out by the regular
      sequence $F$ has at most rational singularities.
    Example
      R = QQ[x_1..x_3];
      hasRationalSing {x_1^2 + x_2^2 + x_3^2}
  SeeAlso
    jumpingCoefficients
///

--- old format (commented out) ---
-- document {
--      Key => {(isInMultiplierIdeal, RingElement, Ideal, QQ), isInMultiplierIdeal, [isInMultiplierIdeal,Strategy],
-- 	  generalizedBFunction, mGeneralizedBFunction},
--      Headline => "multiplier ideal membership test",
--      Usage => "b = isInMultiplierIdeal(g,I,c)",
--      Inputs => {
-- 	  "g" => {"a polynomial"},
-- 	  "I" => {"an ideal in a polynomial ring"},
-- 	  "c" => {"coefficient (or a list of coefficients)"}
-- 	  },
--      Outputs => {
-- 	  "b" => Boolean => {"answers: is in the multiplier ideal ", EM "J_I(c)", "?"}
-- 	  },
--      PARA {
-- 	  "Test if the given polynomial is in the multiplier ideal for given ideal and coefficient. ",
-- 	  "In general, the test is cheaper than computing the whole multiplier ideal. "
-- 	  },
--      "There are two options for strategy:",
-- 	  UL {
-- 	       { BOLD "generalizedBFunction",
-- 	       " -- via computation of the generalized Bernstein-Sato polynomial"},
-- 	       { BOLD "mGeneralizedBFunction",
-- 	       " -- via computation of the m-generalized Bernstein-Sato polynomial"}
-- 	  },
--      "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
--      EXAMPLE lines ///
-- R = QQ[x_1..x_4];
-- isInMultiplierIdeal(x_1, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
-- isInMultiplierIdeal(x_1*x_2, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
--      	  ///,
--      SeeAlso => { "multiplierIdeal", "jumpingCoefficients", "generalB" }
--      }

doc ///
  Key
    (isInMultiplierIdeal, RingElement, Ideal, QQ)
    isInMultiplierIdeal
    [isInMultiplierIdeal, Strategy]
    generalizedBFunction
    mGeneralizedBFunction
  Headline
    multiplier ideal membership test
  Usage
    b = isInMultiplierIdeal(g, I, c)
  Inputs
    g:RingElement
      a polynomial
    I:Ideal
      an ideal in a polynomial ring
    c:QQ
      coefficient
  Outputs
    b:Boolean
      true if $g$ lies in the multiplier ideal $J_I(c)$, otherwise false
  Description
    Text
      Test if the given polynomial is in the multiplier ideal for the given ideal and coefficient.
      In general, the test is cheaper than computing the whole multiplier ideal.

      There are two options for strategy:

      @UL {
	   { TT "generalizedBFunction",
	   " -- via computation of the generalized Bernstein-Sato polynomial"},
	   { TT "mGeneralizedBFunction",
	   " -- via computation of the m-generalized Bernstein-Sato polynomial"}
	  }@

      See [@TO2 ("WeylAlgebras :: Works Cited", "BL10")@] for details.
    Example
      R = QQ[x_1..x_4];
      isInMultiplierIdeal(x_1, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
      isInMultiplierIdeal(x_1*x_2, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
  SeeAlso
    multiplierIdeal
    jumpingCoefficients
    generalB
///
