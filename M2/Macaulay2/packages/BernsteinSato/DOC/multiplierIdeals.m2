document { 
     Key => {(multiplierIdeal, Ideal, QQ), (multiplierIdeal, Ideal, ZZ), (multiplierIdeal, Ideal, List), 
	  [multiplierIdeal, Strategy], [jumpingCoefficients, Strategy], 
	  [multiplierIdeal, DegreeLimit], [jumpingCoefficients, DegreeLimit], 
	  multiplierIdeal},
     Headline => "multiplier ideal",
     Usage => "mI = multiplierIdeal(I,c)",
     Inputs => {
	  "I" => {"an ideal in a polynomial ring"},
	  "c" => {"coefficient (or a list of coefficients)"}
	  },
     Outputs => {
	  "mI" => Ideal => {"multiplier ideal ", EM "J_I(c)", " (or a list of)"}
	  },
     PARA {
	  "Computes the multiplier ideal for given ideal and coefficient. "
	  },
     "There are three options for ", BOLD "Strategy", ":",
	  UL { 
	       { BOLD "ViaElimination", " -- the default;"},
	       { BOLD "ViaLinearAlgebra", " -- skips one expensive elimination step by using linear algebra;"},
	       { BOLD "ViaColonIdeal", " -- same as elimination, but may be slightly faster."}
	  },
     "The option ", BOLD "DegreeLimit", 
     " specifies the maximal degree of polynomials to consider for membership in the multiplier ideal.",
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
     Caveat => {
	  "When ", BOLD "Strategy=>ViaLinearAlgebra", " the option ", BOLD "DegreeLimit", 
	  " must be specified. The output it guaranteed to be the whole multiplier ideal only when dim(I)=0. ",
	  "For positive-dimensional input the up-to-specified-degree part of the multiplier ideal is returned."
	  },
     EXAMPLE lines ///
R = QQ[x_1..x_4];
multiplierIdeal(ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
     	  ///,
     SeeAlso => { "jumpingCoefficients" }
     }

document { 
     Key => {(jumpingCoefficients, Ideal), (jumpingCoefficients, Ideal, QQ, QQ), (jumpingCoefficients, Ideal, QQ, ZZ), (jumpingCoefficients, Ideal, ZZ, QQ), (jumpingCoefficients, Ideal, ZZ, ZZ), jumpingCoefficients},
     Headline => "jumping coefficients and corresponding multiplier ideals",
     Usage => "(cs, mI) = jumpingCoefficients I, (cs, mI) = jumpingCoefficients(I,a,b)",
     Inputs => {
	  "I" => {"an ideal in a polynomial ring"}
	  },
     Outputs => {
	  "cs" => List => {"the list of jumping coefficients"},
	  "mI" => List => {"the list of corresponding multiplier ideals"}
	  },
     PARA {
	  "Computes the jumping coefficients and their multiplier ideals in an open interval (a,b). By default a = 0, b = ", TO "analyticSpread", " I. ",
	  "The options are passed to ", TO "multiplierIdeal",".",
	  },
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
     EXAMPLE lines ///
R = QQ[x_1..x_4];
jumpingCoefficients ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}
     	  ///,
     SeeAlso => { "multiplierIdeal" }
     }

document { 
     Key => {(hasRationalSing, List), hasRationalSing},
     Headline => "check if a complete intersection has at most rational singularities",
     Usage => "b = hasRationalSing F",
     Inputs => {
	  "F" => {"a regular sequence (of polynomials)"}
	  },
     Outputs => {
	  "b" => Boolean => {"answers: are the singularities of the given variety at most rational?"}
	  },
     PARA {
	  
	  },
     EXAMPLE lines ///
R = QQ[x_1..x_4];
multiplierIdeal(ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
     	  ///,
     SeeAlso => { "jumpingCoefficients" }
     }


document { 
     Key => {(isInMultiplierIdeal, RingElement, Ideal, QQ), isInMultiplierIdeal, [isInMultiplierIdeal,Strategy], 
	  generalizedBFunction, mGeneralizedBFunction},
     Headline => "multiplier ideal membership test",
     Usage => "b = isInMultiplierIdeal(g,I,c)",
     Inputs => {
	  "g" => {"a polynomial"},
	  "I" => {"an ideal in a polynomial ring"},
	  "c" => {"coefficient (or a list of coefficients)"}
	  },
     Outputs => {
	  "b" => Boolean => {"answers: is in the multiplier ideal ", EM "J_I(c)", "?"}
	  },
     PARA {
	  "Test if the given polynomial is in the multiplier ideal for given ideal and coefficient. ",
	  "In general, the test is cheaper than computing the whole multiplier ideal. "
	  },
     "There are two options for strategy:",
	  UL { 
	       { BOLD "generalizedBFunction", 
	       " -- via computation of the generalized Bernstein-Sato polynomial"},
	       { BOLD "mGeneralizedBFunction", 
	       " -- via computation of the m-generalized Bernstein-Sato polynomial"}
	  },
     "See ", EM "Berkesch and Leykin", " ``Algorithms for Bernstein-Sato polynomials and multiplier ideals'' for details.",
     EXAMPLE lines ///
R = QQ[x_1..x_4];
isInMultiplierIdeal(x_1, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
isInMultiplierIdeal(x_1*x_2, ideal {x_1^3 - x_2^2, x_2^3 - x_3^2}, 31/18)
     	  ///,
     SeeAlso => { "multiplierIdeal", "jumpingCoefficients", "generalB" }
     }
