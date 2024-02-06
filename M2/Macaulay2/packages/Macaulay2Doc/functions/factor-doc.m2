document {
     Key => factor,
     Headline => "factor a ring element or a ZZ-module",
     PARA {
	  "(Disambiguation: for division of matrices, which can be thought of as factoring
	  one homomorphism through another, instead see ", TO (symbol //,Matrix, Matrix), ".  For
	  lifting a map between modules to a map between their free resolutions, see ", TO extend, ".)"
	  }
     }

document {
     Key => (factor,Module),
     Headline => "factor a ZZ-module",
     Usage => "factor M",
     Inputs => { "M" },
     Outputs => {{ "a symbolic expression describing the decomposition of ", TT "M", " into a direct sum of principal modules"}},
     "The ring of ", TT "M", " must be ", TO "ZZ", ".",
     PARA {},
     "In the following example we construct a module with a known (but disguised) factorization.",
     EXAMPLE lines ///
     	  f = random(ZZ^6, ZZ^4)
          M = subquotient ( f * diagonalMatrix{2,3,8,21}, f * diagonalMatrix{2*11,3*5*13,0,21*5} )
	  factor M
     ///}

document {
     Key => {(factor,RingElement),(factor,QQ),(factor,ZZ)},
     Headline => "factor a ring element",
     Usage => "factor x",
     Inputs => {"x" => {"or ", ofClass{QQ,ZZ}}},
     Outputs => {Product => {"the factorization of ", TT "x"}},
     PARA{
	  "The result is a ", TO "Product", " each of whose factors is a
	  ", TO "Power", " whose base is one of the factors found and whose
	  exponent is an integer.",
	  },
     EXAMPLE {
         "factor 124744878111332355674003415153753485211381849014286981744945",
         "y = (2^15-4)/(2^15-5)",
      	 "x = factor y",
      	 "value x"
     },
     PARA {
	  "We may ", TO "peek", " inside ", TT "x", " to a high depth to see
	  its true structure as ", TO "Expression", "."
	  },
     EXAMPLE "peek'(100,x)",
     PARA {
	  "For integers, factorization is done by ", TO "FLINT", TEX ", and the factors $x$ are actually
	  just probably prime, as described in the documentation of ", TO "isPseudoprime", "."
	  },
     PARA {
	  "For multivariate polynomials the
	  factorization is done with code of Michael Messollen (see
	  ", TO "Singular-Factory", ").  For univariate
	  polynomials the factorization is in turn done with code of
	  Gert-Martin Greuel and Ruediger Stobbe (see ", TO "Singular-Factory", ").",
	  },
     EXAMPLE {
	  "R = ZZ/101[u]",
      	  "factor (u^3-1)",
	  },
     "The constant term is provided as the last factor, if it's not equal
     to 1.",
     EXAMPLE {
	  "F = frac(ZZ/101[t])",
      	  "factor ((t^3-1)/(t^3+1))",
	  }
     }
