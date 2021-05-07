-- also see comodule-doc.m2
-- also see packages/Saturation/quotient-doc.m2
document { Key => quotient, Headline => "quotient or division" }

document { Key => {(quotient, Matrix, GroebnerBasis), (quotient, Matrix, Matrix)},
     Headline => "matrix quotient",
     Usage => "q = quotient(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same target as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon division by ", TT "g"}
	  },
     "The equation ", TT "g*q+r == f", " will hold, where ", TT "r", " is the map provided by ", TO "remainder", ".  The source of ", TT "f", " should be a free module.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^2,R^{2:-1})
	  g = vars R ++ vars R
	  quotient(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  quotient(f,g)
     ///,
     SeeAlso => {quotientRemainder,quotient'}
     }

document { Key => {quotient',(quotient', Matrix, Matrix)},
     Headline => "matrix quotient (opposite)",
     Usage => "q = quotient'(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same source as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon (opposite) division by ", TT "g"},
	  },
     "The equation ", TT "q*g+r == f", " will hold, where ", TT "r", " is the map provided by ", TO "remainder'", " .  The sources and targets of the maps should be free modules.
     This function is obtained from ", TT "quotient", " by transposing the inputs and outputs.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^{2:1},R^2)
	  g = transpose (vars R ++ vars R)
	  quotient'(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  quotient'(f,g)
     ///,
     SeeAlso => {quotientRemainder,quotient},
     SourceCode => {quotient'}
     }

document { Key => {quotientRemainder,(quotientRemainder, Matrix, GroebnerBasis), (quotientRemainder, Matrix, Matrix)},
     Headline => "matrix quotient and remainder",
     Usage => "(q,r) = quotientRemainder(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same target as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon division by ", TT "g"},
	  "r" => {"the remainder of ", TT "f", " upon division by ", TT "g"}
	  },
     "The equation ", TT "g*q+r == f", " will hold.  The source of ", TT "f", " should be a free module.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^2,R^{2:-1})
	  g = vars R ++ vars R
	  (q,r) = quotientRemainder(f,g)
	  g*q+r == f
	  f = f + map(target f, source f, id_(R^2))
	  (q,r) = quotientRemainder(f,g)
	  g*q+r == f
     ///,
     SeeAlso => {quotientRemainder'}
     }

document { Key => {quotientRemainder',(quotientRemainder', Matrix, Matrix)},
     Headline => "matrix quotient and remainder (opposite)",
     Usage => "(q,r) = quotientRemainder'(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same source as ", TT "f"}},
     Outputs => {
	  "q" => {"the quotient of ", TT "f", " upon (opposite) division by ", TT "g"},
	  "r" => {"the remainder of ", TT "f", " upon (opposite) division by ", TT "g"}
	  },
     "The equation ", TT "q*g+r == f", " will hold.  The sources and targets of the maps should be free modules.
     This function is obtained from ", TT "quotientRemainder", " by transposing the inputs and outputs.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^{2:1},R^2)
	  g = transpose (vars R ++ vars R)
	  (q,r) = quotientRemainder'(f,g)
	  q*g+r == f
	  f = f + map(target f, source f, id_(R^2))
	  (q,r) = quotientRemainder'(f,g)
	  q*g+r == f
     ///,
     SeeAlso => {quotientRemainder},
     SourceCode => {quotientRemainder'}
     }
     
document { Key => {remainder,(remainder, Matrix, GroebnerBasis), (remainder, Matrix, Matrix)},
     Headline => "matrix remainder",
     Usage => "r = remainder(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same target as ", TT "f"}},
     Outputs => {
	  "r" => {"the remainder of ", TT "f", " upon division by ", TT "g"}
	  },
     PARA{"This operation is the same as ", TO (symbol %, Matrix, GroebnerBasis), "."},
     PARA{"The equation ", TT "g*q+r == f", " will hold, where ", TT "q", " is the map provided by ", TO "quotient", ".  The source of ", TT "f", " should be a free module."},
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^2,R^{2:-1})
	  g = vars R ++ vars R
	  remainder(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  remainder(f,g)
     ///,
     SeeAlso => {quotientRemainder,remainder'}
     }

document { Key => {remainder',(remainder', Matrix, Matrix)},
     Headline => "matrix quotient and remainder (opposite)",
     Usage => "r = remainder'(f,g)",
     Inputs => { "f" => Matrix, "g" => {ofClass{GroebnerBasis,Matrix}, ", with the same source as ", TT "f"}},
     Outputs => {
	  "r" => {"the remainder of ", TT "f", " upon (opposite) division by ", TT "g"}
	  },
     "The equation ", TT "q*g+r == f", " will hold, where ", TT "q", " is the map provided by ", TO "quotient'", ".  The sources and targets of the maps should be free modules.
     This function is obtained from ", TT "remainder", " by transposing the inputs and outputs.",
     EXAMPLE lines ///
     	  R = ZZ[x,y]
	  f = random(R^{2:1},R^2)
	  g = transpose (vars R ++ vars R)
	  remainder'(f,g)
	  f = f + map(target f, source f, id_(R^2))
	  remainder'(f,g)
     ///,
     SeeAlso => {quotientRemainder,remainder},
     SourceCode => {remainder'}
     }
