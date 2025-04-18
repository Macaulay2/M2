-- also see comodule-doc.m2
-- also see packages/Saturation/quotient-doc.m2
document { Key => quotient, Headline => "quotient or division" }

doc ///
Node
  Key
    (quotient, Matrix, Matrix)
    (quotient, Matrix, GroebnerBasis)
    (symbol //, Matrix, Matrix)
    (symbol //, Matrix, MonomialIdeal)
    (symbol //, Matrix, GroebnerBasis)
    (symbol //, Matrix, RingElement)
    (symbol //, Matrix, Number)
    (symbol //, RingElement, Matrix)
    (symbol //, RingElement, MonomialIdeal)
    (symbol //, RingElement, GroebnerBasis)
    (symbol //, Number, Matrix)
    --(symbol //, Number, MonomialIdeal)
    --(symbol //, Number, GroebnerBasis)
  Headline
    factor a map through another with the same target
  Usage
    h = quotient(f, g)
    h = f // g
  Inputs
    f:Matrix
    g:{Matrix,GroebnerBasis} -- with the same target as @TT "f"@.
  Outputs
    h:Matrix
      the quotient of @TT "f"@ upon division by @TT "g"@,
      as a map from source of @TT "f"@ to source of @TT "g"@.
  Description
    Text
      This function produces a matrix @TT "h"@ such that @TT "f - g*h"@ is the
      reduction of @TT "f"@ modulo a Gröbner basis for the image of @TT "g"@.
      See @TO quotient'@ for the dual notion.

      If the remainder @TT "f - g*h"@ is zero, then the quotient @TT "f//g"@
      satisfies the equation @TT "f === g * (f//g)"@. Otherwise, the equation
      @TT "g * h + r === f"@ will hold, where @TT "r"@ is the map provided by
      @TO remainder@.
    Example
      R = ZZ[x,y]
      M = (ker vars R)^2
      f = map(R^2, M, random(R^2, R^{2:-2}))
      g = vars R ++ vars R
      h = quotient(f, g)
      g * h == f
    Text
      Note, however, that currently the function @TO remainder@ only works if the source of @TT "f"@ is free.
    Example
      f = random(R^2, R^{2:-1})
      f = f + id_(R^2)
      h = quotient(f, g)
      r = remainder(f, g)
      g * h + r == f
  Synopsis
    Heading
      factor a ring element or matrix through a set of generators
    Usage
      f // g
    Inputs
      f:{RingElement,Matrix}
      g:{Matrix,MonomialIdeal,GroebnerBasis}
    Description
      Text
        If either @TT "f"@ or @TT "g"@ is a ring element, then it is taken to be a
        scalar matrix acting on @TT "H"@. If @TT "g"@ is a @TO "MonomialIdeal"@,
        then it is taken to be the matrix of generators of @TT "g"@.  Finally, if
        @TT "g"@ is a @TO "GroebnerBasis"@ object, then the Gröbner basis as so far
        computed is used.  In these latter two cases, no Gröbner bases will be computed.
      Example
        g = matrix{{x^2+y^2,x*y}}
        h = (x-y)^2 // g
  Synopsis
    Heading
      factor a number through a set of generators
    Usage
      n // g
    Inputs
      n:Number
      g:Matrix
    Description
      Text
        One common use is the following. If an ideal contains 1,
        then we may write 1 in terms of the generators of the ideal.
        First we make an ideal.
      Example
        A = ZZ/101[x,y,z]
        F = x^4 - y*z*(1-x)^2 - z - y^3
        I = ideal(F, diff(x,F), diff(y,F), diff(z,F))
      Text
        Transposing the (row) matrix of generators of the ideal puts the generators on separate lines and shows the degrees.
      Example
        transpose gens I
      Text
        Next we test whether 1 is in the ideal.
      Example
        1 % I == 0
      Text
        We see that 1 is in the ideal. Now we represent 1 in terms of the generators of @TT "I"@.
      Example
        h = 1 // gens I
        gens I * h
        --g = matrix{{2*x-1,x}}
        --h = 1 // g
        --1 == g * h
  Synopsis
    Heading
      quotient with remainder of a matrix by a number or ring element
    Usage
      f // g
    Inputs
      f:{Matrix}
      g:{Number,RingElement}
    Description
      Example
        matrix{{1,2},{3,4}} // 2
  SeeAlso
    quotientRemainder
    remainder
    quotient'
    (symbol //, Matrix, Matrix)
///

undocumented apply({BasisElementLimit, PairLimit, DegreeLimit}, opt -> [quotient', opt])

doc ///
Node
  Key
    quotient'
   (quotient', Matrix, Matrix)
   (symbol \\, Matrix, Matrix)
   --(symbol \\, Matrix, RingElement)
   --(symbol \\, RingElement, Matrix)
  Headline
    factor a map through another with the same source
  Usage
    h = quotient'(f, g)
    h = g \\ f
  Inputs
    f:Matrix
    g:Matrix -- with the same source as @TT "f"@.
  Outputs
    h:Matrix
      the quotient of @TT "f"@ upon (opposite) division by @TT "g"@,
      as a map from target of @TT "g"@ to target of @TT "f"@.
  Description
    Text
      This function produces a matrix @TT "h"@ such that @TT "f - h*g"@ is the
      reduction of @TT "f"@ modulo a Gröbner basis for the image of @TT "dual g"@.
      See @TO quotient@ for the dual notion.

      If the remainder @TT "f - h*g"@ is zero, then the quotient @TT "g\\f"@
      satisfies the equation @TT "f === (g\\f) * g"@. Otherwise, the equation
      @TT "h * g + r === f"@ will hold, where @TT "r"@ is the map provided by
      @TO remainder'@.
    Example
      S = QQ[x,y,z]
      K = koszul vars S
      f = K.dd_2
      g = inducedMap(coker K.dd_3, K_2)
      h = quotient'(f, g)
      h * g == f
      (g \\ f) * g == f
    Text
      Note, however, that currently the function @TO remainder'@ only works if the source and target modules are free.
    Example
      R = ZZ[x,y]
      f = random(R^{2:1}, R^2)
      f = f + id_(R^2)
      g = transpose(vars R ++ vars R)
      h = quotient'(f, g)
      r = remainder'(f, g)
      h * g + r == f
  SeeAlso
     quotientRemainder'
     remainder'
     quotient
     (symbol \\, Matrix, Matrix)
///

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
     Headline => "matrix remainder (opposite)",
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
