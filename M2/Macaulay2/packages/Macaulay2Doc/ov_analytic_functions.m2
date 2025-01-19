undocumented {
    (cos, RingElement),
    (cosh, RingElement),
    (sec, RingElement),
    (sech, RingElement),
    (sin, RingElement),
    (asin, RingElement),
    (sinh, RingElement),
    (tan, RingElement),
    (atan, RingElement),
    (tanh, RingElement),
    (expm1, RingElement),
    (log1p, RingElement),
}

doc ///
Node
  Key
    "analytic functions"
  Subnodes
    Constant -- the class of constants
    :Special functions
      integrate
      abs
      sign
      floor
      (floor, Number)
      ceiling
      (ceiling, Number)
      (truncate, Number)
      sqrt
      log
      log1p
      exp
      (exp, RingElement)
      expm1
      erf
      erfc
      inverseErf
      eint
      Beta
      regularizedBeta
      inverseRegularizedBeta
      Gamma
      lngamma
      Digamma
      regularizedGamma
      inverseRegularizedGamma
      zeta
      BesselJ
      BesselY
      agm
    :Trigonometric functions
      sin
      asin
      sinh
      asinh
      cos
      acos
      cosh
      acosh
      csc
      csch
      sec
      sech
      tan
      atan
      atan2
      tanh
      atanh
      cot
      acot
      coth
      acoth
///

document {
     Key => Constant,
     PARA {
	  "A constant is a symbolic entity that can be approximated by a real or complex
	  number to any desired accuracy.  It is converted to a numeric value of the
	  correct precision, when necessary."
	  },
     EXAMPLE lines ///
     pi
     +pi
     numeric_100 pi
     2. * pi
     2p100 * pi
     exp(2*pi*ii/17)
     ///,
     SeeAlso => { numeric, "defaultPrecision" },
     Subnodes => {
	 TO isConstant,
	 TO ii,
	 TO pi,
	 TO EulerConstant,
	 TO CatalanConstant,
         },
     }

document {
     Key => ii,
     Headline => "the square root of -1",
     Usage => "ii",
     Outputs => { CC => { "the square root of -1, converted to a numeric value of the correct precision, when necessary." }},
     EXAMPLE lines ///
     	  ii
     	  ii+1p100
	  numeric ii
	  numeric_100 ii
     ///,
     "Here is the simplest way to convert ii to a numeric value:",
     EXAMPLE "+ii",
     SeeAlso => {numeric}
     }

document { Key => pi,
     PARA {
	  TEX "This constant represents the mathematical constant $\\pi$, symbolically."
	  },
     EXAMPLE lines ///
     	  pi
     	  +pi
     	  numeric_100 pi
	  2. * pi
     ///
     }

document { Key => {EulerConstant},
     Usage => "EulerConstant",
     Headline => "the Euler-Mascheroni constant",
     Outputs => { RR => { "the Euler-Mascheroni constant, converted to a numeric value of the correct precision, when necessary." }},
     EXAMPLE lines ///
     	  EulerConstant
     	  +EulerConstant
     	  EulerConstant+100p100
     ///,
     PARA {"See ", wikipedia "Euler-Mascheroni constant", "."}
     }
document { Key => {CatalanConstant},
     Usage => "CatalanConstant",
     Headline => "Catalan's constant",
     Outputs => { RR => { "Catalan's constant, converted to a numeric value of the correct precision, when necessary." }},
     EXAMPLE lines ///
	  CatalanConstant
	  numeric_100 CatalanConstant
	  ///,
     PARA {"See ", wikipedia "Catalan's constant", "."}
     }

doc ///
  Key
    log1p
    (log1p,CC)
    (log1p,RR)
    (log1p,RRi)
  Headline
    logarithm of 1+x
  Usage
    log1p x
    log1p I
  Inputs
    x:{CC,RR}
    I:RRi
  Outputs
    :{RR,CC}
      the logarithm of @TT "1+x"@
    :{RRi}
      an interval containing logarithm of 1 plus the points of @TT "I"@
  Description
    Example
      log1p 1p100e-10
      log(1 + 1p100e-10)
///

doc ///
  Key
    expm1
    (expm1,CC)
    (expm1,RR)
    (expm1,RRi)
  Headline
    exponential minus 1
  Usage
    expm1 x
    expm1 I
  Inputs
    x:{CC,RR}
    I:RRi
  Outputs
    :{RR,CC}
      the quantity @TT "exp(x)-1"@
    :RRi
      an interval containing the exponential of the points of @TT "I"@ minus one
  Description
    Example
      expm1 1p100e-10
      exp(1p100e-10)-1
///

document { Key => {eint,(eint, RR),(eint,CC),(eint,RRi)},
     Usage => "eint x",
     Headline => "exponential integral",
     Inputs => { "x" },
     Outputs => { RR => { "the exponential integral of ", TT "x" }},
     EXAMPLE lines ///
     	  eint 2
     ///,
     PARA {"See ", wikipedia "Exponential integral", "."}
     }

document { Key => {Digamma,(Digamma, RR),(Digamma,CC),(Digamma,RRi)},
     Usage => "Digamma x",
     Headline => "Digamma function",
     Inputs => { "x" },
     Outputs => { RR => { "the digamma function (logarithmic derivative of the gamma function) of ", TT "x" }},
     EXAMPLE lines ///
	  Digamma 6
     ///,
     PARA {"See ", wikipedia "Digamma function", "."},
     SeeAlso => {Gamma}
     }

document { Key => {zeta,(zeta, RR),(zeta,CC),(zeta,RRi)},
     Usage => "zeta x",
     Headline => "Riemann zeta function",
     Inputs => { "x" },
     Outputs => { RR => { "the zeta function of ", TT "x" }},
     EXAMPLE lines ///
     	  zeta 2
     ///,
     PARA {"See ", wikipedia "Riemann zeta function", "."}
     }

document {
     --- author(s): L. Gold, Dan Grayson
     Key => {acos,(acos,RR),(acos,CC),(acos, RRi)},
     Headline => "arccosine",
     Usage => "acos x\nacos I",
     Inputs => { "x", "I" => RRi },
     Outputs => { Number => { "the arccosine (in radians) of ", TT "x"},
                  RRi => { "an interval containing the arccosines of the points of ", TT "I" }},
     EXAMPLE lines ///
     acos 0.5
     ///,
     PARA {"See ", wikipedia "Trigonometric function", "."}
     }

document {
     --- author(s): L. Gold, Dan Grayson
     Key => {asin,(asin,RR),(asin,CC),(asin, RRi)},
     Headline => "arcsine",
     Usage => "asin x\nasin I",
     Inputs => { "x", "I" => RRi },
     Outputs => {
	  Number => {"the arcsine (in radians) of ", TT "x"},
      RRi => { "an interval containing the arcsines of the points of ", TT "I" }
	  },
     EXAMPLE {
	  "asin 1"
	  },
     PARA {"See ", wikipedia "Trigonometric function", "."}
     }

document {
     --- author(s): L. Gold
     Key => {cosh, (cosh,RR),(cosh,CC),(cosh,RRi)},
     Headline => "compute the hyperbolic cosine",
     Usage => "cosh x\ncosh I",
     Inputs => { "x", "I"=>RRi},
     Outputs => { Number => { "the hyperbolic cosine of ", TT "x" },
         RRi => { "an interval containing the hyerbolic cosines of the points of ", TT "I" } },
     EXAMPLE lines ///
     cosh .2
     ///,
     }

document {
     Key => {acosh,(acosh,Number)},
     Headline => "inverse hyperbolic cosine",
     Usage => "acosh x",
     Inputs => { "x" },
     Outputs => { Number => { "the inverse hyperbolic cosine of ", TT "x"} },
     EXAMPLE lines ///
     acosh .2
     cosh oo
     ///,
     PARA {"See ", wikipedia "Hyperbolic function", "."}
     }

document {
     Key => {asinh,(asinh,Number)},
     Headline => "inverse hyperbolic sine",
     Usage => "asinh x",
     Inputs => { "x" },
     Outputs => { Number => {"the inverse hyperbolic sine of ", TT "x"} },
     EXAMPLE lines ///
     asinh .2
     ///,
     PARA {"See ", wikipedia "Hyperbolic function", "."}
     }

document { Key => {sec,(sec,CC),(sec, RR),(sec, RRi)},
     Usage => "sec x\nsec I",
     Headline => "secant",
     Inputs => { "x", "I" => RRi },
     Outputs => { RR => { "the secant of ", TT "x" },
	 RRi => { "an interval containing the secants of the points of ", TT "I" }
     },
     EXAMPLE lines ///
     	  sec(pi/3)
     ///,
     PARA {"See ", wikipedia "Trigonometric function", "."}
     }

document { Key => {csc,(csc,CC),(csc, RR),(csc,RRi)},
     Usage => "csc x\ncsc I",
     Headline => "cosecant",
     Inputs => { "x","I"=>RRi },
     Outputs => { RR => { "the cosecant of ", TT "x" },
        RRi => { "an interval containing the cosecants of the points of ", TT "I" }
    },
     EXAMPLE lines ///
     	  csc(pi/3)
     ///,
     PARA {"See ", wikipedia "Trigonometric function", "."}
     }

document { Key => {cot,(cot, RR),(cot,CC),(cot,RRi)},
     Usage => "cot x\ncot I",
     Headline => "cotangent",
     Inputs => { "x", "I"=>RRi },
     Outputs => { RR => { "the cotangent of ", TT "x" },
            RRi => { "an interval containing the cotangents of points of ", TT "I"}
    },
     EXAMPLE lines ///
     	  cot(pi/3)
     ///,
     PARA {"See ", wikipedia "Trigonometric function", "."}
     }

document { Key => {sech,(sech,CC),(sech, RR),(sech, RRi)},
     Usage => "sech x\nsech I",
     Headline => "hyperbolic secant",
     Inputs => { "x", "I" => RRi },
     Outputs => { RR => { "the hyperbolic secant of ", TT "x" },
	 RRi => { "an interval containing the hyerbolic secants of the points of ", TT "I" }
     },
     EXAMPLE lines ///
     	  sech(pi/3)
     ///,
     PARA {"See ", wikipedia "Hyperbolic function", "."}
     }

document { Key => {csch,(csch,CC),(csch, RR),(csch,RRi)},
     Usage => "csch x\ncsch I",
     Headline => "hyperbolic cosecant",
     Inputs => { "x", "I"=>RRi },
     Outputs => { RR => { "the hyperbolic cosecant of ", TT "x" },
	 RRi => { "an interval containing the hyperbolic cosecants of the points of ", TT "I" }
     },
     EXAMPLE lines ///
     	  csch(pi/3)
     ///,
     PARA {"See ", wikipedia "Hyperbolic function", "."}
     }

document { Key => {coth,(coth,CC),(coth, RR),(coth,RRi)},
     Usage => "coth x\ncoth I",
     Headline => "hyperbolic cotangent",
     Inputs => { "x","I"=>RRi},
     Outputs => { RR => { "the hyperbolic cotangent of ", TT "x" },
        RRi => { "an interval containing the hyperbolic cotangents of the points of ", TT "I" }
        },
     EXAMPLE lines ///
     	  coth(pi/3)
     ///,
     PARA {"See ", wikipedia "Hyperbolic function", "."}
     }

document { Key => {BesselJ,(BesselJ, ZZ, Number),(BesselJ,Number,Number)},
     Usage => "BesselJ(n,x)\nBesselJ_n x",
     Headline => "Bessel function of the first kind",
     Inputs => {
	  "n" => ZZ => { "the order" },
	  "x" => { ofClass{ZZ,QQ,RR} }
	  },
     Outputs => {
	  {"the Bessel function of the first kind of order ", TT "n", " at ", TT "x"}
	  },
     EXAMPLE lines ///
          BesselJ_0 .5
     	  BesselJ_2 3p200
     ///,
     PARA {"See ", wikipedia "Bessel function", "."},
     SeeAlso => { BesselY }
     }

document { Key => {BesselY,(BesselY, ZZ, Number),(BesselY,Number,Number)},
     Usage => "BesselY(n,x)\nBesselY_n x",
     Headline => "Bessel function of the second kind",
     Inputs => {
	  "n" => ZZ => { "the order" },
	  "x" => { ofClass{ZZ,QQ,RR} }
	  },
     Outputs => {
	  {"the Bessel function of the second kind of order ", TT "n", " at ", TT "x"}
	  },
     EXAMPLE lines ///
          BesselY_0 .5
     	  BesselY_2 3p200
     ///,
     PARA {"See ", wikipedia "Bessel function", "."},
     SeeAlso => { BesselJ }
     }

document { Key => {agm, (agm, RR, RR), (agm,CC,CC), (agm,CC,RR), (agm,RR,CC)},
     Usage => "agm(x,y)",
     Inputs => { "x" => "a number", "y" => "a number" },
     Outputs => { {"the arithmetic-geometric mean of ", TT "x", " and ", TT "y"}},
     Headline => "arithmetic-geometric mean",
     EXAMPLE lines ///
     	  agm(1,2p200)
     ///,
     PARA {"See ", wikipedia "Arithmetic-geometric mean", "."}
     }

-- TODO: find a better place for these
document { Key => {size2, (size2,CC), (size2,RR), (size2,ZZ), (size2,RRi)},
     Usage => "size2 x",
     Headline => "number of binary digits to the left of the point",
     Inputs => {"x" => Number},
     Outputs => {{"the smallest number ", TT "n", " such that ", TT "2^n", " is strictly greater than the absolute value of ", TT "x"}},
     "This function is not implemented for rational numbers.",
     EXAMPLE lines ///
     size2 4
     size2 3
     size2 4.
     size2 3.99999999
     size2 0
     size2 0.
     size2 (1/0.)
     size2 (1/0.-1/0.)
     ///}

document { Key => {isReal,(isReal,CC),(isReal,QQ),(isReal,RR),(isReal,ZZ),
	(isReal, RRi), (isReal, Constant), (isReal, InfiniteNumber)},
     Usage => "isReal x",
     Headline => "whether a number is real",
     Inputs => { "x" => Number },
     Outputs => { Boolean => {"whether ", TT "x", " is real"} },
     EXAMPLE lines ///
     isReal 2.
     isReal sqrt(-1)
     ///
     }

document { Key => {
	isFinite,
	(isFinite,Constant),
	(isFinite,InfiniteNumber),
	(isFinite,Number)},
     Usage => "isFinite x",
     Headline => "whether a number is finite",
     Inputs => { "x" => Number },
     Outputs => { Boolean => {"whether ", TT "x", " is finite, i.e., is not an infinite real number, nor
	       ", EM "not a number", "."} },
     EXAMPLE lines ///
     sqrt 2.
     isFinite oo
     i = 1/0.
     isFinite oo
     i-i
     isFinite oo
     ///,
     SeeAlso => {isANumber, isInfinite}
     }

document { Key => {isInfinite, (isInfinite,Number), (isInfinite, Constant),
	(isInfinite, InfiniteNumber)},
     Usage => "isInfinite x",
     Headline => "whether a number is infinite",
     Inputs => { "x" => Number },
     Outputs => { Boolean => {"whether ", TT "x", " is an infinite number"} },
     EXAMPLE lines ///
     isInfinite 3.
     inf = 1/0.
     isInfinite inf
     nan = inf - inf
     isInfinite nan
     ///,
     SeeAlso => {isFinite, isANumber}
     }

document { Key => {isANumber, (isANumber,Number)},
     Usage => "isANumber x",
     Headline => "whether a number is not not-a-number",
     Inputs => { "x" => Number },
     Outputs => { Boolean => {"whether ", TT "x", " is not ", EM "not a number", "."} },
     PARA {
	  "For a discussion of the notion of not-a-number in floating point arithmetic, see ", HREF "http://en.wikipedia.org/wiki/NaN", "."
	  },
     EXAMPLE lines ///
     isANumber 3.
     inf = 1/0.
     isANumber inf
     nan = inf - inf
     isANumber nan
     ///,
     SeeAlso => {isFinite, isInfinite}
     }

document {
     Key => {norm,
	  (norm, InfiniteNumber, Matrix),(norm, Matrix),(norm, RR, Matrix),(norm, InfiniteNumber, RingElement),(norm, MutableMatrix),
	  (norm, RingElement),(norm, InexactField, MutableMatrix),(norm, RR, MutableMatrix),(norm, RR, RingElement),
	  (norm,List),(norm,Vector),(norm,Number),(norm,RR,Number),(norm,InfiniteNumber,Number)
	  },
     Usage => "norm M\nnorm(p,M)",
     Inputs => {
	  "M"=>{ofClass{MutableMatrix,Matrix,RingElement,Number,Vector,List}},
	  "p"=>{ofClass{RR,InfiniteNumber}, ", specifying which norm to compute.  Currently, only ", TT "p=infinity", " is accepted."}
	  },
     Outputs => {TEX{"the $L^p$-norm of ", TT "M", " computed to the minimum of the precisions of ", TT "M", " and of ", TT "p",
	       "."}},
     EXAMPLE lines ///
	  printingPrecision = 2
	  R = RR_100
	  M = 10*random(R^3,R^10)
	  norm M
	  norm_(numeric_20 infinity) M
	  norm {3/2,4,-5}
	  ///,
     "The norm of a polynomial is the norm of the vector of its coefficients.",
     EXAMPLE lines ///
	  RR[x]
	  (1+x)^5
	  norm oo
	  ///
     }

document { Key => {commonRing, (commonRing,List)},
     Usage => "commonRing v",
     Headline => "find smallest containing ring",
     Inputs => { "v" => "a list of numbers, ring elements, matrices, or things with rings, see ", TO "ring" },
     Outputs => { Ring => "the smallest ring to which all members of the list can be promoted" },
     EXAMPLE lines ///
     	  commonRing {1,2/3}
     	  commonRing {1,2.}
     	  commonRing {1,2p100,matrix{{3p200}}}
	  R = RR[x];
     	  commonRing {1,2p100,matrix{{3p200}},0_R}
     	  ///
     }

document {
     Key => {default,(default, InexactFieldFamily)},
     Headline => "default member of a family",
     Usage => "default T",
     Inputs => { "T" },
     Outputs => {{ "the default member of the family shadowed by ", TT "T" }},
     EXAMPLE lines ///
     	  default RR
	  defaultPrecision = 100
	  default CC
     ///,
     "This function is used internally, and its implementation may change.",
     Subnodes => { TO "defaultPrecision" },
     }

document { Key => "defaultPrecision",
     Headline => "default precision of numbers",
     Usage => "defaultPrecision = n",
     Inputs => { "n" => ZZ },
     Consequences => {{"henceforth, when a real or complex number is to be created,
	       and no precision is otherwise specified, the precision will be ", TT "n"}},
     EXAMPLE lines ///
     defaultPrecision
     1/3.
     defaultPrecision=100
     1/3.
     RR[x]
     numeric pi
     ///
     }

undocumented {
    ((symbol _*, symbol =), RingFamily),
}

document {
     Key => (symbol _*,RingFamily),
     Headline => "the common parent for rings in a family",
     Usage => "R_*",
     Inputs => {"R"},
     Outputs => {{"the common parent for the rings in the family ", TT "R"}},
     EXAMPLE lines ///
     4.
     ring 4.
     parent ring 4.
     RR_*
     ///
     }
