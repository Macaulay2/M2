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
      polylog
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

doc ///
  Key
    Constant
  Description
    Text
      A constant is a symbolic entity that can be approximated by a real or
      complex number to any desired accuracy. It is converted to a numeric
      value of the correct precision, when necessary.
    Example
      pi
      +pi
      numeric_100 pi
      2. * pi
      2p100 * pi
      exp(2*pi*ii/17)
  SeeAlso
    numeric
    "defaultPrecision"
  Subnodes
    isConstant
    ii
    pi
    EulerConstant
    CatalanConstant
///

doc ///
  Key
    ii
  Headline
    the square root of -1
  Usage
    ii
  Outputs
    :CC
      the square root of -1, converted to a numeric value of the correct
      precision, when necessary.
  Description
    Example
      ii
      ii+1p100
      numeric ii
      numeric_100 ii
    Text
      Here is the simplest way to convert ii to a numeric value:
    Example
      +ii
  SeeAlso
    numeric
///

doc ///
  Key
    pi
  Description
    Text
      This constant represents the mathematical constant $\pi$, symbolically.
    Example
      pi
      +pi
      numeric_100 pi
      2. * pi
///

doc ///
  Key
    EulerConstant
  Headline
    the Euler-Mascheroni constant
  Usage
    EulerConstant
  Outputs
    :RR
      the Euler-Mascheroni constant, converted to a numeric value of the
      correct precision, when necessary.
  Description
    Example
      EulerConstant
      +EulerConstant
      EulerConstant+100p100
    Text
      See @wikipedia "Euler-Mascheroni constant"@.
///
doc ///
  Key
    CatalanConstant
  Headline
    Catalan's constant
  Usage
    CatalanConstant
  Outputs
    :RR
      Catalan's constant, converted to a numeric value of the correct
      precision, when necessary.
  Description
    Example
      CatalanConstant
      numeric_100 CatalanConstant
    Text
      See @wikipedia "Catalan's constant"@.
///

doc ///
  Key
    log1p
    (log1p,InexactNumber)
  Headline
    logarithm of 1+x
  Usage
    log1p x
    log1p I
  Inputs
    x:{RR,CC}
    I:{RRi,CCi}
  Outputs
    :{RR,CC}
      the logarithm of @TT "1+x"@
    :{RRi,CCi}
      an interval containing logarithm of 1 plus the points of @TT "I"@
  Description
    Example
      log1p 1p100e-10
      log(1 + 1p100e-10)
      log1p interval(2,3)
      log1p interval(1+2*ii,3+4*ii)
///

doc ///
  Key
    expm1
    (expm1,InexactNumber)
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
    :{RRi,CCi}
      an interval containing the exponential of the points of @TT "I"@ minus one
  Description
    Example
      expm1 1p100e-10
      exp(1p100e-10)-1
      expm1 interval(2,3)
      expm1 interval(1+2*ii,3+4*ii)
///

doc ///
  Key
    eint
    (eint, InexactNumber)
  Headline
    exponential integral
  Usage
    eint x
  Inputs
    x:
  Outputs
    :RR
      the exponential integral of @TT "x"@
    :RRi
      an interval containing the exponential integral of the points of @TT "x"@
    :CCi
      a complex interval containing the exponential integral of the points of
      @TT "x"@
  Description
    Example
      eint 2
    Text
      See @wikipedia "Exponential integral"@.
///

doc ///
  Key
    Digamma
    (Digamma, InexactNumber)
  Headline
    Digamma function
  Usage
    Digamma x
  Inputs
    x:
  Outputs
    :RR
      the digamma function (logarithmic derivative of the gamma function) of
      @TT "x"@
    :RRi
      an interval containing the digamma function (logarithmic derivative of
      the gamma function) of the points of @TT "x"@
    :CCi
      a complex interval containing the digamma function (logarithmic
      derivative of the gamma function) of the points of @TT "x"@
  Description
    Example
      Digamma 6
    Text
      See @wikipedia "Digamma function"@.
  SeeAlso
    Gamma
///

doc ///
  Key
    zeta
    (zeta, InexactNumber)
  Headline
    Riemann zeta function
  Usage
    zeta x
  Inputs
    x:
  Outputs
    :RR
      the zeta function of @TT "x"@
    :RRi
      an interval containing the the zeta function of the points of @TT "x"@
    :CCi
      a complex interval containing the the zeta function of the points of
      @TT "x"@
  Description
    Example
      zeta 2
    Text
      See @wikipedia "Riemann zeta function"@.
///

doc ///
  Key
    --- author(s): L. Gold, Dan Grayson
    acos
    (acos, InexactNumber)
  Headline
    arccosine
  Usage
    acos x
    acos I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :Number
      the arccosine (in radians) of @TT "x"@
    :RRi
      an interval containing the arccosines of the points of @TT "I"@
    :CCi
      a complex interval containing the arccosines of the points of @TT "I"@
  Description
    Example
      acos 0.5
    Text
      See @wikipedia "Trigonometric function"@.
///

doc ///
  Key
    --- author(s): L. Gold, Dan Grayson
    asin
    (asin,InexactNumber)
  Headline
    arcsine
  Usage
    asin x
    asin I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :Number
      the arcsine (in radians) of @TT "x"@
    :RRi
      an interval containing the arcsines of the points of @TT "I"@
    :CCi
      a complex interval containing the arcsines of the points of @TT "I"@
  Description
    Example
      asin 1
    Text
      See @wikipedia "Trigonometric function"@.
///

doc ///
  Key
    --- author(s): L. Gold
    cosh
    (cosh,InexactNumber)
  Headline
    compute the hyperbolic cosine
  Usage
    cosh x
    cosh I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :Number
      the hyperbolic cosine of @TT "x"@
    :RRi
      an interval containing the hyerbolic cosines of the points of @TT "I"@
    :CCi
      a complex interval containing the hyerbolic cosines of the points of
      @TT "I"@
  Description
    Example
      cosh .2
///

doc ///
  Key
    acosh
    (acosh,Number)
  Headline
    inverse hyperbolic cosine
  Usage
    acosh x
  Inputs
    x:
  Outputs
    :Number
      the inverse hyperbolic cosine of @TT "x"@
  Description
    Example
      acosh .2
      cosh oo
    Text
      See @wikipedia "Hyperbolic function"@.
///

doc ///
  Key
    asinh
    (asinh,Number)
  Headline
    inverse hyperbolic sine
  Usage
    asinh x
  Inputs
    x:
  Outputs
    :Number
      the inverse hyperbolic sine of @TT "x"@
  Description
    Example
      asinh .2
    Text
      See @wikipedia "Hyperbolic function"@.
///

doc ///
  Key
    sec
    (sec,InexactNumber)
  Headline
    secant
  Usage
    sec x
    sec I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :RR
      the secant of @TT "x"@
    :RRi
      an interval containing the secants of the points of @TT "I"@
    :CCi
      a complex interval containing the secants of the points of @TT "I"@
  Description
    Example
      sec(pi/3)
    Text
      See @wikipedia "Trigonometric function"@.
///

doc ///
  Key
    csc
    (csc,InexactNumber)
  Headline
    cosecant
  Usage
    csc x
    csc I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :RR
      the cosecant of @TT "x"@
    :RRi
      an interval containing the cosecants of the points of @TT "I"@
    :CCi
      a complex interval containing the cosecants of the points of @TT "I"@
  Description
    Example
      csc(pi/3)
    Text
      See @wikipedia "Trigonometric function"@.
///

doc ///
  Key
    cot
    (cot, InexactNumber)
  Headline
    cotangent
  Usage
    cot x
    cot I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :RR
      the cotangent of @TT "x"@
    :RRi
      an interval containing the cotangents of points of @TT "I"@
    :CCi
      a complex interval containing the cotangents of points of @TT "I"@
  Description
    Example
      cot(pi/3)
    Text
      See @wikipedia "Trigonometric function"@.
///

doc ///
  Key
    sech
    (sech,InexactNumber)
  Headline
    hyperbolic secant
  Usage
    sech x
    sech I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :RR
      the hyperbolic secant of @TT "x"@
    :RRi
      an interval containing the hyerbolic secants of the points of @TT "I"@
    :CCi
      a complex interval containing the hyerbolic secants of the points of
      @TT "I"@
  Description
    Example
      sech(pi/3)
    Text
      See @wikipedia "Hyperbolic function"@.
///

doc ///
  Key
    csch
    (csch,InexactNumber)
  Headline
    hyperbolic cosecant
  Usage
    csch x
    csch I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :RR
      the hyperbolic cosecant of @TT "x"@
    :RRi
      an interval containing the hyperbolic cosecants of the points of @TT "I"@
    :CCi
      a complex interval containing the hyperbolic cosecants of the points of
      @TT "I"@
  Description
    Example
      csch(pi/3)
    Text
      See @wikipedia "Hyperbolic function"@.
///

doc ///
  Key
    coth
    (coth,InexactNumber)
  Headline
    hyperbolic cotangent
  Usage
    coth x
    coth I
  Inputs
    x:RR
    x:CC
    I:RRi
    I:CCi
  Outputs
    :RR
      the hyperbolic cotangent of @TT "x"@
    :RRi
      an interval containing the hyperbolic cotangents of the points of
      @TT "I"@
    :CCi
      a complex interval containing the hyperbolic cotangents of the points of
      @TT "I"@
  Description
    Example
      coth(pi/3)
    Text
      See @wikipedia "Hyperbolic function"@.
///

doc ///
  Key
    BesselJ
    (BesselJ, ZZ, Number)
    (BesselJ,Number,Number)
  Headline
    Bessel function of the first kind
  Usage
    BesselJ(n,x)
    BesselJ_n x
  Inputs
    n:ZZ
      the order
    x:
      @ofClass{ZZ, QQ, RR}@
  Outputs
    :
      the Bessel function of the first kind of order @TT "n"@ at @TT "x"@
  Description
    Example
      BesselJ_0 .5
      BesselJ_2 3p200
    Text
      See @wikipedia "Bessel function"@.
  SeeAlso
    BesselY
///

doc ///
  Key
    BesselY
    (BesselY, ZZ, Number)
    (BesselY,Number,Number)
  Headline
    Bessel function of the second kind
  Usage
    BesselY(n,x)
    BesselY_n x
  Inputs
    n:ZZ
      the order
    x:
      @ofClass{ZZ, QQ, RR}@
  Outputs
    :
      the Bessel function of the second kind of order @TT "n"@ at @TT "x"@
  Description
    Example
      BesselY_0 .5
      BesselY_2 3p200
    Text
      See @wikipedia "Bessel function"@.
  SeeAlso
    BesselJ
///

doc ///
  Key
    agm
    (agm,InexactNumber,InexactNumber)
  Headline
    arithmetic-geometric mean
  Usage
    agm(x,y)
  Inputs
    x:
      a number
    y:
      a number
  Outputs
    :
      the arithmetic-geometric mean of @TT "x"@ and @TT "y"@
  Description
    Example
      agm(1,2p200)
    Text
      See @wikipedia "Arithmetic-geometric mean"@.
///

-- TODO: find a better place for these
doc ///
  Key
    size2
    (size2,CC)
    (size2,RR)
    (size2,ZZ)
    (size2,RRi)
    (size2,CCi)
  Headline
    number of binary digits to the left of the point
  Usage
    size2 x
  Inputs
    x:Number
  Outputs
    :
      the smallest number @TT "n"@ such that @TT "2^n"@ is strictly greater
      than the absolute value of @TT "x"@
  Description
    Text
      This function is not implemented for rational numbers.
    Example
      size2 4
      size2 3
      size2 4.
      size2 3.99999999
      size2 0
      size2 0.
      size2 (1/0.)
      size2 (1/0.-1/0.)
///

doc ///
  Key
    isReal
    (isReal,CC)
    (isReal,CCi)
    (isReal,QQ)
    (isReal,RR)
    (isReal,ZZ)
    (isReal, RRi)
    (isReal, Constant)
    (isReal, InfiniteNumber)
  Headline
    whether a number is real
  Usage
    isReal x
  Inputs
    x:Number
  Outputs
    :Boolean
      whether @TT "x"@ is real
  Description
    Example
      isReal 2.
      isReal sqrt(-1)
///

doc ///
  Key
    isFinite
    (isFinite,Constant)
    (isFinite,InfiniteNumber)
    (isFinite,Number)
  Headline
    whether a number is finite
  Usage
    isFinite x
  Inputs
    x:Number
  Outputs
    :Boolean
      whether @TT "x"@ is finite, i.e., is not an infinite real number, nor
      @EM "not a number"@.
  Description
    Example
      sqrt 2.
      isFinite oo
      i = 1/0.
      isFinite oo
      i-i
      isFinite oo
  SeeAlso
    isANumber
    isInfinite
///

doc ///
  Key
    isInfinite
    (isInfinite,Number)
    (isInfinite, Constant)
    (isInfinite, InfiniteNumber)
  Headline
    whether a number is infinite
  Usage
    isInfinite x
  Inputs
    x:Number
  Outputs
    :Boolean
      whether @TT "x"@ is an infinite number
  Description
    Example
      isInfinite 3.
      inf = 1/0.
      isInfinite inf
      nan = inf - inf
      isInfinite nan
  SeeAlso
    isFinite
    isANumber
///

doc ///
  Key
    isANumber
    (isANumber,Number)
  Headline
    whether a number is not not-a-number
  Usage
    isANumber x
  Inputs
    x:Number
  Outputs
    :Boolean
      whether @TT "x"@ is not @EM "not a number"@.
  Description
    Text
      For a discussion of the notion of not-a-number in floating point
      arithmetic, see @HREF "http://en.wikipedia.org/wiki/NaN"@.
    Example
      isANumber 3.
      inf = 1/0.
      isANumber inf
      nan = inf - inf
      isANumber nan
  SeeAlso
    isFinite
    isInfinite
///

doc ///
  Key
    norm
    (norm,List)
    (norm,Matrix)
    (norm,MutableMatrix)
    (norm,Number)
    (norm,Number,List)
    (norm,Number,Matrix)
    (norm,Number,MutableMatrix)
    (norm,Number,Number)
    (norm,Number,RingElement)
    (norm,Number,Vector)
    (norm,RingElement)
    (norm,Vector)
  Headline
    l^p norm
  Usage
    norm M
    norm(p, M)
  Inputs
    p:Number -- specifying which norm to compute
    M:{List, Matrix, MutableMatrix, Number, RingElement, Vector}
  Outputs
    :Number
      The $\ell^p$-norm of @VAR "M"@, computed to the minimum of the precisions
      of @VAR "M"@ and @VAR "p"@.
  Description
    Text
      By default, the $\ell^\infty$ norm giving the maximum absolute entry is returned.
    Example
      printingPrecision = 2
      R = RR_100
      M = 10*random(R^3,R^10)
      norm M
      norm_(numeric_20 infinity) M
      norm {3/2,4,-5}
    Text
      For finite $p$, the $\ell^p$ norm
      $$\|M\|_p = \left(\sum_{i,j} |M_{ij}|^p\right)^{1/p}$$
      is returned.  For example, when $p = 2$, this is the Frobenius norm.
    Example
      M = matrix {{3, 4}, {12, 84}}
      norm_2 M
    Text
      The norm of a polynomial is the norm of the vector of its coefficients.
    Example
      RR[x]
      (1+x)^5
      norm oo
///

doc ///
  Key
    commonRing
    (commonRing,List)
  Headline
    find smallest containing ring
  Usage
    commonRing v
  Inputs
    v:
      a list of numbers, ring elements, matrices, or things with rings, see
    :
      @TO "ring"@
  Outputs
    :Ring
      the smallest ring to which all members of the list can be promoted
  Description
    Example
      commonRing {1,2/3}
      commonRing {1,2.}
      commonRing {1,2p100,matrix{{3p200}}}
      R = RR[x];
      commonRing {1,2p100,matrix{{3p200}},0_R}
///

doc ///
  Key
    default
    (default, InexactFieldFamily)
  Headline
    default member of a family
  Usage
    default T
  Inputs
    T:
  Outputs
    :
      the default member of the family shadowed by @TT "T"@
  Description
    Example
      default RR
      defaultPrecision = 100
      default CC
    Text
      This function is used internally, and its implementation may change.
  Subnodes
    "defaultPrecision"
///

doc ///
  Key
    "defaultPrecision"
  Headline
    default precision of numbers
  Usage
    defaultPrecision = n
  Inputs
    n:ZZ
  Consequences
    Item
      henceforth, when a real or complex number is to be created, and no
      precision is otherwise specified, the precision will be @TT "n"@
  Description
    Example
      defaultPrecision
      1/3.
      defaultPrecision=100
      1/3.
      RR[x]
      numeric pi
///

undocumented {
    ((symbol _*, symbol =), RingFamily),
}

doc ///
  Key
    (symbol _*,RingFamily)
  Headline
    the common parent for rings in a family
  Usage
    R_*
  Inputs
    R:
  Outputs
    :
      the common parent for the rings in the family @TT "R"@
  Description
    Example
      4.
      ring 4.
      parent ring 4.
      RR_*
///
