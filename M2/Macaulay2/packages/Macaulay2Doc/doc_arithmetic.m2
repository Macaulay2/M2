doc ///
  Key
    round
    (round, Number)
    (round, QQ)
    (round, ZZ)
    (round, ZZ, CC)
    (round, ZZ, Number)
    (round, ZZ, RR)
    (round, ZZ, RRi)
  Headline
    round a number
  Synopsis
    Usage
      round x
    Inputs
      x:
        a number
    Outputs
      :
        the integer nearest to @TT "x"@
    Description
      Example
        round(-2.3)
        round(-2.3+5*ii)
        round(2/3)
  Synopsis
    Usage
      round(n,x)
    Inputs
      n:ZZ
      x:RR
    Outputs
      :
        the real number with just n decimal digits to the right of the decimal
        point nearest to @TT "x"@
    Description
      Example
        round(2,1234.5678)
        round(-2,1234.5678)
  SeeAlso
    floor
    ceiling
///
doc ///
  Key
    numeric
    (numeric,Matrix)
    (numeric,ZZ,Matrix)
    (numeric, Vector)
    (numeric, ZZ, Vector)
    (numeric, ZZ, CC)
    (numeric, RR)
    (numeric, RRi)
    (numeric, CCi)
    (numeric, ZZ, RRi)
    (numeric, ZZ, CCi)
    (numeric, CC)
    (numeric, ZZ, VisibleList)
    (numeric, VisibleList)
    (numeric, ZZ, Constant)
    (numeric, Constant)
    (numeric, InfiniteNumber, Constant)
    (numeric, ZZ, Number)
    (numeric, Number)
    (numeric, ZZ, InfiniteNumber)
    (numeric, InfiniteNumber)
    (numeric, ZZ, IndeterminateNumber)
    (numeric, IndeterminateNumber)
  Headline
    convert to floating point
  Usage
    numeric x
    numeric(prec,x)
    numeric_prec x
  Inputs
    x:
    prec:ZZ
      the number of bits of precision to use in the computation
  Outputs
    :
      the expression obtained from @TT "x"@ by converting the integers,
      rational numbers, and symbolic constants within it to double floating
      point numbers.
  Description
    Example
      x = {1,2/3,pi}
      numeric oo
      numeric_200 pi
      numeric_100 oo
  SeeAlso
    RR
    CC
    Constant
///

doc ///
  Key
    odd
  Headline
    tell whether an integer is odd
  Description
    Text
      @TT "odd x"@ -- returns true or false, tells whether x is an odd integer.

      See also @TO "even"@.
///
doc ///
  Key
    even
  Headline
    tell whether an integer is even
  Description
    Text
      @TT "even x"@ -- returns true or false, tells whether x is an even
      integer.

      See also @TO "odd"@.
///
doc ///
  Key
    zero
  Headline
    whether something is zero
  Usage
    zero x
  Inputs
    x:
  Outputs
    :
      whether @TT "x"@ is equal to 0
  SourceCode
    zero
///

doc ///
  Key
    realPart
    (realPart,Number)
    (realPart,QQ)
    (realPart,ZZ)
    (realPart,InexactNumber)
    "(realPart,CC)"
    "(realPart,CCi)"
  Headline
    real part
  Usage
    realPart z
  Inputs
    z:
      an integer, rational, real or complex number, or a real or complex
      interval
  Outputs
    :
      the real part of the complex number z or an interval containing the real
      parts of real or complex interval z
  Description
    Example
      realPart(3/4)
      realPart(1.5+2*ii)
      realPart(interval(1+2*ii,3+4*ii))
  SeeAlso
    CC
    imaginaryPart
///
doc ///
  Key
    imaginaryPart
    (imaginaryPart,Number)
    (imaginaryPart,QQ)
    (imaginaryPart,ZZ)
    (imaginaryPart,InexactNumber)
    "(imaginaryPart,CCi)"
    "(imaginaryPart,CC)"
  Headline
    imaginary part
  Usage
    imaginaryPart z
  Inputs
    z:
      an integer, rational, real or complex number or a real or complex
      interval
  Outputs
    :
      the imaginary part of the complex number z or an interval containing the
      complex parts of real or complex interval z.
  Description
    Example
      imaginaryPart(3/4)
      imaginaryPart(1.5+2*ii)
      imaginaryPart(interval(1+2*ii,3+4*ii))
  SeeAlso
    CC
    realPart
///

doc ///
  Key
    conjugate
    (conjugate,CC)
    (conjugate,Number)
    (conjugate,CCi)
    (conjugate,Constant)
  Headline
    complex conjugate
  Usage
    conjugate z
  Inputs
    z:
  Outputs
    :CC
      the complex conjugate of @TT "z"@
    :CCi
      a complex interval containing all complex conjugate of points of @TT "z"@
  Description
    Example
      conjugate(1+2.5*ii)
      conjugate 3
      conjugate interval(1+2*ii,3+4*ii)
///

doc ///
  Key
    gcdCoefficients
    (gcdCoefficients, RingElement, RingElement)
    (gcdCoefficients, RingElement, ZZ)
    (gcdCoefficients, ZZ, RingElement)
    (gcdCoefficients, ZZ, ZZ)
  Headline
    greatest common divisor with coefficients
  Usage
    gcdCoefficients(a, b)
  Inputs
    a:{RingElement, ZZ}
    b:{RingElement, ZZ}
  Description
    Text
      This returns a sequence of the form @CODE "(d, r, s)"@, where $d=\gcd(a, b)$
      and $r$ and $s$ are the minimal Bézout coefficients satisfying the
      equation $d = ar + bs$.

      It works for integers or elements of polynomial rings in one variable.
    Example
      (d, r, s) = gcdCoefficients(46, 240)
      gcd(46, 240)
      46 * r + 240 * s
      R = ZZ/2[x]
      f = x^8 + x^4 + x^3 + x + 1
      g = x^6 + x^4 + x + 1
      (d, r, s) = gcdCoefficients(f, g)
      gcd(f, g)
      f * r + g * s
  SeeAlso
    gcd
///

doc ///
  Key
    mod
  Headline
    reduce modulo an integer
  Usage
    mod(i,n)
  Inputs
    i:ZZ
    n:ZZ
  Outputs
    :ZZ
      the integer @TT "i"@ modulo @TT "n"@, as an element of @TT "ZZ/n"@.
  SeeAlso
    (symbol %, ZZ, ZZ)
///

undocumented {(isConstant, Number)}
doc ///
  Key
    isConstant
    (isConstant, RingElement)
  Headline
    whether a ring element is constant
  Usage
    isConstant f
  Inputs
    f:
  Outputs
    :
      whether f is constant, i.e., is in the coefficient ring
  Description
    Example
      isConstant 3
      QQ[a,b][x,y];
      isConstant (x+a-x)
      isConstant x
  SeeAlso
    coefficientRing
  SourceCode
    (isConstant,RingElement)
///
