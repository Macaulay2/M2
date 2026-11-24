-- -*- coding: utf-8 -*-
----------- File Mike is Working on! -------------------------
doc ///
  Key
    "rings"
  Description
    Text
      Macaulay2 differs from other computer algebra systems such as Maple and
      Mathematica, in that before making a polynomial, you must create a ring
      to contain it, deciding first the complete list of indeterminates and the
      type of coefficients permitted. Recall that a ring is a set with addition
      and multiplication operations satisfying familiar axioms, such as the
      distributive rule. Examples include the ring of integers (@TO "ZZ"@), the
      ring of rational numbers (@TO "QQ"@), and the most important rings in
      Macaulay2, polynomial rings.

      The sections below describe the types of rings available and how to use
      them.

      For additional common operations and a comprehensive list of all routines
      in Macaulay2 which return or use rings, see @TO "Ring"@.
  Subnodes
    :Rings
    "basic rings of numbers"
    "integers modulo a prime"
    "finite fields"
    "polynomial rings"
    "quotient rings"
    "manipulating polynomials"
    "factoring polynomials"
    "substitution and maps between rings"
    :Fields
    "fraction fields"
    "finite field extensions"
    :Other algebras
    "exterior algebras"
    "symmetric algebras"
    "tensor products of rings"
    "Weyl algebras"
    "local rings"
    "Schur rings"
    "associative algebras"
///

doc ///
  Key
    "basic rings of numbers"
  Description
    Text
      The following rings are initially present in every session with
      Macaulay2.

      @UL {TO "ZZ", TO "QQ", TO "RR", TO "RRi", TO "CC", TO "CCi"}@

      The names of some of these rings are double letters so the corresponding
      symbols with single letters are preserved for use as variables.

      Numbers in these rings are constructed as follows.
    Example
      1234
      123/4
      123.4
      1.234e-20
      123+4*ii
    Text
      Integers may be entered in bases 2, 8, or 16 using particular prefixes.
    Example
      0b10011010010 -- binary
      0o2322 -- octal
      0x4d2 -- hexadecimal
    Text
      The usual arithmetic operations are available.
    Example
      4/5 + 2/3
      10^20
      3*5*7
      3.1^2.1
      sqrt 3.
    Text
      An additional pair of division operations that produce integer quotients
      and remainders is available.
    Example
      1234//100
      1234%100
    Text
      Numbers can be promoted to larger rings as follows, see
      @TO (symbol _, RingElement, Ring)@.
    Example
      1_QQ
      (2/3)_CC
    Text
      One way to enter real and complex numbers with more precision is to
      insert the desired number of bits of precision after the letter p at the
      end of the number, but before the possible e that indicates the exponent
      of 10.
    Example
      1p300
      1p300e-30
    Text
      Numbers can be lifted to smaller rings as follows, see @TO "lift"@.
    Example
      x = 2/3*ii/ii
      lift(x,RR)
      lift(x,QQ)
  Subnodes
    "ZZ"
    "QQ"
    "RR"
    "RRi"
    "CC"
    "CCi"
///

doc ///
  Key
    Number
  Headline
    the class of all numbers
  Subnodes
    isANumber
    InfiniteNumber
    IndeterminateNumber
    InexactNumber
    InexactNumber'
///

doc ///
  Key
    InfiniteNumber
  Headline
    the class of all infinite numbers
  Subnodes
    isFinite
    isInfinite
    infinity
///

doc ///
  Key
    infinity
  Headline
    infinity
///

doc ///
  Key
    IndeterminateNumber
  Headline
    the class of all indeterminate numbers
  Description
    Text
      Indeterminate numbers result, for example, from multiplying 0 by
      infinity. There is only one instance of this class.
  Subnodes
    indeterminate
///

doc ///
  Key
    indeterminate
  Headline
    an indeterminate number
  Description
    Text
      @TT "indeterminate"@ -- a representation of an indeterminate number, such
      as might result from multiplying 0 by infinity.
///

doc ///
  Key
    InexactNumber
  Description
    Text
      This type of number is intended to serve as a parent class for those
      types of numbers that are inexactly represented in the computer.
  Subnodes
    precision
    numeric
    round
    size2
    clean
    norm
    "minExponent"
    "maxExponent"
///

doc ///
  Key
    InexactNumber'
  Description
    Text
      This class is the common parent of the classes of complex fields and real
      fields.
  Subnodes
    "RR_*"
    "CC_*"
    (symbol _*, RingFamily)
///

doc ///
  Key
    ZZ
  Headline
    the class of all integers
  Subnodes
    -- TO (factor, ZZ),
    zero
    even
    odd
    (isPrime, ZZ)
    (isPseudoprime, ZZ)
    nextPrime
    changeBase
///

doc ///
  Key
    QQ
  Headline
    the class of all rational numbers
  Description
    Example
      1/2 + 3/5
///

doc ///
  Key
    RR
  Headline
    the class of all real numbers
  Description
    Text
      A real number is entered as a sequence of decimal digits with a point. It
      is stored internally as an arbitrary precision floating point number,
      using the @TO "MPFR"@ library.
    Example
      3.14159
    Text
      The precision is measured in bits, is visible in the ring displayed on
      the second of each pair of output lines, and can be recovered using
      @TO "precision"@.
    Example
      precision 3.14159
    Text
      For real numbers, the functions @TO "class"@ and @TO "ring"@ yield
      different results. That allows numbers of various precisions to be used
      without creating a new ring for each precision.
    Example
      class 3.1
      ring 3.1
    Text
      The precision can be specified on input by appending the letter @TT "p"@
      and a positive number.
    Example
      3p300
    Text
      An optional exponent (for the power of ten to multiply by) can be
      specified on input by appending the letter @TT "e"@ and a number.
    Example
      3e3
      -3e-3
      -3p111e-3
    Text
      Numbers that appear alone on an output line are displayed with all their
      meaningful digits. (Specifying 100 bits of precision yields about 30
      decimal digits of precision.)
    Example
      1/3.
      1/3p100
      100 * log(10,2)
    Text
      Numbers displayed inside more complicated objects are printed with the
      number of digits specified by @TO "printingPrecision"@.
    Example
      printingPrecision
      {1/3.,1/3p100}
    Text
      The notion of equality tested by @TO "=="@ amounts to equality of the
      internal binary digits.
    Example
      .5p100 == .5p30
      .2p100 == .2p30
    Text
      The notion of (strict) equality tested by @TO "==="@ also takes the
      precision into account.
    Example
      .5p100 === .5p30
      .2p100 === .2p30
    Text
      Perhaps surprisingly, the IEEE floating point standard also specifies
      that every number, including 0, has a sign bit, and strict equality
      testing takes it into account, as it must do, because some arithmetic and
      transcendental functions take it into account.
    Example
      0.
      -0.
      1/0.
      1/-0.
      log 0
      csc (0.)
      csc (-0.)
    Text
      Use @TO "toExternalString"@ to produce something that, when encountered
      as input, will reproduce exactly what you had before.
    Example
      x = {1/3.,1/3p100}
      x == {.333333, .333333}
      y = toExternalString x
      x === value y
    Text
      Transcendental constants and functions are available to high precision,
      with @TO "numeric"@.
    Example
      numeric pi
      numeric_200 pi
      Gamma oo
  SeeAlso
    numeric
    precision
    format
    "printingPrecision"
    "printingAccuracy"
    "printingLeadLimit"
    "printingTrailLimit"
    "printingSeparator"
    "maxExponent"
    "minExponent"
  Subnodes
    toRR
    isReal
///

doc ///
  Key
    toRR
  Headline
    convert to high-precision real number
  Usage
    toRR(prec,x)
  Inputs
    prec:ZZ
      the number of bits of precision desired
    x:
      @ofClass{RR, ZZ, QQ}@
  Outputs
    :RR
      the result of converting @TT "x"@ to a high-precision real number
  Description
    Example
      toRR(200,1/7)
      precision oo
///

doc ///
  Key
    "RR_*"
  Headline
    the parent class of all rings of real numbers
  Description
    Text
      Floating point real numbers are treated in a special way. Recall first to
      create a polynomial, one must first create a polynomial ring to contain
      it. And then, the polynomial ring is the class of the polynomial.
    Example
      R = QQ[x,y,z]
      x^2
      class x^2
    Text
      Floating point real numbers, however, can be created directly, as
      follows, without creating a ring.
    Example
      r = 4.5
      s = 4.3p300
    Text
      The floating point numbers created above have different precisions, and
      thus are regarded as being elements of different rings, whose elements
      all have the same precision.
    Example
      precision r
      precision s
      ring r
      ring s
    Text
      In order to make it convenient to define methods that apply to all such
      rings, those rings have a common parent, namely @TT "RR'"@. Notice that
      @TT "RR'"@ is printed in a special way.
    Example
      RR'
      parent ring r
      parent ring s
      parent ring s === RR'
///

doc ///
  Key
    CC
  Headline
    the class of all complex numbers
  Description
    Text
      In Macaulay2, complex numbers are represented as floating point numbers,
      and so are only approximate. The symbol @TO "ii"@ represents the square
      root of -1 in many numeric contexts. A complex number is obtained by
      using the symbolic constant @TO "ii"@ or the conversion functions
      @TO "toCC"@ and @TO "numeric"@, in combination with real numbers (see
      @TO "RR"@). It is stored internally as a pair of arbitrary precision
      floating point real numbers, using the @TO "MPFR"@ library.
    Example
      z = 3-4*ii
      z^5
      1/z
      +ii
      numeric_200 ii
    Text
      Complex numbers are ordered lexicographically, mingled with real numbers.
    Example
      sort {1+ii,2+ii,1-ii,2-ii,1/2,2.1,7/5}
    Text
      The precision is measured in bits, is visible in the ring displayed on
      the second of each pair of output lines, and can be recovered using
      @TO "precision"@.
    Example
      precision z
    Text
      For complex numbers, the functions @TO "class"@ and @TO "ring"@ yield
      different results. That allows numbers of various precisions to be used
      without creating a new ring for each precision.
    Example
      class z
      ring z
    Text
      A computation involving numbers of different precisions has a result with
      the minimal precision occurring. Numbers that appear alone on an output
      line are displayed with all their meaningful digits. (Specifying 100 bits
      of precision yields about 30 decimal digits of precision.)
    Example
      3p100+2p90e3*ii
    Text
      Numbers displayed inside more complicated objects are printed with the
      number of digits specified by @TO "printingPrecision"@.
    Example
      printingPrecision
      x = {1/3.*ii,1/3p100*ii}
    Text
      Use @TO "toExternalString"@ to produce something that, when encountered
      as input, will reproduce exactly what you had before.
    Example
      y = toExternalString x
      value y === x
  Caveat
    Currently, most transcendental functions are not implemented for complex
    arguments.
  SeeAlso
    "ii"
    toRR
    numeric
    precision
    format
    "printingPrecision"
    "printingAccuracy"
    "printingLeadLimit"
    "printingTrailLimit"
    "printingSeparator"
  Subnodes
    toCC
    conjugate
    realPart
    imaginaryPart
///

doc ///
  Key
    toCC
    (toCC, ZZ, ZZ)
    (toCC, ZZ, QQ)
    (toCC, ZZ, RR)
    (toCC, ZZ, CC)
    (toCC, RR, RR)
    (toCC, ZZ, ZZ, ZZ)
    (toCC, ZZ, ZZ, QQ)
    (toCC, ZZ, QQ, ZZ)
    (toCC, ZZ)
    (toCC, ZZ, QQ, QQ)
    (toCC, QQ)
    (toCC, ZZ, RR, ZZ)
    (toCC, ZZ, ZZ, RR)
    (toCC, ZZ, RR, QQ)
    (toCC, ZZ, QQ, RR)
    (toCC, RR)
    (toCC, CC)
    (toCC, ZZ, RR, RR)
  Headline
    convert to high-precision complex number
  Synopsis
    Usage
      toCC(prec,x,y)
      toCC(prec,x)
    Inputs
      prec:ZZ
        the number of bits of precision desired
      x:
        @ofClass{ZZ, QQ, RR}@
      y:
        @ofClass{ZZ, QQ, RR}@
    Outputs
      :CC
        the complex number with real part @TT "x"@ and complex part @TT "y"@.
        If @TT "y"@ is omitted, the imaginary part is zero.
    Description
      Example
        toCC(200,7)
        toCC(100,7,3.)
  Synopsis
    Usage
      toCC(x,y)
      toCC x
    Inputs
      x:RR
      y:RR
    Outputs
      :CC
        the complex number with real part @TT "x"@ and complex part @TT "y"@.
        If @TT "y"@ is omitted, the imaginary part is zero. The precision of
        the result is the minimum precision of the arguments.
    Description
      Example
        toCC(3.,4.)
        toCC(3.p100,4.p200)
///

doc ///
  Key
    "CC_*"
  Headline
    the parent class of all rings of complex numbers
  Description
    Text
      Floating point complex numbers are treated in a special way. Recall first
      to create a polynomial, one must first create a polynomial ring to
      contain it. And then, the polynomial ring is the class of the polynomial.
    Example
      R = QQ[x,y,z]
      x^2
      class x^2
    Text
      Floating point complex numbers, however, can be created directly, as
      follows, without creating a ring.
    Example
      r = 4.5 * ii
      s = 4.3p300 * ii
    Text
      The floating point numbers created above have different precisions, and
      thus are regarded as being elements of different rings, whose elements
      all have the same precision.
    Example
      precision r
      precision s
      ring r
      ring s
    Text
      In order to make it convenient to define methods that apply to all such
      rings, those rings have a common parent, namely @TT "CC'"@. Notice that
      @TT "CC'"@ is printed in a special way.
    Example
      CC'
      parent ring r
      parent ring s
      parent ring s === CC'
///

undocumented {"RRi_*"}

doc ///
  Key
    RRi
  Headline
    the class of all real intervals
  Description
    Text
      A real interval is entered as a pair of real numbers to the interval
      function. It is stored internally as an arbitrary precision interval
      using the @TO "MPFI"@ library.
    Example
      interval(3.1415,3.1416)
    Text
      The precision is measured in bits, is visible in the ring displayed on
      the second of each pair of output lines, and can be recovered using
      @TO "precision"@.
    Example
      precision interval(3.1415,3.1416)
    Text
      For real intervals, the functions @TO "class"@ and @TO "ring"@ yield
      different results. That allows numbers of various precisions to be used
      without creating a new ring for each precision.
    Example
      class interval(3.1,3.5)
      ring interval(3.1,3.5)
    Text
      The precision can be specified on input by specifying the precision of
      both input @TO "RR"@ numbers. Alternatively, the precision can be
      specified by including the option @TT "Precision"@.
    Example
      interval(2.5p100,3.2p1000)
      interval(2.5,3.2,Precision=>200)
    Text
      Intervals can also be created using @TO (span, Sequence)@ to create the
      smallest interval containing the inputs.
    Example
      span(2,Precision=>100)
      span(2,3,interval(-1.5,-0.5),73)
    Text
      Operations using intervals are computed as sets so that the resulting
      intervals contain all possible outputs from pairs of points in input
      intervals.
    Example
      interval(1,3)+interval(2,4)
      interval(-1,1)*interval(2,3)
      interval(0,1)-interval(0,1)
      interval(1,2)/interval(1,2)
    Text
      The notion of equality tested by @TO "=="@ amounts to checking the
      equality of the endpoints of intervals.The notion of equality tested by
      @TO "==="@ takes into account the precision of the inputs as well.
    Example
      interval(1,3) == interval(1,3,Precision=>100)
      interval(1,3) === interval(1,3,Precision=>100)
      interval(1/3,1,Precision=>100)==interval(1/3,1,Precision=>1000)
    Text
      The notion of inequalities for intervals amounts to testing the
      inequality for all points in the intervals. In particular, @TO "<="@ is
      not the same as @TO "<"@ or @TO "=="@.
    Example
      interval(1,2)<=interval(2,3)
      interval(1,2)<=interval(1,2)
      interval(1,2)<interval(2,3)
      interval(1,2)<interval(3,4)
    Text
      Transcendental functions on intervals produce intervals containing the
      image of the function on the interval.
    Example
      exp(interval(2,4))
      cos(interval(1,1.3))
      sqrt(interval(2))
    Text
      Transcendental functions are available to high precision, with
      @TO "numericInterval"@.
    Example
      numericInterval(100,pi)
      numericInterval_200 EulerConstant
    Text
      The left and right endpoints of an interval can be accessed with
      @TO "left"@ and @TO "right"@. Similarly, the midpoint and the length of
      an interval can be found with @TO "midpoint"@ and @TO "diameter"@.
    Example
      left interval(1,5)
      right interval(1,5)
      midpoint interval(1,5)
      diameter interval(1,5)
  SeeAlso
    CCi
    toRRi
    numericInterval
    precision
    interval
    (span,Sequence)
    (span,List)
  Subnodes
    toRRi
    interval
    diameter
    left
    right
    midpoint
    numericInterval
    (intersect, RRi, RRi)
    (isMember, QQ, RRi)
    (isEmpty, RRi)
    (isSubset, RRi, RRi)
    span
///

undocumented {"CCi_*"}

doc ///
  Key
    CCi
  Headline
    the class of all complex intervals
  Description
    Text
      A complex interval is entered as a pair of real intervals to the
      @TO "interval"@ function. It is stored internally as a pair of arbitrary
      precision intervals using the @TO "MPFI"@ library.
    Example
      interval(interval(3.1415,3.1416), interval(2.7182,2.7183))
    Text
      The precision is measured in bits, is visible in the ring displayed on
      the second of each pair of output lines, and can be recovered using
      @TO "precision"@.
    Example
      precision interval(interval(3.1415,3.1416), interval(2.7182,2.7183))
    Text
      Complex intervals are objects in @TO "class"@ CCi which is an
      @TO "InexactFieldFamily"@.For complex intervals, the functions
      @TO "class"@ and @TO "ring"@ yield different results. That allows numbers
      of various precisions to be used without creating a new ring for each
      precision.
    Example
      class interval(interval(3.1,3.5),interval(1.1,1.2))
    Text
      The precision can be specified on input by specifying the precision of
      both input @TO "CC"@ numbers. Alternatively, the precision can be
      specified by including the option @TT "Precision"@.
    Example
      interval(interval(2.5p1000,3.2p100),interval(2.3,3.1,Precision=>200))
    Text
      Complex intervals can also be created using @TO (span, Sequence)@ to
      create the smallest complex axis-aligned rectangle containing the inputs.
    Example
      span(2+3*ii,Precision=>100)
      span(2,3*ii,interval(interval(-1.5,-0.5),interval(3,3.1)),73)
    Text
      Operations using complex intervals are computed as sets so that the
      resulting intervals contain all possible outputs from pairs of points in
      input intervals.
    Example
      (I, J, K) = (interval(interval(.5,.8),interval(.6,.9)), interval(interval(.54,.78),interval(.65,.89)), interval(interval(.45,.6),interval(.3,.78)));
      I + J
      I - J
      I * K
      I / K
      I ^ 3
      2 * I
      (2+3*ii) * I
    Text
      The real and imaginary parts of a complex interval can be accessed with
      @TO "realPart"@ and @TO "imaginaryPart"@. These are real intervals of
      class @TO "RRi"@. The @TO "diameter"@ of a complex interval is the
      (rounded) approximation to the length of its diagonal, while its
      @TO "midpoint"@ is the complex midpoint of its real and imaginary parts
    Example
      realPart interval(1+2*ii,3+4*ii)
      imaginaryPart interval(1+2*ii,3+4*ii)
      diameter interval(1+2*ii,3+4*ii)
      midpoint interval(1+2*ii,3+4*ii)
  SeeAlso
    RRi
    toCCi
    precision
    interval
    midpoint
    diameter
    realPart
    imaginaryPart
    intersect
    isEmpty
    isSubset
    span
  Subnodes
    toCCi
///

doc ///
  Key
    "integers modulo a prime"
  Description
    Text
      Create the ring of integers modulo a prime number @TT "p"@ as follows.
    Example
      R = ZZ/101
    Text
      We can create elements of the ring as follows.
    Example
      9_R
      103_R
    Text
      The usual arithmetic operations are available.
    Example
      9_R * 11_R
      9_R ^ 11
      9_R * 11_R == -2_R
    Text
      Find the inverse of an integer modulo a prime as follows.
    Example
      17_R^-1
    Text
      To view this element as an element of @TT "ZZ"@ use the @TO "lift"@
      command.
    Example
      lift (17_R^-1, ZZ)
  Subnodes
    char
    getPrimeWithRootOfUnity
///

doc ///
  Key
    "finite fields"
  Description
    Text
      -- Also include: getting the variable, its equation.
      -- Current restrictions on p, p^n.
      -- example should include: making these, simple arithmetic
      -- Pointer to finite fields II.
      Two basic finite fields are:

      @UL {TO2 {"integers modulo a prime", "ZZ/p"}, TO2 {"GF", "GF(p^n)"}}@

      Create a finite field with $q = p^n$ elements using
    Example
      F = GF(81,Variable=>a)
    Text
      This creates the ring of characteristic 3, having 3^4 = 81 elements. The
      elements of this ring are 0, a, a^2, a^3, ..., a^80.
    Example
      a^80
      a^40
    Text
      Use @TO "ambient"@ to see the quotient ring the field is made from.
    Example
      ambient F
    Text
      Now check that @TT "a"@ satisfies this equation.
    Example
      a^4 + a - 1
    Text
      It is often preferable to view elements of @TT "F"@ as polynomials in
      @TT "a"@ rather than as powers of @TT "a"@. This can be accomplished by
      lifting the elements back to this ambient ring.
    Example
      lift(a^20, ambient F)
      apply({20,40,80}, i -> lift(a^i, ambient F))
    Text
      (for more details on lift, see @TO "working with multiple rings"@).

      Finite fields can be used as base rings for polynomial rings.
    Example
      R = F[x,y,z]
      f = random(2,R)
      f = (leadCoefficient f)^(-1) * f
    Text
      Gröbner bases, and all related computations work in these rings.

      The prime finite fields can be made easily as quotient rings of
      @TO "ZZ"@.
    Example
      ZZ/101
    Text
      In general, to make a finite field with @TT "q"@ elements, we use
      @TO "GF"@.
    Example
      k = GF 81
    Text
      The generator of the field is available as the variable @VAR "a"@ or it
      can be obtained as usual.
    Example
      a
      k_0
    Text
      You may use @TO "ambient"@ to see the quotient ring the field is made
      from.
    Example
      ambient k
    Text
      Use @TO "ideal"@ to see the ideal that defined that quotient ring.
    Example
      ideal oo
    Text
      Finally, you may use @TO "_"@ to recover the generator of the ideal.
    Example
      oo_0
    Text
      To specify a different name for the generator when the field is created,
      use the @TO "Variable"@ option.
    Example
      F = GF(16, Variable => b)
      b^20 + 1
      random F
    Text
      Finite fields can be used as base rings for polynomial rings.
    Example
      R = F[x,y,z]
      random(2,R)
    Text
      If you have a quotient ring that you know is a finite field, then you can
      convert it to ring that is known by the system to be a finite field.
    Example
      GF (ZZ/2[T]/(T^9+T+1), Variable => T) -* no-capture-flag *-
    Text
      You may also provide your own choice of primitive element. Internally,
      elements of the finite field are stored as powers of the primitive
      element. First we assign our quotient ring to a global variable to ensure
      that @TT "T"@ gets set to a value in the quotient ring, and then we call
      @TT "GF"@.
    Example
      A = ZZ/2[T]/(T^9+T+1)
      k = GF (A, PrimitiveElement => T^3+1)
    Text
      Notice that @TT "T"@ is now recorded as an element of this finite field.
    Example
      T
    Text
      The generator of A can be obtained this way:
    Example
      A_0
    Text
      Use @TO "substitute"@ to map it to an element of the finite field.
    Example
      substitute(A_0,k)
    Text
      Conversely, a given element of the finite field can be transferred back
      to the quotient ring with @TO "lift"@.
    Example
      lift(k_0, ring T)
    Text
      We can even lift it back to the polynomial ring.
    Example
      lift(k_0, ambient ring T)
    Text
      For more information see @TO "GaloisField"@.
  Subnodes
    isFinitePrimeField
///

doc ///
  Key
    "polynomial rings"
  Description
    Text
      A polynomial ring can be created with the usual mathematical notation.
    Example
      ZZ[x,y,z]
    Text
      If you try to construct this ring again, you will get a different answer.
      We use the strict comparison operator @TO "==="@ to demonstrate this.
    Example
      ZZ[x,y,z]===ZZ[x,y,z]
    Text
      Thus it is a good idea to assign a new ring to a variable for future
      reference.
    Example
      R = QQ[a,b,c,d,e,f]
    Text
      Notice that after assignment to a global variable, Macaulay2 knows the
      ring's name, and this name is used when printing the ring.
    Example
      R
    Text
      The original description of the ring can be recovered with
      @TO "describe"@.
    Example
      describe R
    Text
      Use the following subscript notation to obtain 0,1, or any multiple of 1,
      as elements in the ring.
    Example
      0_R
      1_R
      11_R
    Text
      Obtain the variables (generators) of the ring by subscripting the name of
      the ring. As always in Macaulay2, indexing starts at 0.
    Example
      R_0^10+R_1^3+R_2
    Text
      It is also possible to obtain the variables in a ring from strings
      containing their names.
    Example
      "a"_R^10+"b"_R^3+"c"_R
    Text
      The number of variables is provided by @TO "numgens"@.
    Example
      numgens R
      apply(numgens R, i -> R_i^i)
      sum(numgens R, i -> R_i^i)
    Text
      (See @TO "apply"@ and @TO "sum"@.) Use @TO "generators"@ to obtain a list
      of the variables of the ring.
    Example
      gens R
    Text
      A matrix (with one row) containing the variables of the ring can be
      obtained using @TO (vars, Ring)@.
    Example
      vars R
    Text
      The @TO "index"@ of a variable:
    Example
      index x, index y, index z
    Text
      The coefficient ring can be recovered with @TO "coefficientRing"@.
    Example
      coefficientRing R
    Text
      An element of the coefficient ring can be promoted to the polynomial
      ring.
    Example
      promote(11/2,R)
    Text
      Conversely, an element of the polynomial ring that is known to be a
      scalar can be lifted back to the coefficient ring.
    Example
      sc = (a-2)^2-a^2+4*a
      lift(sc,QQ)
    Text
      In programs, the function @TO "liftable"@ can be used to see whether this
      is possible.
    Example
      liftable(sc,QQ)
      liftable(c^3,QQ)
    Text
      A random homogeneous element can be obtained with @TO "random"@.
    Example
      random(2,R)
    Text
      A basis of the subspace of ring elements of a given degree can be
      obtained in matrix form with @TO "basis"@.
    Example
      basis(2,R)
    Text
      We may construct polynomial rings over polynomial rings.
    Example
      R = ZZ[a,b,c][d,e,f];
    Text
      When displaying an element of an iterated polynomial ring, parentheses
      are used to organize the coefficients recursively, which may themselves
      be polynomials.
    Example
      (a+d+1)^2
    Text
      Internally, the polynomials in such towers are expressed in terms of a
      flattened monoid containing all the variables, obtainable with the key
      @TO "FlatMonoid"@.
    Example
      R.FlatMonoid
    Text
      Variable names may be words.
    Example
      QQ[rho,sigma,tau];
      (rho - sigma)^2
    Text
      There are various other ways to specify the variables in a polynomial
      ring. A sequence of variables can be obtained as follows.
    Example
      ZZ[b..k];
    Text
      In this example, if you had previously assigned either b or k a value
      that was not a ring generator, then Macaulay2 would complain about this:
      it would no longer understand what variables you wanted. To get around
      this, we could either do
    Example
      ZZ[symbol b .. symbol k];
    Text
      or we may obtain the single-letter variables with @TO "vars"@.
    Example
      vars (0..4)
      ZZ[vars (0..4),vars(26..30),vars 51]
    Text
      Subscripted variables can be used, provided the base for the subscripted
      variable has not been used for something else.
    Example
      ZZ[t,p_0,p_1,q_0,q_1];
    Text
      Sequences of subscripted variables can also be used.
    Example
      ZZ[p_(0,0) .. p_(2,1),q_0..q_5]
      (p_(0,0)+q_2-1)^2
    Text
      The subscripts can be much more general, but care is required when using
      symbols as subscripts, for the symbols may acquire values later that
      would interfere with your original use of them as symbols. Thus you
      should protect symbols that will be used in this way.
    Example
      protect xx; protect yy; protect zz;
      ZZ[ee_[xx],ee_[yy],ee_[zz]]
    Text
      A basis of the subspace of ring elements of a given degree can be
      obtained in matrix form with @TO "basis"@.
    Example
      R = QQ[a,b,c,d,e,f];
      basis(2,R)
    Text
      The Hilbert series of a polynomial ring can be obtained. Its power series
      expansion is the generating function for the dimensions of the degree
      @TT "n"@ parts.
    Example
      hilbertSeries R
    Text
      We may use the option @TO "Degrees"@ to produce rings where the
      generators have degrees other than 1.
    Example
      S = ZZ/101[a,b,c,d,Degrees=>{1,2,3,4}]
      random(5,S)
      hilbertSeries S
    Text
      Some things to watch out for when using polynomial rings:

      @UL {
          LI (
              "Defining a ring twice gives different rings, as far as Macaulay2 is concerned: We use the strict comparison operator ",
              TO "===",
              " to demonstrate this.",
              EXAMPLE "ZZ[a,b,c] === ZZ[a,b,c]",
              "Thus it is a good idea to assign a new ring to a variable for future reference."
              )
          }@
  SeeAlso
    "heft vectors"
    "division in polynomial rings with monomials less than 1"
  Subnodes
    isPolynomialRing
    (symbol SPACE, Ring, Array)
    (symbol SPACE, Ring, Monoid)
    coefficientRing
    vars
    (vars, List)
    "get a ring variable by index"
    "get a ring variable by name"
    "get a monomial by exponent vector"
    "graded and multigraded polynomial rings"
    "monomial orderings"
///

doc ///
  Key
    "graded and multigraded polynomial rings"
  Description
    Text
      It is possible to set up a polynomial ring so that the degree of an
      element is a vector of integers. For this, the option @TO "Degrees"@ is
      used, together with a list of degrees for the variables in the ring. Each
      degree is itself a list of integers. The degrees given must all be of the
      same length, and length zero is allowed, to get an ungraded ring.
    Example
      R = ZZ/101[a,b,c,Degrees=>{{1,2},{2,1},{1,0}}]
      describe R
    Example
      degree a
      degree b^2
      degree 0_R
      degree 1_R
    Text
      A random element of bi-degree @TT "{m,n}"@ can be obtained with
      @TO "random"@.
    Example
      random({15,15},R)
    Text
      The function @TO "degree"@ applied to a polynomial will return the least
      upper bound of the degrees of its monomials.
    Example
      degree (a+b)
    Text
      We may recover the number of integers in each degree list for our ring as
      follows.
    Example
      degreeLength R
      degreeLength ZZ
    Text
      One restriction on degrees of variables is that the entries be small
      integer values, possibly zero or negative. The notion of small depends on
      the size of exponents one wants: the degree of each monomial occurring
      should fit in a 32 bit integer (or 64 bit integer, on 64 bit machines).

      Another restriction on degrees, at least if all the computational
      facilities of Macaulay2 are needed, is that a heft vector exists for
      them. A heft vector is a list of integers whose length is the same as the
      length of the degrees (see @TO degreeLength@), such that its dot product
      with the degree of each variable is positive. Heft vectors are computed
      automatically for you, as in the following example, or they may be
      provided by the user (see @TO "Heft"@).
    Example
      R = QQ[a,b,c,Degrees=>{{1,0},{-2,1},{-3,1}}];
      random({1,1},R)
      basis({1,1},R)
    Text
      The heft vector computed behind the scenes is available to the user.
    Example
      (options R).Heft
    Text
      If the heft vector is not provided, many computations will work (e.g.,
      Gröbner bases and computation of resolutions), but certain other
      operations (such as @TT "basis"@ and @TT "random"@) will raise errors.
  Subnodes
    "heft vectors"
///

doc ///
  Key
    "quotient rings"
  Description
    Text
      -- R/I.  GB of I is needed for arithmetic.
      -- The variables get set?  Doing a quotient ring twice: ie. R/I, then R/I
      -- gives DIFFERENT rings.  Pointer to working with multiple rings.
      The usual notation is used to form quotient rings. For quotients of
      polynomial rings, a Gröbner basis is computed and used to reduce ring
      elements to normal form after arithmetic operations.
    Example
      R = ZZ/11
      6_R + 7_R
    Example
      S = QQ[x,y,z]/(x^2-y, y^3-z)
      {1,x,x^2,x^3,x^4,x^5,x^6,x^7,x^8}
    Text
      In the example above you might have wondered whether typing @TT "x"@
      would give an element of @TT "S"@ or an element of @TT "QQ[x,y,z]"@. Our
      convention is that typing @TT "x"@ gives an element of the last ring that
      has been assigned to a global variable. Here is another example.
    Example
      T = ZZ/101[r,s,t]
      T/(r^3+s^3+t^3)
      r^3+s^3+t^3
    Text
      Notice that this time, the variables end up in the ring @TT "T"@, because
      we didn't assign the quotient ring to a global variable. The command
      @TO "use"@ would install the variables for us, or we could assign the
      ring to a global variable.
    Example
      U = ooo
      r^3+s^3+t^3
    Text
      The functions @TO "lift"@ and @TO "substitute"@ can be used to transfer
      elements between the polynomial ring and its quotient ring.
    Example
      lift("r"_U,T)
      substitute("r"_T,U)
    Text
      A random element of degree @TT "n"@ can be obtained with @TO "random"@.
    Example
      random(2,S)
    Text
      In a program we can tell whether a ring is a quotient ring.
    Example
      isQuotientRing ZZ
      isQuotientRing S
    Text
      We can recover the ring of which a given ring is a quotient.
    Example
      ambient S
    Text
      We can also recover the coefficient ring, as we could for the original
      polynomial ring.
    Example
      coefficientRing S
    Text
      Here's how we can tell whether the defining relations of a quotient ring
      were homogeneous.
    Example
      isHomogeneous S
      isHomogeneous U
    Text
      We can obtain the characteristic of a ring with @TO "char"@.
    Example
      char (ZZ/11)
      char S
      char U
    Text
      The presentation of the quotient ring can be obtained as a matrix with
      @TO "presentation"@.
    Example
      presentation S
    Text
      If a quotient ring has redundant defining relations, a new ring can be
      made in which these are eliminated with @TO "trim"@.
    Example
      R = ZZ/101[x,y,z]/(x-y,y-z,z-x)
      trim R
    Text
      For more information see @TO "QuotientRing"@.
  Subnodes
    (symbol /, Ring, Ideal)
    isQuotientRing
    isQuotientOf
///

doc ///
  Key
    "manipulating polynomials"
  Description
    Text
      -- TO "f _ ZZ",
      -- TO "f _ monomial",
      @UL {
          TO "+",
          TO "-",
          TO "*",
          TO "^",
          TO "//",
          TO "%",
          TO "terms",
          TO "someTerms",
          TO "part",
          TO "parts",
          TO "diff",
          TO "contract",
          TO "content",
          TO "listForm",
          TO "degree",
          TO "homogenize",
          TO "exponents",
          TO "leadCoefficient",
          TO "leadTerm",
          TO "size"
          }@

      Let's set up some polynomials.
    Example
      R = ZZ/10007[a,b];
      f = (2*a+3)^4 + 5
      g = (2*a+b+1)^3
    Text
      The number of terms in a polynomial is obtained with @TO "size"@.
    Example
      size f, size g
    Text
      The degree of a polynomial is obtained with @TO "degree"@.
    Example
      degree f
      degree g
    Text
      (Notice that the degree is a list containing one integer, rather than an
      integer. The degree is actually a vector of integers, represented as a
      list, with one component by default.)

      The list of terms of a polynomial is obtained with @TO "terms"@.
    Example
      terms g
    Text
      We may combine that with @TO "select"@ to select terms satisfying certain
      conditions. Here we select the terms of degree 2, subsequently summing
      them, keeping in mind that the degree of a polynomial is always a list of
      integers.
    Example
      select(terms g, i -> degree i == {2})
      sum oo
    Text
      Of course, if the list of selected terms is empty, the sum would turn out
      to be the zero integer, rather than the zero element of the ring
      @TT "R"@. Fortunately, we have another way to select the elements of
      given degree or multi-degree (see @TO part@).
    Example
      part(0,g)
      part(1,g)
      part(2,g)
      part(3,g)
    Text
      A string representing the polynomial, suitable for entry into other
      programs, can be obtained with @TO "toString"@.
    Example
      toString f
      toString g
    Text
      The usual algebraic operations on polynomials are available, but there
      are some special remarks to make about division. The result of division
      depends on the ordering of monomials chosen when the ring is created, for
      division of @TT "f"@ by @TT "g"@ proceeds by locating monomials in
      @TT "f"@ divisible by the leading monomial of @TT "g"@, and substituting
      for it the negation of the rest of @TT "g"@. The quotient is provided by
      the expression @TT "f//g"@, and the remainder is obtained with
      @TT "f%g"@.
    Example
      quot = f//g
      rem = f%g
      f == quot * g + rem
    Text
      Notice that as in the example above, comparison of polynomials is done
      with the operator @TO "=="@.

      Polynomials can be homogenized with respect to one of the variables in
      the ring with @TO "homogenize"@.
    Example
      homogenize(f,b)
    Text
      The ring containing a ring element can be obtained with @TO "ring"@.
    Example
      ring f
    Text
      You can use this in a program to check whether two ring elements come
      from the same ring.
    Example
      ring f === ring g
    Text
      Notice that in the comparison above, the strict equality operator
      @TO "==="@ is used.

      The coefficient of a monomial in a polynomial can be obtained with
      @TO "_"@.
    Example
      part(1,f)
      f_a
      g_(a*b)
    Text
      (Notice that the coefficients are elements of the coefficient ring.)

      We may get parts of the leading term of a polynomial as follows.
    Example
      leadTerm g
      leadCoefficient g
      leadMonomial g
    Text
      The exponents of a monomial or term can be extracted with
      @TO "exponents"@.
    Example
      exponents leadMonomial g
      exponents leadTerm g
    Text
      We can get all of the coefficients at once, assembled in a one-rowed
      matrix, along with a matrix containing the corresponding monomials.
    Example
      coefficients f
      coefficients g
    Text
      A list of lists of exponents appearing in a polynomial can be obtained
      with @TO "exponents"@.
    Example
      exponents f
      exponents g
    Text
      The entire structure of a polynomial can be provided in an accessible
      form based on lists with @TO "listForm"@.
    Example
      listForm f
      S = listForm g
    Text
      The lists above are lists of pairs, where the first member of each pair
      is a list of exponents in a monomial, and the second member is the
      corresponding coefficient. Standard list operations can be used to
      manipulate the result.
    Example
      S / print;
    Text
      The structure of a polynomial can also be provided in a form based on
      hash tables with @TO "standardForm"@.
    Example
      S = standardForm f
      standardForm g
    Text
      The hash tables above present the same information, except that only
      nonzero exponents need to be provided. The information can be extracted
      with @TO "#"@.
    Example
      S#(new HashTable from {0 => 2})
    Text
      --      "Monomials (monoid elements) have an accessible form that is implicitly used
      --      above.",
      --      EXAMPLE {
      -- 	  "listForm leadMonomial g",
      --       	  "standardForm leadMonomial g",
      -- 	  },
      Comparison of polynomials is possible, and proceeds by simply examining
      the lead monomials and comparing them.
    Example
      f < g
      sort {b^2-1,a*b,a+1,a,b}
    Text
      The comparison operator @TO "?"@ returns a symbol indicating how two
      polynomials, or rather, their lead monomials, stand with respect to each
      other in the monomial ordering.
    Example
      f ? g
  Subnodes
    degree
    homogenize
    weightRange
    monomials
    coefficient
    coefficients
    content
    exponents
    index
    indices
    part
    parts
    someTerms
    terms
    topCoefficients
    leadCoefficient
    leadMonomial
    leadTerm
    size
    standardForm
    listForm
///

doc ///
  Key
    "factoring polynomials"
  Description
    Text
      Polynomials can be factored with @TO "factor"@. Factorization works in
      polynomial rings over prime finite fields, ZZ, or QQ.
    Example
      R = ZZ/10007[a,b];
      f = (2*a+3)^4 + 5
      g = (2*a+b+1)^3
      S = factor f
      T = factor g
    Text
      The results have been packaged for easy viewing. The number of factors is
      obtained using
    Example
      #T
    Text
      Each factor is represented as a power (exponents equal to 1 don't appear
      in the display.) The parts can be extracted with @TO "#"@.
    Example
      T#0
      T#0#0
      T#0#1
  Subnodes
    (roots, RingElement)
///

doc ///
  Key
    "fraction fields"
  Description
    Text
      The fraction field of a ring (which must be an integral domain) is
      obtained with the function @TO "frac"@.
    Example
      frac ZZ
      R = ZZ/101[x,y]/(x^3 + 1 + y^3)
      frac R
    Text
      After defining a ring such as @TT "R"@, fractions in its fraction field
      can be obtained by writing them explicitly.
    Example
      x
      1/x
      x/1
    Text
      Alternatively, after applying the function @TO "use"@, or assigning the
      fraction ring to a global variable, the symbols you used become
      associated with the corresponding elements of the fraction field.
    Example
      use frac R
      x
    Text
      Fractions are reduced to the extent possible. This is done by computing
      the syzygies between the numerator and denominator, and picking one of
      low degree.
    Example
      f = (x-y)/(x^6-y^6)
      (x^3 - y^3) * f
    Text
      The parts of a fraction may be extracted.
    Example
      numerator f
      denominator f
    Text
      Alternatively, the functions @TO "lift"@ and @TO "liftable"@ can be used.
    Example
      liftable(1/f,R)
      liftable(f,R)
      lift(1/f,R)
    Text
      Note that computations, such as Gröbner bases, over fraction fields can
      be quite slow.
  SeeAlso
    liftable
    lift
    (kernel,RingMap)
  Subnodes
    frac
    fraction
    numerator
    denominator
///

doc ///
  Key
    "finite field extensions"
  Subnodes
    isField
    toField
    getNonUnit
    isPrimitive
    -- writeup under "toField" is a good start,
    -- needs an example
///
doc ///
    Key
        "exterior algebras"
    Headline
        a polynomial ring with skew-commutative variables
    Description
        Text
            An exterior algebra is a polynomial ring $R$ where multiplication of
            the variables obeys the commutation relation $xy = (-1)^{\textrm{deg}(x)
            \textrm{deg}(y)}yx$. One notable consequence of this is
            that if $\textrm{deg}(x)$ is odd, then $x^2 = 0$.

            Here, $\textrm{deg}(x)$ is the degree of $x$ - or the first
            degree of $x$ in case $R$ is multi-graded. By default, the
            degree of each variable in a polynomial ring is 1, so in this case
            we have the simple rule $xy = -yx$ for multiplying variables.
        Text
            Create an exterior algebra with explicit generators by creating a polynomial
            ring with the option @TO "SkewCommutative"@.
        Example
            R = QQ[x,y,z, SkewCommutative => true]
            y*x
            (x+y+z)^2
            basis R
            basis(2,R)
        Text
            You can declare that only a subset of the variables are skew-commutative by passing them explicitly.
        Example
            R = QQ[a..c, SkewCommutative => {b, c}]
            a*b == b*a
            a*c == c*a
            b*c == -c*b
        Text
            You can also declare skew variables by passing indices into the
            array of variables. As usual, negative indices can be used and these are
            interpreted as an offset from the end of the variables array.
        Example
            R = QQ[a..c, SkewCommutative => {-1, 1}]
            a*b == b*a
            a*c == c*a
            b*c == -c*b
        Text
            The degree of the variables can be specified just as in the commutative case.
        Example
            R = QQ[a,b,r,s,t, SkewCommutative=>true, Degrees=>{2,2,1,1,1}]
            r*a == a*r
            a*a
            f = a*r+b*s; f^2
            basis(2,R)
        Text
            As usual in Macaulay2, matrices are actually matrices over
            $R^\textrm{op}$ and so matrix arithmetic over exterior algebras is
            slightly different from what you see in the commutative case.
        Example
            R = QQ[a..d, SkewCommutative => true]
            A = matrix {{a, b}, {1, 1}}
            B = matrix {{c, 1}, {d, 1}}
            A * B
        Text
            See  @TO "right modules or left modules?"@ for more details.
        Text
            You may compute Gröbner bases, syzygies, and form quotient rings of these skew
            commutative rings. Warning that quotienting by an ideal which is not a
            2-sided ideal will produce quotient ring where multiplication is not well
            defined on coset representatives.
        Example
            R = QQ[a..d, SkewCommutative => true]
            I = ideal {a*b + c}
            promote(I_0 * d, R/I)
            promote(I_0, R/I) * promote(d, R/I)
    Subnodes
        "isSkewCommutative"
        "antipode"
        center
        centerMap
///

doc ///
  Key
    "symmetric algebras"
  Description
    Text
      Polynomial rings are symmetric algebras with explicit generators, and we
      have already seen how to construct them. But if you have a module, then
      its symmetric algebra can be constructed with @TO "symmetricAlgebra"@.
    Example
      R = QQ[a..d];
      symmetricAlgebra R^3
    Text
      Maps between symmetric algebras can be constructed functorially.
    Example
      vars R
      symmetricAlgebra vars R
      symmetricAlgebra transpose vars R
    Text
      Until the ring is used with @TO "use"@ or assigned to a global variable,
      its generators are not assigned to global variables.
    Example
      a
      p_0
      S = o2;
      a
      p_0
    Text
      To specify the names of the variables when creating the ring, use the
      @TO "Variables"@ option or the @TO "VariableBaseName"@ option.
    Example
      symmetricAlgebra(R^3, Variables => {t,u,v})
      symmetricAlgebra(R^3, VariableBaseName => t)
    Text
      We can construct the symmetric algebra of a module that isn't necessarily
      free.
    Example
      use R
      symmetricAlgebra(R^1/(a,b^3))
///

doc ///
  Key
    "tensor products of rings"
  Description
    Text
      -- **, tensor.  Options for changing monomial orders.
      -- What is the default monomial order
      -- What if the names of the variables clash.
      -- The tensor product of two quotients of poly rings is
      -- a quotient of another polynomial ring.
      The operator @TO "**"@ or the function @TO "tensor"@ can be used to
      construct tensor products of rings.
    Example
      ZZ/101[x,y]/(x^2-y^2) ** ZZ/101[a,b]/(a^3+b^3)
    Text
      Other monomial orderings can be specified.
    Example
      T = tensor(ZZ/101[x,y], ZZ/101[a,b], MonomialOrder => Eliminate 2)
    Text
      The options to @TT "tensor"@ can be discovered with @TO "options"@.
    Example
      options tensor
    Text
      Given two (quotients of) polynomial rings, say, R = A[x1, ..., xn]/I, S =
      A[y1,...,yn]/J, then R ** S = A[x1,...,xn,y1, ..., yn]/(I + J). The
      variables in the two rings are always considered as different. If they
      have name conflicts, you may still use the variables with indexing, but
      the display will be confusing:
    Example
      R = QQ[x,y]/(x^3-y^2);
      T = R ** R
      generators T
      {T_0 + T_1, T_0 + T_2}
    Text
      We can change the variable names with the @TO "Variables"@ option.
    Example
      U = tensor(R,R,Variables => {x,y,x',y'})
      x + y + x' + y'
///

doc ///
  Key
    "Weyl algebras"
    isWeylAlgebra
    (isWeylAlgebra, PolynomialRing)
    (isWeylAlgebra, QuotientRing)
    (isWeylAlgebra, Ring)
  Description
    Text
      A Weyl algebra is the non-commutative algebra of algebraic differential
      operators on a polynomial ring. To each variable @TT "x"@ corresponds the
      operator @TT "dx"@ that differentiates with respect to that variable. The
      evident commutation relation takes the form @TT "dx*x == x*dx + 1"@.

      We can give any names we like to the variables in a Weyl algebra,
      provided we specify the correspondence between the variables and the
      derivatives, with the @TO "WeylAlgebra"@ option, as follows.
    Example
      R = QQ[x,y,dx,dy,t,WeylAlgebra => {x=>dx, y=>dy}]
      dx*dy*x*y
      dx*x^5
    Text
      All modules over Weyl algebras are, in Macaulay2, right modules. This
      means that multiplication of matrices is from the opposite side:
    Example
      dx*x
      matrix{{dx}} * matrix{{x}}
    Text
      All Gröbner basis and related computations work over this ring. For an
      extensive collection of D-module routines (A D-module is a module over a
      Weyl algebra), see @TO "Dmodules::Dmodules"@.

      The function @TT "isWeylAlgebra"@ can be used to determine whether a
      polynomial ring has been constructed as a Weyl algebra.
    Example
      isWeylAlgebra R
      S = QQ[x,y]
      isWeylAlgebra S
///

doc ///
  Key
    "associative algebras"
  Description
    Text
      Associative (i.e., not necessarily commutative) algebras are implemented
      in the @TO "AssociativeAlgebras::AssociativeAlgebras"@ package.
///

doc ///
  Key
    "local rings"
  Description
    Text
      Localizations of polynomial rings with respect to prime ideals are
      implemented in the @TO "LocalRings::LocalRings"@ package.
  Subnodes
    :alternative ways to construct a local ring
    (symbol SPACE, Ring, List)
///

doc ///
  Key
    "Schur rings"
  Description
    Text
      Representation rings of general linear groups and of symmetric groups are
      implemented in the @TO "SchurRings::SchurRings"@ package.
///

-*
     "An element of the coefficient ring can be promoted to the polynomial ring.",
     EXAMPLE "promote(11/2,R)",
     "Conversely, an element of the polynomial ring that is known to be a scalar
     can be lifted back to the coefficient ring.",
     EXAMPLE {
	  "sc = (a-2)^2-a^2+4*a",
      	  "lift(sc,QQ)",
	  },
     "In programs, the function ", TO "liftable", " can be used to see whether
     this is possible.",
     EXAMPLE {
	  "liftable(sc,QQ)",
      	  "liftable(c^3,QQ)",
	  },

     "The Hilbert series of a polynomial ring can be obtained.  Its power
     series expansion is the generating function for the dimensions of the
     degree ", TT "n", " parts.",
     EXAMPLE "hilbertSeries R",
     "We may use the option ", TO "Degrees", " to produce rings where the
     generators have degrees other than 1.",
     EXAMPLE {
	  "S = ZZ/101[a,b,c,d,Degrees=>{1,2,3,4}]",
      	  "random(5,S)",
      	  "hilbertSeries S"
	  },
     SeeAlso => { "monomial orderings",  "PolynomialRing"}
*-



-------------------
-- GB nodes -------
-------------------

-*
-- Mike wanted this: 
document {
     Key => "Hilbert functions",
     }

document {
     Key => "syzygies",
     }

document {
     Key => "saturation",
     }

document {
     Key => "fibers of a map between varieties",
     }

document {
     Key => "solving systems of polynomial equations",
     }
*-

/// 
Plan for the next node:
-- groebner basis object
-- getting it, 'snapshot'
-- information about a GB computation:
--   verbose
--   summary
-- using the Hilbert function
-- computing only partial gbs
-- tricks which might help
--   change monomial order
--   homogenize
--   compact monomials
--   remove linear equations, and the corresponding variable.
--   computing up to a given degree
///

///
-- document these routines DO THIS
-- schreyerMatrix F -- DO THIS

-*
leadTerm(ZZ,RingElement) := (n,f) -> (leadTerm(n,matrix{{f}}))_(0,0)
  -- leadTerm should call a ggleadterm routine?  DO THIS
*-

-*
installGroebner = method()
-- DO THIS
*-

-*
gbRemove = method()
gbRemove Module := (M) -> remove((generators M).cache, {false,0})
gbRemove Ideal := (I) -> remove((generators I).cache, {false,0})
*-
  -- PROBLEM: what about the other GB
  
R = QQ[a..d,Weights=>{-1,0,0,0},Global=>false]
f = a+b^2+c^3-2*d^4+1+a*b*c*d
leadTerm f
leadCoefficient f
leadTerm(1,f)

M = image vars R
gbSnapshot(M)
gb(M,PairLimit=>2)
m1 = gbSnapshot(M)
m1s = toString m1
gb(M,PairLimit=>4)
m1  -- This has changed!  We probably don't want that
assert( toString m1 == m1s )				    -- it seems okay, now [dan]

///

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
