doc ///
  Key
    (symbol +, JuliaObject, JuliaObject)
    (symbol +, JuliaObject, Thing)
    (symbol +, Thing, JuliaObject)
    (symbol +, JuliaObject)
  Headline
    add Julia objects
  Usage
    x + y
    +x
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the sum of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "+"@ function.  It may also be used as a
      unary operator.
    Example
      x = JuliaObject 5
      y = JuliaObject 2
      x + y
      +x
    Text
      One of the operands may be a Macaulay2 object.  It is converted to a
      @TO JuliaObject@ first.
    Example
      x + 2
      5 + y
  SeeAlso
    (symbol -, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol -, JuliaObject, JuliaObject)
    (symbol -, JuliaObject, Thing)
    (symbol -, Thing, JuliaObject)
    (symbol -, JuliaObject)
  Headline
    subtract Julia objects
  Usage
    x - y
    -x
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the difference of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "-"@ function.  It may also be used as a
      unary operator.
    Example
      x = JuliaObject 5
      y = JuliaObject 2
      x - y
      -x
    Text
      One of the operands may be a Macaulay2 object.  It is converted to a
      @TO JuliaObject@ first.
    Example
      x - 2
      5 - y
  SeeAlso
    (symbol +, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol *, JuliaObject, JuliaObject)
    (symbol *, JuliaObject, Thing)
    (symbol *, Thing, JuliaObject)
  Headline
    multiply Julia objects
  Usage
    x * y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the product of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "*"@ function.  One of the operands may be a
      Macaulay2 object.  It is converted to a @TO JuliaObject@ first.
    Example
      x = JuliaObject 5
      y = JuliaObject 2
      x * y
      x * 2
      5 * y
    Text
      Note that in Julia, @SAMP "*"@ also concatenates strings.
    Example
      JuliaObject "foo" * "bar"
  SeeAlso
    (symbol /, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol /, JuliaObject, JuliaObject)
    (symbol /, JuliaObject, Thing)
    (symbol /, Thing, JuliaObject)
  Headline
    divide Julia objects
  Usage
    x / y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the quotient of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "/"@ function, which returns a
      floating-point number when given two integers.  One of the operands
      may be a Macaulay2 object.  It is converted to a @TO JuliaObject@
      first.
    Example
      x = JuliaObject 5
      y = JuliaObject 2
      x / y
      x / 2
      5 / y
  SeeAlso
    (symbol \, JuliaObject, JuliaObject)
    (symbol //, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol \, JuliaObject, JuliaObject)
    (symbol \, JuliaObject, Thing)
    (symbol \, Thing, JuliaObject)
  Headline
    left division of Julia objects
  Usage
    x \ y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the quotient of @VAR "y"@ and @VAR "x"@
  Description
    Text
      This calls Julia's @SAMP "\\"@ function.  For numbers, @SAMP "x \\ y"@
      is @SAMP "y / x"@.  For matrices, it solves the linear system
      @SAMP "x * z = y"@ for @SAMP "z"@.  One of the operands may be a
      Macaulay2 object.  It is converted to a @TO JuliaObject@ first.
    Example
      x = JuliaObject 2
      y = JuliaObject 5
      x \ y
      x \ 5
      2 \ y
      JuliaObject matrix {{2, 0}, {0, 4}} \ {2, 8}
  SeeAlso
    (symbol /, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol //, JuliaObject, JuliaObject)
    (symbol //, JuliaObject, Thing)
    (symbol //, Thing, JuliaObject)
  Headline
    integer division of Julia objects
  Usage
    x // y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the integer quotient of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "÷"@ (@SAMP "div"@) function.  Note that
      unlike Macaulay2's @TO symbol //@, the quotient is rounded toward
      zero.
    Example
      x = JuliaObject 7
      y = JuliaObject 2
      x // y
      (-x) // y
    Text
      One of the operands may be a Macaulay2 object.  It is converted to a
      @TO JuliaObject@ first.
    Example
      x // 2
      7 // y
  SeeAlso
    (symbol %, JuliaObject, JuliaObject)
    (quotientRemainder, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol %, JuliaObject, JuliaObject)
    (symbol %, JuliaObject, Thing)
    (symbol %, Thing, JuliaObject)
  Headline
    remainder of Julia objects
  Usage
    x % y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the remainder when @VAR "x"@ is divided by @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "%"@ (@SAMP "rem"@) function.  Note that
      unlike Macaulay2's @TO symbol %@, the result has the same sign as
      @VAR "x"@.
    Example
      x = JuliaObject 7
      y = JuliaObject 2
      x % y
      (-x) % y
    Text
      One of the operands may be a Macaulay2 object.  It is converted to a
      @TO JuliaObject@ first.
    Example
      x % 2
      7 % y
  SeeAlso
    (symbol //, JuliaObject, JuliaObject)
    (quotientRemainder, JuliaObject, JuliaObject)
///

doc ///
  Key
    (quotientRemainder, JuliaObject, JuliaObject)
    (quotientRemainder, JuliaObject, Thing)
    (quotientRemainder, Thing, JuliaObject)
  Headline
    quotient and remainder of Julia objects
  Usage
    quotientRemainder(x, y)
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :Sequence -- containing @SAMP "x // y"@ and @SAMP "x % y"@
  Description
    Text
      This calls Julia's @SAMP "divrem"@ function.  One of the operands may
      be a Macaulay2 object.  It is converted to a @TO JuliaObject@ first.
    Example
      quotientRemainder(JuliaObject 7, JuliaObject 2)
      quotientRemainder(JuliaObject(-7), 2)
  SeeAlso
    (symbol //, JuliaObject, JuliaObject)
    (symbol %, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol ^, JuliaObject, JuliaObject)
    (symbol ^, JuliaObject, Thing)
    (symbol ^, Thing, JuliaObject)
  Headline
    exponentiation of Julia objects
  Usage
    x ^ y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- @VAR "x"@ raised to the power @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "^"@ function.  One of the operands may be a
      Macaulay2 object.  It is converted to a @TO JuliaObject@ first.
    Example
      x = JuliaObject 5
      y = JuliaObject 2
      x^y
      x^2
      5^y
    Text
      Note that for fixed-width integers, the result may overflow.
    Example
      (JuliaObject 2)^64
  SeeAlso
    (sqrt, JuliaObject)
    (exp, JuliaObject)
///
