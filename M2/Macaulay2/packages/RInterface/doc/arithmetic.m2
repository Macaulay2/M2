--------------------------
-- arithmetic operators --
--------------------------

doc ///
  Key
    (symbol +, RObject, RObject)
    (symbol +, RObject, Thing)
    (symbol +, Thing, RObject)
    (symbol +, RObject)
  Headline
    add R objects
  Usage
    x + y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the sum of x and y
  Description
    Text
      Add two R objects.
    Example
      x = RObject 5
      y = RObject 2
      x + y
    Text
      It may also be used as a unary operator.
    Example
      +x
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x + 2
      5 + y
  SeeAlso
    (symbol -, RObject, RObject)
    (symbol *, RObject, RObject)
///

doc ///
  Key
    (symbol -, RObject, RObject)
    (symbol -, RObject, Thing)
    (symbol -, Thing, RObject)
    (symbol -, RObject)
  Headline
    subtract R objects
  Usage
    x - y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the difference of x and y
  Description
    Text
      Subtract two R objects.
    Example
      x = RObject 5
      y = RObject 2
      x - y
    Text
      It may also be used as a unary operator.
    Example
      -x
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x - 2
      5 - y
  SeeAlso
    (symbol +, RObject, RObject)
    (symbol *, RObject, RObject)
///

doc ///
  Key
    (symbol *, RObject, RObject)
    (symbol *, RObject, Thing)
    (symbol *, Thing, RObject)
  Headline
    multiply R objects
  Usage
    x * y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the product of x and y
  Description
    Text
      Multiply two R objects.
    Example
      x = RObject 5
      y = RObject 2
      x * y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x * 2
      5 * y
  SeeAlso
    (symbol /, RObject, RObject)
    (product, RObject)
///

doc ///
  Key
    (symbol /, RObject, RObject)
    (symbol /, RObject, Thing)
    (symbol /, Thing, RObject)
  Headline
    divide R objects
  Usage
    x / y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the quotient of x and y
  Description
    Text
      Divide two R objects.
    Example
      x = RObject 5
      y = RObject 2
      x / y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x / 2
      5 / y
  SeeAlso
    (symbol *, RObject, RObject)
    (symbol //, RObject, RObject)
    (symbol %, RObject, RObject)
///

doc ///
  Key
    (symbol ^, RObject, RObject)
    (symbol ^, RObject, Thing)
    (symbol ^, Thing, RObject)
  Headline
    exponentiation of R objects
  Usage
    x ^ y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- @VAR "x"@ raised to the @VAR "y"@ power
  Description
    Text
      Raise one R object to the power of another.
    Example
      x = RObject 5
      y = RObject 2
      x^y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x^2
      5^y
  SeeAlso
    (symbol *, RObject, RObject)
    (sqrt, RObject)
    (exp, RObject)
///

doc ///
  Key
    (symbol %, RObject, RObject)
    (symbol %, RObject, Thing)
    (symbol %, Thing, RObject)
  Headline
    modulo of R objects
  Usage
    x % y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the remainder when @VAR "x"@ is divided by @VAR "y"@
  Description
    Text
      Compute the modulo of two R objects, wrapping R's @SAMP "%%"@.
    Example
      x = RObject 5
      y = RObject 2
      x % y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x % 2
      5 % y
  SeeAlso
    (symbol //, RObject, RObject)
    (symbol /, RObject, RObject)
///

doc ///
  Key
    (symbol //, RObject, RObject)
    (symbol //, RObject, Thing)
    (symbol //, Thing, RObject)
  Headline
    floor division of R objects
  Usage
    x // y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the floor quotient of @VAR "x"@ divided by @VAR "y"@
  Description
    Text
      Perform floor division (integer division) of two R objects, wrapping
      R's @SAMP "%/%"@.
    Example
      x = RObject 5
      y = RObject 2
      x // y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x // 2
      5 // y
  SeeAlso
    (symbol %, RObject, RObject)
    (symbol /, RObject, RObject)
///

doc ///
  Key
    (symbol :, RObject, RObject)
    (symbol :, RObject, Thing)
    (symbol :, Thing, RObject)
    (symbol .., RObject, RObject)
    (symbol .., RObject, Thing)
    (symbol .., Thing, RObject)
  Headline
    R colon operator
  Usage
    x : y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the sequence of integers from @VAR "x"@ to @VAR "y"@
  Description
    Text
      Generate the sequence between two numbers with step size 1 (or
      -1 if the first number is greater than the second).  This
      operator is available both as @SAMP ":"@, as in R, or @SAMP ".."@,
      its Macaulay2 equivalent.
    Example
      x = RObject 2
      y = RObject 7
      x:y
      y:x
      x..y
    Text
      One of the operands may be a Macaulay2 object.  It will be
      converted to an @TO RObject@ before the operation is performed.
      An exception is that when using the @SAMP ":"@ operator and the
      first argument given is a Macaulay2 @TO ZZ@ object,
      @TO (symbol :, ZZ, Thing)@ will be called instead.
    Example
      x:7
      2:y
      2..y
  SeeAlso
    (symbol +, RObject, RObject)
///
