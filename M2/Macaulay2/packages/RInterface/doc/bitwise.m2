-----------------------
-- bitwise operators --
-----------------------

doc ///
  Key
    (symbol ~, RObject)
  Headline
    bitwise NOT of an R object, or create a one-sided formula
  Usage
    ~x
  Inputs
    x:RObject
  Outputs
    :RObject -- the bitwise NOT of @VAR "x"@ (integer or real input),
              or a one-sided R formula @SAMP "~x"@ (all other inputs)
  Description
    Text
      For integer and real R vectors, @TO symbol ~@ computes bitwise NOT,
      calling R's @SAMP "bitwNot"@.
    Example
      x = RObject 12
      ~x
    Text
      For all other types, @TO symbol ~@ creates a one-sided R formula,
      equivalent to R's @SAMP "~x"@.  This is the same operator used for
      two-sided formulas; see @TO (symbol ~, RObject, RObject)@.
    Example
      ~ RQuote "x"
  SeeAlso
    (symbol ~, RObject, RObject)
    (symbol &, RObject, RObject)
    (symbol |, RObject, RObject)
    (symbol ^^, RObject, RObject)
    (symbol not, RObject)
///

doc ///
  Key
    (symbol &, RObject, RObject)
    (symbol &, RObject, Thing)
    (symbol &, Thing, RObject)
  Headline
    bitwise AND of R objects
  Usage
    x & y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the bitwise AND of @VAR "x"@ and @VAR "y"@
  Description
    Text
      Compute the bitwise AND of two R integer vectors,
      calling R's @SAMP "bitwAnd"@.
    Example
      x = RObject 12
      y = RObject 10
      x & y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x & 10
  SeeAlso
    (symbol ~, RObject)
    (symbol |, RObject, RObject)
    (symbol ^^, RObject, RObject)
    (symbol and, RObject, RObject)
///

doc ///
  Key
    (symbol |, RObject, RObject)
    (symbol |, RObject, Thing)
    (symbol |, Thing, RObject)
  Headline
    bitwise OR of R objects
  Usage
    x | y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the bitwise OR of @VAR "x"@ and @VAR "y"@
  Description
    Text
      Compute the bitwise OR of two R integer vectors,
      calling R's @SAMP "bitwOr"@.
    Example
      x = RObject 12
      y = RObject 10
      x | y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      12 | y
  SeeAlso
    (symbol ~, RObject)
    (symbol &, RObject, RObject)
    (symbol ^^, RObject, RObject)
    (symbol or, RObject, RObject)
///

doc ///
  Key
    (symbol ^^, RObject, RObject)
    (symbol ^^, RObject, Thing)
    (symbol ^^, Thing, RObject)
  Headline
    bitwise XOR of R objects
  Usage
    x ^^ y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the bitwise XOR of @VAR "x"@ and @VAR "y"@
  Description
    Text
      Compute the bitwise XOR of two R integer vectors,
      calling R's @SAMP "bitwXor"@.
    Example
      x = RObject 12
      y = RObject 10
      x ^^ y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      12 ^^ y
  SeeAlso
    (symbol ~, RObject)
    (symbol &, RObject, RObject)
    (symbol |, RObject, RObject)
    (symbol xor, RObject, RObject)
///

doc ///
  Key
    (symbol <<, RObject, RObject)
    (symbol <<, RObject, Thing)
    (symbol <<, Thing, RObject)
  Headline
    bitwise left shift of an R object
  Usage
    x << y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- @VAR "x"@ shifted left by @VAR "y"@ bits
  Description
    Text
      Compute the bitwise left shift of an R integer vector,
      calling R's @SAMP "bitwShiftL"@.
    Example
      x = RObject 12
      y = RObject 2
      x << y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x << 2
  SeeAlso
    (symbol >>, RObject, RObject)
///

doc ///
  Key
    (symbol >>, RObject, RObject)
    (symbol >>, RObject, Thing)
    (symbol >>, Thing, RObject)
  Headline
    bitwise right shift of an R object
  Usage
    x >> y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- @VAR "x"@ shifted right by @VAR "y"@ bits
  Description
    Text
      Compute the bitwise right shift of an R integer vector,
      calling R's @SAMP "bitwShiftR"@.
    Example
      x = RObject 12
      y = RObject 2
      x << y
      oo >> y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x >> 2
  SeeAlso
    (symbol <<, RObject, RObject)
///
