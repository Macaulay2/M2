-------------
-- rounding --
-------------

doc ///
  Key
    (ceiling, RObject)
  Headline
    ceiling of an R object
  Usage
    ceiling x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise ceiling of @VAR "x"@
  Description
    Text
      Round each element of @VAR "x"@ up to the nearest integer,
      calling R's @SAMP "ceiling"@.
    Example
      x = RObject exp 1
      ceiling x
  SeeAlso
    (floor, RObject)
    (truncate, RObject)
    (round, RObject)
///

doc ///
  Key
    (floor, RObject)
  Headline
    floor of an R object
  Usage
    floor x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise floor of @VAR "x"@
  Description
    Text
      Round each element of @VAR "x"@ down to the nearest integer,
      calling R's @SAMP "floor"@.
    Example
      x = RObject exp 1
      floor x
  SeeAlso
    (ceiling, RObject)
    (truncate, RObject)
    (round, RObject)
///

doc ///
  Key
    (truncate, RObject)
  Headline
    truncation of an R object
  Usage
    truncate x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise truncation of @VAR "x"@ toward zero
  Description
    Text
      Round each element of @VAR "x"@ toward zero,
      calling R's @SAMP "trunc"@.
    Example
      x = RObject exp 1
      truncate x
      truncate(-x)
  SeeAlso
    (ceiling, RObject)
    (floor, RObject)
    (round, RObject)
///

doc ///
  Key
    (round, RObject, RObject)
    (round, RObject, Thing)
    (round, Thing, RObject)
    (round, RObject)
  Headline
    rounding of an R object
  Usage
    round x
    round(x, n)
  Inputs
    x:RObject
    n:RObject -- number of decimal places (default 0)
  Description
    Text
      Round each element of @VAR "x"@ to the nearest integer,
      calling R's @SAMP "round"@, which rounds to even (banker's rounding),
      just as in Macaulay2.
    Example
      round RObject 2.5
      round RObject 3.5
    Text
      An optional second argument, or equivalently a @SAMP "digits"@ option,
      specifies the number of decimal places to use when rounding.
    Example
      x = RObject exp 1
      round(x, 2)
      round(x, digits => 3)
  SeeAlso
    (ceiling, RObject)
    (floor, RObject)
    (truncate, RObject)
///
