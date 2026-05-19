--------------------------
-- relational operators --
--------------------------

doc ///
  Key
    (symbol ==, RObject, RObject)
    (symbol ==, RObject, Thing)
    (symbol ==, Thing, RObject)
  Headline
    equality of R objects
  Usage
    x == y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :Boolean
  Description
    Text
      Test whether two R objects are equal.
    Example
      x = RObject 5
      y = RObject 2
      x == y
      x == x
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x == 5
      5 == y
    Text
      Note that these operators return Macaulay2 @TO Boolean@ objects
      and not R logical vectors.  In particular, they are not useful
      for comparing vectors of length greater than 1.  To do this, you
      may use @TO RFunction@ to get the corresponding R operator as a
      function.
    Example
      (RFunction "==")({1, 2, 3}, {2, 2, 2})
  SeeAlso
    (symbol ?, RObject, RObject)
///

doc ///
  Key
    (symbol ?, RObject, RObject)
    (symbol ?, RObject, Thing)
    (symbol ?, Thing, RObject)
  Headline
    comparison of R objects
  Usage
    x < y
    x > y
    x <= y
    x >= y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :Boolean
  Description
    Text
      The standard comparison operators work on R objects.
    Example
      x = RObject 5
      y = RObject 2
      x > y
      x < y
      x >= y
      x <= y
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      x > 2
      5 < y
    Text
      Note that these operators return Macaulay2 @TO Boolean@ objects
      and not R logical vectors.  In particular, they are not useful
      for comparing vectors of length greater than 1.  To do this, you
      may use @TO RFunction@ to get the corresponding R operator as a
      function.
    Example
      (RFunction ">=")({1, 2, 3}, {2, 2, 2})
    Text
      Internally, these operators are implemented via Macaulay2's
      @TO symbol ?@ method, which returns @CODE "<"@, @CODE ">"@,
      @CODE "=="@, or @TO symbol incomparable@.
  SeeAlso
    (symbol ==, RObject, RObject)
///
