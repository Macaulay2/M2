------------------------------------
-- maxima, minima, sums, products --
------------------------------------

doc ///
  Key
    (max, RObject)
  Headline
    maximum of an R object
  Usage
    max x
  Inputs
    x:RObject
  Outputs
    :RObject -- the maximum value in @VAR "x"@
  Description
    Text
      Return the maximum value of an R vector.
    Example
      x = RObject {1, 3, 5}
      max x
  SeeAlso
    (min, RObject)
///

doc ///
  Key
    (min, RObject)
  Headline
    minimum of an R object
  Usage
    min x
  Inputs
    x:RObject
  Outputs
    :RObject -- the minimum value in @VAR "x"@
  Description
    Text
      Return the minimum value of an R vector.
    Example
      x = RObject {1, 3, 5}
      min x
  SeeAlso
    (max, RObject)
///

doc ///
  Key
    (sum, RObject)
  Headline
    sum of elements of an R object
  Usage
    sum x
  Inputs
    x:RObject
  Outputs
    :RObject -- the sum of all values in @VAR "x"@
  Description
    Text
      Return the sum of all values in an R vector.
      This wraps R's @SAMP "sum"@ function.
    Example
      x = RObject {2, 4, 6}
      sum x
  SeeAlso
    (product, RObject)
    (max, RObject)
    (min, RObject)
///

doc ///
  Key
    (product, RObject)
  Headline
    product of elements of an R object
  Usage
    product x
  Inputs
    x:RObject
  Outputs
    :RObject -- the product of all values in @VAR "x"@
  Description
    Text
      Return the product of all values in an R vector.
      This wraps R's @SAMP "prod"@ function.
    Example
      x = RObject {2, 4, 6}
      product x
  SeeAlso
    (sum, RObject)
    (symbol *, RObject, RObject)
///
