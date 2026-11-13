doc ///
  Key
    (value, RObject)
  Headline
    convert R object to Macaulay2
  Usage
    value x
  Inputs
    x:RObject
  Outputs
    :Thing -- the Macaulay2 equivalent of @VAR "x"@
  Description
    Text
      If possible, the Macaulay2 equivalent of the given @TO RObject@
      is returned.

      Note that most R objects are vectors.  When a vector has length
      1, the corresponding Macaulay2 object is returned as a scalar.

      When the input is R's @SAMP "NULL"@, @TO null@ is returned.
    Example
      x = RObject null
      value x === null
    Text
      When the input is a logical vector, a @TO Boolean@ is returned.
    Example
      x = RObject true
      value x
    Text
      When the input is an integer vector, a @TO ZZ@ object is returned.
    Example
      x = RObject 5
      value x
    Text
      When the input is a double vector, a @TO RR@ object is returned.
    Example
      x = RObject (3/2)
      value x
    Text
      When the input is a complex vector, a @TO CC@ object is returned.
    Example
      x = RObject ii
      value x
    Text
      When the input is a character vector, a @TO String@ object is returned.
    Example
      x = RObject foo
      value x
    Text
      When the input is a vector with more than one element, a @TO List@
      object is returned.
    Example
      x = RObject {1, 3, 5, 7}
      value x
    Text
      When the input is a @EM "pairlist"@ (R's linked list type), a
      @TO Sequence@ is returned.
    Example
      x = RObject (3, 6, 9)
      value x
    Text
      There also exists a @EM "list"@ type in R, created by the function
      @SAMP "list"@, that may contain elements of of any type, much like
      Macaulay2 lists.  When the input is such a list, a @TO List@ object
      is returned.
    Example
      RList = RFunction "list"
      x = RList(2, 4, 6, 8)
      value x
    Text
      When the input is a matrix or array, a nested @TO List@ object
      is returned.  Note that R uses column-major order for
      matrices, unlike Macaulay2, which uses row-major order.  No
      attempt is made to change the order, unlike @TO (NewFromMethod,
      RObject, Matrix)@, which does do this conversion.  For the most part,
      @SAMP "value"@ and @TO RObject@ are inverses of one another, but this
      is an exception.
    Example
      A = random(ZZ^2, ZZ^3)
      x = RObject A
      value x
      A == transpose matrix oo
    Text
      When the input has any names, the elements with names are returned
      as @TO Option@ objects.
    Example
      x = RObject {foo => 1, 2, bar => 3}
      value x
///
