-----------------------
-- logical operators --
-----------------------

doc ///
  Key
    (symbol not, RObject)
  Headline
    logical negation of an R object
  Usage
    not x
  Inputs
    x:RObject
  Outputs
    :RObject -- the element-wise logical negation of @VAR "x"@
  Description
    Text
      Compute the logical negation of an R logical vector.
    Example
      not RObject true
    Text
      When the input contains multiple elements, the output will also
      contain multiple elements.
    Example
      not RObject {true, false, true, false}
  SeeAlso
    (symbol and, RObject, RObject)
    (symbol or, RObject, RObject)
    (symbol xor, RObject, RObject)
///

doc ///
  Key
    (symbol and, RObject, RObject)
    (symbol and, RObject, Thing)
    (symbol and, Thing, RObject)
  Headline
    logical conjunction of R objects
  Usage
    x and y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the element-wise logical conjunction of @VAR "x"@ and @VAR "y"@
  Description
    Text
      Compute the element-wise logical conjunction of two R logical vectors,
      calling R's @SAMP "&"@.
    Example
      RObject true and RObject false
    Text
      When the inputs contain multiple elements, the output will also
      contain multiple elements.
    Example
      RObject {true, true, false, false} and RObject {true, false, true, false}
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      RObject {true, true, false, false} and {true, false, true, false}
  SeeAlso
    (symbol not, RObject)
    (symbol or, RObject, RObject)
    (symbol xor, RObject, RObject)
    (symbol &, RObject, RObject)
///

doc ///
  Key
    (symbol or, RObject, RObject)
    (symbol or, RObject, Thing)
    (symbol or, Thing, RObject)
  Headline
    logical disjunction of R objects
  Usage
    x or y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the element-wise logical disjunction of @VAR "x"@ and @VAR "y"@
  Description
    Text
      Compute the element-wise logical disjunction of two R logical vectors,
      calling R's @SAMP "|"@.
    Example
      RObject true or RObject false
    Text
      When the inputs contain multiple elements, the output will also
      contain multiple elements.
    Example
      RObject {true, true, false, false} or RObject {true, false, true, false}
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      {true, true, false, false} or RObject {true, false, true, false}
  SeeAlso
    (symbol not, RObject)
    (symbol and, RObject, RObject)
    (symbol xor, RObject, RObject)
    (symbol |, RObject, RObject)
///

doc ///
  Key
    (symbol xor, RObject, RObject)
    (symbol xor, RObject, Thing)
    (symbol xor, Thing, RObject)
  Headline
    logical exclusive disjunction of R objects
  Usage
    x xor y
  Inputs
    x:RObject
    y:RObject
  Outputs
    :RObject -- the element-wise exclusive disjunction of @VAR "x"@ and @VAR "y"@
  Description
    Text
      Compute the element-wise exclusive disjunction of two R logical vectors,
      calling R's @SAMP "xor"@.
    Example
      RObject true xor RObject false
      RObject true xor RObject true
    Text
      When the inputs contain multiple elements, the output will also
      contain multiple elements.
    Example
      RObject {true, true, false, false} xor RObject {true, false, true, false}
    Text
      One of the operands may be a Macaulay2 object.  It will be converted
      to an @TO RObject@ before the operation is performed.
    Example
      RObject {true, true, false, false} xor {true, false, true, false}
  SeeAlso
    (symbol not, RObject)
    (symbol and, RObject, RObject)
    (symbol or, RObject, RObject)
    (symbol ^^, RObject, RObject)
///
