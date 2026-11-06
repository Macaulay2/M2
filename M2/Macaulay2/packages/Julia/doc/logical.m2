doc ///
  Key
    (symbol not, JuliaObject)
  Headline
    logical negation of a Julia object
  Usage
    not x
  Inputs
    x:JuliaObject -- a Julia @SAMP "Bool"@
  Outputs
    :JuliaObject -- the negation of @VAR "x"@
  Description
    Text
      This calls Julia's @SAMP "!"@ function.
    Example
      not JuliaObject true
  SeeAlso
    (symbol and, JuliaObject, JuliaObject)
    (symbol or, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol and, JuliaObject, JuliaObject)
    (symbol and, JuliaObject, Thing)
    (symbol and, Boolean, JuliaObject)
  Headline
    logical conjunction of Julia objects
  Usage
    x and y
  Inputs
    x:JuliaObject -- a Julia @SAMP "Bool"@
    y:JuliaObject -- a Julia @SAMP "Bool"@
  Outputs
    :Boolean
  Description
    Text
      The operands are converted to Macaulay2 booleans, and the result is a
      Macaulay2 @TO Boolean@.  One of the operands may be a Macaulay2
      boolean.
    Example
      JuliaObject true and JuliaObject false
      JuliaObject true and true
      true and JuliaObject true
    Text
      If the first operand is false, then the result is false, whatever
      the second operand is.  Note that the second operand is still
      evaluated.
    Example
      JuliaObject false and 5
  SeeAlso
    (symbol or, JuliaObject, JuliaObject)
    (symbol not, JuliaObject)
///

doc ///
  Key
    (symbol or, JuliaObject, JuliaObject)
    (symbol or, JuliaObject, Thing)
    (symbol or, Boolean, JuliaObject)
  Headline
    logical disjunction of Julia objects
  Usage
    x or y
  Inputs
    x:JuliaObject -- a Julia @SAMP "Bool"@
    y:JuliaObject -- a Julia @SAMP "Bool"@
  Outputs
    :Boolean
  Description
    Text
      The operands are converted to Macaulay2 booleans, and the result is a
      Macaulay2 @TO Boolean@.  One of the operands may be a Macaulay2
      boolean.
    Example
      JuliaObject false or JuliaObject true
      JuliaObject false or false
      false or JuliaObject true
    Text
      If the first operand is true, then the result is true, whatever the
      second operand is.  Note that the second operand is still evaluated.
    Example
      JuliaObject true or 5
  SeeAlso
    (symbol and, JuliaObject, JuliaObject)
    (symbol not, JuliaObject)
///
