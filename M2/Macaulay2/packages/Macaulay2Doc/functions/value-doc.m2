undocumented {(value, RingElement), (value, Nothing), (value, IndexedVariableTable)}

doc ///
  Key
    value
  Headline
    evaluate
///

doc ///
  Key
    (value, Expression)
  Description
    Text
      Expressions can be evaluated using @TO "value"@.
    Example
      QQ[x];
      f = (x+1)^5
      e = expression f
      value e
      value e == f
      c = factor f
      value c
///

doc ///
  Key
    (value, Symbol)
  Headline
    retrieve the value of a symbol
  Usage
    value s
  Inputs
    s:
  Outputs
    :
      the value of @TT "s"@
  Description
    Example
      x = s
      s = 11111111111
      x
      value x
///

doc ///
  Key
    (value, IndexedVariable)
  Headline
    retrieve the value of an indexed variable
  Usage
    value s
  Inputs
    s:
  Outputs
    :
      the value of @TT "s"@
  Description
    Example
      y = x_3
      x_3 = 4
      x_3
      y
      value y
///

doc ///
  Key
    (value,String)
    "currentString"
  Headline
    evaluate a string
  Usage
    value s
  Inputs
    s:
  Outputs
    :
      the value obtained by evaluating the code in @TT "s"@
  Description
    Text
      The contents of @TT "s"@ are treated as code in the Macaulay2 language,
      parsed it in its own scope (the same way a file is) and evaluated. The
      string may contain multiple lines.
    Example
      value "2 + 2"
      value "a := 33
                  a+a"
      a
    Text
      Since the local assignment to @TT "a"@ above occurred in a new scope, the
      value of the global variable @TT "a"@ is unaffected.

      During evaluation of the string, error messages will refer to the
      location of the error as @TT "currentString"@ with a line number and a
      column number, and the value of the variable @TO "currentString"@ is set
      to the string, to aid in debugging.
    Example
      debuggingMode = stopIfError = false;
      value "1/0"
      debuggingMode = true;
      value "1/0"
      break
  SeeAlso
    "debugging"
    "stopIfError"
    "debuggingMode"
///
