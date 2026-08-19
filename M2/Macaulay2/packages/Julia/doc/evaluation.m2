doc ///
  Key
    juliaValue
    (juliaValue, String)
    (juliaValue, Sequence)
    [juliaValue, Global]
  Headline
    evaluate Julia code
  Usage
    juliaValue s
  Inputs
    s:{String,Sequence} -- Julia code
    Global => JuliaObject -- a Julia module in which to evaluate the code
  Outputs
    :JuliaObject -- the result of evaluating the code
  Description
    Text
      Evaluate a string of Julia code and return the result.
    Example
      juliaValue "1 + 1"
      juliaValue "[1, 2, 3] .^ 2"
    Text
      When the input is a sequence, its elements are converted to strings
      and concatenated first.
    Example
      n = 5
      juliaValue("sum(1:", n, ")")
    Text
      By default, the code is evaluated in Julia's @SAMP "Main"@ module, so
      definitions persist from one call to the next.
    Example
      juliaValue "double(x) = 2x"
      juliaValue "double(21)"
    Text
      Use the @SAMP "Global"@ option to evaluate the code in a different
      module instead.
    Example
      M = juliaValue "Module()"
      juliaValue("y = 5", Global => M)
      juliaValue("y + 1", Global => M)
    Text
      If Julia throws an exception, then a Macaulay2 error is raised with
      Julia's error message.
  SeeAlso
    JuliaContext
    juliaGetGlobal
///

doc ///
  Key
    juliaGetGlobal
    (juliaGetGlobal, String)
    (juliaGetGlobal, Thing)
    (juliaGetGlobal, JuliaObject, String)
    (juliaGetGlobal, JuliaObject, Thing)
  Headline
    get a Julia global variable
  Usage
    juliaGetGlobal s
    juliaGetGlobal(M, s)
  Inputs
    M:JuliaObject -- a Julia module (default @SAMP "Main"@)
    s:{String,Thing} -- the name of the variable
  Outputs
    :JuliaObject -- the value of the global variable @VAR "s"@ in @VAR "M"@
  Description
    Text
      Get the value of a global variable in a Julia module.  If no module
      is given, then @SAMP "Main"@ is used.
    Example
      juliaValue "answer = 42"
      juliaGetGlobal "answer"
      BaseModule = juliaGetGlobal "Base"
      juliaGetGlobal(BaseModule, "pi")
    Text
      If @VAR "s"@ is not a string, then it is converted to one first.  In
      particular, the names of Macaulay2 symbols and functions may be used.
    Example
      juliaGetGlobal(BaseModule, sqrt)
  SeeAlso
    juliaValue
    (symbol @@, JuliaObject, Thing)
///

doc ///
  Key
    juliaSymbol
    (juliaSymbol, String)
    (juliaSymbol, Thing)
  Headline
    create a Julia symbol
  Usage
    juliaSymbol s
  Inputs
    s:{String,Thing}
  Outputs
    :JuliaObject -- a Julia @SAMP "Symbol"@
  Description
    Text
      Create a Julia @SAMP "Symbol"@ with the given name.  If @VAR "s"@ is
      not a string, then it is converted to one first.
    Example
      juliaSymbol "foo"
      juliaSymbol bar
    Text
      Julia symbols are converted to Macaulay2 strings by
      @TO (value, JuliaObject)@.
    Example
      value oo
///

doc ///
  Key
    using
    (using, String)
  Headline
    load a Julia package
  Usage
    using s
  Inputs
    s:String -- the name of a Julia package
  Outputs
    :JuliaObject -- the package's module
  Description
    Text
      Load a Julia package, as with Julia's @SAMP "using"@ statement, and
      return its module.  The names that the package exports become
      available in @SAMP "Main"@.
    Example
      LA = using "LinearAlgebra"
      (JuliaFunction(LA, "det")) matrix {{1, 2}, {3, 4}}
      (JuliaFunction "tr") matrix {{1, 2}, {3, 4}}
    Text
      The package must already be installed in the Julia environment.
  SeeAlso
    juliaGetGlobal
///

doc ///
  Key
    (help#0, JuliaObject)
  Headline
    view the documentation of a Julia object
  Usage
    help x
  Inputs
    x:JuliaObject
  Outputs
    :Net -- the Julia documentation for @VAR "x"@
  Description
    Text
      Display the docstring of a Julia object, as given by Julia's
      @SAMP "?"@ help mode.
    Example
      help juliaGetGlobal "isodd"
///
