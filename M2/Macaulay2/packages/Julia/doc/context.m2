doc ///
  Key
    JuliaContext
    (NewMethod, JuliaContext)
  Headline
    a Julia context
  Usage
    new JuliaContext
  Outputs
    :JuliaContext
  Description
    Text
      A @SAMP "JuliaContext"@ is a wrapper around an anonymous Julia module,
      in which Julia code may be evaluated without affecting Julia's
      @SAMP "Main"@ module or other contexts.
    Example
      ctx = new JuliaContext
      ctx "x = 5"
      ctx "x + 1"
    Text
      The underlying module is stored under the key @SAMP "Global"@, and it
      may be passed to the @TO [juliaValue, Global]@ option.
    Example
      juliaValue("x^2", Global => ctx.Global)
  Subnodes
    (NewFromMethod, JuliaContext, String)
    (symbol SPACE, JuliaContext, String)
    (symbol _, JuliaContext, String)
    (listSymbols, JuliaContext)
    (use, JuliaContext)
  SeeAlso
    juliaValue
///

doc ///
  Key
    (NewFromMethod, JuliaContext, String)
  Headline
    create a Julia context from Julia code
  Usage
    JuliaContext s
  Inputs
    s:String -- Julia code
  Outputs
    :JuliaContext
  Description
    Text
      Create a new context and evaluate @VAR "s"@ in it.
    Example
      ctx = JuliaContext "x = 5; f(y) = 2y"
      ctx "f(x)"
///

doc ///
  Key
    (symbol SPACE, JuliaContext, String)
  Headline
    evaluate Julia code in a context
  Usage
    ctx s
  Inputs
    ctx:JuliaContext
    s:String -- Julia code
  Outputs
    :JuliaObject -- the result of evaluating @VAR "s"@
  Description
    Text
      Evaluate Julia code in the module of the context.  Definitions
      persist from one call to the next.
    Example
      ctx = new JuliaContext
      ctx "x = 5"
      ctx "y = x + 1"
      ctx "x * y"
///

doc ///
  Key
    (symbol _, JuliaContext, String)
  Headline
    get a variable from a Julia context
  Usage
    ctx_s
  Inputs
    ctx:JuliaContext
    s:String -- the name of a variable
  Outputs
    :JuliaObject -- the value of @VAR "s"@ in the context
  Description
    Example
      ctx = JuliaContext "x = 42"
      ctx_"x"
///

doc ///
  Key
    (listSymbols, JuliaContext)
    (listSymbols, JuliaObject)
  Headline
    list the variables in a Julia context or module
  Usage
    listSymbols x
  Inputs
    x:{JuliaContext,JuliaObject} -- a context or Julia module
  Outputs
    :TABLE -- with the name, type, and value of each variable
  Description
    Text
      List the variables defined in a Julia context or module.
    Example
      ctx = JuliaContext "x = 5; y = \"foo\""
      listSymbols ctx
  SeeAlso
    (use, JuliaContext)
///

doc ///
  Key
    (use, JuliaContext)
  Headline
    import the variables of a Julia context into Macaulay2
  Usage
    use ctx
  Inputs
    ctx:JuliaContext
  Consequences
    Item
      each variable in @VAR "ctx"@ is assigned to the Macaulay2 symbol with
      the same name
  Description
    Text
      Variables whose names are not valid Macaulay2 symbols are skipped.
    Example
      ctx = JuliaContext "a = 5; b = 10"
      use ctx
      a
      b
  SeeAlso
    (listSymbols, JuliaContext)
///
