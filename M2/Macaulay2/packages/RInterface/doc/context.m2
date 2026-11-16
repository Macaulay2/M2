doc ///
  Key
    RContext
    (NewMethod, RContext)
  Headline
    R context
  Description
    Text
      An @SAMP "RContext"@ is a wrapper around an R environment that keeps
      track of variable bindings across a series of R computations.

      Create an empty context with @SAMP "new RContext"@:
    Example
      ctx = new RContext
    Text
      Alternatively, pass an R code string to initialize the context with
      variable bindings.  See @TO (NewFromMethod, RContext, String)@.
  Subnodes
    (NewFromMethod, RContext, String)
    (symbol SPACE, RContext, String)
    (symbol _, RContext, String)
    (listSymbols, RContext)
    (use, RContext)
  SeeAlso
    RValue
///

doc ///
  Key
    (NewFromMethod, RContext, String)
  Headline
    construct an R context
  Usage
    RContext s
  Inputs
    s:String -- R code to initialize the context with
  Outputs
    :RContext
  Description
    Text
      Pass an R code string to initialize the context with
      variable bindings:
    Example
      ctx = RContext "x <- 5L; y <- x^2L"
      ctx_"x"
      ctx_"y"
///

doc ///
  Key
    (symbol SPACE, RContext, String)
  Headline
    evaluate R code in a context
  Usage
    ctx s
  Inputs
    ctx:RContext
    s:String -- R code to evaluate
  Outputs
    :RObject -- result of evaluating @VAR "s"@
  Description
    Text
      Evaluates R code in the R environment associated with the context.
      Variable assignments persist across calls.
    Example
      ctx = new RContext
      ctx "x <- 5L"
      ctx "y <- x + 1L"
      ctx "y"
///

doc ///
  Key
    (symbol _, RContext, String)
  Headline
    get a variable from an R context
  Usage
    ctx_s
  Inputs
    ctx:RContext
    s:String -- variable name
  Outputs
    :RObject -- the value of variable @VAR "s"@ in the context
  Description
    Text
      Returns the value of the named variable from the R environment
      associated with the context.
    Example
      ctx = RContext "x <- 42L"
      ctx_"x"
///

doc ///
  Key
    (listSymbols, RObject)
    (listSymbols, RContext)
  Headline
    list symbols in an R environment
  Usage
    listSymbols x
  Inputs
    x:{RObject, RContext} -- an R environment (type @SAMP "environment"@) or context
  Outputs
    :TABLE -- an HTML table showing each symbol's name, R type, and value
  Description
    Text
      Returns a table of the symbols defined in an R environment.  When given an
      @TO RContext@, the symbols in its associated environment are listed.
    Example
      ctx = RContext "x <- 5L; y <- \"hello\""
      listSymbols ctx
  SeeAlso
    (use, RContext)
///

doc ///
  Key
    (use, RContext)
  Headline
    import R context variables into Macaulay2
  Usage
    use ctx
  Inputs
    ctx:RContext
  Consequences
    Item
      Variables in @VAR "ctx"@ whose names do not contain @SAMP "_"@ or
      @SAMP "."@ are assigned to Macaulay2 symbols of the same name
  Description
    Text
      Imports the variables from a context's R environment into Macaulay2's
      global symbol table.
    Example
      ctx = RContext "a <- 5L; b <- 10L"
      use ctx
      a
      b
  SeeAlso
    (listSymbols, RContext)
///
