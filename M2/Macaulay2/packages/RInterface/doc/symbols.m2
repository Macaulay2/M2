doc ///
  Key
    RSymbol
    (RSymbol, String)
    (RSymbol, Thing)
  Headline
    look up an R object by name
  Usage
    RSymbol str
  Inputs
    str:{String, Thing}
  Outputs
    :RObject -- the value of the R object named @VAR "str"@
  Description
    Text
      Look up an R object by name, returning its value.  For example,
      this can be useful for loading datasets.
    Example
      RSymbol "mtcars"
    Text
      A Macaulay2 @TO Symbol@ may also be used.
    Example
      RSymbol mtcars
    Text
      To obtain an unevaluated R symbol rather than its value, use
      @TO RQuote@ instead.  For R functions, use @TO RFunction@ instead.
  SeeAlso
    RQuote
    RFunction
///

doc ///
  Key
    RQuote
    (RQuote, String)
    (RQuote, Thing)
  Headline
    create an unevaluated R symbol
  Usage
    RQuote str
  Inputs
    str:{String, Thing}
  Outputs
    :RObject -- an R symbol (type @SAMP "symbol"@)
  Description
    Text
      Create an unevaluated R symbol.  This is analogous to R's
      @SAMP "quote"@ function, and is useful when a symbol itself
      (rather than its value) needs to be passed to an R function.

      Compare the behavior of @TO RSymbol@, which evaluates the symbol
      and returns its value:
    Example
      RSymbol "pi"
      RQuote "pi"
    Text
      A Macaulay2 @TO Symbol@ may also be used.
    Example
      RQuote pi
  SeeAlso
    RSymbol
///
