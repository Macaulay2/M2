doc ///
  Key
    JuliaFunction
    (NewFromMethod, JuliaFunction, String)
    (NewFromMethod, JuliaFunction, Function)
    (NewFromMethod, JuliaFunction, Symbol)
    (NewFromMethod, JuliaFunction, JuliaObject, String)
    (NewFromMethod, JuliaFunction, JuliaObject, Function)
    (NewFromMethod, JuliaFunction, JuliaObject, Symbol)
    (NewFromMethod, JuliaFunction, JuliaObject)
  Headline
    a Macaulay2 function that calls a Julia function
  Usage
    JuliaFunction s
    JuliaFunction(M, s)
    JuliaFunction f
  Inputs
    s:{String,Function,Symbol} -- the name of a Julia function
    M:JuliaObject -- a Julia module (default @SAMP "Main"@)
    f:JuliaObject -- a Julia function
  Outputs
    :JuliaFunction
  Description
    Text
      A @SAMP "JuliaFunction"@ is a Macaulay2 @TO Function@ that calls a
      Julia function.  Its arguments are converted to Julia objects, and
      it returns a @TO JuliaObject@.
    Example
      f = JuliaFunction "sqrt"
      f 2
    Text
      The name of a Macaulay2 function or symbol may be given in place of
      a string.
    Example
      (JuliaFunction sqrt) 2
      (JuliaFunction symbol +)(1, 2)
    Text
      By default, the function is looked up in Julia's @SAMP "Main"@
      module.  Pass a module as the first argument to use a different one.
    Example
      BaseModule = juliaGetGlobal "Base"
      JuliaFunction(BaseModule, "sqrt")
    Text
      A Julia object that is callable, such as an anonymous function, may
      also be passed directly.
    Example
      g = JuliaFunction juliaValue "(x, y) -> x * y"
      g(3, 4)
    Text
      See @TO (symbol SPACE, JuliaObject, Thing)@ for details about how
      arguments are passed.
  Subnodes
    (toString, JuliaFunction)
  SeeAlso
    JuliaObject
    (symbol SPACE, JuliaObject, Thing)
///

doc ///
  Key
    (symbol SPACE, JuliaObject, Thing)
  Headline
    call a Julia object
  Usage
    f x
    f(x1, x2, ...)
  Inputs
    f:JuliaObject -- a callable Julia object
    x:Thing
  Outputs
    :JuliaObject -- the result of calling @VAR "f"@
  Description
    Text
      Call a Julia function, or any other callable Julia object.  The
      arguments are converted to Julia objects first.
    Example
      f = juliaValue "(x, y) -> x * y"
      f(3, 4)
    Text
      A @TO Sequence@ is treated as a list of arguments.  To pass a Julia
      tuple as a single argument, convert it with @TO JuliaObject@ first.
    Example
      typeOf = juliaGetGlobal "typeof"
      typeOf JuliaObject (1, 2)
    Text
      Options are passed as keyword arguments.  Their keys may be symbols
      or strings.
    Example
      jlRound = juliaGetGlobal "round"
      jlRound(pi, digits => 2)
      jlRound(pi, "digits" => 3)
    Text
      @TO JuliaFunction@ objects take arguments in the same way.
  SeeAlso
    JuliaFunction
///

doc ///
  Key
    (toString, JuliaFunction)
    (net, JuliaFunction)
  Headline
    display a Julia function
  Usage
    toString f
    net f
  Inputs
    f:JuliaFunction
  Outputs
    :String -- the name of the Julia function
  Description
    Example
      f = JuliaFunction "sqrt"
      toString f
///
