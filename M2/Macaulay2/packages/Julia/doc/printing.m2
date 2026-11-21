doc ///
  Key
    (toString, JuliaObject)
  Headline
    convert a Julia object to a string
  Usage
    toString x
  Inputs
    x:JuliaObject
  Outputs
    :String
  Description
    Text
      Convert a Julia object to a string using Julia's @SAMP "string"@
      function.
    Example
      toString JuliaObject {1, 2, 3}
      toString JuliaObject "foo"
  SeeAlso
    (net, JuliaObject)
    (toExternalString, JuliaObject)
///

doc ///
  Key
    (net, JuliaObject)
  Headline
    display a Julia object
  Usage
    net x
  Inputs
    x:JuliaObject
  Outputs
    :Net
  Description
    Text
      Julia objects are displayed as they would be in Julia's REPL, using
      the @SAMP "text/plain"@ representation given by Julia's @SAMP "repr"@
      function.
    Example
      net JuliaObject {1, 2, 3}
  SeeAlso
    (toString, JuliaObject)
///

doc ///
  Key
    (toExternalString, JuliaObject)
    (describe, JuliaObject)
  Headline
    convert a Julia object to a string that can be read back in
  Usage
    toExternalString x
    describe x
  Inputs
    x:JuliaObject
  Outputs
    :{String,Describe}
  Description
    Text
      Return Macaulay2 code that recreates @VAR "x"@ using
      @TO juliaValue@ and Julia's @SAMP "repr"@ function.
    Example
      x = JuliaObject {1, 2, 3}
      describe x
      toExternalString x
      value oo
    Text
      Not every Julia object can be recreated this way.
  SeeAlso
    (toString, JuliaObject)
///
