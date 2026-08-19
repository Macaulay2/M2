doc ///
  Key
    (symbol ==, JuliaObject, JuliaObject)
    (symbol ==, JuliaObject, Thing)
    (symbol ==, Thing, JuliaObject)
  Headline
    equality of Julia objects
  Usage
    x == y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :Boolean -- whether @VAR "x"@ and @VAR "y"@ are equal
  Description
    Text
      This calls Julia's @SAMP "=="@ function and returns a Macaulay2
      @TO Boolean@.  One of the operands may be a Macaulay2 object.  It is
      converted to a @TO JuliaObject@ first.
    Example
      JuliaObject 2 == JuliaObject 2.0
      JuliaObject {1, 2} == {1, 2}
      2 != JuliaObject 3
  SeeAlso
    (symbol ?, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol ?, JuliaObject, JuliaObject)
    (symbol ?, JuliaObject, Thing)
    (symbol ?, Thing, JuliaObject)
  Headline
    compare Julia objects
  Usage
    x ? y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Description
    Text
      Compare two Julia objects using Julia's @SAMP "<"@, @SAMP ">"@, and
      @SAMP "=="@ functions, returning one of @TO symbol <@,
      @TO symbol >@, @TO symbol ==@, or @TO incomparable@.  One of the
      operands may be a Macaulay2 object.  It is converted to a
      @TO JuliaObject@ first.
    Example
      JuliaObject 2 ? JuliaObject 3
      JuliaObject 3 ? 2
      2 ? JuliaObject 2
      JuliaObject "a" ? JuliaObject "b"
      JuliaObject(0/0.) ? 1
    Text
      This also makes the operators @TO symbol <@, @TO symbol <=@,
      @TO symbol >@, and @TO symbol >=@ available, as well as functions
      such as @TO sort@, @TO max@, and @TO min@.
    Example
      JuliaObject 2 < 3
      sort {JuliaObject 3, JuliaObject 1, JuliaObject 2}
      max(JuliaObject 3, JuliaObject 5)
  SeeAlso
    (symbol ==, JuliaObject, JuliaObject)
///
