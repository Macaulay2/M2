doc ///
  Key
    (symbol ~, JuliaObject)
  Headline
    bitwise not of a Julia object
  Usage
    ~x
  Inputs
    x:JuliaObject
  Outputs
    :JuliaObject -- the bitwise complement of @VAR "x"@
  Description
    Text
      This calls Julia's @SAMP "~"@ function.
    Example
      ~JuliaObject 5
  SeeAlso
    (symbol &, JuliaObject, JuliaObject)
    (symbol |, JuliaObject, JuliaObject)
    (symbol ^^, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol &, JuliaObject, JuliaObject)
    (symbol &, JuliaObject, Thing)
    (symbol &, Thing, JuliaObject)
  Headline
    bitwise and of Julia objects
  Usage
    x & y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the bitwise and of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "&"@ function.  One of the operands may be a
      Macaulay2 object.  It is converted to a @TO JuliaObject@ first.
    Example
      JuliaObject 5 & JuliaObject 6
      JuliaObject 5 & 6
      5 & JuliaObject 6
  SeeAlso
    (symbol |, JuliaObject, JuliaObject)
    (symbol ^^, JuliaObject, JuliaObject)
    (symbol ~, JuliaObject)
///

doc ///
  Key
    (symbol |, JuliaObject, JuliaObject)
    (symbol |, JuliaObject, Thing)
    (symbol |, Thing, JuliaObject)
  Headline
    bitwise or of Julia objects
  Usage
    x | y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the bitwise or of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "|"@ function.  One of the operands may be a
      Macaulay2 object.  It is converted to a @TO JuliaObject@ first.
    Example
      JuliaObject 5 | JuliaObject 6
      JuliaObject 5 | 6
      5 | JuliaObject 6
  SeeAlso
    (symbol &, JuliaObject, JuliaObject)
    (symbol ^^, JuliaObject, JuliaObject)
    (symbol ~, JuliaObject)
///

doc ///
  Key
    (symbol ^^, JuliaObject, JuliaObject)
    (symbol ^^, JuliaObject, Thing)
    (symbol ^^, Thing, JuliaObject)
  Headline
    bitwise exclusive or of Julia objects
  Usage
    x ^^ y
  Inputs
    x:JuliaObject
    y:JuliaObject
  Outputs
    :JuliaObject -- the bitwise exclusive or of @VAR "x"@ and @VAR "y"@
  Description
    Text
      This calls Julia's @SAMP "⊻"@ (@SAMP "xor"@) function.  One of the
      operands may be a Macaulay2 object.  It is converted to a
      @TO JuliaObject@ first.
    Example
      JuliaObject 5 ^^ JuliaObject 6
      JuliaObject 5 ^^ 6
      5 ^^ JuliaObject 6
  SeeAlso
    (symbol &, JuliaObject, JuliaObject)
    (symbol |, JuliaObject, JuliaObject)
    (symbol ~, JuliaObject)
///

doc ///
  Key
    (symbol <<, JuliaObject, JuliaObject)
    (symbol <<, JuliaObject, Thing)
    (symbol <<, Thing, JuliaObject)
  Headline
    left bit shift of a Julia object
  Usage
    x << n
  Inputs
    x:JuliaObject
    n:JuliaObject
  Outputs
    :JuliaObject -- @VAR "x"@ shifted left by @VAR "n"@ bits
  Description
    Text
      This calls Julia's @SAMP "<<"@ function.  One of the operands may be
      a Macaulay2 object.  It is converted to a @TO JuliaObject@ first.
    Example
      JuliaObject 6 << JuliaObject 5
      JuliaObject 6 << 5
      6 << JuliaObject 5
  SeeAlso
    (symbol >>, JuliaObject, JuliaObject)
///

doc ///
  Key
    (symbol >>, JuliaObject, JuliaObject)
    (symbol >>, JuliaObject, Thing)
    (symbol >>, Thing, JuliaObject)
  Headline
    right bit shift of a Julia object
  Usage
    x >> n
  Inputs
    x:JuliaObject
    n:JuliaObject
  Outputs
    :JuliaObject -- @VAR "x"@ shifted right by @VAR "n"@ bits
  Description
    Text
      This calls Julia's @SAMP ">>"@ function, an arithmetic shift that
      preserves the sign.  One of the operands may be a Macaulay2 object.
      It is converted to a @TO JuliaObject@ first.
    Example
      JuliaObject 192 >> JuliaObject 5
      JuliaObject 192 >> 5
      192 >> JuliaObject 5
      JuliaObject(-192) >> 5
    Text
      Macaulay2 has no operator corresponding to Julia's logical right
      shift @SAMP ">>>"@, which fills with zeros instead of copying the
      sign bit.  Call it using @TO JuliaFunction@ instead.
    Example
      (JuliaFunction ">>>")(-192, 5)
  SeeAlso
    (symbol <<, JuliaObject, JuliaObject)
///
