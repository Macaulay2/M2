doc ///
  Key
    (NewFromMethod, JuliaObject, Boolean)
  Headline
    create a Julia boolean
  Usage
    JuliaObject x
  Inputs
    x:Boolean
  Outputs
    :JuliaObject -- a Julia @SAMP "Bool"@
  Description
    Example
      JuliaObject true
      JuliaObject false
///

doc ///
  Key
    (NewFromMethod, JuliaObject, ZZ)
  Headline
    create a Julia integer
  Usage
    JuliaObject n
  Inputs
    n:ZZ
  Outputs
    :JuliaObject -- a Julia @SAMP "Int64"@, @SAMP "Int128"@, or @SAMP "BigInt"@
  Description
    Text
      The smallest of Julia's @SAMP "Int64"@, @SAMP "Int128"@, and
      @SAMP "BigInt"@ types that can hold @VAR "n"@ is used.
    Example
      JuliaObject 5
      JuliaObject 2^100
      JuliaObject 2^200
    Text
      Arithmetic on the result follows Julia's semantics, so fixed-width
      integers may overflow.
    Example
      JuliaObject(2^63 - 1) + 1
///

doc ///
  Key
    (NewFromMethod, JuliaObject, QQ)
  Headline
    create a Julia rational number
  Usage
    JuliaObject x
  Inputs
    x:QQ
  Outputs
    :JuliaObject -- a Julia @SAMP "Rational"@
  Description
    Example
      JuliaObject(2/3)
  SeeAlso
    (numerator, JuliaObject)
    (denominator, JuliaObject)
///

doc ///
  Key
    (NewFromMethod, JuliaObject, RR)
    (NewFromMethod, JuliaObject, Number)
  Headline
    create a Julia floating-point number
  Usage
    JuliaObject x
  Inputs
    x:RR
  Outputs
    :JuliaObject -- a Julia @SAMP "Float64"@ or @SAMP "BigFloat"@
  Description
    Text
      Numbers with at most 53 bits of precision become @SAMP "Float64"@
      objects, and numbers with more precision become @SAMP "BigFloat"@
      objects with the same precision.
    Example
      JuliaObject 2.5
      JuliaObject numeric(100, pi)
    Text
      Other numbers, such as @TO pi@, are converted using @TO numeric@
      first.
    Example
      JuliaObject pi
///

doc ///
  Key
    (NewFromMethod, JuliaObject, CC)
  Headline
    create a Julia complex number
  Usage
    JuliaObject z
  Inputs
    z:CC
  Outputs
    :JuliaObject -- a Julia @SAMP "Complex"@
  Description
    Example
      JuliaObject(2 + 3*ii)
  SeeAlso
    (realPart, JuliaObject)
    (imaginaryPart, JuliaObject)
    (conjugate, JuliaObject)
///

doc ///
  Key
    (NewFromMethod, JuliaObject, String)
  Headline
    create a Julia string
  Usage
    JuliaObject s
  Inputs
    s:String
  Outputs
    :JuliaObject -- a Julia @SAMP "String"@
  Description
    Example
      JuliaObject "Hello, world!"
///

doc ///
  Key
    (NewFromMethod, JuliaObject, Nothing)
  Headline
    create Julia's nothing
  Usage
    JuliaObject null
  Inputs
    :Nothing
  Outputs
    :JuliaObject -- Julia's @SAMP "nothing"@
  Description
    Example
      JuliaObject null
      value oo === null
///

doc ///
  Key
    (NewFromMethod, JuliaObject, List)
  Headline
    create a Julia vector
  Usage
    JuliaObject L
  Inputs
    L:List
  Outputs
    :JuliaObject -- a Julia @SAMP "Vector"@
  Description
    Text
      The elements of @VAR "L"@ are converted to Julia objects, and the
      element type of the vector is determined by Julia.
    Example
      JuliaObject {1, 2, 3}
      JuliaObject {1, "foo", 2.5}
      JuliaObject {{1, 2}, {3}}
  SeeAlso
    (NewFromMethod, JuliaObject, Sequence)
///

doc ///
  Key
    (NewFromMethod, JuliaObject, Sequence)
  Headline
    create a Julia tuple
  Usage
    JuliaObject s
  Inputs
    s:Sequence
  Outputs
    :JuliaObject -- a Julia @SAMP "Tuple"@
  Description
    Example
      JuliaObject (1, "foo")
  SeeAlso
    (NewFromMethod, JuliaObject, List)
///

doc ///
  Key
    (NewFromMethod, JuliaObject, HashTable)
  Headline
    create a Julia dictionary
  Usage
    JuliaObject t
  Inputs
    t:HashTable
  Outputs
    :JuliaObject -- a Julia @SAMP "Dict"@
  Description
    Text
      The keys and values of @VAR "t"@ are converted to Julia objects.
    Example
      JuliaObject hashTable {"a" => 1, "b" => 2}
///

doc ///
  Key
    (NewFromMethod, JuliaObject, Set)
  Headline
    create a Julia set
  Usage
    JuliaObject S
  Inputs
    S:Set
  Outputs
    :JuliaObject -- a Julia @SAMP "Set"@
  Description
    Example
      JuliaObject set {1, 2, 3}
///

doc ///
  Key
    (NewFromMethod, JuliaObject, Matrix)
  Headline
    create a Julia matrix
  Usage
    JuliaObject A
  Inputs
    A:Matrix
  Outputs
    :JuliaObject -- a Julia @SAMP "Matrix"@
  Description
    Example
      JuliaObject matrix {{1, 2}, {3, 4}}
      JuliaObject matrix {{1.5, 2}, {3, 4}}
///
