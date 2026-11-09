doc ///
  Key
    (symbol _, JuliaObject, Thing)
    (symbol _, JuliaObject, Sequence)
  Headline
    get an element of a Julia collection
  Usage
    x_i
    x_(i, j, ...)
  Inputs
    x:JuliaObject
    i:Thing -- an index or key
  Outputs
    :JuliaObject -- the element of @VAR "x"@ at @VAR "i"@
  Description
    Text
      Get an element of a Julia collection, using Julia's @SAMP "getindex"@
      function.  Note that Julia arrays are indexed starting at 1.
    Example
      v = JuliaObject {10, 20, 30}
      v_1
      d = JuliaObject hashTable {"a" => 1}
      d_"a"
    Text
      Use a sequence to pass more than one index, e.g., for matrices.
    Example
      A = JuliaObject matrix {{1, 2}, {3, 4}}
      A_(1, 2)
  SeeAlso
    ((symbol _, symbol =), JuliaObject, Thing)
///

doc ///
  Key
    ((symbol _, symbol =), JuliaObject, Thing)
    ((symbol _, symbol =), JuliaObject, Sequence)
  Headline
    set an element of a Julia collection
  Usage
    x_i = e
    x_(i, j, ...) = e
  Inputs
    x:JuliaObject
    i:Thing -- an index or key
    e:Thing
  Consequences
    Item
      the element of @VAR "x"@ at @VAR "i"@ is set to @VAR "e"@
  Description
    Text
      Set an element of a mutable Julia collection, using Julia's
      @SAMP "setindex!"@ function.
    Example
      v = JuliaObject {10, 20, 30}
      v_1 = 15
      v
      A = JuliaObject matrix {{1, 2}, {3, 4}}
      A_(1, 2) = 5
      A
  SeeAlso
    (symbol _, JuliaObject, Thing)
    (delete, JuliaObject, Thing)
///

doc ///
  Key
    (delete, JuliaObject, Thing)
  Headline
    delete a key from a Julia collection
  Usage
    delete(x, k)
  Inputs
    x:JuliaObject
    k:Thing
  Outputs
    :JuliaObject -- @VAR "x"@, with the key @VAR "k"@ removed
  Description
    Text
      Remove a key from a Julia collection, such as a dictionary, using
      Julia's @SAMP "delete!"@ function.
    Example
      d = JuliaObject hashTable {"a" => 1, "b" => 2}
      delete(d, "a")
  SeeAlso
    ((symbol _, symbol =), JuliaObject, Thing)
///

doc ///
  Key
    (symbol @@, JuliaObject, Thing)
  Headline
    get a property of a Julia object
  Usage
    x@@p
  Inputs
    x:JuliaObject
    p:Thing -- the name of the property
  Outputs
    :JuliaObject -- the value of property @VAR "p"@ of @VAR "x"@
  Description
    Text
      Get a property of a Julia object, e.g., a field of a struct or a
      global variable of a module, using Julia's @SAMP "getproperty"@
      function.  This is equivalent to @SAMP "x.p"@ in Julia.
    Example
      z = JuliaObject(2 + 3*ii)
      z@@"re"
      z@@im
      BaseModule = juliaGetGlobal "Base"
      BaseModule@@pi
  SeeAlso
    ((symbol @@, symbol =), JuliaObject, Thing)
    (symbol @@?, JuliaObject, Thing)
    juliaGetGlobal
///

doc ///
  Key
    ((symbol @@, symbol =), JuliaObject, Thing)
  Headline
    set a property of a Julia object
  Usage
    x@@p = e
  Inputs
    x:JuliaObject
    p:Thing -- the name of the property
    e:Thing
  Consequences
    Item
      property @VAR "p"@ of @VAR "x"@ is set to @VAR "e"@
  Description
    Text
      Set a property of a Julia object, e.g., a field of a mutable struct,
      using Julia's @SAMP "setproperty!"@ function.  This is equivalent to
      @SAMP "x.p = e"@ in Julia.
    Example
      juliaValue "mutable struct Counter; count::Int; end"
      c = juliaValue "Counter(0)"
      c@@count = 5
      c
  SeeAlso
    (symbol @@, JuliaObject, Thing)
///

doc ///
  Key
    (symbol @@?, JuliaObject, Thing)
  Headline
    whether a Julia object has a property
  Usage
    x@@?p
  Inputs
    x:JuliaObject
    p:Thing -- the name of the property
  Outputs
    :Boolean -- whether @VAR "x"@ has the property @VAR "p"@
  Description
    Text
      This uses Julia's @SAMP "hasproperty"@ function.
    Example
      z = JuliaObject(2 + 3*ii)
      z@@?re
      z@@?foo
  SeeAlso
    (symbol @@, JuliaObject, Thing)
///

doc ///
  Key
    (iterator, JuliaObject)
  Headline
    iterate over a Julia collection
  Usage
    iterator x
  Inputs
    x:JuliaObject
  Outputs
    :Iterator
  Description
    Text
      Julia collections may be iterated over, using Julia's
      @SAMP "iterate"@ function.  Each element is a @TO JuliaObject@.
    Example
      x = JuliaObject {1, 2, 3}
      for y in x list value y + 1
      toList juliaValue "1:4"
  SeeAlso
    (length, JuliaObject)
///

doc ///
  Key
    (length, JuliaObject)
  Headline
    length of a Julia collection
  Usage
    length x
  Inputs
    x:JuliaObject
  Outputs
    :ZZ -- the number of elements of @VAR "x"@
  Description
    Text
      This uses Julia's @SAMP "length"@ function.
    Example
      length JuliaObject {1, 2, 3}
      length JuliaObject "foo"
      length JuliaObject matrix {{1, 2}, {3, 4}}
  SeeAlso
    (iterator, JuliaObject)
///
