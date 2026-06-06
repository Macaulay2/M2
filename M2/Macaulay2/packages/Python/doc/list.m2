doc ///
  Key
    (symbol _, PythonObject, Thing)
  Headline
    get elements of python sequences
  Usage
    x_y
  Inputs
    x:PythonObject
    y:Thing
  Outputs
   :PythonObject
  Description
    Text
      You may access elements of python sequences using @TT "_"@.
      This is equivalent to square brackets (@TT "[]"@) in Python. For
      example, this works for lists.
    Example
      x = toPython {1, 2, 3, 4}
      x_1
    Text
      It also works for dictionaries.
    Example
      x = toPython hashTable {"spam" => 1, "eggs" => 2}
      x_"eggs"
///

doc ///
  Key
    ((symbol _, symbol =), PythonObject, Thing)
  Headline
    set elements of mutable python sequences
  Usage
    x_y = e
  Inputs
    x:PythonObject
    y:Thing
    e:Thing
  Description
    Text
      You may set elements of mutable python sequences using @TT "_"@.
      This is equivalent to square brackets (@TT "[]"@) in Python. For
      example, this works for lists.
    Example
      x = toPython {1, 2, 3, 4}
      x_0 = 5
      x
    Text
      It also works for dictionaries.
    Example
      x = toPython hashTable {"spam" => 1, "eggs" => 2}
      x_"ham" = 3
      x
///

doc ///
  Key
    (delete, Thing, PythonObject)
  Headline
    delete elements from mutable python sequences
  Usage
    delete(i, x)
  Inputs
    i:Thing
    x:PythonObject
  Description
    Text
      This deletes the element of @VAR "x"@ indexed by @VAR "i"@.  This is
      equivalent to the @CODE "del"@ keyword in Python.
    Example
      x = toPython {3, 5, 7, 9}
      delete(0, x)
      x
      x = toPython hashTable {"spam" => 1, "eggs" => 2}
      delete("eggs", x)
      x
///
