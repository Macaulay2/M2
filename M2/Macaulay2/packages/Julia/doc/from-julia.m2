doc ///
  Key
    (value, JuliaObject)
  Headline
    convert a Julia object to Macaulay2
  Usage
    value x
  Inputs
    x:JuliaObject
  Outputs
    :Thing -- the Macaulay2 equivalent of @VAR "x"@
  Description
    Text
      If possible, the Macaulay2 equivalent of the given Julia object is
      returned.  Julia's @SAMP "Bool"@ becomes a @TO Boolean@, and
      @SAMP "nothing"@ becomes @TO null@.
    Example
      value juliaValue "true"
      value juliaValue "nothing" === null
    Text
      All of Julia's integer types become @TO ZZ@ objects, rational
      numbers become @TO QQ@ objects, and complex numbers become
      @TO CC@ objects.
    Example
      value juliaValue "Int8(5)"
      value juliaValue "big(2)^100"
      value juliaValue "2//3"
      value juliaValue "2.5 + 3.5im"
    Text
      Floating-point numbers become @TO RR@ objects with the same
      precision.
    Example
      value juliaValue "Float32(0.5)"
      precision oo
      value juliaValue "BigFloat(pi; precision = 100)"
      precision oo
    Text
      Strings, characters, and symbols all become @TO String@ objects.
    Example
      value juliaValue "\"foo\""
      value juliaValue "'a'"
      value juliaValue ":bar"
    Text
      Arrays (other than matrices) become lists and tuples become
      sequences.  Their elements are converted recursively.
    Example
      value juliaValue "[1, 2, 3]"
      value juliaValue "1:5"
      value juliaValue "(1, 'a', :b)"
    Text
      Named tuples become sequences of options.
    Example
      value juliaValue "(a = 1, b = \"foo\")"
    Text
      Dictionaries become hash tables, and sets become sets.
    Example
      value juliaValue "Dict(:a => 1, :b => 2)"
      value juliaValue "Set([1, 2, 3])"
    Text
      Two-dimensional arrays become matrices.
    Example
      value juliaValue "[1 2; 3 4]"
    Text
      An error is raised if there is no conversion for the type of
      @VAR "x"@.  Use @TO addJuliaToM2Function@ to add one.
  SeeAlso
    JuliaObject
    addJuliaToM2Function
///

doc ///
  Key
    addJuliaToM2Function
    (addJuliaToM2Function, String, Function)
  Headline
    add a function for converting Julia objects to Macaulay2
  Usage
    addJuliaToM2Function(T, f)
  Inputs
    T:String -- Julia code that evaluates to a Julia type
    f:Function -- that takes a @TO JuliaObject@ of type @VAR "T"@ and
      returns a Macaulay2 object
  Consequences
    Item
      @TO (value, JuliaObject)@ calls @VAR "f"@ on Julia objects of type
      @VAR "T"@
  Description
    Text
      Define how @TO (value, JuliaObject)@ converts objects of a given Julia
      type.  For example, suppose we define a Julia type for points in the
      plane.
    Example
      juliaValue "struct Point; x::Int; y::Int; end"
      p = juliaValue "Point(1, 2)"
    Text
      There is no conversion for this type, so we add one that returns a
      list of coordinates.
    Example
      addJuliaToM2Function("Point", q -> {value q@@"x", value q@@"y"})
      value p
    Text
      The conversion also applies to subtypes of @VAR "T"@, and when more
      than one conversion applies, the one for the most specific type is
      used.  Calling @SAMP "addJuliaToM2Function"@ again with the same type
      replaces its conversion.
  SeeAlso
    (value, JuliaObject)
///
