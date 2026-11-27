--- status: Reviewed
--- author(s): kummini
--- notes: 

doc ///
  Key
    transpose
    (transpose, Matrix)
    (transpose, MutableMatrix)
    (transpose, List)
  Headline
    transpose a matrix or table
  Usage
    transpose f
  Inputs
    f:
      @ofClass {Matrix, List}@ which is a table
  Outputs
    :Matrix
      the transpose of @TT "f"@
  Description
    Text
      Here is an example.
    Example
      S = ZZ/10007[x,y,z];
      f = matrix{{x^3,x*y^2,x},{y*x^2,y^3,y}}
      entries f
      g = transpose f
      transpose entries f
    Text
      The output of @TT "transpose"@ is a map between the duals of the original
      source and target free modules. See:
    Example
      degrees f
      degrees g
  Caveat
    @TT "transpose"@ works only for maps between free modules. Use @TT "dual"@
    for more general maps.
  SeeAlso
    dual
///
