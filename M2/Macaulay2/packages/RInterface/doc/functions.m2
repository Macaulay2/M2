doc ///
  Key
    RFunction
    (NewFromMethod, RFunction, RObject)
    (NewFromMethod, RFunction, String)
    (NewFromMethod, RFunction, Thing)
  Headline
    R function
  Usage
    RFunction x
  Inputs
    x:{String, Thing} -- specifying an R function
  Outputs
    :RFunction
  Description
    Text
      An @SAMP "RFunction"@ is a function that wraps around an R
      function specified by a string.  Its arguments are converted to
      @TO RObject@'s.
    Example
      qnorm = RFunction "qnorm"
      qnorm(0.025, "lower.tail" => false)
    Text
      Any Macaulay2 object may also be used; it is converted to a string with
      @TO toString@.
    Example
      RFunction sin
      oo(pi/4)
  Subnodes
    "library"
///

doc ///
  Key
    "library"
  Headline
    load an R package
  Usage
    library pkg
  Inputs
    pkg:String
  Description
    Text
      This loads an R package.  It is an @TO RFunction@ that calls R's
      @SAMP "library"@ function.

      For example, suppose we want to load the @SAMP "abbey"@ dataset
      with nickel content in a Canadian syenite rock.  It is available in
      the @SAMP "MASS"@ package.
    Example
      library "MASS"
      RSymbol "abbey"
///
