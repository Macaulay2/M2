doc ///
  Key
    (iterator, RObject)
  Headline
    iterate through an R object
  Usage
    iterator x
  Inputs
    x:RObject
  Outputs
    :Iterator
  Description
    Text
      This returns an @TO Iterator@ object that may be used to iterate through
      @SAMP "x"@.
    Example
      i = iterator (RSymbol "iris")_"Petal.Length"
      next i
      next i
      next i
///

doc ///
  Key
    (length, RObject)
  Headline
    the length of an R object
  Usage
    length x
  Inputs
    x:RObject
  Outputs
    :ZZ
  Description
    Text
      This function returns the length of an RObject.
    Example
      length (RSymbol "iris")_"Petal.Length"
///
