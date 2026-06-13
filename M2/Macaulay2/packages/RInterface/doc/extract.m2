doc ///
  Key
    (symbol _, RObject, Thing)
  Headline
    extract a single element of an R object
  Usage
    x_i
  Inputs
    x:RObject
    i:Thing -- the index (one-based)
  Outputs
    :RObject -- the @VAR "i"@-th element of @VAR "x"@
  Description
    Text
      Extract a single element of an R vector or list using R's @SAMP "[["@
      operator.  Note that R uses one-based indexing.
    Example
      x = RObject toList(5..15)
      x_1
  SeeAlso
    (symbol SPACE, RObject, Array)
  Subnodes
    ((symbol _, symbol =), RObject, Thing)
///

doc ///
  Key
    ((symbol _, symbol =), RObject, Thing)
  Headline
    replace a single element of an R object
  Usage
    x_i = e
  Inputs
    x:RObject
    i:Thing -- the index (one-based)
    e:Thing -- the replacement value
  Outputs
    :RObject -- a copy of @VAR "x"@ with its @VAR "i"@-th element replaced by @VAR "e"@
  Description
    Text
      Replace a single element of an R vector or list.  Note that this only
      affects the return value; the original object @VAR "x"@ is not modified.
    Example
      x = RObject toList(5..15)
      x_1 = 3
      x
  SeeAlso
    (symbol _, RObject, Thing)
    ((symbol SPACE, symbol =), RObject, Array)
///

doc ///
  Key
    (symbol SPACE, RObject, Array)
  Headline
    extract multiple elements of an R object
  Usage
    x[i, j, ...]
  Inputs
    x:RObject
    :Array -- indices @VAR "i"@, @VAR "j"@, and so on (one-based)
  Outputs
    :RObject -- the elements of @VAR "x"@ at the given indices
  Description
    Text
      Extract multiple elements of an R vector or list using R's @SAMP "["@
      operator.
    Example
      x = RObject toList(5..15)
      x[1, 4..8]
  SeeAlso
    (symbol _, RObject, Thing)
  Subnodes
    ((symbol SPACE, symbol =), RObject, Array)
///

doc ///
  Key
    ((symbol SPACE, symbol =), RObject, Array)
  Headline
    replace multiple elements of an R object
  Usage
    x[i, j, ...] = e
  Inputs
    x:RObject
    :Array --  indices @VAR "i"@, @VAR "j"@, and so on (one-based)
    e:Thing -- the replacement values
  Outputs
    :RObject -- a copy of @VAR "x"@ with elements at the given indices replaced
  Description
    Text
      Replace multiple elements of an R vector or list.  Note that this only
      affects the return value; the original object @VAR "x"@ is not modified.
    Example
      x = RObject toList(5..15)
      x[1..4, 8] = {1, 2, 3, 4, 5}
      x
  SeeAlso
    (symbol SPACE, RObject, Array)
    ((symbol _, symbol =), RObject, Thing)
///
