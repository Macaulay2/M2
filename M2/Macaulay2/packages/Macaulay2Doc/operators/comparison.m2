doc ///
  Key
    symbol <
  Headline
    less than
  Description
    Text
      @TT "x < y"@ yields @TO "true"@ or @TO "false"@ depending on whether x <
      y.

      Calls upon @TO "?"@ to perform the comparison, if necessary.
///
doc ///
  Key
    symbol <=
  Headline
    less than or equal
  Description
    Text
      @TT "x <= y"@ yields @TO "true"@ or @TO "false"@ depending on whether x
      <= y.

      Calls upon @TO "?"@ to perform the comparison, if necessary.
///
doc ///
  Key
    symbol >
  Headline
    greater than
  Description
    Text
      @TT "x > y"@ yields @TO "true"@ or @TO "false"@ depending on whether x >
      y.

      Calls upon @TO "?"@ to perform the comparison, if necessary.
///
doc ///
  Key
    symbol >=
  Headline
    greater than or equal
  Usage
    x >= y
  Inputs
    x:
    y:
  Outputs
    :Boolean
      depending on whether x >= y
  Description
    Text
      Calls upon @TO "?"@ to perform the comparison, if necessary.
///
undocumented {
     (symbol >, Thing, Thing),
     (symbol <, Thing, Thing),
     (symbol <=, Thing, Thing),
     (symbol >=, Thing, Thing)
     }

doc ///
  Key
    (symbol <<, List, List)
  Headline
    component-wise comparison of lists
  Usage
    L1 << L2
  Inputs
    L1:List
    L2:List
  Outputs
    :Boolean
      whether the first list is less than or equal to the second list in each
      component
  SeeAlso
    (symbol ?, List, List)
  SourceCode
    (symbol <<, List, List)
///

doc ///
  Key
    symbol ?
    (symbol ?, Symbol, IndexedVariable)
    (symbol ?, IndexedVariable, IndexedVariable)
    (symbol ?, List, List)
    (symbol ?, VirtualTally, VirtualTally)
  Headline
    comparison operator
  Usage
    x ? y
  Inputs
    x:
    y:
  Outputs
    :
      One of the symbols @TT "symbol <"@, @TT "symbol >"@, @TT "symbol =="@, or
      @TT "incomparable"@, depending (respectively) on whether @TT "x < y"@,
      @TT "x > y"@, @TT "x == y"@, or @TT "x"@ and @TT "y"@ are not comparable.
  Description
    Text
      Many types of objects may be compared. Numbers are handled as one would
      expect, and strings, lists and sequences are generally compared
      lexicographically.
    Example
      3 ? 4
      "book" ? "boolean"
      3 ? 3.
      {1,2,3} ? {4,5}
    Text
      Polynomials from the same ring may also be compared. The order depends on
      the monomial order in the ring.
    Example
      R = ZZ[a,b,c]
      a*c ? b^2
    Text
      A set is smaller than another if it is a subset; for tallies,
      corresponding counts should all be smaller.
    Example
      set {1,2} ? set {2,3}
      set {1,2} ? set {1,2,3}
      tally {1,1,2} ? tally {1,2,3}
      tally {1,1,2} ? tally {1,1,2,3}
  SeeAlso
    sort
    rsort
    (symbol?, Symbol)
  Subnodes
    incomparable
///

protect incomparable
doc ///
  Key
    incomparable
  Headline
    a result indicating incomparability
  Description
    Text
      @TT "incomparable"@ a symbol that may be returned by @TO "?"@ when the
      two things being compared are incomparable.
///

-- this should be at the end
undocumented select(makeDocumentTag \ methods symbol ?,
    m -> package m === Macaulay2Doc and not isUndocumented m and isMissingDoc m)
