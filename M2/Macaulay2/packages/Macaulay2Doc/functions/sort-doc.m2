--- status: TODO
--- author(s): 
--- notes: 

doc ///
  Key
    sort
  Headline
    sort a list or columns of a matrix
  SeeAlso
    rsort
    sortColumns
    symbol<=
    symbol?
  Subnodes
    (sort, List)
    (sort, Matrix)
    sortColumns
///
doc ///
  Key
    (sort,Matrix)
  Headline
    sort the columns of a matrix
  Usage
    sort m
  Inputs
    m:
      between free modules
  Outputs
    :Matrix
      with the same shape as @TT "m"@ whose columns are sorted in increasing
      order
  Description
    Text
      The default order is to sort the columns in ascending degree first, and
      within each degree, first compare ring elements in the highest non-zero
      component, breaking ties by going to the next lower component. Zero is
      considered earlier in the order than other elements
    Example
      R = ZZ/32003[a..d,MonomialOrder=>Lex];
      m = matrix{{a*d, b^2, b^100, b^50*d^50, c^2*d}}
      sort m
    Text
      The two optional arguments can modify this default order. The value of
      @TO DegreeOrder@ is considered first (Ascending, Descending, or null),
      and after that the monomial (or ring) order is used to break ties, either
      ascending or descending, depending on the value of @TO MonomialOrder@.

      To sort the columns of @TT "m"@ in descending monomial order:
    Example
      options sort
      sort(m, DegreeOrder=>null, MonomialOrder=>Descending)
  SeeAlso
    sortColumns
  Subnodes
    [sort, MonomialOrder]
    [sort, DegreeOrder]
    Ascending
    Descending
///
doc ///
  Key
    (sort,List)
    (sort, List, Function)
  Headline
    sort a list
  Usage
    sort v
  Inputs
    v:
  Outputs
    :List
      of the same length as @TT "v"@ sorted in ascending order
  Description
    Text
      The sort function uses @TO symbol<=@ to compare elements of the list,
      which in turn calls upon the comparison operator @TO symbol?@, so, to
      determine how the elements are sorted, refer to its documentation.
      Methods for new user defined types of objects can be installed for it,
      too, to obtain the desired sorting behavior.
    Example
      sort {c,e,a,f,b,f}
      sort {4,2,6,3,8,2}
    Text
      Comparison of strings is implemented so that symbols come before
      alphanumeric characters, upper and lower case characters are sorted
      together, and strings of digits are sorted by size of the implied number.
    Example
      sort {"a11","a1","a2","A11","A1","A2","B2"}
    Text
      Visible lists (lists, sequences, and arrays) are compared
      lexicographically.
    Example
      sort {(1,1),(2,1),(3,1),(1,2),(3,2)}
    Text
      If the elements of the list are polynomials, then the monomial order is
      used to sort the elements according to their lead terms.
    Example
      R = ZZ[a..d,MonomialOrder=>Lex];
      sort{a*d, b^100, c^3*d}
    Text
      The optional arguments are not used in this version of sort.
  Caveat
    If a list contains elements of different types, the sorting still occurs,
    but might not be particularly intuitive.
  SeeAlso
    rsort
    symbol<=
    symbol?
///
doc ///
  Key
    [sort, MonomialOrder]
  Headline
    specify Ascending or Descending monomial order
  Usage
    sort(...,MonomialOrder=>x)
  Inputs
    x:Symbol
      either @TO "Ascending"@ or @TO "Descending"@
  Consequences
    Item
      After the degree order has been considered, sort the columns of the
      matrix in Ascending or Descending monomial order
  Caveat
    This option only works for sorting columns of a matrix, not the elements of
    a list
///
doc ///
  Key
    [sort, DegreeOrder]
  Headline
    specify Ascending, Descending, or null
  Usage
    sort(...,DegreeOrder=>x)
  Inputs
    x:Symbol
      either @TO "Ascending"@, @TO "Descending"@, or @TO null@
  Consequences
    Item
      If @TT "x"@ is not null, first order the columns of the matrix in
      ascending or descending degree (depending on @TT "x"@). If @TT "x"@ is
      null, then only use the monomial order in the sort
  Caveat
    This option only works for sorting columns of a matrix, not the elements of
    a list
///

doc ///
  Key
    Ascending
  Headline
    specify ascending order
  Description
    Text
      @TT "Ascending"@ -- a symbol used as a value for optional arguments
      @TO "DegreeOrder"@ and @TO "MonomialOrder"@.
///

doc ///
  Key
    Descending
  Headline
    specify descending order
  Description
    Text
      @TT "Descending"@ -- a symbol used as a value for optional arguments
      @TO "DegreeOrder"@ and @TO "MonomialOrder"@.
///
