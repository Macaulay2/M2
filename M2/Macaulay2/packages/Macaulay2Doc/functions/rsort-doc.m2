--- status: TODO
--- author(s): 
--- notes: 

doc ///
  Key
    rsort
    (rsort,Matrix)
    (rsort,List)
    (rsort, List, Function)
  Headline
    sort a list or matrix in reverse order
  Description
    Text
      Same as @TO "sort"@ except that the elements or columns are placed in
      reverse order.
    Example
      rsort {4,2,3,1}
    Text
      The optional parameters are the same as for @TO sort@.
    Example
      R = QQ[a..d];
      m = matrix{{a*b, c*d, a*d^3, b^3*c, 1_R}}
      rsort(m, DegreeOrder=>Ascending)
  SeeAlso
    sort
  Subnodes
    [rsort, MonomialOrder]
    [rsort, DegreeOrder]
///
doc ///
  Key
    [rsort, MonomialOrder]
  Headline
    specify Ascending or Descending monomial order
  Description
    Text
      The meaning is the same as from @TO [sort, MonomialOrder]@.
///
doc ///
  Key
    [rsort, DegreeOrder]
  Headline
    specify Ascending, Descending, or null
  Description
    Text
      The meaning is the same as from @TO [sort, MonomialOrder]@.
///

