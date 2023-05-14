doc ///
  Key
    isMember
    (isMember,Thing,Set)
    (isMember,Thing,VisibleList)
  Headline
    test membership in a list or set
  Usage
    isMember(e,x)
  Inputs
    e:Thing
    x:{List, Sequence, Set}
  Outputs
    :Boolean -- whether e is in the list, sequence, or set x
  Description
    Example
      isMember(c,{a,b,c,d,e})
      isMember(f,(a,b,c,d,e))
      isMember(3,set{1,2,5,6})
    Text
      This function may also be called using the synonym @TT "member"@.
  SeeAlso
    positions
    Set
///
