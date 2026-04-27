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

doc ///
  Key
    (isMember,RingElement,Ideal)
    (isMember,Number,Ideal)
  Headline
    test membership in an ideal
  Usage
    isMember(r,I)
  Inputs
    r:{RingElement,Number}
    I:Ideal
  Outputs
    :Boolean -- whether r is in the ideal I
  Description
    Example
      R = QQ[x,y,z]
      I = ideal(-x^3 + y, x^2*y - z)
      isMember(x*y^3 - z^2 + y^5 - z^3, I)
      J = ideal(x*z - y, x*y + 2*z^2, y - z)
      isMember(x^3*z - 2*y^2, J)
///
