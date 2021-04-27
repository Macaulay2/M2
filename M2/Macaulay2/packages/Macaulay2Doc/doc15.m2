doc ///
  Key
    left
  Headline
    left endpoint of an interval
  Usage
    x = left I
  Inputs
    I:RRi
  Outputs
    x:RR
  Description
    Text
      Returns the left endpoint of the input interval.
  SeeAlso
    right
    midpoint
    diameter
///

doc ///
  Key
    right
Headline
    right endpoint of an interval
Usage
    x = right I
Inputs
    I:RRi
Outputs
    x:RR
Description
  Text
    Returns the right endpoint of the input interval.
SeeAlso
    left
    midpoint
    diameter
///

doc ///
Key
    midpoint
Headline
    midpoint of an interval
Usage
    x = midpoint I
Inputs
    I:RRi
Outputs
    x:RR
Description
  Text
    Returns the midpoint (the average of the endpoints) of the input interval.
SeeAlso
    left
    right
    diameter
///

doc ///
Key
    diameter
Headline
    diameter of an interval
Usage
    x = diameter I
Inputs
    I:RRi
Outputs
    x:RR
Description
  Text
    Returns the diameter (the difference between the endpoints) of the input interval.
SeeAlso
    left
    right
    midpoint
///

undocumented{ (intersect,RRi)}

doc ///
Key
    (intersection, RRi, RRi)
Headline
    computes intersection of input intervals
Usage
    K = intersect(I,J)
Inputs
    I:RRi
    J:RRi
Outputs
    K:RRi
Description
  Text
    Returns the intersection of the two input intervals or an empty interval if they do not intersect.
SeeAlso
    intersect
///

doc ///
Key
    isMember
    (isMember, QQ, RRi)
    (isMember, ZZ, RRi)
    (isMember, RR, RRi)
Headline
    membership test in an interval
Usage
    x = intersect(q,I)
    x = intersect(z,I)
    x = intersect(r,I)
Inputs
    q:QQ
    z:ZZ
    r:RR
Outputs
    x:Boolean
Description
  Text
    Returns true if the input number is in the interval.
SeeAlso
    intersect
///
