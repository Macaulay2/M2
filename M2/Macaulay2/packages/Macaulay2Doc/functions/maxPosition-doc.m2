-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key 
  maxPosition
  (maxPosition, BasicList)
 Headline
  position of the largest element
 Usage
  maxPosition L
 Inputs
  L:BasicList
 Outputs
  i:ZZ
   the index of the largest element in the list {\tt L}
 Description
  Text
   If the largest element occurs more than once, the index of its first occurrence is used.
  Example
   maxPosition {1, 6, 4, 2, 6}
  Text
   If {\tt L} contains elements in a polynomial ring, the @TO MonomialOrder@
   of the ring is used for comparisons.
  Example
   R1 = QQ[x, y, z, MonomialOrder => Lex];
   maxPosition {x*y^2, x*y^2 + z^2, y^4, y*z^5}
   R2 = QQ[x, y, z, MonomialOrder => GRevLex];
   maxPosition (x*y^2, x*y^2 + z^2, y^4, y*z^5)
  Text
   More generally, the order of the elements is determined using the @TO "?"@ operator.
 SeeAlso 
  minPosition
  max
  min
  sort
  position
  positions
  "?"
///
