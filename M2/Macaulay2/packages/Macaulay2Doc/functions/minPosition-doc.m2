-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key 
  minPosition
  (minPosition, BasicList)
 Headline
  position of the smallest element
 Usage
  minPosition L
 Inputs
  L:BasicList
 Outputs
  i:ZZ
   the index of the smallest element in the list {\tt L}
 Description
  Text
   If the smallest element occurs more than once, the index of its first occurrence is used.
  Example
   minPosition {2, 1, 6, 4, 1}
  Text
   If {\tt L} contains elements in a polynomial ring, the @TO MonomialOrder@
   of the ring is used for comparisons.
  Example
   R1 = QQ[x, y, z, MonomialOrder => Lex];
   minPosition {x*y^2, x*y^2 + z^2, y^4, y*z^5}
   R2 = QQ[x, y, z, MonomialOrder => GRevLex];
   minPosition (x*y^2, x*y^2 + z^2, y^4, y*z^5)
  Text
   More generally, the order of the elements is determined using the @TO "?"@ operator.
 SeeAlso 
  maxPosition
  max
  min
  sort
  position
  positions
  "?"
///