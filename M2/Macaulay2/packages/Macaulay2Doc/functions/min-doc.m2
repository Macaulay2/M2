-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  min
  (min, VisibleList)
 Headline
  yields the minimum element in a list or sequence
 Usage
  min X
 Inputs
  X: VisibleList
 Outputs
  m: Thing
 Description
  Example
   X = for i from 1 to 10 list random(100)
   min X
  Text
   If {\tt L} contains elements in a polynomial ring, the @TO MonomialOrder@
   of the ring is used for comparisons.
  Example
   R1 = QQ[x, y, z, MonomialOrder => Lex];
   min {x*y^2, x*y^2 + z^2, y^4, y*z^5}
   R2 = QQ[x, y, z, MonomialOrder => GRevLex];
   min (x*y^2, x*y^2 + z^2, y^4, y*z^5)
  Text
   More generally, the order of the elements is determined using the @TO "?"@ operator.
 
   If {\tt X} is a list of lists, {\tt min} acts on the outermost level.
  Example    
   min {{3, 1, 2}, {2, 9, 6}, {3, 7, 5}}
   min flatten {{3, 1, 2}, {2, 9, 6}, {3, 7, 5}}
 SeeAlso 
  minPosition
  max
  sort
  "?"
///
