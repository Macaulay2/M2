-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc ///
 Key
  isSorted
  (isSorted, VisibleList)
 Headline
  whether a list is sorted
 Usage
  isSorted L
 Inputs
  L: VisibleList
 Outputs
   : Boolean
    whether the elements of {\tt L} are in increasing order
 Description
  Example
   isSorted {1,2,2,3}
   isSorted {1,2,3,2}
   R = ZZ/2[x,y,z, MonomialOrder => Lex]; 
   isSorted (z^3, y^2, x)
   R = ZZ/2[x,y,z, MonomialOrder => GLex]; 
   isSorted (z^3, y^2, x)   
 SeeAlso
  sort
  "?"
  "lists and sequences"
///
