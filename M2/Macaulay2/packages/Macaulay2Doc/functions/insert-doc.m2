-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  insert
  (insert,ZZ,Thing,BasicList)
 Headline
  copy a list, inserting an element
 Usage
  insert(i, x, L)
 Inputs
  i: ZZ
  x: Thing
  L: BasicList
 Outputs
  L2: BasicList
   a copy of {\tt L} in which {\tt x} has been inserted into position {\tt i}
 Description
  Example
   L = 0..10
   insert(4, "hi", L)
   insert(0, "hi", L)
   insert(11, "hi", L)
   insert(-1, "hi", L)
   apply({-1,-3,-5}, i -> L = insert(i, "hi", L)); L
 SeeAlso
  delete
  mingle
  switch  
  "lists and sequences"
///
