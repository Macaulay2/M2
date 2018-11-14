-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  switch
  (switch,ZZ,ZZ,VisibleList)
 Headline
  copy a list, switching two elements
 Usage
  switch(i, j, L)
 Inputs
  i: ZZ
  j: ZZ
  L: VisibleList
 Outputs
  L2:
   a copy of the list {\tt L}, with the elements in positions {\tt i} and {\tt j} interchanged. 
 Description
  Text
   A negative value of {\tt i} or {\tt j} is taken relative to the end of the list.
  Example
   L = 0..10;
   switch(3, 9, L)
   switch(0, -1, L)
   switch(-1, -2, L)
 SeeAlso
  insert
  reverse
  "lists and sequences"
///
