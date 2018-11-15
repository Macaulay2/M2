-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  flatten
  (flatten, VisibleList)
  (flatten, Matrix)
 Headline
  flatten a nested list or a matrix
 Usage
  flatten x
 Inputs
  x:List
   or @TO Matrix@
 Outputs
  :List
   or matrix obtained by removing braces one level in
 Description
  Text
   If {\tt x} is a list, then {\tt flatten x} effectively removes
   the braces surrounding any elements of {\tt x} that happen to
   be lists.
  Example
   flatten {{2,3,4}, {{5}, 6}, 7}
  Text
   If {\tt x} is a matrix, then {\tt flatten x} puts the elements of {\tt x} in a 
   single row, {\em ordered by column}.
   If {\tt x} is an $m\times n$ matrix, then {\tt flatten x} is a $1\times mn$ matrix.
  Example
   R = ZZ/101[a,b,c];
   m = matrix {{2, a},{b^2, 23},{c, c^3}}
   flatten m
 Caveat
  The matrix produced by {\tt flatten m} is {\em not} the same as the
  matrix given by unnesting the list used to define the matrix.
 SeeAlso
  deepSplice
  splice
  "lists and sequences"
///