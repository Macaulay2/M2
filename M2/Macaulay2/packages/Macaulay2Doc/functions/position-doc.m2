-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  position
  (position, ZZ, Function)
  (position, VisibleList, Function)
  (position, VisibleList, VisibleList, Function)
  [position, Reverse]
 Headline
  the first element of a list satisfying a condition
 Usage
  position(A, f)
  position(A, B, f)
  position(A, f, Reverse => true)
  position(n, f)
 Inputs
  A: VisibleList
  B: VisibleList
  n: ZZ
  f: Function
 Outputs
  p: ZZ
   the first index to satisfy the boolean function {\tt f}
 Description
  Text
   {\tt position(A, f)} returns the smallest index {\tt i} such that {\tt f(A#i)} 
   is true. If no element satisfies the condition, @TO null@ is returned.
  Example
   position((10,20,43,105,6,93), odd)
   position((10,20,43,105,6,93), i -> i<0)
  Text
   Use {\tt position(A, B, f)} to return the smallest index {\tt i} such that {\tt f(A#i, B#i)} is true.
  Example
   position((10,20,43,105,6,93),(18,82,12,7,35,92), (a,b) -> a>b)
  Text
   The {\tt Reverse} option will return the largest index instead.
  Example
   position((10,20,43,105,6,93), odd, Reverse => true)  
   position((10,20,43,105,6,93),(18,82,12,7,35,92), (a,b) -> a>b, Reverse => true)
  Text
   Use {\tt position(n, f)} to return the smallest index {\tt i} such that {\tt f(i)} is true.
  Example
   position(17, i -> i^2 % 17 == 13)
  Text
   To find all indices of elements satisfying the condition, see @TO positions@. To return the 
   elements, rather than their indices, see @TO select@. The function @TO number@ counts the
   number of elements satisfying the condition.
  Example
   positions((10,20,43,105,6,93), odd)
   select((10,20,43,105,6,93), odd)
   number((10,20,43,105,6,93), odd)
 SeeAlso
  minPosition
  maxPosition
  number
  positions
  select
  take
  "lists and sequences"
///
