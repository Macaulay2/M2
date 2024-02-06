-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  positions
  (positions, VisibleList, Function)
  (positions, MutableList, Function)
 Headline
  which elements of a list satisfy a condition
 Usage
  positions(A, f)
 Inputs
  A: VisibleList
  f: Function
 Outputs
  p: List
   the list of indices {\tt i} such that {\tt f(A#i)} is true
 Description
  Text
   The indices are listed in ascending order. If no element satisfies the condition, an empty list is returned.
  Example
   positions((10,20,43,105,6,93), odd)
   positions((10,20,43,105,6,93), i -> i<0)
   positions(100..110, isPrime)
  Text
   To find the first or last index of an element satisfying the condition, see @TO position@. To return the 
   elements, rather than their indices, see @TO select@. The function @TO number@ counts the
   number of elements satisfying the condition.
  Example
   position((10,20,43,105,6), odd)  
   position((10,20,43,105,6), odd, Reverse => true)
   select((10,20,43,105,6), odd)
   number((10,20,43,105,6), odd)
 SeeAlso
  minPosition
  maxPosition
  number
  position
  select
  take
  "lists and sequences"
///
