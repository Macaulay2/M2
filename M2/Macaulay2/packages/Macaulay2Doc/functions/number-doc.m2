-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  number
 Headline
  count how many elements of a list satisfy a condition
 Usage
  number(A, f)
 Inputs
  A:
   a list or sequence
  f:
   a boolean function
 Outputs
  c:
   an integer, the number of elements of {\tt A} that satisfy {\tt f}
 Description
  Example
   number(0..100, isPrime)
   number(0..100, odd)
   number(0..100, i -> i==17)
  Text
   To find the first or last index of an element satisfying the condition, see @TO position@. 
   For all indices that match the condition, see @TO positions@. To return the 
   elements, rather than their indices, see @TO select@. 
  Example
   position((10,20,43,105,6), odd)  
   positions((10,20,43,105,6), odd)
   select((10,20,43,105,6), odd)
 SeeAlso
  all
  any
  commonest
  position
  positions
  same
  select
  tally
  "lists and sequences"
///
