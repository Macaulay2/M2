-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  unique
  (unique, VisibleList)
  (unique, Hypertext)
 Headline
  eliminate duplicates from a list
 Usage
  unique(L)
 Inputs
  L:List
   or sequence
 Outputs
  M:List
   the elements of {\tt L} without duplicates
 Description
  Text
   The output list maintains the order of elements in {\tt L}.
  Example
   unique {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}
  Text
   Another way to list the unique elements of {\tt L} is by creating a
   set from {\tt L} and then listing its elements. This may be slightly
   faster than {\tt unique}, but forgets the ordering of {\tt L}.
  Example
   toList set {3,2,1,3,2,4,a,3,2,3,-2,1,2,4}
  Text
   To count occurrences of each element, use @TO tally@. To create
   a sorted list, see @TO sort@. For an overview of lists and sequences,
   see @TO"lists and sequences"@.
 SeeAlso 
  same
  sort
  set
  tally
  uniform
  "lists and sequences"
///
