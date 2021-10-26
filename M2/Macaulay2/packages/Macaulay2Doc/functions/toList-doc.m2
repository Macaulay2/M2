-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  toList
  (toList, BasicList)
  (toList, Set)
  (toList, String)
 Headline
  create a list
 Usage
  toList A
 Inputs
  A:
   a @TO Set@, @TO String@, or a @TO BasicList@ such as a @TO Sequence@
 Outputs
  L:List
   a list whose elements are the elements of {\tt A}
 Description
  Example
   toList "foo"
   A = set(3,7,9,6)
   toList A
  Text
   The command {\tt toList 1..9} will throw an error, because {\tt toList}
   comes before @TO".."@ in Macaulay2's order of operations. To create a list
   from a range, use {\tt toList (1..9)} instead.
  Example
   toList (1..9)
  Text
   Converting between list types may change the order of the elements in
   unexpected ways.
  Example
   toList set {4,5,13}
 SeeAlso
  BasicList
  List
  toSequence
  "lists and sequences"
///
