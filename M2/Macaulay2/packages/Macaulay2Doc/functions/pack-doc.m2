-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  pack
  (pack, BasicList, ZZ)
  (pack, ZZ, BasicList)
 Headline
  pack elements of a list into several shorter lists
 Usage
  pack(A, n)
  pack(n, A)
 Inputs
  A: BasicList
  n: ZZ
   how many elements of {\tt A} to put in each new list
 Outputs
  L: List
   a list of lists, with the elements of {\tt A} taken {\tt n} at a time.
 Description
  Text
   The commands {\tt pack(A, n)} and {\tt pack(n, A)} produce identical results.
  Example
   pack(a..l, 3)
   pack(3, a..l)
  Text
   If {\tt n} doesn't divide the length of {\tt A}, the last list will have fewer
   than {\tt n} elements.
  Example
   pack(a..m, 3)
  Text
   {\tt pack} and @TO mingle@ can be used together to take a transpose of lists
  Example
   pack(2, mingle(a..m, 0..12))
 SeeAlso
  mingle
  sort
  sublists
  take
  "lists and sequences"
///
