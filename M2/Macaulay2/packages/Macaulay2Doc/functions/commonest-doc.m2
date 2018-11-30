-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  commonest
  (commonest, VisibleList)
  (commonest, Set)
  (commonest, Tally)
 Headline
  the most common elements of a list or tally
 Usage
  commonest A
 Inputs
  A:VisibleList
 Outputs
  L:List
   a list of the elements of {\tt A} with the most repetitions
 Description
  Text
   If a single element is the most common, a list of length one is the output.
  Example
   commonest {a,a,a,a,b,b,b,c,c,d,e}
  Text
   In the case of a tie, all commonest elements are returned.
  Example
   A = {a,a,a,a,b,b,b,b,c,c,c,c,d,e}; commonest A
  Text
   {\tt commonest} works on @TO Tally@s and @TO Set@s as well.
  Example
   T = tally A
   commonest T
   S = set A
   commonest S
  Text
   (Since every element of a set is unique, it is unclear why one would need {\tt commonest(Set)}.)
 SeeAlso
  number
  same
  set
  tally
  unique
  "lists and sequences"
///