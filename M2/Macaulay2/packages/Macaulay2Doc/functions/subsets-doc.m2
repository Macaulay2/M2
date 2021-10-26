-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  subsets
  (subsets, ZZ)
  (subsets, ZZ, ZZ)
  (subsets, List)
  (subsets, List, ZZ)
  (subsets, Sequence, ZZ)
  (subsets, Set)
  (subsets, Set, ZZ)
 Headline
  produce the subsets of a set or list
 Usage
  subsets(A)
  subsets(A, n)
 Inputs
  A: List
   , sequence, set or integer
  n: ZZ
   optional input to specify subsets of a particular size
 Outputs
  L: List
   of subsets (of size {\tt n} if given)
 Description
  Text
   If {\tt A} is an integer, {\tt subsets(A)} lists the subsets of {\tt \{0, 1, ..., A-1\}}.
  Example
   subsets(3)
   subsets(5, 3) 
  Text
   {\tt A} can be a list, sequence, or set. The elements need not be of the same type.
  Example
   subsets({"apple", "banana", {1,2,3}, 7.1}, 3)
  Text
   If a list contains repetitions, so will the subsets of that list. 
   Since a @TO Set@ has no repetitions, neither do its subsets. Also, 
   the subsets of a set will again be sets (while the subsets of a list are lists).
  Example
   subsets({"apple", "apple", "banana"})
   subsets(set{"apple", "apple", "banana"})
  Text
   The subsets of a Sequence are lists, not sequences. Also, a subset size {\bf must} be 
   specified when calling {\tt subsets} on a sequence.
 SeeAlso
  partition
  set
  sublists
  substring
  "lists and sequences"
///
