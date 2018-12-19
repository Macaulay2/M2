-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc ///
 Key
  splice
  (splice, BasicList)
 Headline
  remove subsequences from a sequence or list
 Usage
  splice X
 Inputs
  X:BasicList
 Outputs
  Y:BasicList
   the new list resulting from replacing each element of {\tt X} that is a sequence
   with its elements
 Description
  Text
   Unlike the function @TO deepSplice@, which recursively flattens subsequences at all levels,
   {\tt splice} removes the outermost nested level of subsequences only.
  Example
   X = {(), (0, (1, 2, (3, 4))), (5, (6, 7)), 8, 9};
   splice X
   deepSplice X
  Text
   {\tt splice} does not alter elements that are lists, arrays, or anything other than
   sequences.
  Example  
   Z = {(), {0, {1, 2, (3, 4)}}, [5, [6, 7]], 8, 9};
   splice Z
  Text
   {\tt splice} works on sequences, too, and all other objects of @TO class@ BasicList.
   The output matches the class of the input.
  Example
   splice ((), (0, (1, 2, (3, 4))), (5, (6, 7)), 8, 9)
   splice [(), (0, (1, 2, (3, 4))), (5, (6, 7)), 8, 9]   
  Text
   Even if {\tt X} is a @TO MutableList@, {\tt splice} returns a new list rather than
   altering the definition of {\tt X}.
  Example
   M = new MutableList from X
   splice M
   M
 SeeAlso
  deepSplice
  flatten
  "lists and sequences"
///

