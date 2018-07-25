-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc ///
 Key
  deepSplice
  (deepSplice,BasicList)
 Headline
  remove all levels of subsequences from a sequence or list
 Usage
  deepSplice X
 Inputs
  X:BasicList
 Outputs
  Y:BasicList
   the new list resulting from replacing any element of {\tt X} that is a sequence
   with its elements, at every level of nesting
 Description
  Text
   Unlike the function @TO splice@, which removes a single nested level of subsequences,
   {\tt deepSplice} recursively flattens subsequences at all levels.
  Example
   X = {(), (0, (1, 2, (3, 4))), (5, (6, 7)), 8, 9};
   splice X
   deepSplice X
  Text
   {\tt deepSplice} does not alter elements that are lists, arrays, or anything other than
   sequences.
  Example  
   Z = {(), {0, {1, 2, (3, 4)}}, [5, [6, 7]], 8, 9};
   deepSplice Z
  Text
   {\tt deepSplice} works on sequences, too, and all other objects of @TO class@ BasicList.
   The output matches the class of the input.
  Example
   deepSplice ((), (0, (1, 2, (3, 4))), (5, (6, 7)), 8, 9)
  Text
   Even if {\tt X} is a @TO MutableList@, {\tt deepSplice} returns a new list rather than
   altering the definition of {\tt X}.
  Example
   M = new MutableList from X
   deepSplice M
   M
 SeeAlso
  flatten
  splice
  "lists and sequences"
///

