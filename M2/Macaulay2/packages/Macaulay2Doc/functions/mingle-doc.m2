-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  mingle
  (mingle, BasicList)
 Headline
  mingle elements of several lists
 Usage
  mingle(L)
 Inputs
  L:BasicList
   a list of lists {\tt L=\{L1, L2, ..., Ln\}}
 Outputs
  M:List
   a new list mingling the elements of all lists in {\tt L}
 Description
  Text
   The output list {\tt M} takes the first element of each {\tt Li, i=1,...,n}, followed by
   the second element of {\tt Li, i=1,...,n}, and so forth.  
  Example
   mingle {{a1, a2, a3}, {b1, b2, b3}, {c1, c2, c3}}
  Text
   The lists can have different lengths. After a list is exhausted, it
   will be silently ignored.
  Example
   mingle {{a1, a2, a3, a4}, {b1, b2}, {c1}}
  Text
   To transpose a nested list (thinking of it as a matrix), try
   using {\tt mingle} with @TO pack@.
  Example
   pack(3, mingle ((a1, a2, a3), (b1, b2, b3), (c1, c2, c3)))
  Text
   Notice from the previous example that {\tt mingle} accepts sequences and
   other types of @TO BasicList@s as input, but the output will always be a 
   @TO List@. 
  Text
   Further examples:
  Example
   concatenate mingle( {"a","b","c"} , {",",","} )
   netList pack(3, mingle( (0..5), apply(6, i -> i^2), apply(6, i -> i^3)))
 SeeAlso
  apply
  insert
  join
  pack
  sort
  sublists
  "lists and sequences"
///
