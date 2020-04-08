-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  join
 Headline
  join lists and sequences
 Usage
  join(A, B, ...)
 Inputs
  A: BasicList
  B: BasicList
 Outputs
  Z: BasicList
 Description
  Text
   {\tt join(A, B, ...)} joins the elements of the lists or sequences
   {\tt A, B, ...} into a single list or sequence. The inputs may belong
   to different classes; the class of the result will match the class of
   the first argument passed to {\tt join}.
  Example
   join( {1,2,3}, (4,5,6), (7,8,9) )
   join( (1,2,3), {4,5,6}, {7}, (8,9,10) )
  Text
   The operator @TO"|"@ is a convenient shorthand for joining two
   inputs of the same class. 
  Example
   {1,2,3} | {4,5,6}
   (1,2,3) | (4,5,6)
 SeeAlso
  concatenate
  demark
  flatten
  mingle
  "List | List"
  "lists and sequences"
///
