-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  join
 Headline
  join lists, sequences, and iterable objects
 Usage
  join(A, B, ...)
 Inputs
  A: BasicList
  B: BasicList
    or any instance of a class  with the @TO iterator@ method installed
 Outputs
  Z: {BasicList, Iterator}
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
  Text
   If the first argument is a list or sequence and any later arguments are
   instances of a class with the @TO iterator@ method installed, then these
   instances are converted to sequences and are then joined.
  Example
   join({1, 2, 3}, iterator {4, 5, 6})
  Text
   If the first argument is not a list or sequence, then an @TO Iterator@
   object is returned that iterates through each argument sequentially,
   provided that each argument is an iterable object.
  Example
   join(iterator {1, 2, 3}, {4, 5, 6})
   toList oo
 SeeAlso
  concatenate
  demark
  flatten
  mingle
  "List | List"
  "lists and sequences"
///
