--- author: Lily Silverstein

doc ///
 Key
  compositions
  (compositions, ZZ)
  (compositions, ZZ, ZZ)
 Headline
  list the compositions of an integer
 Usage
  compositions(k, n)
 Inputs
  k:ZZ
   a nonnegative integer, the number of parts in each composition
  n:ZZ
   a nonnegative integer, the sum of each composition (if omitted, assumed to be also $k$)
 Outputs
  :List
   of all ordered lists of {\tt k} nonnegative integers 
   that sum to {\tt n}
 Description
  Example
   compositions(4, 2)
   compositions(2, 4)
  Text
   To find all unordered compositions, we can use @TO sort@ or 
   @TO rsort@ to put each composition into a standard form, then
   use the function @TO unique@ to remove duplicates.
  Example
   unique apply(compositions(4, 10), comp -> rsort comp)
  Text
   In the next example, we use @TO select@ to find all the compositions
   of 18 into 5 parts, with each part greater than or equal to 3.
  Example
   select(compositions(5, 18), comp -> all(comp, c -> c>=3))
  Text
   For partitions of {\tt n} into unordered, {\em strictly positive} parts, 
   use @TO partitions@ instead.
   
   If a negative integer is given for {\tt n}, 
   an empty list is returned. If a negative integer is given 
   for {\tt k}, it will cause an error.
 SeeAlso
  partitions
  subsets
///
