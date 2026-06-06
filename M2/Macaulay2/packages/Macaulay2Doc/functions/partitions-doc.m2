--- author(s): Lily Silverstein

doc ///
 Key
  partitions
  (partitions, ZZ)
  (partitions, ZZ, ZZ)
 Headline
  list the partitions of an integer
 Usage
  partitions(n)
  partitions(n, k)
 Inputs
  n:ZZ
   a nonnegative integer
  k:ZZ
   a nonnegative integer, the maximum size of each part
 Outputs
  :List
   of all partitions of {\tt n}
 Description
  Example
   partitions(4)
   partitions(4, 2)
  Text
   Each partition is a basic list of type @TO Partition@.
  Example
   p = new Partition from {2,2,1}
   member(p, partitions(5,2))
   member(p, partitions(5,1))
   conjugate(p)
  Text
   For ordered lists of exactly {\tt k} nonnegative integers
   that sum to {\tt n}, use @TO compositions@ instead. In the
   following example, we create Partition objects from the
   output of {\tt compositions} to find all the partitions of 10, 
   of length exactly 4, with no part greater than 5.
  Example
   --get list of unordered compositions without duplicates
   A = unique apply(compositions(4, 10), comp -> rsort comp);
   --select those with every part positive and no more than 5
   B = select(A, a -> all(a, i -> 0<i and i<6));
   --create Partition object from each one
   apply(B, b -> new Partition from b)
  Text    
   If {\tt partitions n} is called on a negative integer {\tt n}, 
   an empty list is returned. If a negative integer is given 
   for {\tt k}, it will cause an error.
 SeeAlso
  compositions
  subsets
 Subnodes
   Partition
   (symbol _, Partition, ZZ)
   (conjugate, Partition)
///

document {
    Key => Partition,
    Headline => "a type of list representing a partition of a natural number",
    SeeAlso => { partitions, (conjugate,Partition) } }
document {
    Key => (conjugate, Partition),
    Headline => "conjugate a partition",
    Usage => "conjugate p", Inputs => {"p"}, Outputs => {{"the conjugate of ", TT "p" }},
    EXAMPLE lines ///
	  partitions 4
	  conjugate \ oo
    ///
}
