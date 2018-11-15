-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  same
 Headline
  whether everything in a list is the same
 Usage
  same L
 Inputs
  L:
   a list
 Outputs
  b:
   a Boolean
 Description
  Example
   same {1, 1, 1, 1}
   same {1, 2, 1, 1}
  Text
   The comparison is done with "===", which is quick, but not always intuitive. Here is a 
   simple example of what can go wrong:
  Example
   R = QQ[x,y,z]; 
   L = {gcd{x,y}, x/x, 1}
   same L
  Text
   We can see the problem by asking {\tt Macaulay2} to display the class of each element of {\tt L}.
   (Or use the function @TO uniform@, which returns whether or not the elements of a list
       are all of the same class.)
  Example
   apply(L, class)
   uniform L
  Text
   The first {\tt 1} is an element of the ring {\tt R}, the second {\tt 1} is an
   element of the fraction field of {\tt R}, and the third {\tt 1} is an integer. Thus
   {\tt Macaulay2} thinks of these three elements as being pairwise unequal, with respect
   to the operator "===".
 SeeAlso
  commonest
  number
  set
  uniform
  unique
  "lists and sequences"
///
