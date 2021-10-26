-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc ///
 Key
  take
  (take, BasicList, ZZ)
  (take, BasicList, List)
 Headline
  Take some elements from a list or sequence.
 Usage
  take(L, i)
  take(L, {j,k})
 Inputs
  L: BasicList
  i: ZZ
  j: ZZ
  k: ZZ
 Outputs
  L2: BasicList
   the list or sequence containing the first {\tt i} elements of {\tt L},
   (if {\tt i} positive), or the last {\tt i} elements of {\tt L} (if {\tt i} negative), or, if given the
   pair {\tt j,k}, the list or sequence containing the elements of {\tt L} with indices {\tt j} through {\tt k}
 Description
  Example
   take({a,b,c,d,e,f,g}, 3)
   take({a,b,c,d,e,f,g}, -3)
   take({a,b,c,d,e,f,g}, {1,3})
   take({a,b,c,d,e,f,g}, {2,2})    
  Text
   The pair {\tt \{j,k\}} must be given with both entries non-negative, and $j\le k$. Otherwise an empty list is returned.
  Example
   take({a,b,c,d,e,f,g}, {3,1})
   take({a,b,c,d,e,f,g}, {4,-1})
 SeeAlso
  drop
  select
  position
  positions
  "lists and sequences"
///
