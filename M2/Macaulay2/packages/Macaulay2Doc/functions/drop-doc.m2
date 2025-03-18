-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc ///
 Key
  drop
  (drop, BasicList, ZZ)
  (drop, BasicList, List)
 Headline
  drop some elements from a list or sequence
 Usage
  drop(L, i)
  drop(L, {j,k})
 Inputs
  L: BasicList
  i: ZZ
  j: ZZ
  k: ZZ
 Outputs
  L2: BasicList
   the list or sequence obtained by dropping the first {\tt i} elements of {\tt L},
   (if {\tt i} positive), or the last {\tt i} elements of {\tt L} (if {\tt i} negative), or, if given the
   pair {\tt j,k}, the list or sequence obtained by dropping the elements of {\tt L} with indices {\tt j} through {\tt k}
 Description
  Example
   drop({a,b,c,d,e,f,g}, 3)
   drop({a,b,c,d,e,f,g}, -3)
   drop({a,b,c,d,e,f,g}, {1,3})
   drop({a,b,c,d,e,f,g}, {2,2})    
  Text
   The pair {\tt \{j,k\}} must be given with both entries non-negative, and $j\le k$. Otherwise the original list is returned.
  Example
   drop({a,b,c,d,e,f,g}, {3,1})
   drop({a,b,c,d,e,f,g}, {4,-1})
 SeeAlso
  take
  delete
  position
  positions
  select
  "lists and sequences"
///
