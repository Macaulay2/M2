-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc ///
 Key
  take
  (take, BasicList, ZZ)
  (take, BasicList, List)
  (take, Thing, ZZ)
  (take, Thing, List)
 Headline
  take some elements from a list or sequence
 Usage
  take(L, i)
  take(L, {j,k})
  take(x, i)
 Inputs
  L: BasicList
  x: Thing
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
   The pair $\{j,k\}$ must be given with both entries non-negative, and $j\le k$. Otherwise an empty list is returned.
  Example
   take({a,b,c,d,e,f,g}, {3,1})
   take({a,b,c,d,e,f,g}, {4,-1})
  Text
   If @TT "x"@ is any object belonging to a class with the @TO iterator@ method
   installed, then a list containing the first $i$ (or the $(j+1)$th
   through $(k+1)$th) objects returned when @TO next@ is called on the output
   of @M2CODE "iterator x"@ is returned.  If fewer than $i$ (or $k+1$) objects
   are returned before the @TO StopIteration@ symbol is encountered, then the
   list will have shorter than expected length.
  Example
   take("Hello, world!", 5)
   take("Hello, world!", 20)
   take("Hello, world!", {7, 11})
 SeeAlso
  drop
  select
  position
  positions
  "lists and sequences"
///
