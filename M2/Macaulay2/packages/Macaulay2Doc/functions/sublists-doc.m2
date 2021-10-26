-- Author: Lily Silverstein

doc ///
 Key
  sublists
  (sublists, VisibleList, Function, Nothing)
  (sublists, VisibleList, Function, Function, Function)
  (sublists, VisibleList, Function)
  (sublists, VisibleList, Function, Function)
  (sublists, VisibleList, Function, Function, Nothing)
  (sublists, VisibleList, Function, Nothing, Function)
  (sublists, VisibleList, Function, Nothing, Nothing)
 Headline
  process interspersed subsequences of a visible list
 Usage
  sublists(L, f, g, h)
 Inputs
  L:VisibleList
  f:Function
   returning Boolean values
  g:Function
   which acts on VisibleLists, 
   applied to sublists of {\tt L} with {\tt f(i)} true for each {\tt i} in the sublist
  h:Function
   which acts on elements of {\tt L},
   applied to the elements {\tt i} of {\tt L} with {\tt f(i)} false
 Outputs
  :List
   the result obtained by applying {\tt g} to each of the 
   maximal nonempty sequences of consecutive elements with {\tt f} true,
   and applying {\tt h} to the other elements
 Description
  Text
   In the first example, consecutive odd elements are grouped
   into sublists, while each even element is negated.
  Example
   L = {1,2,3,5,7,8,10,12,13,17,18,20,21};
   sublists(L, odd, toList, minus)
  Text
   If {\tt g} or {\tt h} is omitted, the @TO identity@ function is
   used in its place. 
  Example
   sublists(L, odd, toList)
   sublists(L, odd)
  Text
   The sublists will belong to the same class as {\tt L}.
  Example
   L = (1,2,3,5,7,8,10,12,13,17,18,20,21);
   sublists(L, isPrime, , e -> 0)
  Text
   Note that {\tt g} acts on the {\em sublists}, not their elements.
  Example
   sublists(L, isPrime, plus, e -> 0)
  Text
   Because of the grouping of consecutive elements that return {\tt true} when input to {\tt f},
   {\tt sublists(L, f, g, h)} is NOT the same as applying {\tt g} to elements returning {\tt true},
   and applying {\tt h} to elements returning false. This could be achieved with a simple
   @TO "if"@-then-else loop, or with @TO apply@.
  Example
   a = for l in L list if isPrime l then l else -10*l
   b = apply(L, l -> if isPrime l then l else -10*l)
  Text
   On the other hand, if we want to group both "true" and "false" elements into sublists, we
   can achieve this with a second call to {\tt sublists}, selecting those elements not already
   grouped by the first {\tt sublists} command, {\em as long as the original list was not nested}.
  Example
   X = {1, 3, 5, 2, 4, 7, 1, 3, 4, 4, 5, 4, 7, 9, 13};
   sublists(sublists(X, odd), i -> not instance(i, List))
 SeeAlso
  apply
  partition
  positions
  select
  "lists and sequences"
///

