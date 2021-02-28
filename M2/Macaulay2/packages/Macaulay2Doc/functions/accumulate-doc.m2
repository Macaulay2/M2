-- Status: rewritten July 2018
-- Author: Lily Silverstein

doc///
 Key
  accumulate
  (accumulate, Function, Thing, VisibleList)
  (accumulate, Function, VisibleList)
  (accumulate, VisibleList, Thing, Function)
  (accumulate, VisibleList, Function)
 Headline
  apply a binary operator repeatedly
 Usage
  accumulate(f, x, L)
  accumulate(f, L)
  accumulate(L, x, f)
  accumulate(L, f)
 Inputs
  f:Function
  x:Thing
  L:VisibleList
 Outputs
  M:List
 Description
  Text
   Suppose {\tt L=\{x0, x1, ..., xn\}}. Then for any binary operator {\tt f}, 
   {\tt accumulate(f, L)} returns the list {\tt \{f(x0, x1), f(f(x0, x1), x2), ...\} }. 
   In other words, the binary operator is applied
   to the first two elements of {\tt L}, then to that result along with the next unused element of
   {\tt L}, and so forth.
  Example
   accumulate(plus, {0,1,2,3,4,5})
   accumulate(concatenate, {a,b,c,d,e})
   accumulate((i,j) -> i|j|i, {"a","b","c","d","e"})
  Text
   If {\tt accumulate(f, x, L)} is called, the element {\tt x} is used as the first argument of the
   binary function {\tt f}. In other words, {\tt accumulate(f, \{x0, x1, \ldots, xn\})} is 
   equivalent to {\tt accumulate(f, x0, \{x1, \ldots, xn\})}.
  Example
   accumulate(plus, 0, {1,2,3,4,5})
   accumulate((x, y) -> x^y, 2, {3,2,1,2})
  Text
   The function {\tt accumulate(\{x_0, x_1, \ldots, x_n\}, f)} returns the
   list {\tt \{..., f(x_{n-2}, f(x_{n-1}, x_n)), f(x_{n-1}, x_n) \} }. That is, {\tt f} is applied
   to the last two elements of the list, and the result placed at the end of the output. Then 
   the accumulation proceeds backwards through the list. The optional argument {\tt x} in
   {\tt accumulate(L, x, f)} is used as the second argument in the first evaluation of
   {\tt f}. So {\tt accumulate(\{x_0, x_1, \ldots, x_{n-1}\}, x_n, f)} is equivalent
   to {\tt accumulate(\{x_0, x_1, \ldots, x_n\}, f)}.
  Example
   accumulate({a,b,c,d,e}, concatenate)
   accumulate({a,b,c,d}, e, concatenate)  
   accumulate({2,3,2,1}, 2, (x, y) -> x^y)
  Text
   The difference between {\tt fold} and @TO accumulate@ is that {\tt fold} returns the
   final result of all the nested evaluations of {\tt f}, while {\tt accumulate} lists 
   all the intermediate values as well.
  Example
   fold({2,3,2,1}, 2, (x,y) -> x^y)
 SeeAlso
  apply
  fold
  "lists and sequences"
///
